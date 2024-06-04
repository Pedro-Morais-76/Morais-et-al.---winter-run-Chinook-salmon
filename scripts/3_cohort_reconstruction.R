# This script is to assign the most likely age (i.e. brood year) to every fish

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, gridExtra, XLConnect, lubridate)

# Age reads of winter run hatchery and wild fish from the same years as our samples (from Santa Rosa CDFW)
ages = read.csv("data/winter_run_2005-2018_scale_reads.csv") %>%  rename(FL=Fork.Length..mm.)

# Assign fish a final age using CWT known ages where available, otherwise scale est
ages$final_age = NA
ages$final_age[!ages$Readage == 0 & !is.na(ages$Readage)] = ages$Readage[!ages$Readage == 0 & !is.na(ages$Readage)] 
ages$final_age[!ages$Age == 0 & !is.na(ages$Age)] = ages$Age[!ages$Age == 0 & !is.na(ages$Age)] 

# calculate size cutoffs for different aged fish (based both on CWT known and Scale ests)
ages_final = ages %>%  filter(!is.na(final_age),
         # Exclude abnormally sized fish & the one single 5 year old
         FL<2000 & FL>300,
         final_age < 5)  

# 99% CI of FL of all fish (wild and hatchery, males and females) for 3 yr olds 
# We included hatchery fish to increase sample size
age3 = subset(ages_final, final_age==3)
(age3LW = as.numeric(quantile(ages_final$FL, c(.01))))
(age3UP = as.numeric(quantile(ages_final$FL, c(.99))))
cutoffs <- data.frame(Age=c('ALL_3_LW','ALL_3_UP'), cutoff=c(age3LW, age3UP))

#99% CI of FL by age & sex
##---females
Fage3 = subset(ages_final, final_age==3 & Sex=="F")
(Fage3LW = as.numeric(quantile(Fage3$FL, c(.01))))
(Fage3UP = as.numeric(quantile(Fage3$FL, c(.99))))

##---males
Mage3 = subset(ages_final, final_age==3 & Sex=="M")
(Mage3LW = as.numeric(quantile(Mage3$FL, c(.01))))
(Mage3UP = as.numeric(quantile(Mage3$FL, c(.99))))

# Make into two dfs
Fcutoffs <- data.frame(Age=c('F_3_LW','F_3_UP'), cutoff=c(Fage3LW, Fage3UP))
Mcutoffs <- data.frame(Age=c('M_3_LW','M_3_UP'), cutoff=c(Mage3LW, Mage3UP))

##-------------- plot females -------------------------------------------
F_plot = ggplot(subset(ages_final, Sex=="F"), aes(x=FL, fill=factor(final_age), color=factor(final_age))) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 15) + xlim(400,1100) +
  geom_vline(data = Fcutoffs, aes(xintercept=cutoff),linetype="dashed", linewidth = 0.8)+
  labs(y="No. fish", x = "Fork length (mm)", 
       fill = "Age (scale read)",
       color = "Age (scale read)") + 
  scale_fill_manual(values = c("tomato", "turquoise4", "orange3"))+
  scale_color_manual(values = c("tomato", "turquoise4", "orange3"))+
  theme_bw(); F_plot

##-------------- plot males -------------------------------------------
M_plot = ggplot(subset(ages_final, Sex=="M"), aes(x=FL, fill=factor(final_age), 
                                                  fill=factor(final_age), color = factor(final_age))) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 15) + xlim(400,1100) +
  geom_vline(data = Mcutoffs, aes(xintercept=cutoff),linetype="dashed", linewidth = 0.8)+
  scale_fill_manual(values = c("tomato", "turquoise4", "orange3"))+
  scale_color_manual(values = c("tomato", "turquoise4", "orange3"))+
  labs(y="No. fish", x = "Fork length (mm)", 
       fill = "Age (scale read)",
       color = "Age (scale read)") + 
  theme_bw(); M_plot


# combine plots for figure
x = cowplot::plot_grid(F_plot, M_plot, nrow = 2, labels ="AUTO")

ggsave('figures/FigS1_FL_distributions.jpg', x, dpi=300, width=18, height = 24, units = "cm")


###############################################################################################

## Assign ages to our samples

# Read in Sr8786 data (including scale reads where available)
wr_megafile = read.csv('outputs/oto_sr8786_dat_with_assignments.csv') [,-c(1:2)]

# Create a final age column to add ages into
wr_megafile$final_age = NA

# Use scale read age if available (excl zeros - those represent unreadable scales)
wr_megafile$final_age[!wr_megafile$Readage == 0 & !is.na(wr_megafile$Readage)] = 
  wr_megafile$Readage[!wr_megafile$Readage == 0 & !is.na(wr_megafile$Readage)] 

##----------------------------------------------------------------

# For any unaged fish, use FL and sex to conservatively assign 2 or 4 year olds
# Rule = Assume 3 then apply these rules where FL and sex is known.
# 2 = if FL < 0.005th quantile for FLs of 'known' 3 yr olds, 4 = if FL > 99.5th quantile for FLs of known 3 yr olds)

# females 
wr_megafile$final_age[wr_megafile$fork_length < Fage3LW & wr_megafile$sex=="Female" & is.na(wr_megafile$final_age)] = 2
wr_megafile$final_age[wr_megafile$fork_length > Fage3UP & wr_megafile$sex=="Female" & is.na(wr_megafile$final_age)] = 4

# males 
wr_megafile$final_age[wr_megafile$fork_length < Mage3LW  & wr_megafile$sex=="Male" & is.na(wr_megafile$final_age)] = 2
wr_megafile$final_age[wr_megafile$fork_length > Mage3UP & wr_megafile$sex=="Male" & is.na(wr_megafile$final_age)] = 4

# males and unknown sex (use 99% CI for all samples combined - the tail on the wild male 3 yr olds seems anomalously long - i.e. overconservative)
wr_megafile$final_age[wr_megafile$fork_length < age3LW  & wr_megafile$sex=="Unknown" & is.na(wr_megafile$final_age)] = 2
wr_megafile$final_age[wr_megafile$fork_length > age3UP & wr_megafile$sex=="Unknown" & is.na(wr_megafile$final_age)] = 4

# Assume all remaining unaged fish with no sex OR FL data are 3 (i.e. note that these include all the 2007-08 fish - we dont have agency IDs or FL data)
wr_megafile$final_age[is.na(wr_megafile$final_age)] = 3

# Assign a brood year based on final assigned age
wr_megafile$Brood_year = wr_megafile$Escap_yr - wr_megafile$final_age

# Export df
write.csv(wr_megafile, 'outputs/oto_sr8786_dat_with_brood_year.csv')