# This script is to estimate proportion of FW growth in terms of mass assimilated

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, forcats, RColorBrewer, wesanderson, tidyverse, lubridate)

# Read in otolith Sr8786 data
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_rearing_types.csv')[,-1]


# -------- Reconstructing total weight from otolith distance (radius) ------------
# read in fall run juvenile salmon otolith radius (OR) and TW data. OR measured on dorsal axis
or_tw = read.csv('data/juv_fall_run_oto_radius_total_weight.csv') %>%
  filter(!(OR<300 & TW>7)) #exclude one clear bad data

# strong relationship
ggplot(or_tw, aes(y=TW, x=OR))+geom_point()+geom_smooth()
ggplot(or_tw, aes(y=log(TW), x=log(OR)))+geom_point()+geom_smooth(method="lm")

#weight range of calibration fish
min(or_tw$TW); max(or_tw$TW)

# Fit linear regression
mod = lm(log(TW) ~ log(OR), data = or_tw)
summary(mod) # adj r2=.91
plot(resid(mod))
as.vector(coef(mod)[1])*3

# Estimating total otolith distance between exog and FW exit spent in each habitats per fish (i.e. to calculate % FW growth)

# order by fish and oto distance, then calculate distance between spots and exclude all ocean spots
growth_dist = wr_megafile %>%
  #reconstruct mass using otolith distance using equation above
  mutate(mass = exp((as.vector(coef(mod)[2])*(log(Distance_um)))+as.vector(coef(mod)[1]))) %>% 
  arrange(Sample_ID, mass) %>%
  group_by(Sample_ID) %>%
  mutate(mass_change = lead(mass, 1)-mass)%>%
  filter(!is.na(Habitat)) %>% # excludes core (maternal) and ocean values
  select(Sample_ID, Distance_um, mass_change, Sr8786_norm, SE2, SrV2, Habitat, Escap_yr, Brood_year, fork_length)

# what is the mean mass change per spot? 1.3g
(mean_change = mean(growth_dist$mass_change, na.rm=T))

# Assume that any spots with NA growth accrued = mean mass change (only 1 observation)
growth_dist$mass_change[is.na(growth_dist$mass_change)] = mean_change

#summing total growth in FW by fish
growth_dist_tot = growth_dist %>%
  group_by(Sample_ID) %>%
  summarize(tot_mass_change = sum(mass_change, na.rm=T))

#summing growth in each FW habitat by fish & calculating proportion of total FW growth
by_fish_prop_dist = growth_dist %>%
  group_by(Sample_ID, Habitat, Escap_yr, Brood_year, fork_length) %>%
  summarize(mass_change_sum = sum(mass_change, na.rm=T)) %>%
  left_join(., growth_dist_tot) %>%
  mutate(prop_fw_growth = mass_change_sum/tot_mass_change)

# Add column of just percent Sac so we can order barplot using this
percent_sac_df = by_fish_prop_dist %>%
  filter(Habitat == "SAC") %>%
  ungroup() %>%
  select(Sample_ID, prop_sac = prop_fw_growth)

# Add to main df
by_fish_prop_dist = left_join(by_fish_prop_dist, percent_sac_df)

# -------- CALCULATE MEAN PROP FW GROWTH BY HABITAT AND BROOD YEAR-----

# Mean proportion of growth achieved in each habitats per BY
# this pivot wider step allows us to make sure sure each habitat is represented for every fish (even if zero oto dist)
by_fish_prop_dist_wide = by_fish_prop_dist %>%
  select(Sample_ID, Brood_year, Habitat, prop_fw_growth) %>%
  pivot_wider(names_from = Habitat, values_from = prop_fw_growth) %>% 
  replace(is.na(.), 0) ; by_fish_prop_dist_wide # need to replace NAs with zero so mean values includes the zeros too

write.csv(by_fish_prop_dist_wide,'outputs/by_fish_prop_growth_wide_format.csv')

# Go back to long format but now with zeros where true zero growth there
by_fish_prop_dist_long = by_fish_prop_dist_wide %>%
  pivot_longer(cols = c('SAC', 'LAS', 'DEL', 'AME', 'Unassigned')) %>%
  rename(Habitat = name, prop_fw_growth = value) 

# Means by habitat and year
by_year_prop_stats = by_fish_prop_dist_long %>%
  group_by(Brood_year, Habitat) %>%
  summarize(mean_prop_fw_growth = mean(prop_fw_growth),
            min_prop_fw_growth = min(prop_fw_growth),
            max_prop_fw_growth = max(prop_fw_growth),
            se_prop_fw_growth = sd(prop_fw_growth)/sqrt(length(prop_fw_growth)),
            n_fish = n()); by_year_prop_stats

# Export df of summary stats
write.csv(by_year_prop_stats, 'outputs/summary_stats_fw_growth.csv')

# # write to share with corey (this what was used to calculate the annual mean values)
# write.csv(by_fish_prop_dist_long, 'Outputs/by_fish_prop_growth.csv')

# Export df
write.csv(by_fish_prop_dist, 'outputs/prop_fw_growth_by_fish.csv')


## ----- PLOT HABITAT USE BY FISH (GIVES A SENSE OF LH DIVERSITY & THE N SAMPLES PER BROOD YEAR)

# reorder Habitat levels so upstream to downstream for the non-natal habitats. 
by_fish_prop_dist$Habitat <- factor(by_fish_prop_dist$Habitat,
                                    levels = c("Unassigned", "DEL", "AME","LAS",  "SAC")) 

## plot colors
cols = c("grey20","turquoise4","turquoise","tomato2", "grey40")

# PLOT IT (excl 2011 and 2015 bc such low sample sizes)
by_fish_prop_dist %>%
  filter(!Brood_year==2011 & !Brood_year==2015) %>%
  arrange(desc(prop_sac)) %>% # arrange by prop_sac so bars show up in that order 
  ungroup() %>%
  mutate(Sample_ID = factor(Sample_ID, levels=unique(Sample_ID))) %>%
  ggplot(aes(x = Sample_ID, y = mass_change_sum, fill= Habitat)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme_bw() +
  theme(text = element_text(size=18)) +
  xlab('Brood Year') + ylab('Proportion of freshwater growth')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 10, angle = 90, face="bold"),
        panel.spacing.x=unit(0.2, "lines"),
        legend.position = 'top')+
  scale_fill_manual(values = cols, 
       labels=c("Habitat X","Feather River/Delta", "American River", "Lassen Tributaries", "Sacramento River")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_grid(~Brood_year,scales="free",space="free", switch='x')

ggsave('figures/Fig4_prop_fw_growth_by_fish.jpg', dpi = 800, height = 16, width = 29, units = 'cm')