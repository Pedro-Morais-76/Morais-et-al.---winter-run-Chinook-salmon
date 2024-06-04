# This script is to estimate proportion of FW growth (in terms of otolith distance - ie. FL, not mass)  
# in each habitat by fish and brood year, and to assign each fish a 'rearing category' as per Phillis et al 2018

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, forcats, RColorBrewer, wesanderson, tidyverse, lubridate)

# Read in otolith Sr8786 data
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_brood_year.csv')[,-1]

# -------- FW HABITAT USE BY FISH

# Estimating total otolith distance between exog and FW exit spent in each habitats per fish (i.e. to calculate % FW growth)

# order by fish and oto distance, then calculate distance between spots and exclude all ocean spots
growth_dist = wr_megafile %>%
  arrange(Sample_ID, Distance_um) %>%
  group_by(Sample_ID) %>%
  mutate(Spot_width = lead(Distance_um, 1)-Distance_um)%>%
  filter(!is.na(Habitat)) %>% # excludes core of otolith (until exog) and after fish has gone to ocean
  select(Sample_ID, Distance_um, Spot_width, Sr8786_norm, SE2, SrV2, Habitat, Escap_yr, Brood_year, fork_length)

#summing otolith distance increase within each FW habitat by fish
growth_dist_sum = growth_dist %>%
  group_by(Sample_ID, Habitat, Escap_yr, Brood_year, fork_length) %>%
  summarize(oto_dist = sum(Spot_width, na.rm=T)) 


# ------- REARING TYPE (ASSIGNMENTS (1 type per fish after Phillis paper)

# Changed minimum otolith distance to at least 1 spot equivalent (40um) for all habitats except delta as all fish 
# must pass through it. Made min distance for the Delta (3x40um spots = 120um so same as Emily Chen but a little shorter than in Phillis (3x60um spots =180um)

las_rearer = growth_dist_sum %>%
  filter(Habitat=="LAS" & oto_dist >= 40) %>%
  mutate(las_dist = oto_dist)

ame_rearer = growth_dist_sum %>%
  filter(Habitat=="AME" & oto_dist >= 40) %>%
  mutate(ame_dist = oto_dist) 

x_rearer = growth_dist_sum %>%
  filter(Habitat=="Unassigned" & oto_dist >= 40) %>%
  mutate(x_dist = oto_dist)

del_rearer = growth_dist_sum %>%
  filter(Habitat=="DEL" & oto_dist >= 120) %>% 
  mutate(del_dist = oto_dist)

# Combine into a single df
rearing_type_df = growth_dist_sum %>% ungroup() %>% 
  distinct(Sample_ID, Brood_year, fork_length) %>%
  left_join(., las_rearer[c('Sample_ID','las_dist')]) %>%
  left_join(., ame_rearer[c('Sample_ID','ame_dist')]) %>%
  left_join(., x_rearer[c('Sample_ID','x_dist')]) %>%
  left_join(., del_rearer[c('Sample_ID','del_dist')]) %>%
  replace(is.na(.), 0) 

# FINAL REARING TYPE
rearing_type_df$rearing_type ="SAC" # our "null" hypothesis - if hatchery fish are in here this may not be a valid assumption...
rearing_type_df$rearing_type[rearing_type_df$del_dist > 0] = "DEL" #start with most likely based on migratory pathway
rearing_type_df$rearing_type[rearing_type_df$x_dist > 0] = "X" # any unassigned movements?
rearing_type_df$rearing_type[rearing_type_df$ame_dist > rearing_type_df$las_dist] = "AME" #This will overwrite any DEL or X assignments if same fish and pick between AME and LAS fish based on which habitat they grew more in
rearing_type_df$rearing_type[rearing_type_df$las_dist > rearing_type_df$ame_dist] = "LAS" #This will overwrite any AME, DEL or X assignments if same fish and pick between AME and LAS fish based on which habitat they grew more in

write.csv(rearing_type_df, 'outputs/rearing_type_by_fish.csv')


# SUMMARY STATS - REARING TYPES -------------------------------------------------

# Proportion of rearing types by habitat and BY
rearing_type_summary <- rearing_type_df %>%
  group_by(Brood_year, rearing_type) %>%
  summarize(n = n()) %>%
  group_by(Brood_year)%>%
  mutate(tot = sum(n)) %>%
  mutate(freq = n / tot) ; rearing_type_summary

write.csv(rearing_type_summary, 'outputs/rearing_type_by_BY.csv')

# Average fraction of fish that reared in each habitat among years
(mean_non_nat <- rearing_type_summary %>%
  filter(!Brood_year %in% c('2011', '2015')) %>%
  group_by(rearing_type) %>%
  summarize(mean_freq = mean(freq)))

# Average fraction of fish that reared non-natally each BY
prop_non_sac_rearing_type = rearing_type_summary %>%
  filter(rearing_type == "SAC") %>%
  mutate(freq_non_natal_rearers = 1-freq,
         n_non_natal = tot-n) %>%
  select(-rearing_type, -n); prop_non_sac_rearing_type

# Average across all years (after excluding 2011 and 2015 as v low sample sizes) was 39%
mean(prop_non_sac_rearing_type$freq_non_natal_rearers
     [!prop_non_sac_rearing_type$Brood_year %in% c('2011', '2015')])

# N fish per BY 
sample_sizes <- rearing_type_df %>%
  group_by(Brood_year) %>%
  summarize(N_fish = n()); sample_sizes 


## PLOT 1 - REARING TYPE PROPORTIONS -----------------------------------------------------------

# reorder levels for plot
rearing_type_summary$rearing_type <- factor(rearing_type_summary$rearing_type, levels = c("X", "LAS", "DEL", "AME", "SAC")) 

# Fig. 1 plot of rearing types to continue Phillis et al. (2018) time series
habcolor <- function(x){
  color = RColorBrewer::brewer.pal(8, 'RdBu')[x]
  return(color) }

sacc <- habcolor(7)
amec <- habcolor(1)
lasc <- habcolor(3)
xc <- habcolor(4)
delc <- habcolor(2)

ggplot(subset(rearing_type_summary, !Brood_year==2011 & !Brood_year==2015), aes(factor(Brood_year), freq, fill=rearing_type)) +
  geom_bar(colour="black", stat="identity")+
  labs( y= "Proportion of sample", x= "Brood Year", fill = "Rearing location")+
  theme_classic() + 
  scale_fill_manual(values = c(xc, lasc,delc,amec,sacc), labels=c("Habitat X", "Lassen Tributaries", "Delta/Feather River", "American River", "Sacramento River")) +
  theme(text = element_text(size=25),
        legend.position = "none") 

ggsave('figures/Fig1_rearing_type_props.jpg', width = 6.5, height = 9, units = "in", dpi = 300)

## PLOT 2 - REARING TYPE SPAGHETTI PLOTS -----------------------------------------------------------

##---------------------------------------------------
# Figure of all profiles for paper (SPAGHETTI PLOT)

wr_megafile2 = left_join(wr_megafile, rearing_type_df[c('Sample_ID', 'rearing_type')])

#wr_megafile2$rearing_type <- factor(wr_megafile2$rearing_type,levels = c("SAC", "LAS", "DEL", "AME", "X")) 

sample_sizes_rearing_types = filter(wr_megafile2, !rearing_type=="X") %>%
  distinct(Sample_ID, rearing_type) %>%
  group_by(rearing_type)%>%
  summarize(n_fish=n())

sac = ggplot(filter(wr_megafile2, rearing_type=="SAC"), aes(y=Sr8786_norm, x=Distance_um, group=Sample_ID)) +
  annotate("rect", xmin = 0, xmax =1250, ymin = 0.70467, ymax = 0.7061, alpha = .2, fill="turquoise4")+ #Sacramento
  geom_line(alpha = .3, size = .6)+
  labs(x="Distance (um)", y="Otolith Sr87/86", color = "Rearing type")+
  geom_hline(yintercept = 0.70918, alpha = .8, linetype = "dashed")+ #Ocean
  annotate("text", x=1200, y=.7057, label="SAC", size = 4, hjust = 1,  angle = 90)+
  annotate("text", x=1250, y=.7094, label="Ocean",fontface="italic", size = 4, hjust=1) + 
  annotate("text", x=40, y=.710, label=paste('n =',sample_sizes_rearing_types$n_fish[sample_sizes_rearing_types$rearing_type=="SAC"]), 
           size = 5, hjust = 0)+
  theme_classic(base_size = 16)+
  theme(legend.position = "top",
        axis.text.x=element_blank(),
        axis.title.x=element_blank())+
  guides(col = guide_legend(nrow = 2))+
  geom_hline(yintercept=0.70918, linetype="dashed")+
  labs(y = expression(text='Otolith '^87*'Sr/'^86*'Sr')) +
  scale_x_continuous(limits=c(0,1280), expand = c(0, 0))+
  scale_y_continuous(limits=c(0.7037,.7102), expand = c(0, 0)); sac

las = ggplot(filter(wr_megafile2, rearing_type=="LAS"), aes(y=Sr8786_norm, x=Distance_um, group=Sample_ID)) +
  annotate("rect", xmin = 0, xmax =1250, ymin = 0.7037, ymax = 0.70467, alpha = .2, fill="turquoise4")+ #Sacramento
  geom_line(alpha = .3, size = .6)+
  labs(x="Distance (um)", y="Otolith Sr87/86")+
  geom_hline(yintercept = 0.70918, alpha = .8, linetype = "dashed")+ #Ocean
  annotate("text", x=1200, y=.7045, label="LAS", size = 4, hjust = 1,  angle = 90)+
  annotate("text", x=1250, y=.7094, label="Ocean",fontface="italic", size = 4, hjust=1) + 
  annotate("text", x=40, y=.710, label=paste('n =',sample_sizes_rearing_types$n_fish[sample_sizes_rearing_types$rearing_type=="LAS"]), 
           size = 5, hjust = 0)+theme_classic(base_size = 16)+
  theme(legend.position = "top",
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank())+
  guides(col = guide_legend(nrow = 2))+
  geom_hline(yintercept=0.70918, linetype="dashed")+
  labs(y = "")+ 
  scale_x_continuous(limits=c(0,1280), expand = c(0, 0))+
  scale_y_continuous(limits=c(0.7037,.7102), expand = c(0, 0)); las#+

fea = ggplot(filter(wr_megafile2, rearing_type=="DEL"), aes(y=Sr8786_norm, x=Distance_um, group=Sample_ID)) +
  annotate("rect", xmin = 0, xmax =1250, ymin = 0.7061, ymax = 0.70785, alpha = .2, fill="turquoise4")+ #Sacramento
  geom_line(alpha = .3, size = .6)+
  labs(x="Distance (um)", y="Otolith Sr87/86")+
  geom_hline(yintercept = 0.70918, alpha = .8, linetype = "dashed")+ #Ocean
  annotate("text", x=1200, y=.70786, label="FEA/DEL", size = 4, hjust = 1,  angle = 90)+
  annotate("text", x=1250, y=.7094, label="Ocean",fontface="italic", size = 4, hjust=1) + 
  annotate("text", x=40, y=.710, label=paste('n =',sample_sizes_rearing_types$n_fish[sample_sizes_rearing_types$rearing_type=="DEL"]), 
           size = 5, hjust = 0)+theme_classic(base_size = 16)+
  theme_classic(base_size = 16)+
  theme(legend.position = "top",
        axis.text.y=element_blank())+
  guides(col = guide_legend(nrow = 2))+
  geom_hline(yintercept=0.70918, linetype="dashed")+
  labs(y = "", 
       x = "Distance from core (µm)")+
  scale_x_continuous(limits=c(0,1280), expand = c(0, 0))+
  scale_y_continuous(limits=c(0.7037,.7102), expand = c(0, 0)); fea#+

ame = ggplot(filter(wr_megafile2, rearing_type=="AME"), aes(y=Sr8786_norm, x=Distance_um, group=Sample_ID)) +
  annotate("rect", xmin = 0, xmax =1250, ymin = 0.70785, ymax = 0.7102, alpha = .2, fill="turquoise4")+ #Sacramento
  geom_line(alpha = .3, size = .6)+
  labs(x="Distance (um)", y="Otolith Sr87/86")+
  geom_hline(yintercept = 0.70918, alpha = .8, linetype = "dashed")+ #Ocean
  annotate("text", x=1200, y=.709, label="AME", size = 4, hjust = 1,  angle = 90)+
  annotate("text", x=1250, y=.7094, label="Ocean",fontface="italic", size = 4, hjust=1) + 
  annotate("text", x=40, y=.7099, label=paste('n =',sample_sizes_rearing_types$n_fish[sample_sizes_rearing_types$rearing_type=="AME"]), 
           size = 5, hjust = 0)+theme_classic(base_size = 16)+
  theme_classic(base_size = 16)+
  theme(legend.position = "top")+
  guides(col = guide_legend(nrow = 2))+
  geom_hline(yintercept=0.70918, linetype="dashed")+
  labs(y = expression(text='Otolith '^87*'Sr/'^86*'Sr'), 
       x = "Distance from core (µm)")+
  scale_x_continuous(limits=c(0,1280), expand = c(0, 0))+
  scale_y_continuous(limits=c(0.7037,.7102), expand = c(0, 0));  ame

all=cowplot::plot_grid(sac, las, ame, fea, ncol=2, labels="AUTO", rel_widths = c(1.12,1)); all

ggsave('figures/Fig3_rearing_type_spag_plots.tiff', dpi = 320, height = 17, width = 22, units = 'cm')

# write df with rearing type
write.csv(wr_megafile2, 'outputs/oto_sr8786_dat_with_rearing_types.csv')
