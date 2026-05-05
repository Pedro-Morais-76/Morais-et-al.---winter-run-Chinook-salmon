

# MAKE PLOT
# This script is to make a plot of size distributions of fish at FW exit

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

#read in data
df = read.csv('outputs/oto_sr8786_dat_with_brood_year.csv')%>%
  distinct(Sample_ID, FWExit_dist, Brood_year)

hist(df$Brood_year)

#make plot excluding the two years with very low sample sizes
ggplot(subset(df, !Brood_year %in% c(2011, 2015)), 
       aes(x = FWExit_dist, color = factor(Brood_year))) +
  geom_density(size = 0.8, key_glyph = "path") +
  theme_classic(base_size = 15) +
  labs(x = "Otolith radius at freshwater exit (\u03bcm)", y = "Density", color = "Brood year") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5, shape = NA)))

ggsave('figures/FigS4_fw_exit_size.jpg', width = 15, height = 10, units = 'cm')
