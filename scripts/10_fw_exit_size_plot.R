

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
       aes(x = FWExit_dist, fill = factor(Brood_year))) +
  geom_density(alpha = .2, size = 0.5) +
  theme_classic(base_size = 18) +
  labs(x = "Otolith radius at freshwater exit (um)", y = "Density", fill = "Brood year")

ggsave('figures/FigS4_fw_exit_size.jpg', width = 15, height = 10, units = 'cm')
