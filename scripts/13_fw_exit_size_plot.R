#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script generates a density plot showing the distribution of freshwater exit distances for different brood years.
#' 
#' @details
#' **Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' This analysis visualizes the size distributions of fish at freshwater exit, excluding years with low sample sizes (2011, 2015).
#' 
#' **Authors:** 
#' Pedro Morais1,*, Anna Sturrock2,3+, Corey C. Phillis4, George Whitman2, 
#' Stephanie M. Carlson1, Rachel C. Johnson2,5
#' 
#' **Affiliations:**
#' 1 Department of Environmental Science, Policy, and Management, Mulford Hall, 
#'    University of California, Berkeley, Berkeley, CA 94720, USA.
#' 2 Center for Watershed Sciences, University of California, Davis, 1 Shields Ave, 
#'    Davis, CA 95616, USA.
#' 3 School of Life Sciences University of Essex Wivenhoe Park, 
#'    Colchester CO4 3SQ, UK
#' 4 Metropolitan Water District of Southern California, 1121 L Street, 
#'    Suite 900, Sacramento, CA 95814, USA.
#' 5 National Marine Fisheries Service, Southwest Fisheries Science Center, 
#'    Santa Cruz, CA 95060, USA.
#' 
#' + Equal contribution as the first author
#' * Current address: University of Texas Marine Science Institute, 
#'   750 Channel View Dr., Port Aransas, TX 78373, USA.
#' 
#' **Correspondence:** 
#' Pedro Morais; Email: pedro.morais@austin.utexas.edu
#' 
#' **Credits:**
#' This script was originally created by Anna Sturrock and revised by Pedro Morais.
#'


# ------------------------------------------------------------------------------
# 1. Setup and Environment
# ------------------------------------------------------------------------------

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

# ------------------------------------------------------------------------------
# 2. Data Loading and Preparation
# ------------------------------------------------------------------------------

#read in data
df = read.csv('outputs/oto_sr8786_dat_with_brood_year.csv')%>%
  distinct(Sample_ID, FWExit_dist, Brood_year)

hist(df$Brood_year)

# ------------------------------------------------------------------------------
# 3. Visualization: Freshwater Exit Size Distributions
# ------------------------------------------------------------------------------

#make plot excluding the two years with very low sample sizes
ggplot(subset(df, !Brood_year %in% c(2011, 2015)), 
       aes(x = FWExit_dist, color = factor(Brood_year))) +
  geom_density(size = 0.8, key_glyph = "path") +
  theme_classic(base_size = 15) +
  labs(x = "Otolith radius at freshwater exit (\u03bcm)", y = "Density", color = "Brood year") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5, shape = NA)))

ggsave('figures/FigS4_fw_exit_size.jpg', width = 15, height = 10, units = 'cm')
