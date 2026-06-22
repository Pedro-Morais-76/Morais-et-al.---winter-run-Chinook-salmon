#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script performs cohort reconstruction by assigning the most likely 
#' age (i.e., brood year) to every fish in the dataset.
#' 
#' @details
#' **Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use
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
#'   750 Channelview Dr., Port Aransas, TX 78373, USA.
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
rm(list = ls())

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, gridExtra, XLConnect, lubridate, cowplot)

# ------------------------------------------------------------------------------
# 2. Data Loading and Age Distribution Analysis
# ------------------------------------------------------------------------------

# Age reads of winter run hatchery and wild fish from the same years as our samples
ages <- read.csv("data/winter_run_2005-2018_scale_reads.csv") |> 
  rename(FL = Fork.Length..mm.)

# Assign fish a final age using CWT known ages where available, otherwise scale est
ages$final_age <- NA
ages$final_age[!ages$Readage == 0 & !is.na(ages$Readage)] <- ages$Readage[!ages$Readage == 0 & !is.na(ages$Readage)] 
ages$final_age[!ages$Age == 0 & !is.na(ages$Age)] <- ages$Age[!ages$Age == 0 & !is.na(ages$Age)] 

# Filter to use only valid age estimates for cutoff calculations
ages_final <- ages |> 
  filter(
    !is.na(final_age),
    FL < 2000 & FL > 300, # Exclude abnormally sized fish
    final_age < 5         # Exclude any 5+ year olds
  )

# Calculate 99% CI of Fork Length (FL) for 3-year-old fish (wild and hatchery)
age3 <- subset(ages_final, final_age == 3)
age3LW <- as.numeric(quantile(ages_final$FL, 0.01))
age3UP <- as.numeric(quantile(ages_final$FL, 0.99))
cutoffs <- data.frame(Age = c('ALL_3_LW', 'ALL_3_UP'), cutoff = c(age3LW, age3UP))

# 99% CI of FL by age and sex
# --- Females ---
Fage3 <- subset(ages_final, final_age == 3 & Sex == "F")
Fage3LW <- as.numeric(quantile(Fage3$FL, 0.01))
Fage3UP <- as.numeric(quantile(Fage3$FL, 0.99))
Fcutoffs <- data.frame(Age = c('F_3_LW', 'F_3_UP'), cutoff = c(Fage3LW, Fage3UP))

# --- Males ---
Mage3 <- subset(ages_final, final_age == 3 & Sex == "M")
Mage3LW <- as.numeric(quantile(Mage3$FL, 0.01))
Mage3UP <- as.numeric(quantile(Mage3$FL, 0.99))
Mcutoffs <- data.frame(Age = c('M_3_LW', 'M_3_UP'), cutoff = c(Mage3LW, Mage3UP))

# ------------------------------------------------------------------------------
# 3. Visualization of Age Distributions (Figure S1)
# ------------------------------------------------------------------------------

# Define common theme and colors
theme_custom <- theme_bw()
colors_manual <- c("#E69F00", "#56B4E9", "#009E73")

# --- Female Distribution ---
F_plot <- ggplot(subset(ages_final, Sex == "F"), aes(x = FL, fill = factor(final_age), color = factor(final_age))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 15) + 
  xlim(400, 1100) +
  geom_vline(data = Fcutoffs, aes(xintercept = cutoff), linetype = "dashed", linewidth = 0.8) +
  labs(y = "Number of fish", x = "Fork length (mm)", 
       fill = "Age - years\n(scale reads)", color = "Age - years\n(scale reads)") + 
  scale_fill_manual(values = colors_manual) +
  scale_color_manual(values = colors_manual) +
  theme_custom + 
  theme(legend.position = "none")

# --- Male Distribution ---
M_plot <- ggplot(subset(ages_final, Sex == "M"), aes(x = FL, fill = factor(final_age), color = factor(final_age))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 15) + 
  xlim(400, 1100) +
  geom_vline(data = Mcutoffs, aes(xintercept = cutoff), linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = colors_manual) +
  scale_color_manual(values = colors_manual) +
  labs(y = "Number of fish", x = "Fork length (mm)", 
       fill = "Age - years\n(scale reads)", color = "Age - years\n(scale reads)") + 
  theme_custom

# --- Combined Distribution ---
ALL_plot <- ggplot(subset(ages_final, Sex %in% c("F", "M")), aes(x = FL, fill = factor(final_age), color = factor(final_age))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 15) + 
  xlim(400, 1100) +
  geom_vline(data = cutoffs, aes(xintercept = cutoff), linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = colors_manual) +
  scale_color_manual(values = colors_manual) +
  labs(y = "Number of fish", x = "Fork length (mm)", 
       fill = "Age - years\n(scale reads)", color = "Age - years\n(scale reads)") + 
  theme_custom + 
  theme(legend.position = "none")

# Assemble Figure S1
M_legend <- cowplot::get_legend(M_plot)
M_plot_noleg <- M_plot + theme(legend.position = "none")

plots_col <- cowplot::plot_grid(F_plot, M_plot_noleg, ALL_plot, nrow = 3, labels = "AUTO")
legend_col <- cowplot::plot_grid(NULL, M_legend, NULL, nrow = 3, rel_heights = c(1, 1, 1))

fig_s1 <- cowplot::plot_grid(plots_col, legend_col, ncol = 2, rel_widths = c(1, 0.2))

ggsave('figures/FigS1_FL_distributions.jpg', fig_s1, dpi = 300, width = 14, height = 19, units = "cm")

# ------------------------------------------------------------------------------
# 4. Cohort Reconstruction
# ------------------------------------------------------------------------------

# Read in Sr8786 data with previously calculated assignments
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_assignments.csv') |> 
  select(-1, -2)

# Initialize final age column
wr_megafile$final_age <- NA

# 1. Use scale read age if available (excluding zeros)
wr_megafile$final_age[!wr_megafile$Readage == 0 & !is.na(wr_megafile$Readage)] <- 
  wr_megafile$Readage[!wr_megafile$Readage == 0 & !is.na(wr_megafile$Readage)] 

# 2. For unaged fish, use Fork Length (FL) and sex to conservatively assign age 2 or 4
# Based on the assumption that 3-year-olds are the baseline, we assign 2 or 4 
# if the FL falls significantly outside the 3-year-old distribution.

# --- Females ---
wr_megafile$final_age[wr_megafile$fork_length < Fage3LW & wr_megafile$sex == "Female" & is.na(wr_megafile$final_age)] <- 2
wr_megafile$final_age[wr_megafile$fork_length > Fage3UP & wr_megafile$sex == "Female" & is.na(wr_megafile$final_age)] <- 4

# --- Males ---
wr_megafile$final_age[wr_megafile$fork_length < Mage3LW & wr_megafile$sex == "Male" & is.na(wr_megafile$final_age)] <- 2
wr_megafile$final_age[wr_megafile$fork_length > Mage3UP & wr_megafile$sex == "Male" & is.na(wr_megafile$final_age)] <- 4

# --- Unknown Sex ---
# Using combined population cutoffs for unknown sex
wr_megafile$final_age[wr_megafile$fork_length < age3LW & wr_megafile$sex == "Unknown" & is.na(wr_megafile$final_age)] <- 2
wr_megafile$final_age[wr_megafile$fork_length > age3UP & wr_megafile$sex == "Unknown" & is.na(wr_megafile$final_age)] <- 4

# 3. Final Fallback: Assume remaining unaged fish (e.g., 2007-08 cohorts without FL/sex) are 3 years old
wr_megafile$final_age[is.na(wr_megafile$final_age)] <- 3

# Assign brood year
wr_megafile$Brood_year <- wr_megafile$Escap_yr - wr_megafile$final_age

# ------------------------------------------------------------------------------
# 5. Data Export
# ------------------------------------------------------------------------------

write.csv(wr_megafile, 'outputs/oto_sr8786_dat_with_brood_year.csv', row.names = FALSE)
