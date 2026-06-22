#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script assigns rearing type to each fish based on the proportion of 
#' freshwater growth in each habitat, and summarizes rearing type frequencies 
#' by brood year.
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
#' @author Pedro Morais (Revision), Anna Sturrock (Original)

# ------------------------------------------------------------------------------
# 1. Setup and Environment
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, forcats, RColorBrewer, wesanderson, tidyverse, lubridate)

# ------------------------------------------------------------------------------
# 2. Data Loading and Preparation
# ------------------------------------------------------------------------------

# Read in otolith Sr8786 data with habitat assignments and brood year
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_brood_year.csv')

# ------------------------------------------------------------------------------
# 3. Freshwater Habitat Use by Fish
# ------------------------------------------------------------------------------

# Estimate total otolith distance (in micrometers) spent in each habitat per fish
# This is used to calculate the proportion of freshwater growth in each habitat
# 
# Order by fish and otolith distance, then calculate the distance between 
# consecutive spots. Exclude all ocean spots (those after freshwater exit).

growth_dist <- wr_megafile |>
  arrange(Sample_ID, Distance_um) |>
  group_by(Sample_ID) |>
  mutate(Spot_width = lead(Distance_um, 1) - Distance_um) |>
  filter(!is.na(Habitat)) |>
  select(Sample_ID, Distance_um, Spot_width, Sr8786_norm, SE2, SrV2, Habitat, Escap_yr, Brood_year, fork_length)

# Sum otolith distance within each freshwater habitat by fish
growth_dist_sum <- growth_dist |>
  group_by(Sample_ID, Habitat, Escap_yr, Brood_year, fork_length) |>
  summarize(oto_dist = sum(Spot_width, na.rm = TRUE), .groups = 'drop')

# ------------------------------------------------------------------------------
# 4. Rearing Type Assignment
# ------------------------------------------------------------------------------

# Assign rearing type based on the habitat(s) where the fish grew most.
# Minimum otolith distance thresholds:
# - LAS, AME, X (Unassigned): 40 micrometers (minimum of 1 spot equivalent)
# - DEL (Delta): 120 micrometers (minimum of 3 spots equivalent)
#   The Delta threshold is higher because all fish must pass through it during
#   seaward migration, so meaningful rearing is only indicated by extended growth.

las_rearer <- growth_dist_sum |>
  filter(Habitat == "LAS" & oto_dist >= 40) |>
  mutate(las_dist = oto_dist)

ame_rearer <- growth_dist_sum |>
  filter(Habitat == "AME" & oto_dist >= 40) |>
  mutate(ame_dist = oto_dist)

x_rearer <- growth_dist_sum |>
  filter(Habitat == "Unassigned" & oto_dist >= 40) |>
  mutate(x_dist = oto_dist)

del_rearer <- growth_dist_sum |>
  filter(Habitat == "DEL" & oto_dist >= 120) |>
  mutate(del_dist = oto_dist)

# Combine into a single data frame with all habitat distances per fish
rearing_type_df <- growth_dist_sum |>
  distinct(Sample_ID, Brood_year, fork_length) |>
  left_join(las_rearer[c('Sample_ID', 'las_dist')], by = 'Sample_ID') |>
  left_join(ame_rearer[c('Sample_ID', 'ame_dist')], by = 'Sample_ID') |>
  left_join(x_rearer[c('Sample_ID', 'x_dist')], by = 'Sample_ID') |>
  left_join(del_rearer[c('Sample_ID', 'del_dist')], by = 'Sample_ID') |>
  replace_na(list(las_dist = 0, ame_dist = 0, x_dist = 0, del_dist = 0))

# Assign final rearing type based on hierarchical logic:
# 1. Default to Sacramento River (SAC) as the null hypothesis
# 2. If significant Delta growth (>= 120 um), classify as Delta (DEL)
# 3. If unassigned habitat growth, classify as Unassigned (X)
# 4. If American River growth exceeds Lassen, classify as American (AME)
# 5. If Lassen growth exceeds American River, classify as Lassen (LAS)
rearing_type_df <- rearing_type_df |>
  mutate(
    rearing_type = "SAC",
    rearing_type = if_else(del_dist > 0, "DEL", rearing_type),
    rearing_type = if_else(x_dist > 0, "X", rearing_type),
    rearing_type = if_else(ame_dist > las_dist, "AME", rearing_type),
    rearing_type = if_else(las_dist > ame_dist, "LAS", rearing_type)
  )

# Export rearing type assignments
write.csv(rearing_type_df, 'outputs/rearing_type_by_fish.csv', row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Summary Statistics: Rearing Type Frequencies
# ------------------------------------------------------------------------------

# Proportion of rearing types by brood year
rearing_type_summary <- rearing_type_df |>
  group_by(Brood_year, rearing_type) |>
  summarize(n = n(), .groups = 'drop') |>
  group_by(Brood_year) |>
  mutate(
    tot = sum(n),
    freq = n / tot
  )

# Export rearing type summary by brood year
write.csv(rearing_type_summary, 'outputs/rearing_type_by_BY.csv', row.names = FALSE)

# Average proportion of fish in each rearing type across years
# (excluding 2011 and 2015 due to very low sample sizes)
mean_non_nat <- rearing_type_summary |>
  filter(!Brood_year %in% c('2011', '2015')) |>
  group_by(rearing_type) |>
  summarize(mean_freq = mean(freq), .groups = 'drop')

# Proportion of non-Sacramento rearers by brood year
prop_non_sac_rearing_type <- rearing_type_summary |>
  filter(rearing_type == "SAC") |>
  mutate(
    freq_non_natal_rearers = 1 - freq,
    n_non_natal = tot - n
  ) |>
  select(-rearing_type, -n)

# Average proportion of non-Sacramento rearers (excluding low sample size years)
mean_non_sac <- mean(
  prop_non_sac_rearing_type$freq_non_natal_rearers[
    !prop_non_sac_rearing_type$Brood_year %in% c('2011', '2015')
  ]
)

# Sample sizes by brood year
sample_sizes <- rearing_type_df |>
  group_by(Brood_year) |>
  summarize(N_fish = n(), .groups = 'drop')

# ------------------------------------------------------------------------------
# 6. Visualization: Rearing Type Proportions (Figure 1)
# ------------------------------------------------------------------------------

# Reorder factor levels for consistent plotting
rearing_type_summary <- rearing_type_summary |>
  mutate(
    rearing_type = factor(rearing_type, levels = c("X", "LAS", "DEL", "AME", "SAC"))
  )

# Define color palette for rearing types
habcolor <- function(x) {
  RColorBrewer::brewer.pal(8, 'RdBu')[x]
}

sacc <- habcolor(7)
amec <- habcolor(1)
lasc <- habcolor(3)
xc <- habcolor(4)
delc <- habcolor(2)

# Plot rearing type proportions by brood year (excluding low sample size years)
ggplot(
  subset(rearing_type_summary, !Brood_year %in% c(2011, 2015)),
  aes(x = factor(Brood_year), y = freq, fill = rearing_type)
) +
  geom_bar(colour = "black", stat = "identity") +
  labs(
    y = "Proportion of sample",
    x = "Brood Year",
    fill = "Rearing location"
  ) +
  theme_classic() +
  scale_fill_manual(
    values = c(xc, lasc, delc, amec, sacc),
    labels = c("Habitat X", "Lassen Tributaries", "Delta/Feather River", "American River", "Sacramento River")
  ) +
  theme(
    text = element_text(size = 25),
    legend.position = "none"
  )

ggsave('figures/Fig1_rearing_type_props.jpg', width = 6.5, height = 9, units = "in", dpi = 300)

# ------------------------------------------------------------------------------
# 7. Final Data Export with Rearing Types
# ------------------------------------------------------------------------------

# Merge rearing type assignments back to the full otolith dataset
wr_megafile2 <- left_join(wr_megafile, rearing_type_df[c('Sample_ID', 'rearing_type')], by = 'Sample_ID')

# Summary of sample sizes by rearing type (excluding unassigned habitats)
sample_sizes_rearing_types <- wr_megafile2 |>
  filter(rearing_type != "X") |>
  distinct(Sample_ID, rearing_type) |>
  group_by(rearing_type) |>
  summarize(n_fish = n(), .groups = 'drop')

# Export full dataset with rearing type assignments
write.csv(wr_megafile2, 'outputs/oto_sr8786_dat_with_rearing_types.csv', row.names = FALSE)
