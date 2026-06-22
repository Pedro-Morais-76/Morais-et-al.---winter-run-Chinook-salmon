#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script estimates the proportion of freshwater growth (in terms of mass 
#' assimilated) in each habitat and assigns rearing types.
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
p_load(ggplot2, dplyr, shape, forcats, RColorBrewer, wesanderson, tidyverse, lubridate)

# ------------------------------------------------------------------------------
# 2. Data Loading and Mass Reconstruction Model
# ------------------------------------------------------------------------------

# Read in otolith Sr8786 data
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_rearing_types.csv') |>
  arrange(Brood_year, Sample_ID)

# Read in fall run juvenile salmon otolith radius (OR) and total weight (TW) data. 
# OR measured on dorsal axis.
or_tw <- read.csv('data/juv_fall_run_oto_radius_total_weight.csv') |>
  filter(!(OR < 300 & TW > 7)) # Exclude one clear bad data point

# Fit linear regression for mass reconstruction
# Relationship between log(TW) and log(OR)
mod <- lm(log(TW) ~ log(OR), data = or_tw)
summary(mod) # Expected adj R2 ~ 0.91

# ------------------------------------------------------------------------------
# 3. Estimating Mass and Freshwater Habitat Use
# ------------------------------------------------------------------------------

# Reconstruct mass using the log-log relationship:
# log(mass) = intercept + slope * log(Distance_um)
# mass = exp(intercept + slope * log(Distance_um))

growth_dist <- wr_megafile |>
  mutate(mass = exp((as.vector(coef(mod)[2]) * (log(Distance_um))) + as.vector(coef(mod)[1]))) |>
  arrange(Sample_ID, mass) |>
  group_by(Sample_ID) |>
  mutate(mass_change = lead(mass, 1) - mass) |>
  filter(!is.na(Habitat)) |> # Excludes core (maternal) and ocean values
  select(Sample_ID, Distance_um, mass_change, Sr8786_norm, SE2, SrV2, Habitat, Escap_yr, Brood_year, fork_length)

# Calculate mean mass change per spot for imputation
mean_change <- mean(growth_dist$mass_change, na.rm = TRUE)

# Assume that any spots with NA growth accrued the mean mass change
growth_dist$mass_change[is.na(growth_dist$mass_change)] <- mean_change

# Summing total growth in FW by fish
growth_dist_tot <- growth_dist |>
  group_by(Sample_ID) |>
  summarize(tot_mass_change = sum(mass_change, na.rm = TRUE), .groups = 'drop')

# Summing growth in each FW habitat by fish & calculating proportion of total FW growth
by_fish_prop_dist <- growth_dist |>
  group_by(Sample_ID, Habitat, Escap_yr, Brood_year, fork_length) |>
  summarize(mass_change_sum = sum(mass_change, na.rm = TRUE), .groups = 'drop') |>
  left_join(growth_dist_tot, by = 'Sample_ID') |>
  mutate(prop_fw_growth = mass_change_sum / tot_mass_change)

# Add column of SAC proportion for ordering later
percent_sac_df <- by_fish_prop_dist |>
  filter(Habitat == "SAC") |>
  ungroup() |>
  select(Sample_ID, prop_sac = prop_fw_growth)

by_fish_prop_dist <- left_join(by_fish_prop_dist, percent_sac_df, by = 'Sample_ID')

# ------------------------------------------------------------------------------
# 4. Summary Statistics and Wide Format Conversion
# ------------------------------------------------------------------------------

# Convert to wide format to ensure every habitat is represented for every fish (even if zero growth)
by_fish_prop_dist_wide <- by_fish_prop_dist |>
  select(Sample_ID, Brood_year, Habitat, prop_fw_growth) |>
  pivot_wider(names_from = Habitat, values_from = prop_fw_growth) |>
  replace_na(list(SAC = 0, LAS = 0, DEL = 0, AME = 0, Unassigned = 0))

# Export wide format
write.csv(by_fish_prop_dist_wide, 'outputs/by_fish_prop_growth_wide_format.csv', row.names = FALSE)

# Return to long format for summary statistics
by_fish_prop_dist_long <- by_fish_prop_dist_wide |>
  pivot_longer(cols = c('SAC', 'LAS', 'DEL', 'AME', 'Unassigned'), names_to = "Habitat", values_to = "prop_fw_growth")

# Calculate means and standard errors by habitat and brood year
by_year_prop_stats <- by_fish_prop_dist_long |>
  group_by(Brood_year, Habitat) %>%
  summarize(
    mean_prop_fw_growth = mean(prop_fw_growth),
    min_prop_fw_growth = min(prop_fw_growth),
    max_prop_fw_growth = max(prop_fw_growth),
    se_prop_fw_growth = sd(prop_fw_growth) / sqrt(n()),
    n_fish = n(),
    .groups = 'drop'
  )

write.csv(by_year_prop_stats, 'outputs/summary_stats_fw_growth.csv', row.names = FALSE)

# Export detailed proportion by fish
write.csv(by_fish_prop_dist, 'outputs/prop_fw_growth_by_fish.csv', row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Visualization: Habitat Use Diversity (Figure 3)
# ------------------------------------------------------------------------------

# Reorder Habitat levels for plotting (upstream to downstream)
by_fish_prop_dist$Habitat <- factor(by_fish_prop_dist$Habitat,
                                    levels = c("Unassigned", "DEL", "AME", "LAS", "SAC"))

# Define colors matching Figure 3 shaded areas
cols <- c("Unassigned" = "grey20", "DEL" = "turquoise4", "AME" = "turquoise",
          "LAS" = "tomato2", "SAC" = "grey40")

# Helper function to create stacked bar panels per brood year
make_panel <- function(by, show_legend = FALSE, show_y = FALSE, show_y_text = TRUE) {
  by_fish_prop_dist |>
    filter(Brood_year == by) |>
    arrange(desc(prop_sac)) |>
    ungroup() |>
    mutate(Sample_ID = factor(Sample_ID, levels = unique(Sample_ID))) |>
    ggplot(aes(x = Sample_ID, y = mass_change_sum, fill = Habitat)) +
    geom_bar(stat = "identity", position = "fill", key_glyph = "point", width = 1, colour = NA) +
    theme_bw() +
    labs(x = paste("Brood Year", by), y = if (show_y) 'Proportion of freshwater growth' else '') +
    theme(axis.text.x   = element_blank(),
          axis.ticks.x  = element_blank(),
          axis.text.y   = if (show_y_text) element_text() else element_blank(),
          axis.ticks.y  = if (show_y_text) element_line() else element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = if (show_legend) 'bottom' else 'none',
          text = element_text(size = 11)) +
    scale_fill_manual(values = cols,
                      labels = c("Habitat X", "Feather River/Delta", "American River",
                                 "Lassen Tributaries", "Sacramento River")) +
    guides(fill = guide_legend(reverse = TRUE, title = "",
                                override.aes = list(shape = 21, size = 3)))
}

# Assemble Figure 3 grid
# Row 1: 2004, 2005, 2006
p2004 <- make_panel(2004, show_y = FALSE, show_y_text = TRUE)
p2005 <- make_panel(2005, show_y = FALSE, show_y_text = FALSE)
p2006 <- make_panel(2006, show_y = FALSE, show_y_text = FALSE)
row1 <- cowplot::plot_grid(p2004, p2005, p2006, nrow = 1, rel_widths = c(1, 1, 1))

# Row 2: 2012
p2012 <- make_panel(2012, show_legend = FALSE, show_y = TRUE, show_y_text = TRUE)
row2 <- cowplot::plot_grid(p2012, nrow = 1)

# Row 3: 2013, 2014
p2013 <- make_panel(2013, show_y = FALSE, show_y_text = TRUE)
p2014 <- make_panel(2014, show_y = FALSE, show_y_text = FALSE)
row3 <- cowplot::plot_grid(p2013, p2014, nrow = 1, rel_widths = c(1, 1))

# Row 4: Legend
legend_row <- cowplot::get_legend(
  p2012 + theme(legend.position = 'bottom', legend.key.size = unit(0.15, 'cm'))
)

# Final composition
Fig3 <- cowplot::plot_grid(row1, row2, row3, legend_row, ncol = 1, rel_heights = c(1, 1, 1, 0.2))

ggsave('figures/Fig3_prop_fw_growth_by_fish.jpg', Fig3, dpi = 800, height = 19, width = 15, units = 'cm')
