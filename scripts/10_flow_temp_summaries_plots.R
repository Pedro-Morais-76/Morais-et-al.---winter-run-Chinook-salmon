#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script calculates mean annual flow and temperature in the Sacramento River 
#' for the August-to-May emigration period.
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
p_load(readxl, dplyr, lubridate, ggplot2, ggrepel)

# ------------------------------------------------------------------------------
# 2. Data Loading and Cleaning
# ------------------------------------------------------------------------------

# Read in temperature and flow data from USGS
df <- read_xlsx('data/Sac_flow_temp_USGS11390500_2000to21.xlsx', sheet = 'forR')

## Clean up dates
df$julian_day <- yday(as.Date(df$datetime, origin = "1899-12-30")) 
df$year <- year(as.Date(df$datetime, origin = "1899-12-30"))
df$month <- month(as.Date(df$datetime, origin = "1899-12-30"))

# Convert flow to cubic meters per second (cms)
df$flow_cms <- df$mean_daily_flow_cfs / 35.314666212661

# Add days since July 1 (start of Brood Year)
df$broodyr_day <- df$julian_day - 181
df$broodyr_day[df$julian_day < 182] <- df$julian_day[df$julian_day < 182] + 184

# Adjust for leap years
leapyrs <- c(2000, 2004, 2008, 2012, 2016)
df$broodyr_day[df$year %in% leapyrs] <- df$julian_day[df$year %in% leapyrs] - 182
df$broodyr_day[df$year %in% leapyrs & df$julian_day < 183] <- df$julian_day[df$year %in% leapyrs & df$julian_day < 183] + 184

# Add Brood Year (BY)
df$BY <- df$year 
df$BY[df$month < 7] <- df$year[df$month < 7] - 1 # July 1 onwards marks the start of the BY

# Keep essential columns
df <- df |> 
  select(datetime, julian_day, broodyr_day, month, year, BY, flow_cms, max_daily_temp_C, median_daily_temp_C)

# Define years of interest
yrs <- c(2004, 2005, 2006, 2011, 2012, 2013, 2014, 2015)

# ------------------------------------------------------------------------------
# 3. Summary Statistics: August to January Emigration Period
# ------------------------------------------------------------------------------

# Identify August to January as the peak emigration period
df <- df |>
  mutate(emigration_period = case_when(
    month >= 8 ~ "y",
    month <= 1 ~ "y",
    TRUE ~ "n"
  ))

# Create summary table for the emigration period
summary_AugtoJan_only <- df |>
  filter(BY %in% yrs & emigration_period == "y") |>
  group_by(BY) |>
  summarize(
    mean_max_daily_temp_AugtoJan = mean(max_daily_temp_C, na.rm = TRUE),
    median_max_daily_temp_AugtoJan = median(max_daily_temp_C, na.rm = TRUE),
    sd_max_daily_temp_AugtoJan = sd(max_daily_temp_C, na.rm = TRUE),
    mean_mean_daily_flow_AugtoJan = mean(flow_cms, na.rm = TRUE),
    median_mean_daily_flow_AugtoJan = median(flow_cms, na.rm = TRUE),
    sd_mean_daily_flow_AugtoJan = sd(flow_cms, na.rm = TRUE),
    .groups = "drop"
  )

# Export summary statistics
write.csv(summary_AugtoJan_only, 'outputs/flow_temp_stats_AugtoJan_only.csv', row.names = FALSE)
write.csv(df, "outputs/daily_flow_temp.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. Visualization: Annual Hydrographs (Figure S3)
# ------------------------------------------------------------------------------

# Helper function to create standardized hydrograph panels
plot_hydrograph <- function(data_subset, label, color_vals) {
  ggplot(data_subset, aes(x = broodyr_day, y = flow_cms, color = factor(BY))) +
    geom_line(linewidth = 1.1) +
    theme_classic(base_size = 15) +
    ylim(0, 1000) +
    labs(
      x = "Days since July 1", 
      y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), 
      color = "Brood year"
    ) +
    annotate("text", label = label, x = 4, y = 1000, size = 5, hjust = 0, fontface = "bold") +
    scale_color_manual(values = color_vals)
}

# Define common color palette
common_colors <- c("#009E73", "#56B4E9", "#E69F00")

# Generate individual year plots
ry07 <- plot_hydrograph(filter(df, BY %in% c(2003, 2004, 2005)), "A  |  Return year 2007", common_colors)
ry08 <- plot_hydrograph(filter(df, BY %in% c(2004, 2005, 2006)), "B  |  Return year 2008", common_colors)
ry09 <- plot_hydrograph(filter(df, BY %in% c(2005, 2006, 2007)), "C  |  Return year 2009", common_colors)
ry15 <- plot_hydrograph(filter(df, BY %in% c(2011, 2012, 2013)), "D  |  Return year 2015", common_colors)
ry16 <- plot_hydrograph(filter(df, BY %in% c(2012, 2013, 2014)), "E  |  Return year 2016", common_colors)
ry17 <- plot_hydrograph(filter(df, BY %in% c(2013, 2014, 2015)), "F  |  Return year 2017", common_colors)

# Assemble composite figure
composite_hydrograph <- cowplot::plot_grid(ry07, ry08, ry09, ry15, ry16, ry17, nrow = 3)

# Save high-resolution figure
ggsave('figures/FigS3_flows_per_year.jpg', composite_hydrograph, dpi = 600, height = 30, width = 25, units = 'cm')
