#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script visualizes the relationship between the mean proportion of freshwater growth 
#' in different habitats, annual hydrographs (flow), and juvenile passage estimates.
#' 
#' @details
#' **Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' **Journal:** Ecosphere
#'
#' **Year:** 2026
#'
#' **DOI:** https://doi.org/10.1002/ecs2.70779
#' 
#' **Authors:** 
#' Pedro Morais1,*, Anna M. Sturrock2,3+, Corey C. Phillis4, George Whitman2, 
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
#' This script was originally created by Anna M. Sturrock and revised by Pedro Morais.
#' 

# ------------------------------------------------------------------------------
# 1. Setup and Environment
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(egg)

# ------------------------------------------------------------------------------
# 2. Juvenile Data Loading and Passage Data Processing
# ------------------------------------------------------------------------------

# Read in mean % FW growth by brood year
mean_fw_growth <- read.csv("outputs/summary_stats_fw_growth.csv")

# Read in juvenile RST passage data obtained from Sac Pass 04/01/2021
juv_files <- file.path("data", c(
  "redbluffdaily_1609788856_999-2004.csv",
  "redbluffdaily_1609788890_111-2005.csv",
  "redbluffdaily_1609788903_675-2006.csv",
  "redbluffdaily_1609788913_979-2007.csv",
  "redbluffdaily_1609788962_73-2012.csv",
  "redbluffdaily_1609788972_925-2013.csv",
  "redbluffdaily_1609788981_630-2014.csv",
  "redbluffdaily_1609788990_951-2015.csv"
))

leapyrs <- c(2000, 2004, 2008, 2012, 2016)

juv_rbd <- lapply(juv_files, read.csv) |>
  bind_rows() |>
  select(Date, Winter.Chinook.Passage.Estimate) |>
  mutate(
    passage = as.numeric(Winter.Chinook.Passage.Estimate),
    date_obj = as.Date(as.character(Date), format = "%Y-%m-%d"),
    julian_day = yday(date_obj),
    year = year(date_obj),
    month = month(date_obj),
    broodyr_day = case_when(
      year %in% leapyrs & julian_day < 183 ~ julian_day + 184,
      year %in% leapyrs                    ~ julian_day - 182,
      julian_day < 182                     ~ julian_day + 184,
      TRUE                                 ~ julian_day - 181
    ),
    BY = if_else(month < 7, year - 1, year)
  ) |>
  filter(!is.na(julian_day))

# ------------------------------------------------------------------------------
# 3. Flow Data Processing
# ------------------------------------------------------------------------------

# Read in daily flow and temperature data for USGS 11390500 SACRAMENTO R BL WILKINS SLOUGH
flow <- read.csv("outputs/daily_flow_temp.csv")

# Define years of interest (with adequate sample sizes)
yrs_of_interest <- c(2004, 2005, 2006, 2012, 2013, 2014)

# Find first date per BY that flow exceeded 400 cms (from Del Rosario et al.)
flow_exceed <- flow |>
  filter(flow_cms >= 400, BY %in% yrs_of_interest) |>
  group_by(BY) |>
  slice(which.min(broodyr_day)) |>
  ungroup()

# Annual mean flow stats for Aug-Jan
flow_8.1 <- flow |>
  filter(month %in% c(8, 9, 10, 11, 12, 1), BY %in% yrs_of_interest) |>
  group_by(BY) |>
  summarize(av_flow8.1 = mean(flow_cms, na.rm = TRUE), .groups = "drop") |>
  left_join(flow_exceed |> select(BY, broodyr_day), by = "BY")

# Write flow summary
write.csv(flow_8.1, "outputs/mean_aug_jan_flows.csv", row.names = FALSE)

# Combine flow and passage daily data
# Drop broodyr_day from juv_rbd to avoid duplicate columns; flow already carries it
flow_passage <- left_join(
  flow,
  juv_rbd |> select(-broodyr_day),
  by = c("BY", "year", "month", "julian_day")
)

# ------------------------------------------------------------------------------
# 5. Visualization
# ------------------------------------------------------------------------------

# Figure size scaling
fig_width_cm_original <- 22
fig_height_cm_original <- 17
fig_width_cm <- 18

scale_factor <- fig_width_cm / fig_width_cm_original
fig_height_cm <- fig_height_cm_original * scale_factor

base_text_size <- 15 * scale_factor
tag_text_size <- 16 * scale_factor
bar_border_width <- 0.4 * scale_factor
flow_line_width <- 1.1 * scale_factor
vline_width <- 0.4 * scale_factor
annotation_text_size <- 3.4 * scale_factor


# Plot aesthetics
habitats <- c("SAC", "LAS", "Unassigned", "AME", "DEL")
yrs_factor <- c("2004", "2005", "2006", "2012", "2013", "2014")
cols <- c("grey60", "tomato2", "grey20", "turquoise", "turquoise4")
habitat_labels <- c("Sacramento River", "Lassen Tributaries", "Habitat X",
                    "American River", "Feather River/Delta")

# Keep only brood years shown in Figure 5, then set plotting order
mean_fw_growth <- mean_fw_growth |>
  mutate(Brood_year = as.character(Brood_year)) |>
  filter(Brood_year %in% yrs_factor)

mean_fw_growth$Habitat <- factor(mean_fw_growth$Habitat, levels = rev(habitats))
mean_fw_growth$Brood_year <- factor(mean_fw_growth$Brood_year, levels = rev(yrs_factor))

# Plot A: Mean proportion of FW growth per habitat by brood year
plot_a <- mean_fw_growth |>
  ggplot(aes(x = Brood_year, y = mean_prop_fw_growth, fill = Habitat)) +
  geom_bar(color = "black", linewidth = bar_border_width,
           stat = "identity", position = "fill") +
  theme_bw(base_size = base_text_size) +
  theme(
    text = element_text(size = base_text_size),
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(0.35 * scale_factor, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  ) +
  labs(x = "Brood Year", y = "Mean proportion of freshwater growth") +
  coord_flip() +
  scale_fill_manual(values = rev(cols), labels = rev(habitat_labels))+
  guides(fill = guide_legend(reverse = TRUE, ncol = 2))

# Set coefficient to scale the secondary y-axis for dual-axis plot
coeff <- 2

# Prepare flow exceedance data with mean flow labels for annotation
flow_exceed_with_labels <- flow_exceed |>
  left_join(flow_8.1 |> select(BY, av_flow8.1), by = "BY") |>
  mutate(
  label_text = paste0(
    "italic(",
    round(av_flow8.1, 0),
    "~m^3~s^{-1})"
  )
)

# Plot B: Hydrographs with juvenile passage overlaid per brood year
plot_b <- ggplot(filter(flow_passage, BY %in% yrs_of_interest), aes(x = broodyr_day)) +
  geom_bar(aes(y = passage / 1000), stat = "identity", width = 1,
           fill = "darkorange3", alpha = 0.9) +
  geom_line(aes(y = flow_cms / coeff), linewidth = flow_line_width) +
  geom_vline(data = flow_exceed_with_labels, aes(xintercept = broodyr_day),
             colour = "grey22", linetype = "dashed", linewidth = vline_width) +
  geom_text(data = flow_exceed_with_labels, aes(label = label_text),
            x = 40, y = 390, parse = TRUE, size = annotation_text_size) +
  facet_wrap(~BY, ncol = 1) +
  scale_y_continuous(
    name = "Daily passage (\u00d7 1000)",
    sec.axis = sec_axis(~ . * coeff, name = expression("Mean daily flow (m"^3~"s"^{-1}*")"))
  ) +
  scale_x_continuous(
    limits = c(1, 366),
    breaks = c(1, 62, 122, 183, 243, 304, 366),
    labels = c("Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul")
  ) +
  labs(x = "Month") +
  theme_bw(base_size = base_text_size) +
  theme(
    axis.text.y.left  = element_text(size = 8),
    axis.text.y.right = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    text = element_text(size = base_text_size),
    strip.text.x = element_blank()
  )

# ------------------------------------------------------------------------------
# 6. Combine and Export
# ------------------------------------------------------------------------------

main_fig <- egg::ggarrange(
  plot_a, plot_b,
  ncol = 2,
  widths = c(1, 1.1),
  labels = c("A", "B"),
  label.args = list(gp = grid::gpar(fontsize = tag_text_size, fontface = "bold"))
)

ggsave(
  filename = "figures/Fig6_fw_growth_passage_flow.tif",
  plot     = main_fig,
  width    = fig_width_cm,
  height   = fig_height_cm,
  dpi      = 300,
  units    = "cm",
  bg       = "white"
)
