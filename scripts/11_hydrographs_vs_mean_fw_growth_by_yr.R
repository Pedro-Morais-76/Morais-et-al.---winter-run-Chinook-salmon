#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script visualizes the relationship between the mean proportion of freshwater growth 
#' in different habitats, annual hydrographs (flow), and juvenile passage estimates.
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
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(egg)

# ------------------------------------------------------------------------------
# 2. Data Loading
# ------------------------------------------------------------------------------

# Read in mean % FW growth by brood year
mean_fw_growth <- read.csv("outputs/summary_stats_fw_growth.csv")

# Read in juvenile RST passage data obtained from Sac Pass 04/01/2021
juv04 <- read.csv("data/redbluffdaily_1609788856_999-2004.csv")
juv05 <- read.csv("data/redbluffdaily_1609788890_111-2005.csv")
juv06 <- read.csv("data/redbluffdaily_1609788903_675-2006.csv")
juv07 <- read.csv("data/redbluffdaily_1609788913_979-2007.csv")
juv12 <- read.csv("data/redbluffdaily_1609788962_73-2012.csv")
juv13 <- read.csv("data/redbluffdaily_1609788972_925-2013.csv")
juv14 <- read.csv("data/redbluffdaily_1609788981_630-2014.csv")
juv15 <- read.csv("data/redbluffdaily_1609788990_951-2015.csv")

# ------------------------------------------------------------------------------
# 3. Juvenile Passage Data Processing
# ------------------------------------------------------------------------------

# Combine annual passage files
juv_rbd <- rbind(juv04, juv05, juv06, juv07, juv12, juv13, juv14, juv15) |>
  select(Date, Winter.Chinook.Passage.Estimate) |>
  mutate(passage = as.numeric(Winter.Chinook.Passage.Estimate))

# Add date components
juv_rbd <- juv_rbd |>
  mutate(
    date_obj   = as.Date(as.character(Date), format = "%Y-%m-%d"),
    julian_day = yday(date_obj),
    year       = year(date_obj),
    month      = month(date_obj)
  ) |>
  filter(!is.na(julian_day))

# Add days since Jul 1 (first day of brood year)
juv_rbd <- juv_rbd |>
  mutate(
    broodyr_day = ifelse(julian_day < 182, julian_day + 184, julian_day - 181)
  )

# Adjust broodyr_day for leap years
leapyrs <- c(2000, 2004, 2008, 2012, 2016)
juv_rbd <- juv_rbd |>
  mutate(
    broodyr_day = case_when(
      year %in% leapyrs & julian_day < 183 ~ julian_day + 184,
      year %in% leapyrs                    ~ julian_day - 182,
      TRUE                                 ~ broodyr_day
    )
  )

# Assign brood year (Jul 1 onwards = start of BY; see RBDD compendium report)
juv_rbd <- juv_rbd |>
  mutate(BY = ifelse(month < 7, year - 1, year))

# ------------------------------------------------------------------------------
# 4. Flow Data Processing
# ------------------------------------------------------------------------------

# Read in daily flow and temperature data for USGS 11390500 SACRAMENTO R BL WILKINS SLOUGH
flow <- read.csv("outputs/daily_flow_temp.csv")

# Define years of interest (with adequate sample sizes)
yrs_of_interest <- c(2004, 2005, 2006, 2012, 2013, 2014)

# Flag days exceeding 400 cms (exceedance value from Del Rosario et al.)
flow <- flow |>
  mutate(greater_than_400 = if_else(flow_cms >= 400, "y", "n"))

# Find first date per BY that flow exceeded 400 cms
flow_exceed <- flow |>
  filter(greater_than_400 == "y", BY %in% yrs_of_interest) |>
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

# Plot aesthetics
habitats <- c("SAC", "LAS", "Unassigned", "AME", "DEL")
yrs_factor <- c("2004", "2005", "2006", "2012", "2013", "2014")
cols <- c("grey60", "tomato2", "grey20", "turquoise", "turquoise4")
labs <- c("Sacramento River", "Lassen Tributaries", "Habitat X", "American River", "Feather River/Delta")

# Reorder factor levels (upstream to downstream, reversed for coord_flip)
mean_fw_growth$Habitat    <- factor(mean_fw_growth$Habitat, levels = rev(habitats))
mean_fw_growth$Brood_year <- factor(mean_fw_growth$Brood_year, levels = rev(yrs_factor))

# Plot A: Mean proportion of FW growth per habitat by brood year
plot_a <- mean_fw_growth |>
  filter(!Brood_year %in% c(2011, 2015)) |>
  ggplot(aes(x = Brood_year, y = mean_prop_fw_growth, fill = Habitat)) +
  geom_bar(color = "black", linewidth = 0.4, stat = "identity", position = "fill") +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    panel.spacing.x = unit(0.2, "lines"),
    legend.position = "top"
  ) +
  labs(x = "Brood Year", y = "Mean proportion of freshwater growth") +
  coord_flip() +
  scale_fill_manual(values = rev(cols), labels = rev(labs)) +
  guides(fill = guide_legend(reverse = TRUE, ncol = 2))

# Set coefficient to scale the secondary y-axis for dual-axis plot
coeff <- 2

# Prepare flow exceedance data with mean flow labels for annotation
flow_exceed_with_labels <- flow_exceed |>
  left_join(flow_8.1 |> select(BY, av_flow8.1), by = "BY") |>
  mutate(label_text = paste0(round(av_flow8.1, 0), " m3 s-1"))

# Plot B: Hydrographs with juvenile passage overlaid per brood year
plot_b <- ggplot(filter(flow_passage, BY %in% yrs_of_interest), aes(x = broodyr_day)) +
  geom_bar(aes(y = passage / 1000), stat = "identity", width = 1,
           fill = "darkorange3", alpha = 0.9) +
  geom_line(aes(y = flow_cms / coeff), linewidth = 1.1) +
  geom_vline(data = flow_exceed_with_labels, aes(xintercept = broodyr_day),
             colour = "grey22", linetype = "dashed") +
  geom_text(data = flow_exceed_with_labels, aes(label = label_text),
            x = 40, y = 390, fontface = "italic") +
  facet_wrap(~BY, ncol = 1) +
  scale_y_continuous(
    name = "Daily passage in thousands",
    sec.axis = sec_axis(~ . * coeff, name = expression("Mean daily flow (m"^3~"s"^{-1}*")"))
  ) +
  scale_x_continuous(
    limits = c(1, 366),
    breaks = c(1, 62, 122, 183, 243, 304, 366),
    labels = c("Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul")
  ) +
  labs(x = "Month") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    text = element_text(size = 15),
    strip.text.x = element_blank()
  )

# ------------------------------------------------------------------------------
# 6. Combine and Export
# ------------------------------------------------------------------------------

main_fig <- egg::ggarrange(plot_a, plot_b, ncol = 2, widths = c(1, 1.1))

ggsave(
  filename = "figures/Fig5_fw_growth_passage_flow.jpg",
  plot     = main_fig,
  width    = 22,
  height   = 17,
  dpi      = 300,
  units    = "cm"
)
