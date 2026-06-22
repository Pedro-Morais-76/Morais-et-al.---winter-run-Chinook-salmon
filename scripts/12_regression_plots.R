#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script performs regression analyses to estimate relationships between non-natal 
#' rearing (frequency by rearing type and proportion of freshwater growth) versus 
#' upper Sacramento River flow and juvenile abundance (density dependence).
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
rm(list = ls())

# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggrepel)
library(cowplot)

# ------------------------------------------------------------------------------
# 2. Data Loading and Preparation
# ------------------------------------------------------------------------------

# Define years of interest (brood years with adequate sample sizes)
yrs <- c(2004, 2005, 2006, 2012, 2013, 2014)

# Read in mean Sacramento flows for Aug-Jan of each brood year
flow_8.1 <- read.csv("outputs/mean_aug_jan_flows.csv")

# Read in rearing categories and filter for Sacramento River (SAC)
rearing_type_summary <- read.csv("outputs/rearing_type_by_BY.csv") |>
  filter(rearing_type == "SAC") |>
  mutate(prop_non_nat_rearers = 1 - freq) |>
  select(-rearing_type, -n)

# Create combined data table for regression analyses
annual_df <- left_join(
  flow_8.1, 
  rearing_type_summary |> select(Brood_year, tot, prop_non_nat_rearers), 
  by = c("BY" = "Brood_year")
)

# Read in mean annual freshwater growth
mean_fw_growth <- read.csv("outputs/summary_stats_fw_growth.csv")

# Estimate total fraction of FW growth assimilated in any non-natal habitat
tot_non_nat_rearing <- mean_fw_growth |>
  filter(Brood_year %in% yrs, !Habitat %in% c("SAC")) |>
  group_by(Brood_year) |>
  summarize(tot_non_natal = sum(mean_prop_fw_growth), .groups = "drop")

# Estimate total fraction of FW growth assimilated in a downstream non-natal habitat
down_rearing <- mean_fw_growth |>
  filter(Brood_year %in% yrs, !Habitat %in% c("SAC", "Unassigned", "LAS")) |>
  group_by(Brood_year) |>
  summarize(prop_fw_growth_down = sum(mean_prop_fw_growth), .groups = "drop")

# Add rearing estimates to the combined dataframe
annual_df <- annual_df |>
  left_join(tot_non_nat_rearing, by = c("BY" = "Brood_year")) |>
  left_join(down_rearing, by = c("BY" = "Brood_year"))

# Read in total juvenile production per year (JPI estimate)
juv_production <- read.csv("data/RBDD_RST_Juv_Production.csv")

# Add juvenile production to the combined dataframe
annual_df <- left_join(annual_df, juv_production, by = "BY")

# ------------------------------------------------------------------------------
# 3. Regression: Non-Natal Rearing vs. Flow
# ------------------------------------------------------------------------------

# Plot: Proportion of fish reared non-natally as a function of mean Aug-Jan flow
non_nat_plot <- ggplot(annual_df, aes(x = av_flow8.1, y = prop_non_nat_rearers, color = tot)) +
  geom_smooth(method = "glm", aes(weight = tot), fill = "grey", colour = "black", linewidth = 0.6, alpha = 0.3, fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, alpha = 0.9) + 
  geom_text_repel(
    aes(label = BY), 
    size = 3, 
    min.segment.length = Inf, 
    box.padding = 0,
    force = 0, 
    force_pull = 0,
    nudge_x = c(0, -0.1, 0, 1.4, 3, -0.7)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))],
    nudge_y = c(-0.025, -0.035, 0.03, 0.05, -0.035, -0.025)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))]
  ) +
  theme_classic(base_size = 11) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.2)) +
  scale_color_gradient(low = "orangered3", high = "slateblue4") +
  scale_x_continuous(limits = c(130, 370), breaks = seq(150, 350, by = 50)) +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(
    x = expression("Mean flow Aug-Jan (m"^3*"s"^{-1}*")"),
    y = "Fraction of individuals that reared non-natally",
    size = "Adults (n)", 
    color = "Adults (n)"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8, margin = margin(b = 2)),
    legend.spacing.y = unit(2, "pt"),
    plot.margin = margin(5, 12, 5, 5)
  )

# Add regression statistics (R2 and equation) to the plot
lm_non_nat <- lm(prop_non_nat_rearers ~ av_flow8.1, weights = tot, data = annual_df)
r2_non_nat <- format(summary(lm_non_nat)$adj.r.squared, nsmall = 3, digits = 3)
pval_non_nat <- summary(lm_non_nat)$coefficients[2, 4]
pval_non_nat_text <- if_else(pval_non_nat < 0.001, "p < 0.001", paste0("p = ", format(round(pval_non_nat, 3), nsmall = 3)))
eq_non_nat <- sprintf("y == %.3f %+.3fx", coef(lm_non_nat)[1], coef(lm_non_nat)[2])

non_nat_plot_final <- non_nat_plot +
  annotate("text", x = 135, y = 0.695, label = eq_non_nat, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 135, y = 0.662, label = paste0("r\u00b2 = ", r2_non_nat, ", ", pval_non_nat_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 365, y = 0.03, label = "A", size = 4, fontface = "bold", hjust = 1)

# ------------------------------------------------------------------------------
# 4. Regression: 'Rest Stop' Hypothesis
# ------------------------------------------------------------------------------

# Plot: Proportion of FW growth in non-natal habitats vs. flow
rest_plot <- ggplot(annual_df, aes(x = av_flow8.1, y = tot_non_natal, color = tot)) +
  geom_smooth(method = "glm", aes(weight = tot), fill = "grey", colour = "black", linewidth = 0.6, alpha = 0.3, fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, alpha = 0.9) + 
  geom_text_repel(
    aes(label = BY), 
    size = 3, 
    min.segment.length = Inf, 
    box.padding = 0,
    force = 0, 
    force_pull = 0,
    nudge_x = c(0.05, 0, -0.3, 0, 0.3, 0)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))],
    nudge_y = c(0.025, 0.032, 0.03, -0.045, -0.045, 0.025)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))]
  ) +
  theme_classic(base_size = 11) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.2)) +
  scale_color_gradient(low = "orangered3", high = "slateblue4") +
  scale_x_continuous(limits = c(130, 370), breaks = seq(150, 350, by = 50)) +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(
    x = expression("Mean flow Aug-Jan (m"^3*"s"^{-1}*")"),
    y = "Mean fraction of growth in non-natal habitats",
    size = "Adults (n)", 
    color = "Adults (n)"
  ) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.spacing.y = unit(2, "pt"),
    plot.margin = margin(5, 12, 5, 5)
  ) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2, title = NULL, override.aes = list(colour = "grey50")))

# Add regression statistics to the plot
lm_rest <- lm(tot_non_natal ~ av_flow8.1, weights = tot, data = annual_df)
r2_rest <- format(summary(lm_rest)$adj.r.squared, nsmall = 3, digits = 3)
pval_rest <- summary(lm_rest)$coefficients[2, 4]
pval_rest_text <- if_else(pval_rest < 0.001, "p < 0.001", paste0("p = ", format(round(pval_rest, 3), nsmall = 3)))
eq_rest <- sprintf("y == %.3f %+.3fx", coef(lm_rest)[1], coef(lm_rest)[2])

rest_plot_final <- rest_plot +
  annotate("text", x = 135, y = 0.695, label = eq_rest, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 135, y = 0.662, label = paste0("r\u00b2 = ", r2_rest, ", ", pval_rest_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 365, y = 0.03, label = "B", size = 4, fontface = "bold", hjust = 1)

# Combine non-natal and rest stop plots
fig6 <- cowplot::plot_grid(non_nat_plot_final, rest_plot_final, ncol = 2, rel_widths = c(1, 1.4))

ggsave(filename = "figures/Fig4_rest_stop.jpg", plot = fig6, width = 15, height = 7.5, dpi = 300, units = "cm")

# ------------------------------------------------------------------------------
# 5. Downstream Occupancy Regression (Cues)
# ------------------------------------------------------------------------------

# Hypothesis: Negative relationship between date of first freshet and downstream growth
cues_plot <- ggplot(annual_df, aes(x = broodyr_day, y = prop_fw_growth_down, color = av_flow8.1)) + 
  geom_smooth(method = "glm", fill = "grey", colour = "black", linewidth = 0.6, alpha = 0.3, aes(weight = tot), fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, fill = "black", alpha = 0.65) + 
  geom_text_repel(
    aes(label = BY), 
    size = 3, 
    min.segment.length = Inf, 
    box.padding = 0,
    nudge_x = c(0, 0, 0.3, 1.32, -1.75, 4.71)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))],
    nudge_y = c(-0.02, 0.032, 0.03, -0.034, -0.035, -0.026)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))]
  ) +
  theme_classic(base_size = 11) +
  labs(
    x = expression("First day >400 m"^3*"s"^{-1}), 
    y = "Mean fraction of growth in downstream habitats",
    size = "Adults (n)"
  ) + 
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) +
  scale_x_continuous(limits = c(150, 230), breaks = seq(150, 230, by = 20)) +
  ylim(0.0, 0.6) +
  scale_color_gradient(low = "orangered3", high = "slateblue4")

# Add regression statistics to cues plot
lm_cues <- lm(prop_fw_growth_down ~ broodyr_day, weights = tot, data = annual_df)
r2_cues <- format(summary(lm_cues)$adj.r.squared, nsmall = 3, digits = 3)
pval_cues <- summary(lm_cues)$coefficients[2, 4]
pval_cues_text <- if_else(pval_cues < 0.001, "p < 0.001", paste0("p = ", format(round(pval_cues, 3), nsmall = 3)))
eq_cues <- sprintf("y = %.3f %s %.3fx", coef(lm_cues)[1], if_else(coef(lm_cues)[2] >= 0, "+", "−"), abs(coef(lm_cues)[2]))

cues_plot_final <- cues_plot +
  annotate("text", x = 155.6, y = 0.595, label = eq_cues, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 155.6, y = 0.562, label = paste0("r\u00b2 = ", r2_cues, ", ", pval_cues_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 228, y = 0.03, label = "A", size = 4, fontface = "bold", hjust = 1)

# ------------------------------------------------------------------------------
# 6. Regression: Density-Dependent Movement
# ------------------------------------------------------------------------------

# Hypothesis: Positive relationship between juvenile abundance and downstream movement
annual_df$JPI_mill <- annual_df$Fry_equiv_JPI / 1000000

juv_abundance_plot <- ggplot(annual_df, aes(x = JPI_mill, y = prop_fw_growth_down, color = av_flow8.1)) + 
  geom_smooth(method = "glm", fill = "grey", colour = "black", linewidth = 0.6, alpha = 0.3, aes(weight = tot), fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, fill = "black", alpha = 0.65) + 
  geom_text_repel(
    aes(label = BY), 
    size = 3, 
    min.segment.length = Inf, 
    box.padding = 0,
    nudge_x = c(1.4, -0.06, -0.5, 1.1, 0.005, -0.04)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))],
    nudge_y = c(0, 0.03, 0.03, 0.04, -0.032, -0.031)[match(annual_df$BY, c(2004, 2005, 2006, 2012, 2013, 2014))]
  ) +
  theme_classic(base_size = 11) +
  xlim(0.0, 10) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) +
  scale_color_gradient(low = "orangered3", high = "slateblue4") +
  labs(
    x = "Juvenile production (millions)", 
    y = " ",
    size = "Adults (n)",
    color = expression("Mean Aug-Jan flow (m"^3*"s"^{-1}*")")
  )

# Add regression statistics to the plot
lm_juv <- lm(prop_fw_growth_down ~ JPI_mill, weights = tot, data = annual_df)
r2_juv <- format(summary(lm_juv)$adj.r.squared, nsmall = 3, digits = 3)
pval_juv <- summary(lm_juv)$coefficients[2, 4]
pval_juv_text <- if_else(pval_juv < 0.001, "p < 0.001", paste0("p = ", format(round(pval_juv, 3), nsmall = 3)))
eq_juv <- sprintf("y = %.3f %s %.3fx", coef(lm_juv)[1], if_else(coef(lm_juv)[2] >= 0, "+", "−"), abs(coef(lm_juv)[2]))

juv_abundance_plot_final <- juv_abundance_plot +
  annotate("text", x = 0.7, y = 0.595, label = eq_juv, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 0.7, y = 0.562, label = paste0("r\u00b2 = ", r2_juv, ", ", pval_juv_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 9.7, y = 0.03, label = "B", size = 4, fontface = "bold", hjust = 1)

# Combine cues and abundance plots into Figure 6
fig6 <- cowplot::plot_grid(cues_plot_final, juv_abundance_plot_final, ncol = 2, rel_widths = c(1, 1.52))

ggsave(filename = "figures/Fig6_downstream_occupancy.jpg", plot = fig6, width = 15, height = 7.5, dpi = 300, units = "cm")

# ------------------------------------------------------------------------------
# 7. Final Combined Model
# ------------------------------------------------------------------------------

# Linear model for downstream occupancy prediction combining both terms
lm_combined <- lm(prop_fw_growth_down ~ JPI_mill + broodyr_day, weights = tot, data = annual_df)
summary(lm_combined)
