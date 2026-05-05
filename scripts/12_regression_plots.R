# This script is to estimate relationships between non-natal rearing (freq by rearing type and prop of FW growth)
# vs upper Sac flow and juvenile abundance (density dependence)

# Clear workspace
rm(list=ls())

#Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, tidyverse, rsq, ggrepel)

#-------------------------------

# define years of interest (brood years with OK sample sizes)
yrs = c(2004, 2005, 2006, 2012, 2013, 2014)

# Read in mean Sac flows for Aug-Jan of each BY
flow_8.1 = read.csv("outputs/mean_aug_jan_flows.csv")

# Read in rearing categories
rearing_type_summary = read.csv('outputs/rearing_type_by_BY.csv') %>%
  filter(rearing_type == "SAC") %>%
  mutate(prop_non_nat_rearers = 1-freq) %>%
  select(-rearing_type, -n)

# Create combined data table for regression analyses
annual_df = left_join(flow_8.1, 
                      rearing_type_summary[c("Brood_year", "tot", "prop_non_nat_rearers")], 
                      by = c("BY" = "Brood_year"))

# Read in mean annual fw growth
mean_fw_growth = read.csv('outputs/summary_stats_fw_growth.csv')

# estimate total fraction of FW growth assimilated in any non natal habitat
tot_non_nat_rearing = mean_fw_growth %>%
  filter(Brood_year %in% yrs, !Habitat %in% c("SAC")) %>%
  group_by(Brood_year) %>%
  summarize(tot_non_natal = sum(mean_prop_fw_growth))

# estimate total fraction of FW growth assimilated in a downstream non natal habitat
down_rearing = mean_fw_growth %>%
  filter(Brood_year %in% yrs, !Habitat %in% c("SAC", "Unassigned", "LAS")) %>%
  group_by(Brood_year) %>%
  summarize(prop_fw_growth_down = sum(mean_prop_fw_growth))

#add to combined df
annual_df = left_join(annual_df, tot_non_nat_rearing, by = c("BY" = "Brood_year"))
annual_df = left_join(annual_df, down_rearing, by = c("BY" = "Brood_year"))

# read in total juvenile production per year from FWS (JPI estimate)
juv_production = read.csv('data/RBDD_RST_Juv_Production.csv')

#add to combined df
annual_df = left_join(annual_df, juv_production)

##------------------------------------------------------------------------------------------

# PROPORTION OF FISH EACH YEAR ASSIGNED TO A REARING TYPE OTHER THAN SAC (i.e. REARED NON-NATALLY)
# AS A FUNCTION OF MEAN AUG-APR FLOW

non_nat = ggplot(annual_df, aes(x = av_flow8.1, y = prop_non_nat_rearers, color = tot))+
  geom_smooth(method = "glm", aes(weight = tot), fill="grey", colour="black", size=0.6, alpha = 0.3,
              fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, alpha = 0.9) + 
  geom_text_repel(aes(label=BY), size = 3, min.segment.length = Inf, box.padding = 0.0,
                  force = 0, force_pull = 0,
                  nudge_x = case_when(annual_df$BY == 2004 ~ 0, annual_df$BY == 2005 ~ -0.1, annual_df$BY == 2006 ~ 0,
                                      annual_df$BY == 2012 ~ 1.4, annual_df$BY == 2013 ~ 3, annual_df$BY == 2014 ~ -0.7,
                                      TRUE ~ 0),
                  nudge_y = case_when(annual_df$BY == 2004 ~ -0.025, annual_df$BY == 2005 ~ -0.035, annual_df$BY == 2006 ~ 0.035,
                                      annual_df$BY == 2012 ~ 0.05, annual_df$BY == 2013 ~ -0.035, annual_df$BY == 2014 ~ -0.025,
                                      TRUE ~ 0)) +
  theme_classic(base_size = 11) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.2)) +
  scale_color_gradient(low = "orangered3", high = "slateblue4") +
  scale_x_continuous(limits = c(130, 370), breaks = seq(150, 350, by = 50)) +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(x = expression("Mean flow Aug-Jan (m"^3*"s"^{-1}*")"),
       y = "Fraction of individuals that reared non-natally",
       size = "Adults (n)", color = "Adults (n)") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, margin = margin(b = 2)),
        legend.spacing.y = unit(2, "pt"),
        plot.margin = margin(5, 12, 5, 5))

# add r2 and equation to plot
lm <- lm(prop_non_nat_rearers ~ av_flow8.1, weight = tot, data = annual_df)
summary(lm)
# plot(resid(lm))

r2_non_nat = format(rsq(lm,adj=TRUE), nsmall = 3, digits = 3)
pval_non_nat = summary(lm)$coefficients[2, 4]
pval_non_nat_text = ifelse(pval_non_nat < 0.001, "p< 0.001", paste0("p= ", format(round(pval_non_nat, 3), nsmall = 3)))
eq_non_nat = sprintf("y == %.3f %+.3f * x", coef(lm)[1], coef(lm)[2])
non_nat2 = non_nat +
  annotate("text", x = 135, y = 0.695, label = eq_non_nat, parse = TRUE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 135, y = 0.662, label = paste0("r\u00b2= ", r2_non_nat, ", ", pval_non_nat_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 365, y = 0.03, label = "A", size = 4, fontface = "bold", hjust = 1)

##----------------- PROPORTION OF FW GROWTH IN NON NATAL HABITATS ('Rest stop hypothesis') -------------

rest = ggplot(annual_df, aes(x = av_flow8.1, y = tot_non_natal, color = tot))+
  geom_smooth(method = "glm", aes(weight = tot), fill="grey", colour="black", size=0.6, alpha = 0.3,
              fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, alpha = 0.9) + 
  geom_text_repel(aes(label=BY), size = 3, min.segment.length = Inf, box.padding = 0.0,
                  force = 0, force_pull = 0,
                  nudge_x = case_when(annual_df$BY == 2004 ~ 0.05, annual_df$BY == 2005 ~ 0, annual_df$BY == 2006 ~ -0.3,
                                      annual_df$BY == 2012 ~ 0, annual_df$BY == 2013 ~ 0.3, annual_df$BY == 2014 ~ 0,
                                      TRUE ~ 0),
                  nudge_y = case_when(annual_df$BY == 2004 ~ 0.025, annual_df$BY == 2005 ~ 0.032, annual_df$BY == 2006 ~ 0.03,
                                      annual_df$BY == 2012 ~ -0.045, annual_df$BY == 2013 ~ -0.045, annual_df$BY == 2014 ~ 0.025,
                                      TRUE ~ 0)) +
  theme_classic(base_size = 11) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.2)) +
  scale_color_gradient(low = "orangered3", high = "slateblue4") +
  scale_x_continuous(limits = c(130, 370), breaks = seq(150, 350, by = 50)) +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(x = expression("Mean flow Aug-Jan (m"^3*"s"^{-1}*")"),
       y = "Mean fraction of growth in non-natal habitats",
       size = "Adults (n)", color = "Adults (n)") +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.spacing.y = unit(2, "pt"),
        plot.margin = margin(5, 12, 5, 5)) +
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2, title = NULL, override.aes = list(colour = "grey50")))

# add r2 and equation to plot
lm <- lm(tot_non_natal ~ av_flow8.1, weights = tot, data = annual_df)
summary(lm)
# plot(resid(lm))

r2_rest = format(rsq(lm,adj=TRUE), nsmall = 3, digits = 3)
pval_rest = summary(lm)$coefficients[2, 4]
pval_rest_text = ifelse(pval_rest < 0.001, "p< 0.001", paste0("p= ", format(round(pval_rest, 3), nsmall = 3)))
eq_rest = sprintf("y == %.3f %+.3f * x", coef(lm)[1], coef(lm)[2])
rest2 = rest +
  annotate("text", x = 135, y = 0.695, label = eq_rest, parse = TRUE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 135, y = 0.662, label = paste0("r\u00b2= ", r2_rest, ", ", pval_rest_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 365, y = 0.03, label = "B", size = 4, fontface = "bold", hjust = 1)

#COMBINING BOTH PLOTS INTO ONE
fig6 = cowplot::plot_grid(non_nat2, rest2, ncol=2,
                          rel_widths = c(1,1.4))

ggsave(file="figures/Fig6_rest_stop.jpg", 
       fig6, width = 15, height = 7.5, dpi = 300, units = "cm")


##------------------- DOWWNSTREAM OCCUPANCY --------------------------------------
# downstream nonnatal habitats (FEA/DEL, AME) used more when cues come earlier
# Based on del Rosario et al (2013) ("mainstem Sacramento River flow threshold of 400 m3 s-1 triggers abrupt and substantial winter-run migration into the Delta at Knights Landing") we hypothesize that there will be a negative relationship between the date of the first freshet and the proportion of freshwater growth in downstream non-natal habitats (below Knights Landing, i.e. DEL/FEA & AME combined). 

cues = ggplot(annual_df, aes(x = broodyr_day, y = prop_fw_growth_down, color = av_flow8.1)) + 
  geom_smooth(method = "glm", fill="grey", colour="black", size=0.6, alpha = 0.3,
              aes(weight = tot), fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, fill = "black", alpha = 0.65) + 
  geom_text_repel(aes(label=BY), size = 3, min.segment.length = Inf, box.padding = 0,
                  nudge_x = case_when(annual_df$BY == 2004 ~ 0, annual_df$BY == 2005 ~ 0, annual_df$BY == 2006 ~ 0.3,
                                      annual_df$BY == 2012 ~ 1.32, annual_df$BY == 2013 ~ -1.75, annual_df$BY == 2014 ~ 4.71,
                                      TRUE ~ 0),
                  nudge_y = case_when(annual_df$BY == 2004 ~ -0.02, annual_df$BY == 2005 ~ 0.032, annual_df$BY == 2006 ~ 0.03,
                                      annual_df$BY == 2012 ~ -0.034, annual_df$BY == 2013 ~ -0.035, annual_df$BY == 2014 ~ -0.026,
                                      TRUE ~ 0))  +
  theme_classic(base_size = 11) +
  labs(x = expression("First day >400 m"^3*"s"^{-1}), 
       y = "Mean fraction of growth in downstream habitats",
       size = "Adults (n)")+ 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(150, 230), breaks = seq(150, 230, by = 20)) +
  ylim(0.0, 0.6) +
  scale_color_gradient(low = "orangered3", high = "slateblue4")

# add r2 and equation to plot
lm <- lm(prop_fw_growth_down ~ broodyr_day, weights = tot, data = annual_df)
summary(lm)
# plot(resid(lm))
r2_cues = format(rsq(lm,adj=TRUE), nsmall = 3, digits = 3)
pval_cues = summary(lm)$coefficients[2, 4]
pval_cues_text = ifelse(pval_cues < 0.001, "p< 0.001", paste0("p= ", format(round(pval_cues, 3), nsmall = 3)))
eq_cues = sprintf("y= %.3f %s %.3fx", coef(lm)[1], ifelse(coef(lm)[2] >= 0, "+", "\u2212"), abs(coef(lm)[2]))
cues2 = cues +
  annotate("text", x = 155.6, y = 0.595, label = eq_cues, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 155.6, y = 0.562, label = paste0("r\u00b2= ", r2_cues, ", ", pval_cues_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 228, y = 0.03, label = "A", size = 4, fontface = "bold", hjust = 1)


##---------------------------------------------------------
# DENSITY DEPENDENT MOVEMENT 
# Hypothesis = In years of increased juvenile production there will be increased downstream 
# movement of juveniles (as upstream habitats become saturated; Greene & Beechie, 2004; Hendrix et al., 2017) resulting in a positive relationship between juvenile abundance and the proportion of rearing in downstream habitats (American River and Feather River/Delta). 

juv = ggplot(annual_df, aes(x = Fry_equiv_JPI/1000000, y = prop_fw_growth_down, color = av_flow8.1)) + 
  geom_smooth(method = "glm", fill="grey", colour="black", size=0.6, alpha = 0.3,
              aes(weight = tot), fullrange = TRUE) +
  geom_point(aes(size = tot), shape = 16, fill = "black", alpha = 0.65) + 
  labs(x = "Juvenile production (millions)", 
       y = " ",
       size = "Adults (n)",
       color = expression("Mean Aug-Jan flow (m"^3*"s"^{-1}*")")) +
  geom_text_repel(aes(label=BY), size = 3, min.segment.length = Inf, box.padding = 0,
                  nudge_x = case_when(annual_df$BY == 2004 ~ 1.4, annual_df$BY == 2005 ~ -0.06, annual_df$BY == 2006 ~ -0.5,
                                      annual_df$BY == 2012 ~ 1.1,   annual_df$BY == 2013 ~ 0.005, annual_df$BY == 2014 ~ -0.04,
                                      TRUE ~ 0),
                  nudge_y = case_when(annual_df$BY == 2004 ~ 0, annual_df$BY == 2005 ~ 0.03, annual_df$BY == 2006 ~ 0.03,
                                      annual_df$BY == 2012 ~ 0.04, annual_df$BY == 2013 ~ -0.032, annual_df$BY == 2014 ~ -0.031,
                                      TRUE ~ 0)) +
  theme_classic(base_size = 11) +
  xlim(0.0, 10) + coord_cartesian(ylim = c(0, 0.6)) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  scale_color_gradient(low = "orangered3", high = "slateblue4")

# add r2 and equation to plot (use millions scale so equation matches x-axis)
annual_df$JPI_mill_tmp = annual_df$Fry_equiv_JPI / 1000000
lm <- lm(prop_fw_growth_down ~ JPI_mill_tmp, weights = tot, data = annual_df)
summary(lm)
# plot(resid(lm))
r2_juv = format(rsq(lm,adj=TRUE), nsmall = 3, digits = 3)
pval_juv = summary(lm)$coefficients[2, 4]
pval_juv_text = ifelse(pval_juv < 0.001, "p< 0.001", paste0("p= ", format(round(pval_juv, 3), nsmall = 3)))
eq_juv = sprintf("y= %.3f %s %.3fx", coef(lm)[1], ifelse(coef(lm)[2] >= 0, "+", "\u2212"), abs(coef(lm)[2]))
juv2 = juv +
  annotate("text", x = 0.7, y = 0.595, label = eq_juv, parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 0.7, y = 0.562, label = paste0("r\u00b2= ", r2_juv, ", ", pval_juv_text), parse = FALSE, color = "black", size = 3, hjust = 0) +
  annotate("text", x = 9.7, y = 0.03, label = "B", size = 4, fontface = "bold", hjust = 1)

#COMBINE INTO SINGLE FIG
fig8 = cowplot::plot_grid(cues2, juv2, ncol=2,
                          rel_widths = c(1, 1.52))

ggsave(file="figures/Fig8_downstream_occupancy.jpg", 
       fig8, width = 15, height = 7.5, dpi = 300, units = "cm")

##-----------------------------------------------

#Linear model for downstream occupancy prediction combining both terms in one
annual_df$JPI_mill = annual_df$Fry_equiv_JPI/1000000
lm <- lm(prop_fw_growth_down ~ JPI_mill +broodyr_day, weights = tot, data = annual_df)
summary(lm)
