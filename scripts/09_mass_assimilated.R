# This script is to estimate proportion of FW growth in terms of mass assimilated

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, forcats, RColorBrewer, wesanderson, tidyverse, lubridate)

# Read in otolith Sr8786 data
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_rearing_types.csv')[,-1]


# -------- Reconstructing total weight from otolith distance (radius) ------------
# read in fall run juvenile salmon otolith radius (OR) and TW data. OR measured on dorsal axis
or_tw = read.csv('data/juv_fall_run_oto_radius_total_weight.csv') %>%
  filter(!(OR<300 & TW>7)) #exclude one clear bad data

# strong relationship
ggplot(or_tw, aes(y=TW, x=OR))+geom_point()+geom_smooth()
ggplot(or_tw, aes(y=log(TW), x=log(OR)))+geom_point()+geom_smooth(method="lm")

#weight range of calibration fish
min(or_tw$TW); max(or_tw$TW)

# Fit linear regression
mod = lm(log(TW) ~ log(OR), data = or_tw)
summary(mod) # adj r2=.91
plot(resid(mod))
as.vector(coef(mod)[1])*3

# Estimating total otolith distance between exog and FW exit spent in each habitats per fish (i.e. to calculate % FW growth)

# order by fish and oto distance, then calculate distance between spots and exclude all ocean spots
growth_dist = wr_megafile %>%
  #reconstruct mass using otolith distance using equation above
  mutate(mass = exp((as.vector(coef(mod)[2])*(log(Distance_um)))+as.vector(coef(mod)[1]))) %>% 
  arrange(Sample_ID, mass) %>%
  group_by(Sample_ID) %>%
  mutate(mass_change = lead(mass, 1)-mass)%>%
  filter(!is.na(Habitat)) %>% # excludes core (maternal) and ocean values
  select(Sample_ID, Distance_um, mass_change, Sr8786_norm, SE2, SrV2, Habitat, Escap_yr, Brood_year, fork_length)

# what is the mean mass change per spot? 1.3g
(mean_change = mean(growth_dist$mass_change, na.rm=T))

# Assume that any spots with NA growth accrued = mean mass change (only 1 observation)
growth_dist$mass_change[is.na(growth_dist$mass_change)] = mean_change

#summing total growth in FW by fish
growth_dist_tot = growth_dist %>%
  group_by(Sample_ID) %>%
  summarize(tot_mass_change = sum(mass_change, na.rm=T))

#summing growth in each FW habitat by fish & calculating proportion of total FW growth
by_fish_prop_dist = growth_dist %>%
  group_by(Sample_ID, Habitat, Escap_yr, Brood_year, fork_length) %>%
  summarize(mass_change_sum = sum(mass_change, na.rm=T)) %>%
  left_join(., growth_dist_tot) %>%
  mutate(prop_fw_growth = mass_change_sum/tot_mass_change)

# Add column of just percent Sac so we can order barplot using this
percent_sac_df = by_fish_prop_dist %>%
  filter(Habitat == "SAC") %>%
  ungroup() %>%
  select(Sample_ID, prop_sac = prop_fw_growth)

# Add to main df
by_fish_prop_dist = left_join(by_fish_prop_dist, percent_sac_df)

# -------- CALCULATE MEAN PROP FW GROWTH BY HABITAT AND BROOD YEAR-----

# Mean proportion of growth achieved in each habitats per BY
# this pivot wider step allows us to make sure sure each habitat is represented for every fish (even if zero oto dist)
by_fish_prop_dist_wide = by_fish_prop_dist %>%
  select(Sample_ID, Brood_year, Habitat, prop_fw_growth) %>%
  pivot_wider(names_from = Habitat, values_from = prop_fw_growth) %>% 
  replace(is.na(.), 0) ; by_fish_prop_dist_wide # need to replace NAs with zero so mean values includes the zeros too

write.csv(by_fish_prop_dist_wide,'outputs/by_fish_prop_growth_wide_format.csv')

# Go back to long format but now with zeros where true zero growth there
by_fish_prop_dist_long = by_fish_prop_dist_wide %>%
  pivot_longer(cols = c('SAC', 'LAS', 'DEL', 'AME', 'Unassigned')) %>%
  rename(Habitat = name, prop_fw_growth = value) 

# Means by habitat and year
by_year_prop_stats = by_fish_prop_dist_long %>%
  group_by(Brood_year, Habitat) %>%
  summarize(mean_prop_fw_growth = mean(prop_fw_growth),
            min_prop_fw_growth = min(prop_fw_growth),
            max_prop_fw_growth = max(prop_fw_growth),
            se_prop_fw_growth = sd(prop_fw_growth)/sqrt(length(prop_fw_growth)),
            n_fish = n()); by_year_prop_stats

# Export df of summary stats
write.csv(by_year_prop_stats, 'outputs/summary_stats_fw_growth.csv')

# # write to share with corey (this what was used to calculate the annual mean values)
# write.csv(by_fish_prop_dist_long, 'Outputs/by_fish_prop_growth.csv')

# Export df
write.csv(by_fish_prop_dist, 'outputs/prop_fw_growth_by_fish.csv')


## ----- PLOT HABITAT USE BY FISH (GIVES A SENSE OF LH DIVERSITY & THE N SAMPLES PER BROOD YEAR)

# reorder Habitat levels so upstream to downstream for the non-natal habitats. 
by_fish_prop_dist$Habitat <- factor(by_fish_prop_dist$Habitat,
                                    levels = c("Unassigned", "DEL", "AME","LAS",  "SAC")) 

## plot colors matching Fig3 shaded area colors (named to avoid positional mismatches)
cols = c("Unassigned" = "grey20", "DEL" = "turquoise4", "AME" = "turquoise",
         "LAS" = "tomato2", "SAC" = "grey40")

# Helper function to make one panel per brood year
make_panel <- function(by, show_legend = FALSE, show_y = FALSE, show_y_text = TRUE) {
  by_fish_prop_dist %>%
    filter(Brood_year == by) %>%
    arrange(desc(prop_sac)) %>%
    ungroup() %>%
    mutate(Sample_ID = factor(Sample_ID, levels = unique(Sample_ID))) %>%
    ggplot(aes(x = Sample_ID, y = mass_change_sum, fill = Habitat)) +
    geom_bar(stat = "identity", position = "fill", key_glyph = "point",
             width = 1, colour = NA) +
    theme_bw() +
    xlab(paste("Brood Year", by)) +
    ylab(if (show_y) 'Proportion of freshwater growth' else '') +
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

# Build individual panels
# Row 1: left panel has y-axis ticks; center and right do not
p2004 <- make_panel(2004, show_y = FALSE, show_y_text = TRUE)
p2005 <- make_panel(2005, show_y = FALSE, show_y_text = FALSE)
p2006 <- make_panel(2006, show_y = FALSE, show_y_text = FALSE)
# Row 2: center row - show y-axis label and legend at bottom
p2012 <- make_panel(2012, show_legend = FALSE, show_y = TRUE, show_y_text = TRUE)
# Row 3: left panel has y-axis ticks; right does not
p2013 <- make_panel(2013, show_y = FALSE, show_y_text = TRUE)
p2014 <- make_panel(2014, show_y = FALSE, show_y_text = FALSE)

# Row 1: three brood years (2004, 2005, 2006)
row1 <- cowplot::plot_grid(p2004, p2005, p2006, nrow = 1, rel_widths = c(1, 1, 1))

# Row 2: brood year 2012 spanning full width
row2 <- cowplot::plot_grid(p2012, nrow = 1)

# Row 3: two brood years (2013, 2014)
row3 <- cowplot::plot_grid(p2013, p2014, nrow = 1, rel_widths = c(1, 1))

# Row 4: legend only (extracted from p2012, key size reduced by 50%)
legend_row <- cowplot::get_legend(
  p2012 + theme(legend.position = 'bottom',
                legend.key.size = unit(0.15, 'cm'))
)

# Combine rows
fig4 <- cowplot::plot_grid(row1, row2, row3, legend_row, ncol = 1,
                           rel_heights = c(1, 1, 1, 0.2))

ggsave('figures/Fig4_prop_fw_growth_by_fish.jpg', fig4, dpi = 800, height = 19, width = 15, units = 'cm')