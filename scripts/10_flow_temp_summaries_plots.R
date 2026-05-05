# This script is to calculate mean annual flow and temp in the Sac for the Aug-May emigration period

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(readxl, dplyr, lubridate, ggplot2, ggrepel)


# Read in temp and flow data for USGS 11390500 SACRAMENTO R BL WILKINS SLOUGH downloaded Sept 8, 2020
#source  = read_xlsx('data/Sac_flow_temp_USGS11390500_2000to21.xlsx')
df = read_xlsx('data/Sac_flow_temp_USGS11390500_2000to21.xlsx', sheet = 'forR')

## Clean up dates
df$julian_day = yday(as.Date(df$datetime, origin = "1899-12-30")) 
df$year = year(as.Date(df$datetime, origin = "1899-12-30"))
df$month = month(as.Date(df$datetime, origin = "1899-12-30"))

# convert to cms
hist(df$mean_daily_flow_cfs) #nothing looks anomalous
df$flow_cms = df$mean_daily_flow_cfs/35.314666212661

# add days since Jul 1 (first day of BY)
df$broodyr_day = df$julian_day-181
df$broodyr_day[df$julian_day < 182] = df$julian_day[df$julian_day < 182]+184

# slightly different for leap yrs
leapyrs = c(2000, 2004, 2008, 2012, 2016)
df$broodyr_day[df$year %in% leapyrs] = df$julian_day[df$year %in% leapyrs]-182
df$broodyr_day[df$year %in% leapyrs & df$julian_day < 183] = df$julian_day[df$year %in% leapyrs & df$julian_day < 183]+184

# ADD BROOD YEAR
df$BY = df$year 
df$BY[df$month < 7] = df$year[df$month < 7] - 1 #July 1 onwards = start of BY (see RBDD compendium report)

# Keep bare minimum cols
df <- df %>% select(datetime, julian_day, broodyr_day, month, year, BY, flow_cms, max_daily_temp_C, median_daily_temp_C)

# define years of interest
yrs = c(2004, 2005, 2006, 2011, 2012, 2013, 2014, 2015)

### ASSUME AUG TO JAN PEAK EMIGRATION PERIOD TO DEFINE DROUGHT/NON DROUGHT
#Emigration months
df = df %>%
  mutate(emigration_period = case_when(
    month >=8 ~ 'y',
    month <=1 ~ 'y',
    TRUE ~ 'n'))

#Create summary table
summary_AugtoJan_only = df %>%
  filter(BY %in% yrs & emigration_period == 'y') %>%
  group_by(BY) %>%
  summarise(mean_max_daily_temp_AugtoJan = mean(max_daily_temp_C, na.rm = T),
            median_max_daily_temp_AugtoJan = median(max_daily_temp_C, na.rm = T),
            sd_max_daily_temp_AugtoJan = sd(max_daily_temp_C, na.rm = T),
            mean_mean_daily_flow_AugtoJan = mean(flow_cms, na.rm = T),
            median_mean_daily_flow_AugtoJan = median(flow_cms, na.rm = T),
            sd_mean_daily_flow_AugtoJan = sd(flow_cms, na.rm = T))

# Export both dfs
write.csv(summary_AugtoJan_only, 'outputs/flow_temp_stats_AugtoJan_only.csv')

write.csv(df, "outputs/daily_flow_temp.csv")

##------------PLOT HYDROGRAPHS FOR EACH RETURN YEAR (RY)----------------------------

ry07 = ggplot(filter(df, BY %in% c(2003, 2004, 2005)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "A  |  Return year 2007", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry07

ry08 = ggplot(filter(df, BY %in% c(2004, 2005, 2006)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "B  |  Return year 2008", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry08

ry09 = ggplot(filter(df, BY %in% c(2005, 2006, 2007)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "C  |  Return year 2009", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry09

ry15 = ggplot(filter(df, BY %in% c(2011, 2012, 2013)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "D  |  Return year 2015", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry15

ry16 = ggplot(filter(df, BY %in% c(2012, 2013, 2014)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "E  |  Return year 2016", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry16

ry17 = ggplot(filter(df, BY %in% c(2013, 2014, 2015)), aes(x=broodyr_day, y=flow_cms, 
                                                           color = factor(BY)))+
  geom_line(size=1.1)+theme_classic(base_size = 15)+ylim(0,1000) +
  labs(x="Days since July 1", y = expression("Sacramento River flow (m"^3~"s"^{-1}*")"), color = "Brood year")+
  annotate("text", label = "F  |  Return year 2017", x = 4, y = 1000, size = 5, hjust=0, fontface="bold")+
  scale_color_manual(values = c("#009E73", "#56B4E9", "#E69F00")); ry17

composite = cowplot::plot_grid(ry07, ry08, ry09, ry15, ry16, ry17, nrow=3)

ggsave('figures/FigS2_flows_per_year.jpg', composite, dpi = 600, height = 30, width = 25, units = 'cm')