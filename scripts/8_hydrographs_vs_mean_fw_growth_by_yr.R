# This script is to look at mean proportion of FW growth in different habitats vs 
# flow and juvenile abundance over the season

# Clear workspace
rm(list=ls())

#Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, tidyverse, lubridate, egg)

# Read in mean % FW growth by brood year    
mean_fw_growth = read.csv('outputs/summary_stats_fw_growth.csv')

##---------------JUV PASSAGE DATA ------------------------------------------
## Read in juvenile RST passage data obtained from Sac Pass 04/01/2021
juv04 <- read.csv('data/redbluffdaily_1609788856_999-2004.csv')#, na.strings = "--")
juv05 <- read.csv('data/redbluffdaily_1609788890_111-2005.csv')#, na.strings = "--")
juv06 <- read.csv('data/redbluffdaily_1609788903_675-2006.csv')#, na.strings = "--")
juv07 <- read.csv('data/redbluffdaily_1609788913_979-2007.csv')#, na.strings = "--")
juv12 <- read.csv('data/redbluffdaily_1609788962_73-2012.csv')#, na.strings = "--")
juv13 <- read.csv('data/redbluffdaily_1609788972_925-2013.csv')#, na.strings = "--")
juv14 <- read.csv('data/redbluffdaily_1609788981_630-2014.csv')#, na.strings = "--")
juv15 <- read.csv('data/redbluffdaily_1609788990_951-2015.csv')#, na.strings = "--")

# Combine data
juv_rbd <- rbind(juv04, juv05, juv06, juv07, juv12, juv13, juv14, juv15) %>% 
  select(Date, Winter.Chinook.Passage.Estimate) %>%
  mutate(passage = as.numeric(Winter.Chinook.Passage.Estimate))

# Add day column
juv_rbd$julian_day = yday(as.Date(as.character(juv_rbd$Date), format="%Y-%m-%d")) 
juv_rbd$year = year(as.Date(as.character(juv_rbd$Date), format="%Y-%m-%d")) 
juv_rbd$month = month(as.Date(as.character(juv_rbd$Date), format="%Y-%m-%d")) 

# remove empty rows
juv_rbd <- filter(juv_rbd, !is.na(julian_day))

# add days since Jul 1 (first day of BY)
juv_rbd$broodyr_day = juv_rbd$julian_day-181
juv_rbd$broodyr_day[juv_rbd$julian_day < 182] = juv_rbd$julian_day[juv_rbd$julian_day < 182]+184

# slightly different for leap yrs
leapyrs = c(2000, 2004, 2008, 2012, 2016)
juv_rbd$broodyr_day[juv_rbd$year %in% leapyrs] = juv_rbd$julian_day[juv_rbd$year %in% leapyrs]-182
juv_rbd$broodyr_day[juv_rbd$year %in% leapyrs & juv_rbd$julian_day < 183] = juv_rbd$julian_day[juv_rbd$year %in% leapyrs & juv_rbd$julian_day < 183]+184

# Add brood year
juv_rbd$BY = juv_rbd$year 

#July 1 onwards = start of BY (see RBDD compendium report)
juv_rbd$BY[juv_rbd$month < 7] = juv_rbd$year[juv_rbd$month < 7] - 1 


###----------------------------FLOW DATA-----------------------------------

## Read in daily temp and flow data for USGS 11390500 SACRAMENTO R BL WILKINS SLOUGH downloaded Sept 8, 2020
flow = read.csv("outputs/daily_flow_temp.csv")

# which days were greater than 400 cms? (Exceedance value from Del Rosario paper)
flow$greater_than_400 = "n"; flow$greater_than_400[flow$flow_cms >= 400] = "y"

# define years of interest (with OK sample sizes)
yrs = c(2004, 2005, 2006, 2012, 2013, 2014)

# make table of first date per BY that the flow exceeded 400cms
flow_exceed = flow %>%
  filter(greater_than_400 == "y", BY %in% yrs) %>%
  group_by(BY) %>%
  slice(which.min(broodyr_day)); flow_exceed

# Annual flow stats for Aug-Jan
flow_8.1 = flow %>%
  filter(month %in% c(8,9,10,11,12,1),
         BY %in% yrs) %>%
  group_by(BY) %>%
  summarize(av_flow8.1 = mean(flow_cms, na.rm = TRUE)) %>% 
  # Add in flow exceedance day
  left_join(., flow_exceed[c('BY', 'broodyr_day')]); flow_8.1

#write flow summary df
write.csv(flow_8.1, 'outputs/mean_aug_jan_flows.csv')

## combine flow and passage daily data
flow_passage = left_join(flow, juv_rbd)


##------------------PLOT -------------------------------------

# Left hand side = Mean proportion FW growth in diff habitats by BY.
habitats = c("SAC", "LAS", "Unassigned", "AME", "DEL")
yrs = c("2004",  "2005" , "2006", "2012", "2013", "2014")
cols = c("grey60","tomato2", "grey20", "turquoise","turquoise4")
labs = c("Sacramento River", "Lassen Tributaries", "Habitat X", "American River", "Feather River/Delta")

# reorder Habitat levels so upstream to downstream for the non-natal habitats as have to reverse it when I do coord_flip.
mean_fw_growth$Habitat <- factor(mean_fw_growth$Habitat, levels = rev(habitats))
mean_fw_growth$Brood_year <- factor(mean_fw_growth$Brood_year, levels = rev(yrs)) 

# PLOT IT
a = mean_fw_growth %>%
  filter(!Brood_year==2011 & !Brood_year==2015) %>%
  ggplot(aes(x = Brood_year, y = mean_prop_fw_growth, fill = Habitat)) + 
  geom_bar(color = "black", size = .4, stat = "identity", position = "fill") +
  theme_bw() +
  theme(text = element_text(size=15),
        panel.spacing.x=unit(0.2, "lines"),
        legend.position = "top") + 
  xlab('Brood Year') + ylab('Mean proportion of freshwater growth')+
  coord_flip() + 
  scale_fill_manual(values = rev(cols), 
                    labels=rev(labs)) +
  guides(fill = guide_legend(reverse=TRUE,  ncol = 2)) ;  a  

##----- Right hand side = flow and passage -----------

# set coefficient to scale the 2nd y axis so we can have 2 axes on a ggplot
coeff = 2

# COMBINED PLOT    
b = ggplot(filter(flow_passage, BY %in% yrs), aes(x = broodyr_day)) +
  geom_bar(aes(y=passage/1000), stat="identity", width = 1, fill="darkorange3", alpha=.9) + 
  geom_line(aes(y = flow_cms/coeff), size=1.1) + 
  theme_bw(base_size = 12) +
  facet_wrap(~BY, ncol=1) +#, scales = "free_y") +
  scale_y_continuous(
    # Features of the first axis
    name = "Daily passage in thousands",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Mean daily flow (cms)")) + 
  geom_vline(data=filter(flow_exceed, BY=="2004"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  geom_vline(data=filter(flow_exceed, BY=="2005"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  geom_vline(data=filter(flow_exceed, BY=="2006"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  geom_vline(data=filter(flow_exceed, BY=="2012"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  geom_vline(data=filter(flow_exceed, BY=="2013"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  geom_vline(data=filter(flow_exceed, BY=="2014"), aes(xintercept=broodyr_day), colour="grey22", linetype = "dashed") + 
  #geom_hline(yintercept = 400, color = "black", alpha = .8, linetype = "dashed") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=15), 
        strip.text.x = element_blank()) +
  xlab("Month") + #ylab("Mean daily flow (cms)") +
  scale_x_continuous(limits=c(1, 366),
                     breaks=c(1, 62,122,183,243,304,366),
                     labels=c("Jul","Sep","Nov","Jan","Mar","May", "Jul"))  + 
  geom_text(data = filter(flow_exceed, BY=="2004"),label = paste("", round(flow_8.1[1,2],0), "cms"), x = 40, y = 390, fontface = "italic") +
  geom_text(data = filter(flow_exceed, BY=="2005"),label = paste("", round(flow_8.1[2,2],0), "cms"), x = 40, y = 390, fontface = "italic") +
  geom_text(data = filter(flow_exceed, BY=="2006"),label = paste("", round(flow_8.1[3,2],0), "cms"), x = 40, y = 390, fontface = "italic") +
  geom_text(data = filter(flow_exceed, BY=="2012"),label = paste("", round(flow_8.1[4,2],0), "cms"), x = 40, y = 390, fontface = "italic") +
  geom_text(data = filter(flow_exceed, BY=="2013"),label = paste("", round(flow_8.1[5,2],0), "cms"), x = 40, y = 390, fontface = "italic") +
  geom_text(data = filter(flow_exceed, BY=="2014"),label = paste("", round(flow_8.1[6,2],0), "cms"), x = 40, y = 390, fontface = "italic") ; b


##-------------------------------
## COMBINE INTO A SINGLE PLOT THEN SAVE

main_fig = egg::ggarrange(a,b, ncol=2, widths = c(1,1.1))
ggsave(file="figures/Fig7_fw_growth_passage_flow.jpg", main_fig, width = 22, height = 17, dpi = 300, units = "cm")