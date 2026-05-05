# This script is to estimate FW exit distance in winter run otolith profiles using Sr8786 and SrV 

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

## read in all otolith Sr8786 data from Phillis et al. (2018) and new data
wr_megafile <- read.csv('data/oto_sr8786_dat_all_yrs.csv')[,-1]


## FW EXIT DISTANCE USING SrV ----------------------------------------------------------------------------

# Threshold = 66% from Phillis et al (2018)
SrV_FWexit = 0.66 

## Make new SrV col and estimate %SrV within fish as it's a relative proxy for Sr concentration
wr_megafile$SrV2 = wr_megafile$SrV

#excl respot SrV values as instrument performance can change over short time periods
wr_megafile$SrV2[which(wr_megafile$respot=="y")] = NA 

#excl maternal data prior to the exogenous feeding check as the yolk has elevated Sr8786
wr_megafile$SrV2[which(wr_megafile$Distance_um < wr_megafile$Exog_dist)] = NA 

# Calculate SrV percent within each fish
wr_megafile$SrVpercent = NA
wr_megafile <- wr_megafile %>% group_by(Sample_ID) %>% 
  mutate(SrVpercent = ((SrV2 - min(SrV2, na.rm = TRUE))/(max(SrV2, na.rm = TRUE) - min(SrV2, na.rm = TRUE)))) %>% 
  ungroup()

# exclude samples with low quality/missing SrV data - these ones break the loop and we need to use Sr8786 to estimate FW exit
bad_data = c("WR08-50", "WR08-56", "WR08-59") 
wr_megafile_sub = subset(wr_megafile, !Sample_ID %in% bad_data) 

##give each fish a unique ID for the loops
FishID <- unique(wr_megafile_sub$Sample_ID)

# Start new table for loop
FWExitSrV <- NULL
colnames = c("Sample_ID","FWDist_SrV")

for (i in 1:length(FishID)){
  dataSubset<-subset(wr_megafile_sub, wr_megafile_sub$Sample_ID==FishID[i])
  ##exclude na rows (typically respots/vaterite)
  dataSubset<-subset(dataSubset, !is.na(SrVpercent) & !is.na(Distance_um)) 
  x <- dataSubset$SrVpercent
  
  # Identify the SrV % value before and after the FIRST TIME the profile exceeds the threshold value
  y <- c(min(which(x > SrV_FWexit)) - 1, min(which(x > SrV_FWexit)))
  
  # calculate line between the two points
  Mod <- lm(dataSubset$Distance_um[y] ~ x[y])
  PredDist <- Mod$coeff[1] + Mod$coeff[2]*SrV_FWexit
  AddLine <- cbind(dataSubset[1,c("Sample_ID")], as.vector(PredDist))
  names(AddLine) <- colnames
  FWExitSrV <- rbind(FWExitSrV,AddLine)
}

########################################################################################################################

## FW EXIT DISTANCE ESTIMATED USING SR8786  ----------------------------------------------------------------------------

# Threshold = Chipps mean value of 0.70785 (from Phillis et al 2018)
Sr8786_FWexit = 0.70785 

##give each fish a unique ID for the loops
FishID <- unique(wr_megafile$Sample_ID)

# Start new table for loop
FWExit8786 <- NULL
colnames = c("Sample_ID","FWDist_8786")

for (i in 1:length(FishID)){
  dataSubset<-subset(wr_megafile, wr_megafile$Sample_ID==FishID[i])
  dataSubset<-subset(dataSubset, dataSubset$Distance_um>260) ##exclude core values (went slightly bigger here as rare/unlikely a fish went to ocean as tiny fry)
  
  # Check whether Sr8786 is ever less than threshold or contains errors
  x<-dataSubset$Sr8786_norm
  if(min(x,na.rm=T)>Sr8786_FWexit) #if min Sr > threshold
  {
    ErrorRow<-cbind(dataSubset[1,"Sample_ID"],NA) #Too high
    names(ErrorRow)<-colnames
    FWExit8786<-rbind(FWExit8786,ErrorRow)
  } 
  else if(x[length(x)]<Sr8786_FWexit){ #if the last data point in the profile is < threshold
    ErrorRow<-cbind(dataSubset[1,"Sample_ID"],NA) #Incomplete
    names(ErrorRow)<-colnames
    FWExit8786<-rbind(FWExit8786,ErrorRow)
  } 
  else{
    # Identify the Sr8786 value before and after the LAST TIME the profile exceeds the threshold value
    y<-c(max(which(x<Sr8786_FWexit)),max(which(x<Sr8786_FWexit))+1)
    # calculate line between the two points
    Mod<-lm(dataSubset$Distance_um[y] ~ x[y])
    PredDist<-Mod$coeff[1]+Mod$coeff[2]*Sr8786_FWexit
    AddLine<-cbind(dataSubset[1,"Sample_ID"],as.vector(PredDist))
    names(AddLine)<-colnames
    FWExit8786<-rbind(FWExit8786,as.matrix(AddLine))
  }
}

#put all FW exit ests into one data frame & force the Sr8786 est to be numeric
FWExit_final = left_join(FWExit8786, FWExitSrV)
FWExit_final$FWDist_8786 = as.numeric(FWExit_final$FWDist_8786)

# How correlated are they?
ggplot(FWExit_final, aes(x=FWDist_8786, y = FWDist_SrV))+ geom_point()+geom_abline(slope=1, color = "red")

#Use Sr8786 as default - this is different to Phillis et al. (2018), but we found that the Sr8786 data tended to be 
#less noisy and it is only the AME rearers (which are the minority) that require the SrV method.
FWExit_final$FWExit_dist = FWExit_final$FWDist_8786

# But for AME rearers and a few others where Sr8786 was lower quality we used SrV distance
fw_exit_exceptions = read.csv("data/otos_to_use_SrV_exit_dist.csv")

#list of fish to use SrV exit value 
SrV_fish = unique(fw_exit_exceptions$Sample_ID)

# Use SrV distance for these exceptions
FWExit_final$FWExit_dist[FWExit_final$Sample_ID %in% SrV_fish] = FWExit_final$FWDist_SrV[FWExit_final$Sample_ID %in% SrV_fish]

# replace all NA FW exit distances with the Sr8786 estimate
FWExit_final$FWExit_dist[is.na(FWExit_final$FWExit_dist)] = FWExit_final$FWDist_SrV[is.na(FWExit_final$FWExit_dist)]

# Manual FW exit distances for anomalies based on visual estimation from profile shape
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15-5223'] = 679.2780 #one exception where the fish was just leaving so we used final spot dist
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15-7306'] = 600 #one exception where the fish was just leaving so we used final spot dist
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR08-93'] = 720 
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−5015'] = 600 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−80200'] = 790 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−80243'] = 595 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−80359'] = 580 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−80396'] = 585 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15−80616'] = 560 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR17.5054'] = 700 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15-5196'] = 700 #used Sr8786 pattern (but it just exceeded Chipps value)
FWExit_final$FWExit_dist[FWExit_final$Sample_ID == 	'WR15-80616'] = 590 #used Sr8786 pattern (but it just exceeded Chipps value)

# EXPORT FW EXIT DATA
write.csv(FWExit_final, "outputs/WR_FW_Exit_allyrs.csv")

wr_megafile = left_join(wr_megafile, FWExit_final[c('Sample_ID', 'FWExit_dist')])

# EXPORT CLEANED UP MEGAFILE
write.csv(wr_megafile, 'outputs/sr8786_dat_with_fw_exit_dist.csv')
