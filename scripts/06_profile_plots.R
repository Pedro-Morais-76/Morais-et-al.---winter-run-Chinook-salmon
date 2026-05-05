# This script is to look at each fish's profile with the spots colored by its habitat assignment

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, shape, RColorBrewer, wesanderson, XLConnect, tidyverse)

# Read in otolith Sr8786 data with assignments and brood year
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_brood_year.csv')[,-1] %>%
  arrange(Brood_year, Sample_ID)

##--------------- ALL INDIVIDUAL PROFILES (1 plot per fish) --------------

# define colors
sacc = "#F98400"
lasc = "gold1"
delc = "#00A08A"
amec = "#FF0000"
xc ="darkgrey"
bayc = "#302f5e" 

## Add colors to df
wr_megafile = wr_megafile %>%
  mutate(color = case_when(
    Habitat=="SAC" ~ sacc,   
    Habitat=="AME" ~ amec,
    Habitat=="LAS" ~ lasc,
    Habitat=="DEL" ~ delc,
    Habitat=="Unassigned" ~ xc,
    TRUE ~ "white"))

# Make PDF of all profiles
FishID <- unique(wr_megafile$Sample_ID)

pdf(file = "figures/FigS3_Individual_otolith_profiles.pdf",width = 9.5, height = 12)
par(mfrow=c(4,3), mar=c(2, 2, 1, 2), oma=c(3,3,3,3))

for (i in 1:length(FishID)){
  
  dataSubset<-subset(wr_megafile, Sample_ID==FishID[i] & !is.na(Distance_um & Distance_um>=0))
  dataSubset<-dataSubset[order(dataSubset$Distance_um),]
  SrV=dataSubset$SrV
  plot(dataSubset$Distance_um,dataSubset$Sr8786_norm, ylim=c(0.7035,0.7103), 
       xlim=c(0,max(dataSubset$Distance_um)), type="n")
  
  #REFERENCE LINES AND LABELS
  x=c(-50,-50,5000,5000); y=c(0.703, 0.70467, 0.70467, 0.703) ##range of Lassen values used in Phillis et al 2018 
  polygon(x,y, col = alpha("#F2AD00", 0.3), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.70467, 0.7061, 0.7061, 0.70467) ##range of Sac values used in Phillis et al 2018 
  polygon(x,y, col = alpha("#F98400", 0.3), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.7061, 0.70785, 0.70785, 0.7061) ##range of Delta values used in Phillis et al 2018 
  polygon(x,y, col = alpha("#00A08A", 0.2), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.7061, 0.707, 0.707, 0.7061) ##range of Fea/Delta values used in Phillis et al 2018 
  polygon(x,y, col = alpha("#00A08A", 0.1), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.70785, 0.711, 0.711, 0.70785) ##range of Ame values used in Phillis et al 2018 
  polygon(x,y, col = alpha("#F98400", 0.1), border = FALSE)
  
  LAS_max = 0.70467
  SAC_max = 0.7061 
  
  title(main=paste(dataSubset$Sample_ID[1], "[assigned BY=",dataSubset$Brood_year[1], #" | ","FL=", dataSubset$Fork_Length[1],
                   "]"), line=-1.5, cex.main = 0.8)
  abline(h=LAS_max, col="black", lty="dashed",lwd = 0.8) ##min sac
  abline(h=SAC_max, col="black", lty="dashed",lwd = 0.8) ##max sac
  abline(h=0.70918, col="black", lty="dashed",lwd = 0.8) ## ocean ref line
  abline(v=dataSubset$FWExit_dist[1], col=alpha("black",0.9), lwd=1) ##add FW exit distance (estimated by %SrV>66%)
  
  text(max(dataSubset$Distance_um),0.70934,labels="Ocean",font=3, col="black",cex=0.8,pos=2)
  text(-11,0.7084,labels="AME",font=3, col="black",cex=0.5,pos=4)
  text(-11,0.70742,labels="DEL",font=3, col="black",cex=0.5,pos=4)
  text(-11,0.70647,labels="FEA/DEL",font=3, col="black",cex=0.5,pos=4)
  text(-11,0.70527,labels="SAC",font=3, col="black",cex=0.5,pos=4)
  text(-11,0.70391,labels="LAS",font=3, col="black",cex=0.5,pos=4)
  
  ##put points over top
  lines(dataSubset$Distance_um,dataSubset$Sr8786_norm, ylim=c(0.7035,0.7103), xlim=c(0,max(dataSubset$Distance_um)),lwd=2)
  segments(dataSubset$Distance_um,dataSubset$Sr8786_norm+(dataSubset$SE2),dataSubset$Distance_um,dataSubset$Sr8786_norm-(dataSubset$SE2), lwd=0.7)
  points(dataSubset$Distance_um,dataSubset$Sr8786_norm, ylim=c(0.7035,0.7103), 
         xlim=c(0,max(dataSubset$Distance_um)),pch=21, cex = 1.5, bg=dataSubset$color)
  box()
  
  ##Add 2nd y axis for SrV
  par(new = T)
  plot(dataSubset$Distance_um,SrV, pch=16, col=alpha("tomato2",0.7),ylim=c(0,7), xlim=c(0,max(dataSubset$Distance_um)), axes=F, xlab=NA, ylab=NA)
  lines(dataSubset$Distance_um,SrV, lwd=2, col=alpha("tomato2",0.7),ylim=c(0,7), xlim=c(0,max(dataSubset$Distance_um)), xlab=NA, ylab=NA)
  axis(side = 4, col.axis="tomato2")
  
  mtext(text="Distance from otolith core (um)",side=1,line=0,outer=TRUE)
  mtext(text="Otolith 87Sr/86Sr",side=2,line=0,outer=TRUE)
  mtext(text="Otolith Sr V",side=4,line=0,outer=TRUE)
}

dev.off()

##-----------EXAMPLE PROFILE PLOTS FOR PAPER

## Add colors to master spreadsheet
wr_megafile2 = wr_megafile %>%
  mutate(color = case_when(
    Habitat=="SAC" ~ "grey60",   
    Habitat=="AME" ~ "turquoise",
    Habitat=="LAS" ~ "tomato2",
    Habitat=="DEL" ~ "turquoise4",
    Habitat=="Unassigned" ~ "grey10",
    TRUE ~ "white"))

# Pull out IDs of good example profiles
examplelist = c("WR16.5056", "WR15-5163", #SAC upper and SAC upper-lower
                "WR15-80226", #AME
                "WR15-7069", #DEL
                "WR15-7349", #LAS
                "WR09-55")  # X 

examples = wr_megafile2 %>% filter(Sample_ID %in% examplelist)

# FUNCTION FOR ADDING FIGURE LEGENDS IN BASE R OUTPUT
put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.015,0.98),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.015, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.015, 0.98) )
  } else {
    coords <- c(x,y)
  }
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, ...)
}


# Make tiff of example profiles
FishID <- examplelist

tiff(file = "figures/Fig2_example_profiles.tiff",width = 3000, height = 1700, res = 430, compression = "lzw")
par(mfrow=c(2,3), mar=c(2, 2, 1, 2), oma=c(3,3,1,1))

for (i in 1:length(FishID)){
  
  dataSubset<-subset(examples, Sample_ID==FishID[i] & !is.na(Distance_um & Distance_um>=0))
  dataSubset<-dataSubset[order(dataSubset$Distance_um),]
  plot(dataSubset$Distance_um,dataSubset$Sr8786_norm, ylim=c(0.7038,0.71), xlim=c(0,810),type="n")
  
  #REFERENCE LINES AND LABELS
  x=c(-50,-50,5000,5000); y=c(0.703, 0.70467, 0.70467, 0.703) ##range of Lassen values used in Phillis et al 2018 
  polygon(x,y, col = alpha("tomato2", 0.3), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.70467, 0.7061, 0.7061, 0.70467) ##range of Sac values used in Phillis et al 2018 
  polygon(x,y, col = alpha("grey40", 0.3), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.7061, 0.70785, 0.70785, 0.7061) ##range of FEA/Delta values used in Phillis et al 2018 
  polygon(x,y, col = alpha("turquoise4", 0.2), border = FALSE)
  x=c(-50,-50,5000,5000); y=c(0.70785, 0.711, 0.711, 0.70785) ##range of Ame values used in Phillis et al 2018 
  polygon(x,y, col = alpha("turquoise", 0.1), border = FALSE)
  
  LAS_max = 0.70467
  SAC_max = 0.7061 
  
  abline(h=0.70918, col=alpha("black",0.8), lty="dashed",lwd = 0.8) ## ocean ref line
  abline(v=dataSubset$FWExit_dist[1], col=alpha("darkblue",0.9), lwd=1, lty = "dashed") ##add FW exit distance (estimated by %SrV>66%)
  
  exog_dist = subset(dataSubset, !is.na(dataSubset$Habitat))
  abline(v=exog_dist$Distance_um[1], col=alpha("red",0.9), lwd=1, lty = "dashed") ##add exog  distance (estimated by %SrV>66%)
  
  text(815,0.70955,labels="Ocean",font=3, col="black",cex=0.7,pos=2,adj =0)
  text(-20,0.7088,labels="AME",font=3, col="black",cex=0.6,pos=4, adj =0)
  text(-20,0.707,labels="FEA/DEL",font=3, col="black",cex=0.6,pos=4,adj =0)
  text(-20,0.70525,labels="SAC",font=3, col="black",cex=0.6,pos=4, adj =0)
  text(-20,0.704,labels="LAS",font=3, col="black",cex=0.6,pos=4, adj =0)
  
  ##put points over top
  lines(dataSubset$Distance_um,dataSubset$Sr8786_norm,lwd=2)
  segments(dataSubset$Distance_um,dataSubset$Sr8786_norm+(dataSubset$SE2),dataSubset$Distance_um,dataSubset$Sr8786_norm-(dataSubset$SE2), lwd=1)
  points(dataSubset$Distance_um,dataSubset$Sr8786_norm, pch=21, cex = 1.75, bg=dataSubset$color)
  
  # Add A B C etc
  my.label <- paste("          ",toupper(letters[i]),  sep="")
  put.fig.letter(label=my.label, location="topleft", font=2)
  
  box()
  
  mtext(text="Distance from otolith core (µm)",side=1,line=1,outer=TRUE, cex = .9)
  mtext(text=expression(text='Otolith'^87*'Sr/'^86*'Sr'),side=2,line=1,outer=TRUE, cex = .9)

}

dev.off()