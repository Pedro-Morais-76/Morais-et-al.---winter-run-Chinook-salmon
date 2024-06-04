# This script is to assign a habitat to every Sr8786 measurement

# Clear workspace
rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

# Load habitat assignment function
source("scripts/2A_wr_habitat_assignment_function.R")

# Read in otolith Sr8786 data with FW exit distances
wr_megafile = read.csv('outputs/sr8786_dat_with_fw_exit_dist.csv', 
                       na.strings = c("NA", ""), stringsAsFactors = FALSE)

#split into list by Sample_ID
megafile_list <-split(wr_megafile, wr_megafile$Sample_ID)

# HABITAT SR RANGES USED 
# LAS_max = 0.70467 # min Sac value
# SAC_max = 0.7061 # mean Freeport value
# DEL_max = 0.70785 # mean Chipps value

# Apply function to assign each spot between Exog check and FW exit a habitat based on isotopic range
# Only exceptions are when we had SAC-DEL-AME or AME-DEL-SAC. In these cases we reassigned DEL to AME.
res = lapply(megafile_list, wr_habitat_assigner) 

# convert to df (note lower N obs is because we only assigned habitats between Exog check and FW exit!)
megafile_assignments = do.call('rbind', res) 

megafile_assignments2 = megafile_assignments[c("X","Sr8786_norm","Isotopic_range", "Habitat")]

# JOIN ASSIGNMENTS TO ORIGINAL DF SO WE KEEP THE MATERNAL AND OCEAN DATA (excluded in function)
wr_megafile = left_join(wr_megafile, megafile_assignments2) 

# MANUAL TWEAKS TO SPOTS THAT WERE DEEMED WRONG BASED ON VISUAL INSPECTION OF 
# EACH PROFILE & BASED ON EXPERT OPINION OF SALMON BEHAVIOUR

# HABITAT X (Transition or unclear which habitat because increase to only a single spot (e.g. could be DEL/FEA/AME/Other)) 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-49" & wr_megafile$Habitat=="DEL"]="Unassigned" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-12" & wr_megafile$Spot_no==10]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-24" & wr_megafile$Spot_no==8]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-5338" & wr_megafile$Spot_no==13]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7102" & wr_megafile$Habitat=="DEL"]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7352" & wr_megafile$Spot_no==12]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR16.7022" & wr_megafile$Spot_no==22]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-5085" & wr_megafile$Spot_no==13]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR16.5068" & wr_megafile$Spot_no==14]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-15" & wr_megafile$Spot_no==7|wr_megafile$Sample_ID=="WR07-15" & wr_megafile$Spot_no==8]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-05" & wr_megafile$Spot_no==26]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-09" & wr_megafile$Spot_no==9]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-14" & wr_megafile$Spot_no==6]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-24" & wr_megafile$Spot_no==16]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-24" & wr_megafile$Distance_um > 300 & wr_megafile$Distance_um < 450]="Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR09-55" & wr_megafile$Spot_no==9]="Unassigned"


# Spots just outside SAC range where we believe are instrument error and the fish was still in the Sac (i.e. not a true non-natal excursion)
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7371" & wr_megafile$Spot_no==14]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7356" & wr_megafile$Spot_no==21]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7032" & wr_megafile$Spot_no==14]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80264" & wr_megafile$Spot_no==14]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80275" & wr_megafile$Spot_no==14]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80286" & wr_megafile$Spot_no==21]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-09" & wr_megafile$Habitat=="LAS"]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-58" & wr_megafile$Habitat=="LAS"]="SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-91" & wr_megafile$Habitat=="LAS"]="SAC" 


# Spots within a period of Lassen rearing close to the threshold where we are confident of continued LAS residence 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR16.80104" & wr_megafile$Spot_no==15]="LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-5296" & wr_megafile$Spot_no %in% c(14, 17, 22)]="LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80163" & wr_megafile$Spot_no %in% c(11, 13)]="LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80241" & wr_megafile$Spot_no %in% c(7, 14)]="LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80375" & wr_megafile$Spot_no ==12]="LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80471" & wr_megafile$Spot_no %in% c(9,10)]="LAS" 

# Spots within a period of American rearing close to the threshold where we are confident of continued AME residence 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR07-04" & wr_megafile$Habitat=="DEL"]="AME"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-23" & wr_megafile$Spot_no == 8]="AME" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR17.5054" & wr_megafile$Habitat=="DEL"]="AME"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-49" & wr_megafile$Habitat=="Unassigned"]="AME" # just transition or hanging at mouth

# AME rearers where the "Delta dip" between AME and Ocean is clear but not quite low isotopically enough to be in DEL range
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR09-01" & wr_megafile$Spot_no==13]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR09-64" & wr_megafile$Spot_no==16]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-7306" & wr_megafile$Spot_no==16]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80203" & wr_megafile$Spot_no==16]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80284" & wr_megafile$Spot_no==23|wr_megafile$Sample_ID=="WR15-80284" & wr_megafile$Spot_no==16]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80396" & wr_megafile$Spot_no==14]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR17.7051" & wr_megafile$Spot_no==18]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15−5048" & wr_megafile$Habitat=="AME"]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR08-23" & wr_megafile$Spot_no == 19 | wr_megafile$Sample_ID=="WR08-23" & wr_megafile$Spot_no == 11]="DEL" 
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR09-69" & wr_megafile$Spot_no == 11]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15-80252" & wr_megafile$Spot_no %in% c(22,15)]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR15−5048" & wr_megafile$Habitat=="AME"]="DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID=="WR16.7111" & wr_megafile$Habitat=="AME"]="DEL"

# How many spots did we change manually?
n_changed = sum(wr_megafile$Habitat != wr_megafile$Isotopic_range, na.rm = T)
n_same = sum(wr_megafile$Habitat == wr_megafile$Isotopic_range, na.rm = T)

n_changed/n_same # so we changed ~1% of all spot assignments manually

#drop this column
wr_megafile$Isotopic_range = NULL

#EXPORT FINAL DATASET
write.csv(wr_megafile, 'Outputs/oto_sr8786_dat_with_assignments.csv')