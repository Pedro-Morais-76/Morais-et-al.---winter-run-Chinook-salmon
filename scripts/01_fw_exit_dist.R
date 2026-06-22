#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script estimates freshwater (FW) exit distance in winter run otolith profiles 
#' using Sr8786 and SrV.
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
#
# ------------------------------------------------------------------------------
# 1. Setup and Environment
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

# ------------------------------------------------------------------------------
# 2. Data Loading
# ------------------------------------------------------------------------------

## Read in all otolith Sr8786 data from Phillis et al. (2018) and new data
wr_megafile <- read.csv('data/oto_sr8786_dat_all_yrs.csv')[, -1]

# ------------------------------------------------------------------------------
# 3. FW Exit Distance Estimation Using SrV
# ------------------------------------------------------------------------------

# Threshold = 66% from Phillis et al (2018)
SrV_FWexit <- 0.66 

## Create new SrV column and estimate %SrV within fish 
## (SrV is used as a relative proxy for Sr concentration)
wr_megafile$SrV2 <- wr_megafile$SrV

# Exclude respot SrV values as instrument performance can change over short time periods
wr_megafile$SrV2[which(wr_megafile$respot == "y")] <- NA 

# Exclude maternal data prior to the exogenous feeding check 
# (the yolk has elevated Sr8786)
wr_megafile$SrV2[which(wr_megafile$Distance_um < wr_megafile$Exog_dist)] <- NA 

# Calculate SrV percent within each fish
wr_megafile <- wr_megafile |> 
  group_by(Sample_ID) |> 
  mutate(SrVpercent = ((SrV2 - min(SrV2, na.rm = TRUE)) / 
                         (max(SrV2, na.rm = TRUE) - min(SrV2, na.rm = TRUE)))) |> 
  ungroup()

# Exclude samples with low quality/missing SrV data 
# (These would otherwise break the subsequent loop)
bad_data <- c("WR08-50", "WR08-56", "WR08-59") 
wr_megafile_sub <- subset(wr_megafile, !Sample_ID %in% bad_data) 

## Assign unique ID for the loops
FishID_SrV <- unique(wr_megafile_sub$Sample_ID)

# Initialize table for SrV estimates
FWExitSrV <- NULL
colnames_SrV <- c("Sample_ID", "FWDist_SrV")

for (i in seq_along(FishID_SrV)) {
  dataSubset <- subset(wr_megafile_sub, Sample_ID == FishID_SrV[i])
  
  # Exclude NA rows (typically respots/vaterite)
  dataSubset <- subset(dataSubset, !is.na(SrVpercent) & !is.na(Distance_um)) 
  x <- dataSubset$SrVpercent
  
  # Identify the SrV % value before and after the FIRST TIME the profile exceeds the threshold
  idx_threshold <- which(x > SrV_FWexit)
  if (length(idx_threshold) > 0) {
    y <- c(min(idx_threshold) - 1, min(idx_threshold))
    
    # Calculate line between the two points
    Mod <- lm(dataSubset$Distance_um[y] ~ x[y])
    PredDist <- Mod$coeff[1] + Mod$coeff[2] * SrV_FWexit
    
    AddLine <- data.frame(Sample_ID = dataSubset$Sample_ID[1], FWDist_SrV = PredDist)
    FWExitSrV <- rbind(FWExitSrV, AddLine)
  }
}

# ------------------------------------------------------------------------------
# 4. FW Exit Distance Estimation Using Sr8786
# ------------------------------------------------------------------------------

# Threshold = Chipps mean value of 0.70785 (from Phillis et al. 2018)
Sr8786_FWexit <- 0.70785 

## Assign unique ID for the loops
FishID_8786 <- unique(wr_megafile$Sample_ID)

# Initialize table for Sr8786 estimates
FWExit8786 <- NULL
colnames_8786 <- c("Sample_ID", "FWDist_8786")

for (i in seq_along(FishID_8786)) {
  dataSubset <- subset(wr_megafile, Sample_ID == FishID_8786[i])
  
  # Exclude core values (preventing ocean-exit estimation for tiny fry)
  dataSubset <- subset(dataSubset, Distance_um > 260) 
  
  x <- dataSubset$Sr8786_norm
  
  # Logic to handle cases where Sr8786 does not intersect the threshold properly
  if (min(x, na.rm = TRUE) > Sr8786_FWexit) {
    # Min Sr value is already above threshold (too high)
    ErrorRow <- data.frame(Sample_ID = dataSubset$Sample_ID[1], FWDist_8786 = NA)
    FWExit8786 <- rbind(FWExit8786, ErrorRow)
    
  } else if (x[length(x)] < Sr8786_FWexit) {
    # Last data point in profile is still below threshold (incomplete profile)
    ErrorRow <- data.frame(Sample_ID = dataSubset$Sample_ID[1], FWDist_8786 = NA)
    FWExit8786 <- rbind(FWExit8786, ErrorRow)
    
  } else {
    # Identify the Sr8786 value before and after the LAST TIME the profile exceeds the threshold
    idx_below <- which(x < Sr8786_FWexit)
    y <- c(max(idx_below), max(idx_below) + 1)
    
    # Calculate line between the two points
    Mod <- lm(dataSubset$Distance_um[y] ~ x[y])
    PredDist <- Mod$coeff[1] + Mod$coeff[2] * Sr8786_FWexit
    
    AddLine <- data.frame(Sample_ID = dataSubset$Sample_ID[1], FWDist_8786 = PredDist)
    FWExit8786 <- rbind(FWExit8786, AddLine)
  }
}

# ------------------------------------------------------------------------------
# 5. Final Integration and Refinement
# ------------------------------------------------------------------------------

# Combine SrV and Sr8786 estimates
FWExit_final <- left_join(FWExit8786, FWExitSrV, by = "Sample_ID")
FWExit_final$FWDist_8786 <- as.numeric(FWExit_final$FWDist_8786)

# Diagnostic: Correlation between methods
ggplot(FWExit_final, aes(x = FWDist_8786, y = FWDist_SrV)) + 
  geom_point() + 
  geom_abline(slope = 1, color = "red")

# Use Sr8786 as default method. 
# Note: This differs from Phillis et al. (2018); however, Sr8786 tends to be 
# less noisy. The SrV method is reserved for AME rearers and specific exceptions.
FWExit_final$FWExit_dist <- FWExit_final$FWDist_8786

# Apply exceptions where SrV is preferred
fw_exit_exceptions <- read.csv("data/otos_to_use_SrV_exit_dist.csv")
SrV_fish <- unique(fw_exit_exceptions$Sample_ID)

FWExit_final$FWExit_dist[FWExit_final$Sample_ID %in% SrV_fish] <- 
  FWExit_final$FWDist_SrV[FWExit_final$Sample_ID %in% SrV_fish]

# Replace remaining NAs in FWExit_dist with SrV estimates if available
NA_mask <- is.na(FWExit_final$FWExit_dist)
FWExit_final$FWExit_dist[NA_mask] <- FWExit_final$FWDist_SrV[NA_mask]

# Manual FW exit distance adjustments based on visual inspection of profiles
adjustments <- list(
  'WR15-5223' = 679.2780, # Exit near end of profile
  'WR15-7306' = 600,
  'WR08-93'   = 720,
  'WR15-5015' = 600,      # Exceeded Chipps value
  'WR15-80200'= 790,      # Exceeded Chipps value
  'WR15-80243'= 595,      # Exceeded Chipps value
  'WR15-80359'= 580,      # Exceeded Chipps value
  'WR15-80396'= 585,      # Exceeded Chipps value
  'WR15-80616'= 560,      # Exceeded Chipps value
  'WR17.5054' = 700,      # Exceeded Chipps value
  'WR15-5196' = 700,      # Exceeded Chipps value
  'WR15-80616' = 590       # Exceeded Chipps value (duplicate entry/update)
)

for (id in names(adjustments)) {
  FWExit_final$FWExit_dist[FWExit_final$Sample_ID == id] <- adjustments[[id]]
}

# ------------------------------------------------------------------------------
# 6. Data Export
# ------------------------------------------------------------------------------

# Export FW exit summary
write.csv(FWExit_final, "outputs/WR_FW_Exit_allyrs.csv", row.names = FALSE)

# Join back to megafile and export final cleaned dataset
wr_megafile <- left_join(wr_megafile, FWExit_final[, c('Sample_ID', 'FWExit_dist')], by = "Sample_ID")
write.csv(wr_megafile, 'outputs/sr8786_dat_with_fw_exit_dist.csv', row.names = FALSE)
