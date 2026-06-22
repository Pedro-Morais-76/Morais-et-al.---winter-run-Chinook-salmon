#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script assigns habitats to every Sr8786 measurement using an automated 
#' function and manual expert refinement.
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
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr)

# Load habitat assignment function
source("scripts/02_wr_habitat_assignment_function.R")

# ------------------------------------------------------------------------------
# 2. Data Loading
# ------------------------------------------------------------------------------

# Read in otolith Sr8786 data with FW exit distances
wr_megafile <- read.csv('outputs/sr8786_dat_with_fw_exit_dist.csv', 
                        na.strings = c("NA", ""), 
                        stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 3. Automated Habitat Assignment
# ------------------------------------------------------------------------------

# Split into list by Sample_ID for functional application
megafile_list <- split(wr_megafile, wr_megafile$Sample_ID)

# Apply automated assignment function. 
# Note: This handles the isotopic range logic and sequence refinement (DEL replacement).
res <- lapply(megafile_list, wr_habitat_assigner) 

# Convert list back to data frame
megafile_assignments <- do.call('rbind', res) 

# Select columns for joining (keeping only the essential assignment metadata)
megafile_assignments2 <- megafile_assignments[, c("X", "Sr8786_norm", "Isotopic_range", "Habitat")]

# Join assignments back to the original data frame to preserve maternal and ocean data 
# (which are excluded from the automated function's window)
wr_megafile <- left_join(wr_megafile, megafile_assignments2, by = "X")

# ------------------------------------------------------------------------------
# 4. Manual Habitat Refinement (Expert Opinion)
# ------------------------------------------------------------------------------

# The following manual adjustments were made based on visual inspection of 
# isotopic profiles and expert assessment of salmon behavior.

# --- 4.1 Unassigned Habitats ---
# Spots identified as transitions or where habitat is ambiguous
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-49" & wr_megafile$Habitat == "DEL"] <- "Unassigned" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR07-12" & wr_megafile$Spot_no == 10] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-24" & wr_megafile$Spot_no == 8] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-5338" & wr_megafile$Spot_no == 13] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7102" & wr_megafile$Habitat == "DEL"] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7352" & wr_megafile$Spot_no == 12] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR16.7022" & wr_megafile$Spot_no == 22] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-5085" & wr_megafile$Spot_no == 13] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR16.5068" & wr_megafile$Spot_no == 14] <- "Unassigned"
wr_megafile$Habitat[(wr_megafile$Sample_ID == "WR07-15" & wr_megafile$Spot_no %in% c(7, 8))] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR07-05" & wr_megafile$Spot_no == 26] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR07-09" & wr_megafile$Spot_no == 9] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR07-14" & wr_megafile$Spot_no == 6] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-24" & wr_megafile$Spot_no == 16] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-24" & wr_megafile$Distance_um > 300 & wr_megafile$Distance_um < 450] <- "Unassigned"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR09-55" & wr_megafile$Spot_no == 9] <- "Unassigned"

# --- 4.2 SAC Refinements ---
# Correcting spots identified as instrument error near thresholds
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7371" & wr_megafile$Spot_no == 14] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7356" & wr_megafile$Spot_no == 21] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7032" & wr_megafile$Spot_no == 14] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80264" & wr_megafile$Spot_no == 14] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80275" & wr_megafile$Spot_no == 14] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80286" & wr_megafile$Spot_no == 21] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-09" & wr_megafile$Habitat == "LAS"] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-58" & wr_megafile$Habitat == "LAS"] <- "SAC" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-91" & wr_megafile$Habitat == "LAS"] <- "SAC" 

# --- 4.3 LAS Refinements ---
# Ensuring continued residence in Lassen during threshold excursions
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR16.80104" & wr_megafile$Spot_no == 15] <- "LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-5296" & wr_megafile$Spot_no %in% c(14, 17, 22)] <- "LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80163" & wr_megafile$Spot_no %in% c(11, 13)] <- "LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80241" & wr_megafile$Spot_no %in% c(7, 14)] <- "LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80375" & wr_megafile$Spot_no == 12] <- "LAS" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80471" & wr_megafile$Spot_no %in% c(9, 10)] <- "LAS" 

# --- 4.4 AME Refinements ---
# Confirming AME residence when approaching thresholds
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR07-04" & wr_megafile$Habitat == "DEL"] <- "AME"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-23" & wr_megafile$Spot_no == 8] <- "AME" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR17.5054" & wr_megafile$Habitat == "DEL"] <- "AME"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-49" & wr_megafile$Habitat == "Unassigned"] <- "AME"

# --- 4.5 DEL Refinements ---
# Identifying Delta "dips" in American-reared fish
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR09-01" & wr_megafile$Spot_no == 13] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR09-64" & wr_megafile$Spot_no == 16] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-7306" & wr_megafile$Spot_no == 16] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80203" & wr_megafile$Spot_no == 16] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80284" & wr_megafile$Spot_no %in% c(16, 23)] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80396" & wr_megafile$Spot_no == 14] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR17.7051" & wr_megafile$Spot_no == 18] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-5048" & wr_megafile$Habitat == "AME"] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR08-23" & wr_megafile$Spot_no %in% c(11, 19)] <- "DEL" 
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR09-69" & wr_megafile$Spot_no == 11] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-80252" & wr_megafile$Spot_no %in% c(15, 22)] <- "DEL"
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR15-5048" & wr_megafile$Habitat == "AME"] <- "DEL" # duplicate entry in original
wr_megafile$Habitat[wr_megafile$Sample_ID == "WR16.7111" & wr_megafile$Habitat == "AME"] <- "DEL"

# ------------------------------------------------------------------------------
# 5. Validation and Export
# ------------------------------------------------------------------------------

# Quantify the impact of manual refinements
n_changed <- sum(wr_megafile$Habitat != wr_megafile$Isotopic_range, na.rm = TRUE)
n_same <- sum(wr_megafile$Habitat == wr_megafile$Isotopic_range, na.rm = TRUE)

# Calculate percentage of spots modified manually (expected ~1%)
modification_rate <- n_changed / n_same 
message(paste0("Manual modification rate: ", round(modification_rate * 100, 2), "%"))

# Remove intermediate isotopic range column
wr_megafile$Isotopic_range <- NULL

# Export final dataset
write.csv(wr_megafile, 'outputs/oto_sr8786_dat_with_assignments.csv', row.names = FALSE)
