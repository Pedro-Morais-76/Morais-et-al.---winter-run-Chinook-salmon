#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script defines a function to assign habitats to otolith profiles based on 
#' Sr8786 isotopic concentrations.
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

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(dplyr)

# ------------------------------------------------------------------------------
# 2. Habitat Assignment Function
# ------------------------------------------------------------------------------

#' Assign habitats to otolith profiles
#' 
#' @param x A data frame containing otolith profile data (Sr8786_norm, SE2, 
#' Escap_yr, Distance_um, Exog_dist, FWExit_dist).
#' @return A data frame with an additional `Isotopic_range` and `Habitat` column.
wr_habitat_assigner <- function(x) {
  
  # ----------------------------------------------------------------------------
  # 2.1 Set habitat isotopic thresholds
  # ----------------------------------------------------------------------------
  
  # Thresholds derived from Phillis et al. (2018) and field observations
  las_max <- 0.70467 # Min measured upper Sac water value
  sac_max <- 0.7061  # Mean value at Freeport
  del_max <- 0.70785 # Mean Chipps value
  
  # ----------------------------------------------------------------------------
  # 2.2 Isotopic Range Assignment
  # ----------------------------------------------------------------------------
  
  # Assign a habitat to each spot based on measured Sr8786
  # We account for varying data quality (SE2) in later years (Escap_yr > 2009)
  x <- x |> 
    mutate(Isotopic_range = case_when(
      # LAS: High quality data (lower SE) and lower Sr8786
      Sr8786_norm + SE2 < las_max & Escap_yr > 2009 ~ 'LAS',
      Sr8786_norm < las_max & Escap_yr <= 2009    ~ 'LAS',
      
      # SAC: Intermediate Sr8786
      Sr8786_norm + SE2 >= las_max & Sr8786_norm < sac_max & Escap_yr > 2009 ~ 'SAC',
      Sr8786_norm >= las_max & Sr8786_norm < sac_max & Escap_yr <= 2009    ~ 'SAC',
      
      # DEL: Delta range
      Sr8786_norm >= sac_max & Sr8786_norm <= del_max ~ 'DEL',
      
      # AME: Above Delta threshold
      Sr8786_norm > del_max ~ 'AME'
    )) |> 
    # Filter to focus only on the freshwater migration window
    filter(Distance_um >= Exog_dist & Distance_um <= FWExit_dist)
  
  # ----------------------------------------------------------------------------
  # 2.3 Sequence Refinement (DEL replacement)
  # ----------------------------------------------------------------------------
  
  # Create a working habitat column to identify and modify sequences
  x$Habitat <- x$Isotopic_range
  
  # Identify indices of DEL-assigned spots
  which_del <- which(x$Habitat == "DEL")
  
  # Remove DEL if it's the first or last spot in the profile to avoid indexing errors
  which_del <- which_del[!which_del %in% c(1, length(x$Habitat))]
  
  # If no interior DEL spots exist, return as is
  if (length(which_del) == 0) {
    return(x)
  }
  
  # Replace DEL with AME in sequences where it acts as a transition (SAC-DEL-AME or AME-DEL-SAC)
  # This acknowledges that if isotopes deviate significantly on either side, the fish 
  # was likely already in non-natal waters.
  for (i in which_del) {
    
    # Case: SAC -> DEL -> AME
    if (x$Habitat[i - 1] == "SAC" & x$Habitat[i + 1] == "AME") {
      x$Habitat[i] <- "AME"
    }
    
    # Case: AME -> DEL -> SAC
    if (x$Habitat[i - 1] == "AME" & x$Habitat[i + 1] == "SAC") {
      x$Habitat[i] <- "AME"
    }
  }
  
  return(x)
}
