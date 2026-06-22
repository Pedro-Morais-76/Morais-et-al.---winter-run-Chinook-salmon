#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script generates individual otolith profile plots, visualizing Sr8786 
#' and SrV concentrations across the freshwater migration window.
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
p_load(ggplot2, dplyr, shape, RColorBrewer, wesanderson, XLConnect, tidyverse, cowplot)

# ------------------------------------------------------------------------------
# 2. Data Loading and Color Definition
# ------------------------------------------------------------------------------

# Read in otolith Sr8786 data with assignments and brood year
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_brood_year.csv') |> 
  select(-1) |> 
  arrange(Brood_year, Sample_ID)

# Define habitat colors
colors <- list(
  SAC       = "#F98400",
  LAS       = "gold1",
  DEL       = "#00A08A",
  AME       = "#FF0000",
  Unassigned = "darkgrey",
  Bay       = "#302f5e"
)

## Assign colors to the dataframe based on Habitat
wr_megafile <- wr_megafile |> 
  mutate(color = case_when(
    Habitat == "SAC"       ~ colors$SAC,
    Habitat == "AME"       ~ colors$AME,
    Habitat == "LAS"       ~ colors$LAS,
    Habitat == "DEL"       ~ colors$DEL,
    Habitat == "Unassigned" ~ colors$Unassigned,
    TRUE                   ~ "white"
  ))

# ------------------------------------------------------------------------------
# 3. Individual Profile Plotting Loop
# ------------------------------------------------------------------------------

# Get list of unique fish to iterate through
FishID <- unique(wr_megafile$Sample_ID)

# Initialize PDF output
pdf(file = "figures/FigS2_Individual_otolith_profiles.pdf", width = 9.5, height = 12)
par(mfrow = c(4, 3), mar = c(2, 2, 1, 2), oma = c(3, 3, 3, 3))

for (i in seq_along(FishID)) {
  
  # Subset data for the current fish, ensuring we only look at valid distance ranges
  dataSubset <- wr_megafile |> 
    filter(Sample_ID == FishID[i], !is.na(Distance_um), Distance_um >= 0) |> 
    arrange(Distance_um)
  
  if (nrow(dataSubset) == 0) next
  
  SrV <- dataSubset$SrV
  max_dist <- max(dataSubset$Distance_um)
  
  # --- Base Plot Setup ---
  plot(dataSubset$Distance_um, dataSubset$Sr8786_norm, 
       ylim = c(0.7035, 0.7103), 
       xlim = c(0, max_dist), 
       type = "n")
  
  # --- 3.1 Reference Polygons (Habitat Ranges) ---
  # Define x coordinates for polygons spanning the profile
  poly_x <- c(-50, -50, 5000, 5000)
  
  # Lassen (LAS)
  polygon(poly_x, c(0.703, 0.70467, 0.70467, 0.703), col = alpha("#F2AD00", 0.3), border = FALSE)
  # Sac (SAC)
  polygon(poly_x, c(0.70467, 0.7061, 0.7061, 0.70467), col = alpha("#F98400", 0.3), border = FALSE)
  # Delta (DEL)
  polygon(poly_x, c(0.7061, 0.70785, 0.70785, 0.7061), col = alpha("#00A08A", 0.2), border = FALSE)
  # FEA/Delta transition
  polygon(poly_x, c(0.7061, 0.707, 0.707, 0.7061), col = alpha("#00A08A", 0.1), border = FALSE)
  # American (AME)
  polygon(poly_x, c(0.70785, 0.711, 0.711, 0.70785), col = alpha("#F98400", 0.1), border = FALSE)
  
  # --- 3.2 Labels and Threshold Lines ---
  LAS_max <- 0.70467
  SAC_max <- 0.7061 
  
  # Plot title (ID and Brood Year)
  title(main = paste(dataSubset$Sample_ID[1], "[assigned BY=", dataSubset$Brood_year[1], "]"), 
       line = -1.5, cex.main = 0.8)
  
  # Threshold horizontal lines
  abline(h = LAS_max, col = "black", lty = "dashed", lwd = 0.8)
  abline(h = SAC_max, col = "black", lty = "dashed", lwd = 0.8)
  abline(h = 0.70918, col = "black", lty = "dashed", lwd = 0.8) # Ocean reference
  
  # Vertical line for freshwater exit distance
  abline(v = dataSubset$FWExit_dist[1], col = alpha("black", 0.9), lwd = 1)
  
  # Text labels for habitat zones
  text(max_dist, 0.70934, labels = "Ocean", font = 3, col = "black", cex = 0.8, pos = 2)
  text(-11, 0.7084, labels = "AME", font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70742, labels = "DEL", font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70647, labels = "FEA/DEL", font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70527, labels = "SAC", font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70391, labels = "LAS", font = 3, col = "black", cex = 0.5, pos = 4)
  
  # --- 3.3 Data Points and Error Bars ---
  # Main Sr8786 line
  lines(dataSubset$Distance_um, dataSubset$Sr8786_norm, 
        ylim = c(0.7035, 0.7103), xlim = c(0, max_dist), lwd = 2)
  
  # Error bars (SE2)
  segments(dataSubset$Distance_um, dataSubset$Sr8786_norm + dataSubset$SE2, 
           dataSubset$Distance_um, dataSubset$Sr8786_norm - dataSubset$SE2, lwd = 0.7)
  
  # Points colored by habitat
  points(dataSubset$Distance_um, dataSubset$Sr8786_norm, 
         ylim = c(0.7035, 0.7103), xlim = c(0, max_dist), 
         pch = 21, cex = 1.5, bg = dataSubset$color)
  box()
  
  # --- 3.4 Dual Axis: SrV ---
  par(new = TRUE)
  plot(dataSubset$Distance_um, SrV, pch = 16, col = alpha("tomato2", 0.7), 
       ylim = c(0, 7), xlim = c(0, max_dist), axes = FALSE, xlab = NA, ylab = NA)
  lines(dataSubset$Distance_um, SrV, lwd = 2, col = alpha("tomato2", 0.7))
  axis(side = 4, col.axis = "tomato2")
  
  # Outer margin labels
  mtext(text = "Distance from otolith core (μm)", side = 1, line = 0, outer = TRUE, cex = 1.0, font = 1)
  mtext(text = expression(""^"87"*"Sr/"^"86"*"Sr"), side = 2, line = 0, outer = TRUE, cex = 1.0, font = 1)
  mtext(text = "Sr V", side = 4, line = 0, outer = TRUE, cex = 1.0, font = 1)
}

dev.off()
