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
#' @author Pedro Morais (Revision), Anna Sturrock (Original)

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
  arrange(Brood_year, Sample_ID)

# Define habitat colors
colors <- list(
  SAC        = "#F98400",
  LAS        = "gold1",
  DEL        = "#00A08A",
  AME        = "#FF0000",
  Unassigned = "darkgrey",
  Bay        = "#302f5e"
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
# 3. Example Profile Plots for Paper
# ------------------------------------------------------------------------------

## Create color mapping for paper figure
wr_megafile_paper <- wr_megafile |> 
  mutate(color_paper = case_when(
    Habitat == "SAC"       ~ "grey60",
    Habitat == "AME"       ~ "turquoise",
    Habitat == "LAS"       ~ "tomato2",
    Habitat == "DEL"       ~ "turquoise4",
    Habitat == "Unassigned" ~ "grey10",
    TRUE                   ~ "white"
  ))

# Select IDs of representative profiles
example_ids <- c(
  "WR16.5056", "WR15-5163", # SAC upper and SAC upper-lower
  "WR15-80226",             # AME
  "WR15-7069",              # DEL
  "WR15-7349",              # LAS
  "WR09-55"                 # X
)

examples_df <- wr_megafile_paper |> 
  filter(Sample_ID %in% example_ids)

# Helper function to add figure letters (A, B, C...) to base R plots
put_fig_letter <- function(label, location = "topleft", x = NULL, y = NULL, 
                           offset = c(0, 0), ...) {
  if (length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  
  coords <- if (is.null(x) | is.null(y)) {
    switch(location,
           topleft    = c(0.015, 0.98),
           topcenter  = c(0.5525, 0.98),
           topright   = c(0.985, 0.98),
           bottomleft = c(0.015, 0.02), 
           bottomcenter = c(0.5525, 0.02), 
           bottomright = c(0.985, 0.02),
           c(0.015, 0.98))
  } else {
    c(x, y)
  }
  
  this_x <- grconvertX(coords[1] + offset[1], from = "nfc", to = "user")
  this_y <- grconvertY(coords[2] + offset[2], from = "nfc", to = "user")
  text(labels = label[1], x = this_x, y = this_y, xpd = TRUE, ...)
}

# Initialize TIFF output
tiff(file = "figures/Fig2_example_profiles.tiff", width = 3000, height = 1700, res = 430, compression = "lzw")
par(mfrow = c(2, 3), mar = c(2, 2, 1, 2), oma = c(3, 3, 1, 1))

for (i in seq_along(example_ids)) {
  
  # Subset data for specific fish
  data_subset <- examples_df |> 
    filter(Sample_ID == example_ids[i], !is.na(Distance_um), Distance_um >= 0) |> 
    arrange(Distance_um)
  
  if (nrow(data_subset) == 0) next
  
  SrV <- data_subset$SrV
  max_dist <- max(data_subset$Distance_um)
  
  # --- 3.1 Base Plot Setup ---
  plot(data_subset$Distance_um, data_subset$Sr8786_norm, 
       ylim = c(0.7038, 0.71), 
       xlim = c(0, 810), 
       type = "n")
  
  # --- 3.2 Reference Polygons (Habitat Ranges) ---
  # Define x coordinates for polygons
  poly_x <- c(-50, -50, 5000, 5000)
  
  # Lassen (LAS)
  polygon(poly_x, c(0.703, 0.70467, 0.70467, 0.703), col = alpha("tomato2", 0.3), border = FALSE)
  # Sac (SAC)
  polygon(poly_x, c(0.70467, 0.7061, 0.7061, 0.70467), col = alpha("grey40", 0.3), border = FALSE)
  # FEA/Delta (DEL)
  polygon(poly_x, c(0.7061, 0.70785, 0.70785, 0.7061), col = alpha("turquoise4", 0.2), border = FALSE)
  # American (AME)
  polygon(poly_x, c(0.70785, 0.711, 0.711, 0.70785), col = alpha("turquoise", 0.1), border = FALSE)
  
  # --- 3.3 Labels and Threshold Lines ---
  LAS_max <- 0.70467
  SAC_max <- 0.7061 
  
  # Title (ID and Brood Year)
  title(main = paste(data_subset$Sample_ID[1], "[assigned BY=", data_subset$Brood_year[1], "]"), 
       line = -1.5, cex.main = 0.8)
  
  # Threshold Lines
  abline(h = 0.70918, col = alpha("black", 0.8), lty = "dashed", lwd = 0.8) # Ocean ref
  abline(v = data_subset$FWExit_dist[1], col = alpha("darkblue", 0.9), lwd = 1, lty = "dashed") # FW exit
  
  # Exogenous distance line
  exog_dist_val <- data_subset$Distance_um[which(!is.na(data_subset$Habitat))[1]]
  if (!is.na(exog_dist_val)) {
    abline(v = exog_dist_val, col = alpha("red", 0.9), lwd = 1, lty = "dashed")
  }
  
  # Habitat Labels
  text(815, 0.70955, labels = "Ocean", font = 3, col = "black", cex = 0.7, pos = 2, adj = 0)
  text(-20, 0.7088, labels = "AME", font = 3, col = "black", cex = 0.6, pos = 4, adj = 0)
  text(-20, 0.707, labels = "FEA/DEL", font = 3, col = "black", cex = 0.6, pos = 4, adj = 0)
  text(-20, 0.70525, labels = "SAC", font = 3, col = "black", cex = 0.6, pos = 4, adj = 0)
  text(-20, 0.704, labels = "LAS", font = 3, col = "black", cex = 0.6, pos = 4, adj = 0)
  
  # --- 3.4 Data Overlay ---
  # Primary Sr8786 line
  lines(data_subset$Distance_um, data_subset$Sr8786_norm, lwd = 2)
  
  # Error bars (SE2)
  segments(data_subset$Distance_um, data_subset$Sr8786_norm + data_subset$SE2, 
           data_subset$Distance_um, data_subset$Sr8786_norm - data_subset$SE2, lwd = 1)
  
  # Points colored by habitat
  points(data_subset$Distance_um, data_subset$Sr8786_norm, 
         pch = 21, cex = 1.75, bg = data_subset$color_paper)
  box()
  
  # --- 3.5 Dual Axis: SrV ---
  par(new = TRUE)
  plot(data_subset$Distance_um, SrV, pch = 16, col = alpha("tomato2", 0.7), 
       ylim = c(0, 7), xlim = c(0, max_dist), axes = FALSE, xlab = NA, ylab = NA)
  lines(data_subset$Distance_um, SrV, lwd = 2, col = alpha("tomato2", 0.7))
  axis(side = 4, col.axis = "tomato2")
  
  # Outer margin labels
  mtext(text = "Distance from otolith core (μm)", side = 1, line = 1, outer = TRUE, cex = 0.9)
  mtext(text = expression(text('Otolith'^87*'Sr/'^86*'Sr'), ), side = 2, line = 1, outer = TRUE, cex = 0.9)
  mtext(text = "Sr V", side = 4, line = 1, outer = TRUE, cex = 0.9)
  
  # Figure panel letter (A, B, C...)
  my_label <- paste("          ", toupper(letters[i]), sep = "")
  put_fig_letter(label = my_label, location = "topleft", font = 2)
}

dev.off()
