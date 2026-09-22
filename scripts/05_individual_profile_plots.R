#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script generates individual otolith profile plots, visualizing Sr8786 
#' and SrV concentrations across the freshwater migration window.
#' 
#' @details
#' **Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' **Journal:** Ecosphere
#'
#' **Year:** 2026
#'
#' **DOI:** https://doi.org/10.1002/ecs2.70779
#' 
#' **Authors:** 
#' Pedro Morais1,*, Anna M. Sturrock2,3+, Corey C. Phillis4, George Whitman2, 
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
#' This script was originally created by Anna M. Sturrock and revised by Pedro Morais.

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
wr_megafile <- read.csv("outputs/oto_sr8786_dat_with_brood_year.csv") |>
  select(-any_of(c("X", "X.1"))) |>
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

# Assign colors to the dataframe based on Habitat
wr_megafile <- wr_megafile |>
  mutate(color = case_when(
    Habitat == "SAC"        ~ colors$SAC,
    Habitat == "AME"        ~ colors$AME,
    Habitat == "LAS"        ~ colors$LAS,
    Habitat == "DEL"        ~ colors$DEL,
    Habitat == "Unassigned" ~ colors$Unassigned,
    TRUE                     ~ "white"
  ))

# ------------------------------------------------------------------------------
# 3. PDF Output Setup
# ------------------------------------------------------------------------------

# Get the ordered list of fish with valid profile data. Filtering here makes
# the final page predictable so space can be reserved for its figure caption.
FishID <- wr_megafile |>
  filter(!is.na(Distance_um), Distance_um >= 0) |>
  distinct(Sample_ID) |>
  pull(Sample_ID)

# Use Cairo when available so text and axes render cleanly in the vector PDF.
# The fallback remains a vector PDF if Cairo is unavailable.
output_file <- "figures/AppS2_FigS1_Individual_otolith_profiles.pdf"
if (capabilities("cairo")) {
  cairo_pdf(
    filename = output_file,
    width = 9.5,
    height = 12,
    pointsize = 11,
    family = "sans",
    antialias = "subpixel"
  )
} else {
  pdf(
    file = output_file,
    width = 9.5,
    height = 12,
    pointsize = 11,
    family = "sans",
    useDingbats = FALSE
  )
}

# ------------------------------------------------------------------------------
# 4. Front Page (page 1)
# ------------------------------------------------------------------------------

# Draw the cover as a single full-page panel. Coordinates run from 0 to 1 so
# the placement remains proportional if the PDF dimensions are changed later.
par(
  mfrow = c(1, 1),
  mar = c(0, 0, 0, 0),
  oma = c(0, 0, 0, 0),
  family = "Times New Roman"
)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")

cover_left <- 1 / 9.5
cover_right <- 1 - cover_left
cover_cex_12 <- 12 / 11
cover_cex_16 <- 16 / 11
cover_cex_18 <- 18 / 11

text(
  x = cover_left,
  y = 0.915,
  labels = "ECOSPHERE",
  adj = c(0, 0.5),
  cex = cover_cex_16,
  family = "Times New Roman",
  font = 2
)

text(
  x = 0.5,
  y = 0.755,
  labels = "Droughts delay juvenile salmon migration and",
  cex = cover_cex_18,
  family = "Times New Roman",
  font = 2
)
text(
  x = 0.5,
  y = 0.715,
  labels = "truncate diversity in habitat use",
  cex = cover_cex_18,
  family = "Times New Roman",
  font = 2
)

text(
  x = cover_left,
  y = 0.515,
  labels = expression(
    plain("Pedro Morais, Anna Sturrock")^{plain("+")} *
      plain(", Corey C. Phillis, George Whitman, Stephanie M. Carlson,")
  ),
  adj = c(0, 0.5),
  cex = cover_cex_12,
  family = "Times New Roman"
)
text(
  x = cover_left + 0.06,
  y = 0.488,
  labels = "Rachel C. Johnson",
  adj = c(0, 0.5),
  cex = cover_cex_12,
  family = "Times New Roman"
)

text(
  x = cover_left,
  y = 0.415,
  labels = "+ Equal contribution as the first author",
  adj = c(0, 0.5),
  cex = cover_cex_12,
  family = "Times New Roman"
)

# Draw the email separately to reproduce the blue, underlined appearance in
# the supplied front-page example.
correspondence_prefix <- "Correspondence: Pedro Morais; Email: "
correspondence_email <- "pedro.morais@austin.utexas.edu"
correspondence_y <- 0.345

text(
  x = cover_left,
  y = correspondence_y,
  labels = correspondence_prefix,
  adj = c(0, 0.5),
  cex = cover_cex_12,
  family = "Times New Roman"
)

email_x <- cover_left + strwidth(
  correspondence_prefix,
  cex = cover_cex_12,
  units = "user"
)
email_width <- strwidth(
  correspondence_email,
  cex = cover_cex_12,
  units = "user"
)
text(
  x = email_x,
  y = correspondence_y,
  labels = correspondence_email,
  adj = c(0, 0.5),
  cex = cover_cex_12,
  family = "Times New Roman",
  col = "#0563C1"
)
segments(
  x0 = email_x,
  y0 = correspondence_y - 0.008,
  x1 = email_x + email_width,
  y1 = correspondence_y - 0.008,
  col = "#0563C1",
  lwd = 0.6
)

text(
  x = cover_right,
  y = 0.135,
  labels = "Appendix S2",
  adj = c(1, 0.5),
  cex = 1.1,
  font = 2
)

segments(
  x0 = cover_left,
  y0 = 0.75 / 12,
  x1 = cover_right,
  y1 = 0.75 / 12,
  col = "grey45",
  lwd = 0.6
)
text(x = 0.5, y = 0.5 / 12, labels = "1", cex = 0.75, col = "grey35")

# Header and divider rules used on every profile page beginning with page 2.
# Grid coordinates are relative to the full PDF page, so the elements remain
# aligned with the one-inch left and right margins rather than with one panel.
profile_header <- paste0(
  "Appendix S2 \u2013 Droughts delay juvenile salmon migration and ",
  "truncate diversity in habitat use"
)

draw_profile_page_frame <- function() {
  grid::grid.text(
    label = profile_header,
    x = grid::unit(cover_left, "npc"),
    y = grid::unit(1 - 0.5 / 12, "npc"),
    just = c("left", "centre"),
    gp = grid::gpar(
      fontfamily = "Times New Roman",
      fontsize = 11,
      col = "black"
    )
  )
  
  # Divider below the running header.
  grid::grid.lines(
    x = grid::unit(c(cover_left, cover_right), "npc"),
    y = grid::unit(c(1 - 0.75 / 12, 1 - 0.75 / 12), "npc"),
    gp = grid::gpar(col = "grey35", lwd = 0.6)
  )
  
  # Footer divider above the page-number area.
  grid::grid.lines(
    x = grid::unit(c(cover_left, cover_right), "npc"),
    y = grid::unit(c(0.75 / 12, 0.75 / 12), "npc"),
    gp = grid::gpar(col = "grey35", lwd = 0.6)
  )
}

# Place profile-page numbers at an exact position: centered horizontally and
# one-half inch above the bottom edge of the 12-inch page.
draw_profile_page_number <- function(number) {
  grid::grid.text(
    label = number,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5 / 12, "npc"),
    just = "centre",
    gp = grid::gpar(
      fontfamily = "sans",
      fontsize = 8.8,
      col = "black"
    )
  )
}

# Final-page geometry. The fourth row is intentionally left available for the
# x-axis title and the beginning of the caption.
last_page_bottom_omi_in <- 3.3
last_page_top_omi_in <- 1.4
last_page_x_label_y_in <- last_page_bottom_omi_in +
  (12 - last_page_bottom_omi_in - last_page_top_omi_in) / 4

draw_last_page_x_axis_label <- function() {
  grid::grid.text(
    label = "Distance from otolith core (\u00B5m)",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(last_page_x_label_y_in / 12, "npc"),
    just = "centre",
    gp = grid::gpar(
      fontfamily = "sans",
      fontface = "bold",
      fontsize = 12,
      col = "black"
    )
  )
}

# Draw a fully justified, double-spaced caption. Unicode superscript digits
# preserve the isotope notation while allowing accurate word-by-word spacing.
draw_final_figure_caption <- function() {
  caption_text <- paste(
    "Figure S1. Individual otolith strontium-isotope profiles of juvenile winter-run Chinook salmon.",
    "Each panel presents the profile of an individual fish, identified by sample code and assigned brood year.",
    "The normalized otolith \u2078\u2077Sr/\u2078\u2076Sr ratio (black line and points; left y-axis) and Sr signal in volts",
    "(light red line and points; right y-axis) are plotted against distance from the otolith core (\u00B5m).",
    "Vertical error bars around \u2078\u2077Sr/\u2078\u2076Sr measurements represent \u00B12 SE (standard error).",
    "Point colors indicate habitat assignments: Lassen Tributaries (LAS), Sacramento River (SAC),",
    "Feather River or Delta (FEA/DEL), American River (AME), Delta (DEL), and unassigned.",
    "Shaded horizontal bands represent the reference \u2078\u2077Sr/\u2078\u2076Sr ranges associated with LAS, SAC,",
    "FEA/DEL, AME, and DEL habitats. Horizontal dashed lines denote boundaries between selected habitat ranges",
    "and also with the ocean reference threshold. The solid vertical black line indicates the estimated distance",
    "from the otolith core at freshwater exit."
  )
  
  caption_gp <- grid::gpar(
    fontfamily = "Times New Roman",
    fontface = "plain",
    fontsize = 12,
    col = "black"
  )
  left_npc <- 1 / 9.5
  right_npc <- 1 - 1 / 9.5
  available_width <- right_npc - left_npc
  words <- strsplit(caption_text, "\\s+")[[1]]
  
  word_width <- function(word) {
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(word, gp = caption_gp)),
      unitTo = "npc",
      valueOnly = TRUE
    )
  }
  
  widths <- vapply(words, word_width, numeric(1))
  normal_space <- word_width(" ")
  caption_lines <- list()
  current_words <- character(0)
  current_widths <- numeric(0)
  
  for (k in seq_along(words)) {
    proposed_width <- sum(c(current_widths, widths[k])) +
      normal_space * length(current_words)
    
    if (length(current_words) > 0 && proposed_width > available_width) {
      caption_lines[[length(caption_lines) + 1]] <- list(
        words = current_words,
        widths = current_widths
      )
      current_words <- words[k]
      current_widths <- widths[k]
    } else {
      current_words <- c(current_words, words[k])
      current_widths <- c(current_widths, widths[k])
    }
  }
  
  caption_lines[[length(caption_lines) + 1]] <- list(
    words = current_words,
    widths = current_widths
  )
  
  # One empty double-spaced line separates the x-axis title and caption.
  double_line_spacing_in <- 24 / 72
  caption_first_y_in <- last_page_x_label_y_in - 0.5
  
  for (line_index in seq_along(caption_lines)) {
    line <- caption_lines[[line_index]]
    is_last_line <- line_index == length(caption_lines)
    gap <- normal_space
    
    if (!is_last_line && length(line$words) > 1) {
      gap <- (available_width - sum(line$widths)) /
        (length(line$words) - 1)
    }
    
    x_position <- left_npc
    y_position <- (caption_first_y_in -
                     (line_index - 1) * double_line_spacing_in) / 12
    
    for (word_index in seq_along(line$words)) {
      grid::grid.text(
        label = line$words[word_index],
        x = grid::unit(x_position, "npc"),
        y = grid::unit(y_position, "npc"),
        just = c("left", "centre"),
        gp = caption_gp
      )
      x_position <- x_position + line$widths[word_index] + gap
    }
  }
}

# ------------------------------------------------------------------------------
# 5. Individual Profile Plotting Loop (pages 2 onward)
# ------------------------------------------------------------------------------

# Reserve 1.4 inches around the panel matrix. This reduces the plots enough to
# fit the shared axis labels outside the panels while keeping every label at
# least one inch from the corresponding page edge.
par(
  mfrow = c(4, 3),
  mar = c(2.4, 2.8, 1.4, 2.8),
  omi = c(1.4, 1.4, 1.4, 1.4),
  mgp = c(1.6, 0.45, 0),
  tcl = -0.25,
  family = "sans",
  cex.axis = 0.8
)

plots_per_page <- 12
panel_count <- 0
page_number <- 2
last_page_start <- if (length(FishID) > 0) {
  ((length(FishID) - 1) %/% plots_per_page) * plots_per_page + 1
} else {
  NA_integer_
}

for (i in seq_along(FishID)) {
  
  # Enlarge only the last page's bottom margin to create a caption area below
  # the final row of panels. All preceding plot pages retain 1.4-inch margins.
  if (!is.na(last_page_start) && i == last_page_start) {
    par(omi = c(last_page_bottom_omi_in, 1.4, last_page_top_omi_in, 1.4))
  }
  
  # Subset data for the current fish, ensuring we only look at valid distances
  dataSubset <- wr_megafile |>
    filter(Sample_ID == FishID[i], !is.na(Distance_um), Distance_um >= 0) |>
    arrange(Distance_um)
  
  if (nrow(dataSubset) == 0) next
  
  SrV <- dataSubset$SrV
  max_dist <- max(dataSubset$Distance_um)
  
  # --- Base Plot Setup ---
  # Axes are drawn explicitly below to give their text consistent vector output.
  plot(
    dataSubset$Distance_um,
    dataSubset$Sr8786_norm,
    ylim = c(0.7035, 0.7103),
    xlim = c(0, max_dist),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(side = 1, cex.axis = 0.8, lwd = 0.8, lwd.ticks = 0.8)
  axis(side = 2, cex.axis = 0.8, lwd = 0.8, lwd.ticks = 0.8, las = 1)
  box(lwd = 0.8)
  
  # --- 3.1 Reference Polygons (Habitat Ranges) ---
  poly_x <- c(-50, -50, 5000, 5000)
  
  polygon(poly_x, c(0.703, 0.70467, 0.70467, 0.703),
          col = alpha("#F2AD00", 0.3), border = FALSE)
  polygon(poly_x, c(0.70467, 0.7061, 0.7061, 0.70467),
          col = alpha("#F98400", 0.3), border = FALSE)
  polygon(poly_x, c(0.7061, 0.70785, 0.70785, 0.7061),
          col = alpha("#00A08A", 0.2), border = FALSE)
  polygon(poly_x, c(0.7061, 0.707, 0.707, 0.7061),
          col = alpha("#00A08A", 0.1), border = FALSE)
  polygon(poly_x, c(0.70785, 0.711, 0.711, 0.70785),
          col = alpha("#F98400", 0.1), border = FALSE)
  
  # --- 3.2 Labels and Threshold Lines ---
  LAS_max <- 0.70467
  SAC_max <- 0.7061
  
  abline(h = LAS_max, col = "black", lty = "dashed", lwd = 0.8)
  abline(h = SAC_max, col = "black", lty = "dashed", lwd = 0.8)
  abline(h = 0.70918, col = "black", lty = "dashed", lwd = 0.8)
  
  abline(v = dataSubset$FWExit_dist[1], col = alpha("black", 0.9), lwd = 1)
  
  # Shared left alignment for habitat labels. They are drawn after every data
  # layer so their boxes and text remain on top of all plot elements.
  habitat_label_x <- 0.01 * max_dist
  habitat_label_right_x <- 0.98 * max_dist
  
  # --- 3.3 Data Points and Error Bars ---
  lines(dataSubset$Distance_um, dataSubset$Sr8786_norm, lwd = 2)
  
  segments(
    dataSubset$Distance_um,
    dataSubset$Sr8786_norm + dataSubset$SE2,
    dataSubset$Distance_um,
    dataSubset$Sr8786_norm - dataSubset$SE2,
    lwd = 0.7
  )
  
  points(
    dataSubset$Distance_um,
    dataSubset$Sr8786_norm,
    pch = 21,
    cex = 1.5,
    bg = dataSubset$color
  )
  box(lwd = 0.8)
  
  # --- 3.4 Dual Axis: Sr V ---
  par(new = TRUE)
  plot(
    dataSubset$Distance_um,
    SrV,
    pch = 16,
    col = alpha("tomato2", 0.7),
    ylim = c(0, 7),
    xlim = c(0, max_dist),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  lines(dataSubset$Distance_um, SrV, lwd = 2, col = alpha("tomato2", 0.7))
  axis(
    side = 4,
    col = "tomato2",
    col.axis = "tomato2",
    cex.axis = 0.8,
    lwd = 0.8,
    lwd.ticks = 0.8,
    las = 1
  )
  
  # Restore the primary Sr-isotope coordinate system without drawing another
  # data layer, then add the boxed habitat labels as the topmost plot elements.
  par(new = TRUE)
  plot(
    dataSubset$Distance_um,
    dataSubset$Sr8786_norm,
    ylim = c(0.7035, 0.7103),
    xlim = c(0, max_dist),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  
  draw_habitat_label <- function(label, y, x = habitat_label_x,
                                 horizontal_adj = 0, cex = 0.9) {
    label_width <- strwidth(label, cex = cex, font = 1, units = "user")
    label_height <- strheight(label, cex = cex, font = 1, units = "user")
    pad_x <- strwidth("M", cex = cex, font = 1, units = "user") * 0.25
    pad_y <- label_height * 0.20
    text_left <- x - horizontal_adj * label_width
    
    rect(
      xleft = text_left - pad_x,
      ybottom = y - label_height / 2 - pad_y,
      xright = text_left + label_width + pad_x,
      ytop = y + label_height / 2 + pad_y,
      col = alpha("white", 0.7),
      border = "grey30",
      lwd = 0.5
    )
    text(
      x = x,
      y = y,
      labels = label,
      cex = cex,
      font = 1,
      col = "black",
      adj = c(horizontal_adj, 0.5)
    )
  }
  
  draw_habitat_label("Ocean", 0.71005)
  draw_habitat_label("AME", 0.70885)
  draw_habitat_label("DEL", 0.70742,
                     x = habitat_label_right_x, horizontal_adj = 1)
  draw_habitat_label("FEA/DEL", 0.70647,
                     x = habitat_label_right_x, horizontal_adj = 1)
  draw_habitat_label("SAC", 0.70500)
  draw_habitat_label("LAS", 0.70372)
  
  # Place the otolith code/title immediately above each panel.
  mtext(
    text = paste0(dataSubset$Sample_ID[1], " [assigned BY=", dataSubset$Brood_year[1], "]"),
    side = 3,
    line = 0.2,
    adj = 0.02,
    cex = 0.65,
    font = 2
  )
  
  # Draw page-level labels only once per page. Repeatedly overprinting the same
  # text can make it look artificially heavy or jagged in some PDF viewers.
  if (panel_count %% plots_per_page == 0) {
    draw_profile_page_frame()
    
    if (!is.na(last_page_start) && i == last_page_start) {
      draw_last_page_x_axis_label()
    } else {
      mtext(
        text = "Distance from otolith core (\u00B5m)",
        side = 1,
        line = 0.8,
        outer = TRUE,
        cex = 1.1,
        font = 2,
        family = "sans"
      )
    }
    mtext(
      # The source variable is Sr8786_norm, so retain the original 87Sr/86Sr ratio.
      text = expression({}^87 * Sr / {}^86 * Sr),
      side = 2,
      line = 0.8,
      outer = TRUE,
      cex = 1.1,
      font = 2,
      family = "sans"
    )
    mtext(
      text = "Sr V",
      side = 4,
      line = 0.8,
      outer = TRUE,
      cex = 1.1,
      font = 2,
      family = "sans",
      col = "tomato2"
    )
  }
  
  panel_count <- panel_count + 1
  
  if (panel_count %% plots_per_page == 0) {
    draw_profile_page_number(page_number)
    page_number <- page_number + 1
  }
}

if (panel_count %% plots_per_page != 0) {
  draw_profile_page_number(page_number)
}

if (panel_count > 0) {
  draw_final_figure_caption()
}

dev.off()
