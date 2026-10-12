#' @title Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' @description 
#' This script produces PDF files of individual otolith Sr profiles for SAC rearers,
#' sub-classified into three groups:
#'   1. Exclusively Upper Sac: fish whose Sr profiles remain flat within the upper SAC range
#'   2. Borderline Exclusively Upper Sac: fish with ambiguous profiles (subtle rises/bumps)
#'   3. Upper and Lower Sac: fish showing isotopic excursions into the lower SAC/Delta range
#' 
#' @details
#' **Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use
#' 
#' #' **Journal:** Ecosphere
#'
#' **Year:** 2026
#'
#' **DOI:** https://doi.org/10.1002/ecs2.70779
#' **Authors:** 
#' Pedro Morais<sup>1,*</sup>, Anna M. Sturrock<sup>2,3+</sup>,
#' Corey C. Phillis<sup>4</sup>, George Whitman<sup>2</sup>,
#' Stephanie M. Carlson<sup>1</sup>, Rachel C. Johnson<sup>2,5</sup>
#' 
#' **Affiliations:**
#' 1 Department of Environmental Science, Policy, and Management, Mulford Hall, 
#'    University of California, Berkeley, Berkeley, CA 94720, USA.
#' 2 Center for Watershed Sciences, University of California, Davis, 1 Shields Ave, 
#'    Davis, CA 95616, USA.
#' 3 School of Life Sciences, University of Essex, Wivenhoe Park, 
#'    Colchester CO4 3SQ, UK
#' 4 Metropolitan Water District of Southern California, 1121 L Street, 
#'    Suite 900, Sacramento, CA 95814, USA.
#' 5 National Marine Fisheries Service, Southwest Fisheries Science Center, 
#'    Santa Cruz, CA 95060, USA.
#' 
#' + Equal contribution as the first author
#' * Current address. University of Texas Marine Science Institute,
#'   750 Channelview Dr., Port Aransas, TX 78373, USA.
#' 
#' **Correspondence:** 
#' Anna M. Sturrock; Email: anna.sturrock@essex.ac.uk
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
p_load(ggplot2, dplyr, tidyverse, scales)

# ------------------------------------------------------------------------------
# 2. Data Loading and Classification
# ------------------------------------------------------------------------------

# Full otolith data with rearing type assignments
wr_megafile <- read.csv('outputs/oto_sr8786_dat_with_rearing_types.csv') |>
  arrange(Brood_year, Sample_ID)

# Conservative threshold derived from 5 expert-identified reference fish confirmed
# as Exclusively Upper Sac rearers: WR08-25, WR08-32, WR09-13, WR09-17, WR09-19.
ref_ids <- c("WR08-25", "WR08-32", "WR09-13", "WR09-17", "WR09-19")

ref_sac_spots <- wr_megafile |>
  filter(Sample_ID %in% ref_ids,
         !is.na(FWExit_dist),
         Distance_um <= FWExit_dist,
         Habitat == "SAC")

upper_sac_threshold <- max(ref_sac_spots$Sr8786_norm)  # 0.7053813
cat("Upper SAC threshold (max Sr of reference fish SAC spots):", upper_sac_threshold, "\n")

# For each SAC rearer, find max Sr of SAC-labeled spots during FW residence
sac_fw_labeled <- wr_megafile |>
  filter(rearing_type == "SAC",
         !is.na(FWExit_dist),
         Distance_um <= FWExit_dist,
         Habitat == "SAC")

conservative_class <- sac_fw_labeled |>
  group_by(Sample_ID) |>
  summarise(max_sac_sr = max(Sr8786_norm), .groups = "drop") |>
  mutate(upper_sac = max_sac_sr <= upper_sac_threshold)

upper_sac_ids <- conservative_class$Sample_ID[conservative_class$upper_sac]
lower_sac_ids <- conservative_class$Sample_ID[!conservative_class$upper_sac]

cat("Exclusively Upper Sac rearers (Sr threshold):", length(upper_sac_ids), "\n")
cat("Upper and Lower Sac rearers (Sr threshold): ", length(lower_sac_ids), "\n")

# ------------------------------------------------------------------------------
# 3. Colors and Habitat Assignment
# ------------------------------------------------------------------------------

habitat_colors <- list(
  SAC        = "grey60",
  LAS        = "tomato2",
  `FEA/DEL`  = "turquoise4",
  DEL        = "turquoise3",
  AME        = "turquoise",
  Unassigned = "grey10",
  Bay        = "#302f5e"
)

wr_megafile <- wr_megafile |>
  mutate(color = case_when(
    Habitat == "SAC"        ~ habitat_colors$SAC,
    Habitat == "AME"        ~ habitat_colors$AME,
    Habitat == "LAS"        ~ habitat_colors$LAS,
    Habitat %in% c("FEA/DEL", "FEA") ~ habitat_colors[["FEA/DEL"]],
    Habitat == "DEL"        ~ habitat_colors$DEL,
    Habitat == "Unassigned" ~ habitat_colors$Unassigned,
    Habitat == "Bay"        ~ habitat_colors$Bay,
    TRUE ~ "white"
  ))

# ------------------------------------------------------------------------------
# 4. Plotting Functions
# ------------------------------------------------------------------------------

plot_profile <- function(dataSubset) {
  sample_max_dist <- max(dataSubset$Distance_um) + 150
  x_ticks <- pretty(c(0, sample_max_dist), n = 4)
  x_ticks <- x_ticks[x_ticks >= 0 & x_ticks <= sample_max_dist]
  SrV <- dataSubset$SrV
  
  plot(dataSubset$Distance_um, dataSubset$Sr8786_norm,
       ylim = c(0.7035, 0.7103),
       xlim = c(0, sample_max_dist), xaxs = "i",
       type = "n", axes = FALSE, xlab = "", ylab = "")
  axis(side = 1, at = x_ticks, cex.axis = 0.8, lwd = 0.8, lwd.ticks = 0.8)
  axis(side = 2, cex.axis = 0.8, lwd = 0.8, lwd.ticks = 0.8, las = 1)
  box(lwd = 0.8)
  
  # Habitat background bands
  x <- c(0, 0, sample_max_dist, sample_max_dist)
  polygon(x, c(0.703,   0.70467, 0.70467, 0.703),   col = alpha(habitat_colors$LAS, 0.3), border = FALSE) # LAS
  polygon(x, c(0.70467, 0.7061,  0.7061,  0.70467),  col = alpha("grey40", 0.3), border = FALSE) # SAC
  polygon(x, c(0.7061,  0.707,   0.707,   0.7061),   col = alpha(habitat_colors[["FEA/DEL"]], 0.2), border = FALSE) # FEA/DEL
  polygon(x, c(0.7061,  0.70785, 0.70785, 0.7061),   col = alpha(habitat_colors$DEL, 0.3), border = FALSE) # DEL
  polygon(x, c(0.70785, 0.711,   0.711,   0.70785),  col = alpha(habitat_colors$AME, 0.1), border = FALSE) # AME
  
  # Reference lines
  abline(h = 0.70467, col = "black", lty = "dashed", lwd = 0.8) # LAS max
  abline(h = 0.7061,  col = "black", lty = "dashed", lwd = 0.8) # SAC max
  abline(h = 0.70918, col = "black", lty = "dashed", lwd = 0.8) # Ocean
  if (is.finite(dataSubset$FWExit_dist[1])) {
    abline(v = dataSubset$FWExit_dist[1], col = alpha("darkblue", 0.9),
           lwd = 1, lty = "dashed") # Ocean entry
  }
  first_habitat_spot <- which(!is.na(dataSubset$Habitat))[1]
  if (!is.na(first_habitat_spot)) {
    exog_dist_val <- dataSubset$Distance_um[first_habitat_spot]
    if (is.finite(exog_dist_val)) {
      abline(v = exog_dist_val, col = alpha("red", 0.9),
             lwd = 1, lty = "dashed") # Exogenous feeding
    }
  }
  
  # Profile line, error bars, and colored points
  lines(dataSubset$Distance_um, dataSubset$Sr8786_norm, lwd = 2)
  segments(dataSubset$Distance_um,
           dataSubset$Sr8786_norm + dataSubset$SE2,
           dataSubset$Distance_um,
           dataSubset$Sr8786_norm - dataSubset$SE2,
           lwd = 0.7)
  points(dataSubset$Distance_um, dataSubset$Sr8786_norm,
         pch = 21, cex = 1.5, bg = dataSubset$color)
  box(lwd = 0.8)
  
  # Second y-axis: SrV
  par(new = TRUE)
  plot(dataSubset$Distance_um, SrV,
       pch = 16, col = alpha("tomato2", 0.7),
       ylim = c(0, 7), xlim = c(0, sample_max_dist), xaxs = "i",
       axes = FALSE, xlab = "", ylab = "")
  lines(dataSubset$Distance_um, SrV,
        lwd = 2, col = alpha("tomato2", 0.7))
  axis(side = 4, col = "tomato2", col.axis = "tomato2",
       cex.axis = 0.8, lwd = 0.8, lwd.ticks = 0.8, las = 1)
  
  # Restore the isotope coordinates for the right-aligned habitat labels.
  par(new = TRUE)
  plot(dataSubset$Distance_um, dataSubset$Sr8786_norm,
       ylim = c(0.7035, 0.7103),
       xlim = c(0, sample_max_dist), xaxs = "i",
       type = "n", axes = FALSE, xlab = "", ylab = "")
  habitat_labels <- data.frame(
    label = c("Ocean", "AME", "FEA/DEL", "DEL", "SAC", "LAS"),
    x = rep(0.95, 6),
    y = c(0.71000, 0.70820, 0.70730, 0.70640, 0.70495, 0.70365),
    cex = c(0.7, rep(0.6, 5))
  )
  for (j in seq_len(nrow(habitat_labels))) {
    text(x = habitat_labels$x[j] * sample_max_dist,
         y = habitat_labels$y[j], labels = habitat_labels$label[j],
         adj = c(1, 0), font = 1, col = "black", cex = habitat_labels$cex[j])
  }
  
  mtext(text = paste0(dataSubset$Sample_ID[1], " [assigned BY=",
                      dataSubset$Brood_year[1], "]"),
        side = 3, line = 0.2, adj = 0.02, cex = 0.65, font = 2)
}

# Front page, following the supplied example.
draw_cover_page <- function() {
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
    y = 0.615,
    labels = expression(
      plain("Pedro Morais")^{plain("1,*")} *
        plain(", Anna M. Sturrock")^{plain("2,3+")} *
        plain(", Corey C. Phillis")^{plain("4")} *
        plain(", George Whitman")^{plain("2")} *
        plain(", Stephanie M. Carlson")^{plain("1")}
    ),
    adj = c(0, 0.5),
    cex = cover_cex_12,
    family = "Times New Roman"
  )
  text(
    x = cover_left + 0.06,
    y = 0.588,
    labels = expression(plain("Rachel C. Johnson")^{plain("2,5")}),
    adj = c(0, 0.5),
    cex = cover_cex_12,
    family = "Times New Roman"
  )
  
  # Each numbered affiliation is two lines, with a small gap between entries.
  affiliations <- list(
    c("1 Department of Environmental Science, Policy, and Management, Mulford Hall,",
      "   University of California, Berkeley, Berkeley, CA 94720, USA."),
    c("2 Center for Watershed Sciences, University of California, Davis, 1 Shields Ave,",
      "   Davis, CA 95616, USA."),
    c("3 School of Life Sciences, University of Essex, Wivenhoe Park,",
      "   Colchester CO4 3SQ, UK"),
    c("4 Metropolitan Water District of Southern California, 1121 L Street,",
      "   Suite 900, Sacramento, CA 95814, USA."),
    c("5 National Marine Fisheries Service, Southwest Fisheries Science Center,",
      "   Santa Cruz, CA 95060, USA.")
  )
  affiliation_y <- 0.535
  for (affiliation in affiliations) {
    for (line in affiliation) {
      text(x = cover_left, y = affiliation_y, labels = line,
           adj = c(0, 0.5), cex = 1, family = "Times New Roman")
      affiliation_y <- affiliation_y - 0.018
    }
    affiliation_y <- affiliation_y - 0.006
  }
  
  text(x = cover_left, y = 0.305,
       labels = "+ Equal contribution as the first author",
       adj = c(0, 0.5), cex = cover_cex_12,
       family = "Times New Roman")
  
  # Draw the email separately to reproduce the blue, underlined appearance in
  # the supplied front-page example.
  correspondence_prefix <- "Correspondence: Anna M. Sturrock; Email: "
  correspondence_email <- "anna.sturrock@essex.ac.uk"
  correspondence_y <- 0.265
  
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
  
  text(x = cover_left, y = 0.225,
       labels = "* Current address. University of Texas Marine Science Institute,",
       adj = c(0, 0.5), cex = 1, family = "Times New Roman")
  text(x = cover_left, y = 0.207,
       labels = "   750 Channelview Dr., Port Aransas, TX 78373, USA.",
       adj = c(0, 0.5), cex = 1, family = "Times New Roman")
  
  segments(
    x0 = cover_left,
    y0 = 0.75 / 12,
    x1 = cover_right,
    y1 = 0.75 / 12,
    col = "grey45",
    lwd = 0.6
  )
  text(x = 0.5, y = 0.5 / 12, labels = "1", cex = 0.75, col = "grey35")
}

# PDF Helper
draw_profile_page_frame <- function(title_text, page_number) {
  cover_left <- 1 / 9.5
  cover_right <- 1 - cover_left
  previous_xpd <- par(xpd = NA)
  on.exit(par(previous_xpd))
  page_x <- function(x) grconvertX(x, from = "ndc", to = "user")
  page_y <- function(y) grconvertY(y, from = "ndc", to = "user")
  text(page_x(cover_left), page_y(1 - 0.5 / 12),
       labels = title_text,
       adj = c(0, 0.5), family = "Times New Roman", cex = 1)
  segments(page_x(cover_left), page_y(1 - 0.75 / 12),
           page_x(cover_right), page_y(1 - 0.75 / 12),
           col = "grey35", lwd = 0.6)
  segments(page_x(cover_left), page_y(0.75 / 12),
           page_x(cover_right), page_y(0.75 / 12),
           col = "grey35", lwd = 0.6)
  text(page_x(0.5), page_y(0.5 / 12), labels = page_number,
       cex = 0.8, family = "sans")
}

make_pdf <- function(fish_ids, data, filepath, title_text) {
  fish_ids <- fish_ids[order(fish_ids)] # sort for consistency
  
  if (capabilities("cairo")) {
    cairo_pdf(filename = filepath, width = 9.5, height = 12,
              pointsize = 11, family = "sans", antialias = "subpixel")
  } else {
    pdf(file = filepath, width = 9.5, height = 12,
        pointsize = 11, family = "sans", useDingbats = FALSE)
  }
  draw_cover_page()
  
  par(mfrow = c(4, 3), mar = c(2.4, 2.8, 1.4, 2.8),
      omi = c(1.4, 1.4, 1.4, 1.4), mgp = c(1.6, 0.45, 0),
      tcl = -0.25, family = "sans", cex.axis = 0.8)
  panel_count <- 0L
  
  for (id in fish_ids) {
    dataSubset <- data[data$Sample_ID == id & !is.na(data$Distance_um) & data$Distance_um >= 0, ]
    dataSubset <- dataSubset[order(dataSubset$Distance_um), ]
    if (nrow(dataSubset) == 0) next
    plot_profile(dataSubset)
    
    if (panel_count %% 12L == 0L) {
      draw_profile_page_frame(title_text, 2L + panel_count %/% 12L)
      mtext(text = "Distance from otolith core (\u00B5m)", side = 1,
            line = 0.8, outer = TRUE, cex = 1.1, font = 2, family = "sans")
      mtext(text = expression({}^87 * Sr / {}^86 * Sr), side = 2,
            line = 0.8, outer = TRUE, cex = 1.1, font = 2, family = "sans")
      mtext(text = "Sr V", side = 4, line = 0.8, outer = TRUE,
            cex = 1.1, font = 2, family = "sans", col = "tomato2")
    }
    panel_count <- panel_count + 1L
  }
  
  dev.off()
  cat("Saved:", filepath, "\n")
}

# ------------------------------------------------------------------------------
# 5. Stability Criterion and Refinement
# ------------------------------------------------------------------------------

sr_range_threshold <- 0.0005

stability_check <- wr_megafile |>
  filter(Sample_ID %in% upper_sac_ids,
         !is.na(FWExit_dist),
         Distance_um <= FWExit_dist,
         Habitat == "SAC") |>
  arrange(Sample_ID, Distance_um) |>
  group_by(Sample_ID) |>
  summarise(
    sr_range = max(Sr8786_norm[2:n()]) - min(Sr8786_norm[2:n()]),
    passes   = sr_range <= sr_range_threshold,
    .groups  = "drop"
  )

strict_upper_sac_ids <- stability_check$Sample_ID[stability_check$passes]

# Manual exclusion of 3 fish identified by visual inspection
manual_exclusions    <- c("WR16.7070", "WR15-80247", "WR15-80439")
strict_upper_sac_ids <- strict_upper_sac_ids[!strict_upper_sac_ids %in% manual_exclusions]

# Profiles previously treated as borderline; all 21 are now classified as
# exclusively upper SAC after revised analysis by Anna M. Sturrock.
borderline_ids <- c(
  "WR15-7373",  "WR15-80299", "WR16.5015",  "WR16.5031",  "WR16.5069",
  "WR16.5079",  "WR16.5043",  "WR16.5056",  "WR16.7056",  "WR16.7079",
  "WR16.7087",  "WR16.7102",  "WR16.7107",  "WR16.7118",  "WR16.80048",
  "WR16.80053", "WR16.80054", "WR16.80056", "WR16.80062", "WR16.80157",
  "WR17.7005"
)

# Other stability-flagged profiles accepted as exclusively upper SAC on review.
promoted_to_upper_ids <- stability_check$Sample_ID[
  !stability_check$passes &
    !stability_check$Sample_ID %in% borderline_ids
]

all_sac_ids <- sort(unique(wr_megafile$Sample_ID[
  wr_megafile$rearing_type == "SAC" & !is.na(wr_megafile$Sample_ID)
]), method = "radix")

if (anyNA(stability_check$passes)) {
  stop("Some upper-SAC candidates lack enough usable spots for the stability check.")
}
if (length(intersect(borderline_ids, manual_exclusions)) > 0) {
  stop("A reviewed upper-SAC ID also appears among the three lower-SAC exclusions.")
}

final_upper_sac_ids <- sort(unique(c(
  strict_upper_sac_ids, promoted_to_upper_ids, borderline_ids
)), method = "radix")
final_upper_sac_ids <- setdiff(final_upper_sac_ids, manual_exclusions)

# Threshold failures and the three visually excluded fish use both reaches.
# Any reviewed borderline profile takes precedence over a threshold-only result.
lower_sac_ids <- sort(setdiff(
  unique(c(lower_sac_ids, manual_exclusions)), final_upper_sac_ids
), method = "radix")

unexpected_ids <- setdiff(union(final_upper_sac_ids, lower_sac_ids), all_sac_ids)
unclassified_ids <- setdiff(all_sac_ids, union(final_upper_sac_ids, lower_sac_ids))
if (length(unexpected_ids) > 0 || length(unclassified_ids) > 0) {
  stop(paste0(
    "The two final groups do not partition the SAC rearers. Unexpected IDs: ",
    paste(unexpected_ids, collapse = ", "), "; unclassified IDs: ",
    paste(unclassified_ids, collapse = ", ")
  ))
}

# ------------------------------------------------------------------------------
# 6. Combined Classification Criteria and Alphabetical ID List
# ------------------------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
id_section <- c(
  "4. Alphabetical sample IDs by final group",
  "Sample IDs are sorted alphabetically and comma-separated within each final group.",
  "The 21 previously borderline profiles are included in UPPER SACRAMENTO group",
  "",
  paste0("UPPER SACRAMENTO RIVER (n = ", length(final_upper_sac_ids), ")"),
  paste(final_upper_sac_ids, collapse = ", "),
  "",
  paste0("UPPER AND LOWER SACRAMENTO RIVER (n = ",
         length(lower_sac_ids), ")"),
  paste(lower_sac_ids, collapse = ", ")
)

criteria_file <- "outputs/SAC_Upper_Lower_classification_Criteria_and_IDs.txt"
writeLines(
  c(
    "SACRAMENTO RIVER REARING CLASSIFICATION CRITERIA",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    paste0("All SAC rearers: n = ", length(all_sac_ids)),
    paste0("Final upper Sacramento only: n = ", length(final_upper_sac_ids)),
    paste0("Final upper and lower Sacramento: n = ", length(lower_sac_ids)),
    "Only the two final rearing groups have separate profile PDFs.",
    "",
    "1. Reference threshold",
    paste0("   Maximum SAC-labelled freshwater 87Sr/86Sr among five reference fish: ",
           format(upper_sac_threshold, digits = 8)),
    paste0("   Reference IDs: ", paste(ref_ids, collapse = ", ")),
    paste0("   Provisional upper-SAC threshold passes: n = ", length(upper_sac_ids)),
    "   SAC-labelled freshwater spots at or below this maximum provisionally",
    "   pass as upper Sacramento only; fish exceeding it enter upper and lower SAC.",
    "",
    "2. Profile stability and visual review",
    paste0("   Stability range threshold, excluding the first SAC spot: ",
           sr_range_threshold),
    "   A larger range flags a profile for visual review; it does not by itself",
    "   assign the fish to upper and lower Sacramento.",
    "",
    paste0("   Profiles flagged for visual review: n = ",
           sum(!stability_check$passes)),
    paste0("   Their IDs: ",
           paste(sort(unique(stability_check$Sample_ID[!stability_check$passes]),
                      method = "radix"), collapse = ", ")),
    "",
    paste0("   Other stability-flagged profiles accepted as upper SAC: n = ",
           length(promoted_to_upper_ids)),
    paste0("   Their IDs: ",
           paste(sort(unique(promoted_to_upper_ids), method = "radix"),
                 collapse = ", ")),
    "",
    paste0("   Previously borderline profiles reassigned to upper SAC after",
           " revised analysis by Anna M. Sturrock: n = ", length(borderline_ids)),
    paste0("   Their IDs: ",
           paste(sort(unique(borderline_ids), method = "radix"),
                 collapse = ", ")),
    "",
    paste0("   Visually excluded profiles assigned to upper and lower SAC: n = ",
           length(manual_exclusions)),
    paste0("   Their IDs: ",
           paste(sort(unique(manual_exclusions), method = "radix"),
                 collapse = ", ")),
    "",
    "3. Final groups",
    "   Upper SAC includes stability passes, visually accepted flagged profiles,",
    "   and all previously borderline profiles; it excludes the three manual",
    "   lower-SAC exclusions.",
    "   Upper and lower SAC includes threshold failures and the three manual",
    "   exclusions, excluding any profile reassigned to upper SAC on review.",
    "   Final profile PDFs: outputs/SAC_Upper_Sac_Only.pdf and",
    "   outputs/SAC_Upper_and_Lower_Sac.pdf",
    "",
    id_section
  ),
  con = criteria_file
)
cat("Saved:", criteria_file, "\n")

# ------------------------------------------------------------------------------
# 7. PDF Generation
# ------------------------------------------------------------------------------

# Overview of all SAC rearers
make_pdf(
  fish_ids   = all_sac_ids,
  data       = wr_megafile,
  filepath   = "outputs/SAC_All_Rearers.pdf",
  title_text = "Winter-run Chinook salmon that reared in the Sacramento River"
)

# Final upper Sacramento only, including the 21 reviewed profiles
make_pdf(
  fish_ids   = final_upper_sac_ids,
  data       = wr_megafile,
  filepath   = "outputs/SAC_Upper_Sac_Only.pdf",
  title_text = "Winter-run Chinook salmon that reared in the upper Sacramento River"
)

# Final upper and lower Sacramento
make_pdf(
  fish_ids   = lower_sac_ids,
  data       = wr_megafile,
  filepath   = "outputs/SAC_Upper_and_Lower_Sac.pdf",
  title_text = "Winter-run Chinook salmon that reared in the upper and lower Sacramento River"
)

