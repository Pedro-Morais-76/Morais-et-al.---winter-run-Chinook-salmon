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

sacc  <- "#F98400"
lasc  <- "gold1"
delc  <- "#00A08A"
amec  <- "#FF0000"
xc    <- "darkgrey"

wr_megafile <- wr_megafile |>
  mutate(color = case_when(
    Habitat == "SAC"        ~ sacc,
    Habitat == "AME"        ~ amec,
    Habitat == "LAS"        ~ lasc,
    Habitat == "DEL"        ~ delc,
    Habitat == "Unassigned" ~ xc,
    TRUE ~ "white"
  ))

# ------------------------------------------------------------------------------
# 4. Plotting Functions
# ------------------------------------------------------------------------------

plot_profile <- function(dataSubset) {
  SrV <- dataSubset$SrV
  
  plot(dataSubset$Distance_um, dataSubset$Sr8786_norm,
       ylim = c(0.7035, 0.7103),
       xlim = c(0, max(dataSubset$Distance_um)),
       type = "n")
  
  # Habitat background bands
  x <- c(-50, -50, 5000, 5000)
  polygon(x, c(0.703,   0.70467, 0.70467, 0.703),   col = alpha("#F2AD00", 0.3), border = FALSE) # LAS
  polygon(x, c(0.70467, 0.7061,  0.7061,  0.70467),  col = alpha("#F98400", 0.3), border = FALSE) # SAC
  polygon(x, c(0.7061,  0.70785, 0.70785, 0.7061),   col = alpha("#00A08A", 0.2), border = FALSE) # DEL
  polygon(x, c(0.7061,  0.707,   0.707,   0.7061),   col = alpha("#00A08A", 0.1), border = FALSE) # FEA/DEL
  polygon(x, c(0.70785, 0.711,   0.711,   0.70785),  col = alpha("#F98400", 0.1), border = FALSE) # AME
  
  # Title
  title(main = paste(dataSubset$Sample_ID[1], "[assigned BY =", dataSubset$Brood_year[1], "]"),
        line = -1.5, cex.main = 0.8)
  
  # Reference lines
  abline(h = 0.70467, col = "black", lty = "dashed", lwd = 0.8) # LAS max
  abline(h = 0.7061,  col = "black", lty = "dashed", lwd = 0.8) # SAC max
  abline(h = 0.70918, col = "black", lty = "dashed", lwd = 0.8) # Ocean
  abline(v = dataSubset$FWExit_dist[1], col = alpha("black", 0.9), lwd = 1) # FW exit
  
  # Labels
  text(max(dataSubset$Distance_um), 0.70934, labels = "Ocean", font = 3, col = "black", cex = 0.8, pos = 2)
  text(-11, 0.7084,  labels = "AME",     font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70742, labels = "DEL",     font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70647, labels = "FEA/DEL", font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70527, labels = "SAC",     font = 3, col = "black", cex = 0.5, pos = 4)
  text(-11, 0.70391, labels = "LAS",     font = 3, col = "black", cex = 0.5, pos = 4)
  
  # Profile line, error bars, and colored points
  lines(dataSubset$Distance_um, dataSubset$Sr8786_norm, lwd = 2)
  segments(dataSubset$Distance_um,
           dataSubset$Sr8786_norm + dataSubset$SE2,
           dataSubset$Distance_um,
           dataSubset$Sr8786_norm - dataSubset$SE2,
           lwd = 0.7)
  points(dataSubset$Distance_um, dataSubset$Sr8786_norm,
         pch = 21, cex = 1.5, bg = dataSubset$color)
  box()
  
  # Second y-axis: SrV
  par(new = TRUE)
  plot(dataSubset$Distance_um, SrV,
       pch = 16, col = alpha("tomato2", 0.7),
       ylim = c(0, 7), xlim = c(0, max(dataSubset$Distance_um)),
       axes = FALSE, xlab = NA, ylab = NA)
  lines(dataSubset$Distance_um, SrV,
        lwd = 2, col = alpha("tomato2", 0.7))
  axis(side = 4, col.axis = "tomato2")
}

# PDF Helper
make_pdf <- function(fish_ids, data, filepath, title_text) {
  fish_ids <- fish_ids[order(fish_ids)] # sort for consistency
  
  pdf(file = filepath, width = 8.5, height = 11)
  par(mfrow = c(4, 3), mar = c(2, 2, 1, 2), oma = c(3, 3, 3, 3))
  
  for (id in fish_ids) {
    dataSubset <- data[data$Sample_ID == id & !is.na(data$Distance_um) & data$Distance_um >= 0, ]
    dataSubset <- dataSubset[order(dataSubset$Distance_um), ]
    if (nrow(dataSubset) == 0) next
    plot_profile(dataSubset)
  }
  
  mtext(text = "Distance from otolith core (um)", side = 1, line = 0, outer = TRUE)
  mtext(text = "Otolith 87Sr/86Sr",               side = 2, line = 0, outer = TRUE)
  mtext(text = "Otolith Sr V",                     side = 4, line = 0, outer = TRUE)
  mtext(text = title_text,                          side = 3, line = 0, outer = TRUE, font = 2)
  
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

# Borderline Exclusively Upper SAC (21 fish)
borderline_ids <- c(
  "WR15-7373",  "WR15-80299", "WR16.5015",  "WR16.5031",  "WR16.5069",
  "WR16.5079",  "WR16.5043",  "WR16.5056",  "WR16.7056",  "WR16.7079",
  "WR16.7087",  "WR16.7102",  "WR16.7107",  "WR16.7118",  "WR16.80048",
  "WR16.80053", "WR16.80054", "WR16.80056", "WR16.80062", "WR16.80157",
  "WR17.7005"
)

# Promoted to Exclusively Upper SAC
promoted_to_upper_ids <- stability_check$Sample_ID[
  !stability_check$passes &
  !stability_check$Sample_ID %in% borderline_ids
]

final_upper_sac_ids <- c(strict_upper_sac_ids, promoted_to_upper_ids)
all_sac_ids         <- unique(wr_megafile$Sample_ID[wr_megafile$rearing_type == "SAC"])

# Reclassified to Lower Sac
lower_sac_ids <- unique(c(lower_sac_ids, manual_exclusions))

# ------------------------------------------------------------------------------
# 6. PDF Generation
# ------------------------------------------------------------------------------

# PDF 1: all 481 SAC rearers
make_pdf(
  fish_ids   = all_sac_ids,
  data       = wr_megafile,
  filepath   = "figures/SAC_all_rearers.pdf",
  title_text = paste0("All Sacramento Rearers (n = ", length(all_sac_ids), ")")
)

# PDF 2: all fish meeting the Sr threshold — pre-visual-review
make_pdf(
  fish_ids   = upper_sac_ids,
  data       = wr_megafile,
  filepath   = "figures/Exclusively_Upper_Sac_Sr_threshold_only.pdf",
  title_text = paste0("Exclusively Upper Sac — Sr threshold ≤ 0.7054 only (n = ", length(upper_sac_ids), ")")
)

# PDF 3: final Exclusively Upper Sac rearers
make_pdf(
  fish_ids   = final_upper_sac_ids,
  data       = wr_megafile,
  filepath   = "figures/Exclusively_Upper_Sac_final.pdf",
  title_text = paste0("Exclusively Upper Sac — final (n = ", length(final_upper_sac_ids), ")")
)

# PDF 4: Borderline Exclusively Upper Sac rearers
make_pdf(
  fish_ids   = borderline_ids,
  data       = wr_megafile,
  filepath   = "figures/Borderline_Exclusively_Upper_Sac.pdf",
  title_text = paste0("Borderline Exclusively Upper Sac — ambiguous profiles (n = ", length(borderline_ids), ")")
)

# PDF 5: Upper and Lower Sac rearers
make_pdf(
  fish_ids   = lower_sac_ids,
  data       = wr_megafile,
  filepath   = "figures/Lower_Sacramento_rearers.pdf",
  title_text = paste0("Upper and Lower Sac rearers (n = ", length(lower_sac_ids), ")")
)

# ------------------------------------------------------------------------------
# 7. Documentation Export
# ------------------------------------------------------------------------------

writeLines(
  c(
    "============================================================",
    "CLASSIFICATION CRITERIA: EXCLUSIVELY UPPER SAC vs UPPER AND LOWER SAC REARERS",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "============================================================",
    "",
    "OVERVIEW",
    "--------",
    "Fish assigned rearing_type == 'SAC' (n = 481) were sub-classified into",
    "three groups based on their otolith 87Sr/86Sr profiles during freshwater",
    "(FW) residence:",
    "",
    "  Exclusively Upper Sac: isotopic values remain consistently close to",
    "  ~0.705 throughout the FW phase, with no excursion into the lower",
    "  Sacramento / Delta range (~0.706).",
    "",
    "  Borderline Exclusively Upper Sac: profiles are ambiguous, with subtle",
    "  isotopic rises or minor bumps that cannot be confidently attributed to",
    "  either exclusive upper Sacramento rearing or lower mainstem influence.",
    "",
    "  Upper and Lower Sac: isotopic values show a characteristic mid-profile",
    "  bump rising into the ~0.706 range before ocean entry, indicating use of",
    "  the lower mainstem Sacramento River during rearing.",
    "",
    "Three classification levels were applied, each progressively more",
    "conservative.",
    "",
    "------------------------------------------------------------",
    "LEVEL 1 — ALL SAC REARERS",
    "------------------------------------------------------------",
    paste0("  n = ", length(all_sac_ids)),
    "  All fish assigned rearing_type == 'SAC' in script 07_rearing_types.R.",
    "  PDF: figures/SAC_all_rearers.pdf",
    "",
    "------------------------------------------------------------",
    "LEVEL 2 — EXCLUSIVELY UPPER SAC (Sr threshold)",
    "------------------------------------------------------------",
    paste0("  n = ", length(upper_sac_ids)),
    "  Criterion: all SAC-labelled FW spots (Habitat == 'SAC',",
    "  Distance_um <= FWExit_dist) must have Sr8786_norm <= upper_sac_threshold.",
    "",
    paste0("  upper_sac_threshold = ", round(upper_sac_threshold, 7)),
    "  This threshold is the maximum 87Sr/86Sr value observed among SAC-labelled",
    "  FW spots of 5 expert-identified reference fish whose profiles were",
    "  visually confirmed as Exclusively Upper Sac rearers:",
    paste0("    ", paste(ref_ids, collapse = ", ")),
    "",
    "  Rationale: fish rearing exclusively in the upper Sacramento River",
    "  show SAC-labelled spots tightly clustered near 0.705. Any spot",
    "  exceeding the reference maximum indicates influence from the lower",
    "  Sacramento River or Delta, whose Sr signature is higher (~0.706).",
    "  PDF: figures/Exclusively_Upper_Sac_Sr_threshold_only.pdf",
    "",
    "------------------------------------------------------------",
    "LEVEL 3 — EXCLUSIVELY UPPER SAC FINAL (Sr threshold + visual review)",
    "------------------------------------------------------------",
    paste0("  n = ", length(final_upper_sac_ids)),
    "  Applied on top of Level 2. Two filters, then visual review:",
    "",
    "  (a) ISOTOPIC RANGE STABILITY",
    paste0("      sr_range_threshold = ", sr_range_threshold),
    "      For each fish, the range (max - min) of SAC-labelled FW spots",
    "      excluding the first spot is computed. The first spot is excluded",
    "      because it may retain a residual maternal isotopic signal.",
    "      Fish whose range exceeds the threshold are flagged for review.",
    "",
    "      Threshold derivation: the maximum range among the 5 reference",
    "      fish was ", round(max(stability_check$sr_range[stability_check$Sample_ID %in% ref_ids]), 7), ".",
    "      Problem fish with visually confirmed rising or bumpy profiles",
    paste0("      had a minimum range of 0.000545, leaving a clean gap."),
    paste0("      A threshold of ", sr_range_threshold, " was chosen to sit within this gap."),
    paste0("      This criterion flagged ", sum(!stability_check$passes), " fish from Level 2 for visual review."),
    "",
    "  (b) VISUAL REVIEW OF FLAGGED FISH",
    "      All 39 flagged fish were individually inspected. Of these:",
    paste0("      - ", length(promoted_to_upper_ids), " were confirmed as Exclusively Upper Sac rearers."),
    paste0("      - ", length(borderline_ids), " were classified as Borderline Exclusively Upper Sac."),
    "      Borderline fish show subtle rises or minor bumps that could reflect",
    "      real lower-Sacramento influence or measurement noise.",
    "  PDF: figures/Exclusively_Upper_Sac_final.pdf",
    "",
    "------------------------------------------------------------",
    "BORDERLINE EXCLUSIVELY UPPER SAC REARERS",
    "------------------------------------------------------------",
    paste0("  n = ", length(borderline_ids)),
    "  Fish that passed the Sr threshold (Level 2) but failed the stability",
    "  criterion and showed ambiguous profiles upon visual inspection.",
    "  Their otolith profiles have subtle isotopic variation that cannot be",
    "  confidently attributed to either Exclusively Upper Sac or Upper and",
    "  Lower Sac rearing.",
    "  PDF: figures/Borderline_Exclusively_Upper_Sac.pdf",
    "",
    "------------------------------------------------------------",
    "UPPER AND LOWER SAC REARERS",
    "------------------------------------------------------------",
    paste0("  n = ", length(lower_sac_ids)),
    "  Comprises two groups:",
    paste0("    (i)  SAC rearers not meeting the Level 2 Sr threshold (n = ",
           length(lower_sac_ids) - length(manual_exclusions), "):"),
    "         Fish with SAC-labelled FW spots exceeding the upper SAC threshold,",
    paste0("         consistent with use of the lower Sacramento River (Sr > ", round(upper_sac_threshold, 7), ")."),
    paste0("    (ii) Fish reclassified from Level 2 passes after visual review (n = ", length(manual_exclusions), "):"),
    "         Three fish passed the algorithmic Sr range stability criterion but",
    "         were excluded after individual visual inspection because their",
    "         profiles showed patterns inconsistent with Exclusively Upper Sac rearing:",
    "           WR16.7070  — monotonic rise in the last 3 SAC spots",
    "           WR15-80247 — single large upward jump between spots 4 and 5",
    "           WR15-80439 — bump spanning spots 5 to 7",
    "         These fish are counted as Upper and Lower Sac rearers.",
    "  PDF: figures/Lower_Sacramento_rearers.pdf",
    "",
    "============================================================",
    "SUMMARY TABLE",
    "============================================================",
    sprintf("  %-50s %5s %6s", "Group", "n", "%"),
    sprintf("  %-50s %5s %6s", "-----", "-", "---"),
    sprintf("  %-50s %5d %5.1f%%", "All SAC rearers (Level 1)",                         length(all_sac_ids),         100),
    sprintf("  %-50s %5d %5.1f%%", "Exclusively Upper Sac — Sr threshold (Level 2)",    length(upper_sac_ids),       length(upper_sac_ids)       / length(all_sac_ids) * 100),
    sprintf("  %-50s %5d %5.1f%%", "Exclusively Upper Sac — final",                     length(final_upper_sac_ids), length(final_upper_sac_ids) / length(all_sac_ids) * 100),
    sprintf("  %-50s %5d %5.1f%%", "Borderline Exclusively Upper Sac",                  length(borderline_ids),      length(borderline_ids)      / length(all_sac_ids) * 100),
    sprintf("  %-50s %5d %5.1f%%", "Upper and Lower Sac (incl. 3 reclassified)",        length(lower_sac_ids),       length(lower_sac_ids)       / length(all_sac_ids) * 100),
    "============================================================",
    "",
    "============================================================",
    "SAMPLE ID LISTS BY GROUP",
    "============================================================",
    "(sorted alphabetically within each group)",
    "",
    "------------------------------------------------------------",
    paste0("EXCLUSIVELY UPPER SAC REARERS (n = ", length(final_upper_sac_ids), ")"),
    "------------------------------------------------------------",
    paste0("  ", sort(final_upper_sac_ids)),
    "",
    "------------------------------------------------------------",
    paste0("BORDERLINE EXCLUSIVELY UPPER SAC REARERS (n = ", length(borderline_ids), ")"),
    "------------------------------------------------------------",
    paste0("  ", sort(borderline_ids)),
    "",
    "------------------------------------------------------------",
    paste0("UPPER AND LOWER SAC REARERS (n = ", length(lower_sac_ids), ", incl. 3 reclassified)"),
    "------------------------------------------------------------",
    paste0("  ", sort(lower_sac_ids)),
    "============================================================"
  ),
  con = "outputs/SAC_classification_criteria.txt"
)

cat("Saved: outputs/SAC_classification_criteria.txt\n")
