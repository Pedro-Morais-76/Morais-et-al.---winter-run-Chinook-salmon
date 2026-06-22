# Winter-Run Chinook Salmon Freshwater Rearing Habitat Use: Otolith Microchemistry Analysis

**Publication:** Morais et al., *Ecosphere* (2026)  
**Project Title:** Droughts delay juvenile salmon migration and truncate diversity in habitat use

---

## Project Overview

This repository contains R scripts, data files, and output tables from a comprehensive otolith microchemistry analysis of winter-run Chinook salmon. The analysis uses strontium isotope ratios (⁸⁷Sr/⁸⁶Sr) measured along otolith profiles to reconstruct the freshwater rearing habitat use of individual fish, with integration of environmental (river flow, temperature) and juvenile passage data to understand how habitat use and growth relate to environmental conditions during rearing.

**Sample:** 705 winter-run Chinook salmon otoliths  
**Brood Years:** 2004–2006 and 2011–2015 (adult escapement 2007–2009 and 2015–2017)  
**Geographic Focus:** Sacramento River system, California  

---

## Directory Structure & File Descriptions

### 📁 **scripts/** — R Analysis Pipeline (13 files)

Scripts are numbered to represent sequential workflow steps. Each script reads input data and generates output used by subsequent scripts. All scripts use R packages: `dplyr`, `ggplot2`, and others as specified in code.

| Script | Purpose | Input | Output |
|--------|---------|-------|--------|
| `01_fw_exit_dist.R` | Estimate freshwater exit distance from otolith using two methods: SrV (Sr concentration proxy) and Sr⁸⁷/⁸⁶Sr thresholds. Interpolates exit distance when profile crosses threshold value. | `data/oto_sr8786_dat_all_yrs.csv`, `data/otos_to_use_SrV_exit_dist.csv` | `outputs/sr8786_dat_with_fw_exit_dist.csv`, `outputs/WR_FW_Exit_allyrs.csv` |
| `02_wr_habitat_assignment_function.R` | Defines `wr_habitat_assigner()` function that assigns a habitat label (LAS, SAC, DEL, or AME) to each otolith measurement spot based on its Sr⁸⁷/⁸⁶Sr ratio. Includes logic to reclassify Delta-assigned spots in certain sequences. | — | — (function definition) |
| `03_habitat_assignments.R` | Applies habitat assignment function to all fish. Manual quality-control adjustments made to specific spots based on visual review of profile anomalies. | `outputs/sr8786_dat_with_fw_exit_dist.csv` | `outputs/oto_sr8786_dat_with_assignments.csv` |
| `04_cohort_reconstruction.R` | Assigns age and brood year to each fish using coded wire tag (CWT) data and scale read ages from `winter_run_2005-2018_scale_reads.csv`. Uses fork length thresholds to apply age cutoffs for different cohorts. | `outputs/oto_sr8786_dat_with_assignments.csv`, `data/winter_run_2005-2018_scale_reads.csv` | `outputs/oto_sr8786_dat_with_brood_year.csv`, `figures/FigS1_FL_distributions.jpg` |
| `05_individual_profile_plots.R` | Generates individual otolith Sr⁸⁷/⁸⁶Sr profiles displayed on 4×3 grids (multiple PDFs). Profiles show all spots for a single fish plotted along distance from otolith core. | `outputs/oto_sr8786_dat_with_brood_year.csv` | `figures/FigS2_Individual_otolith_profiles.pdf` |
| `06_profile_plots.R` | Shows examples of six juvenile rearing patterns observed in returning winter-run Chinook salmon based on which habitat accounts for the largest proportion of freshwater growth. | `outputs/oto_sr8786_dat_with_brood_year.csv` | `figures/Fig2_example_profiles.tiff` |
| `07_rearing_types.R` | Assigns a primary rearing type (LAS, SAC, DEL, AME, or X=unassigned) to each fish based on which habitat accounts for largest proportion of freshwater growth. Calculates proportion of FW growth per habitat per fish. | `outputs/oto_sr8786_dat_with_brood_year.csv` | `outputs/oto_sr8786_dat_with_rearing_types.csv`, `outputs/rearing_type_by_fish.csv`, `outputs/rearing_type_by_BY.csv`, `figures/Fig1_rearing_type_props.jpg` |
| `08_upper_lower_sac_profiles.R` | Sub-classifies all SAC rearers (n=481) into three groups: Exclusively Upper Sacramento (final n=63), Borderline Exclusively Upper Sacramento (n=21), and Upper & Lower Sacramento (n=397, including 3 visually reclassified fish). Uses Sr⁸⁷/⁸⁶Sr threshold (0.7053813) and isotopic range stability criterion followed by visual review. Generates PDFs for each classification group and documentation text. | `outputs/oto_sr8786_dat_with_rearing_types.csv` | `figures/Borderline_Exclusively_Upper_Sac.pdf`, `figures/Exclusively_Upper_Sac_final.pdf`, `figures/Exclusively_Upper_Sac_Sr_threshold_only.pdf`, `figures/SAC_all_rearers.pdf`, `figures/Lower_Sacramento_rearers.pdf`, `outputs/SAC_classification_criteria.txt` |
| `09_mass_assimilated.R` | Reconstructs total body mass from otolith radius using allometric relationship from `juv_fall_run_oto_radius_total_weight.csv`. Estimates proportion of total mass gained during freshwater vs. ocean residence, and within freshwater residences by habitat. | `outputs/oto_sr8786_dat_with_rearing_types.csv`, `data/juv_fall_run_oto_radius_total_weight.csv` | `outputs/prop_fw_growth_by_fish.csv`, `outputs/summary_stats_fw_growth.csv`, `figures/Fig3_prop_fw_growth_by_fish.jpg`, `outputs/by_fish_prop_growth_wide_format.csv` |
| `10_flow_temp_summaries_plots.R` | Summarizes daily Sacramento River flow and temperature from USGS gauge (11390500) for analysis period. Generates annual hydrographs. | `data/Sac_flow_temp_USGS11390500_2000to21.xlsx` | `outputs/daily_flow_temp.csv`, `outputs/flow_temp_stats_AugtoJan_only.csv`, `figures/FigS3_flows_per_year.jpg` |
| `11_hydrographs_vs_mean_fw_growth_by_yr.R` | Compares river hydrographs (flow, temperature) with mean freshwater growth by brood year. Integrates juvenile passage data from Red Bluff Diversion Dam. | `outputs/daily_flow_temp.csv`, `data/redbluffdaily_1609788856_999-2004.csv`, `data/redbluffdaily_1609788890_111-2005.csv`, `data/redbluffdaily_1609788903_675-2006.csv`, `data/redbluffdaily_1609788913_979-2007.csv`, `data/redbluffdaily_1609788962_73-2012.csv`, `data/redbluffdaily_1609788972_925-2013.csv`, `data/redbluffdaily_1609788981_630-2014.csv`, `data/redbluffdaily_1609788990_951-2015.csv`, `outputs/summary_stats_fw_growth.csv` | `outputs/mean_aug_jan_flows.csv`, `figures/Fig5_fw_growth_passage_flow.jpg` |
| `12_regression_plots.R` | Performs statistical analyses testing relationships: (1) non-natal rearing frequency vs. river flow; (2) freshwater growth in non-natal habitats vs. flow; (3) density-dependent effects. Generates regression plots and model summaries. | `outputs/mean_aug_jan_flows.csv`, `outputs/rearing_type_by_BY.csv`, `outputs/summary_stats_fw_growth.csv`, `data/RBDD_RST_Juv_Production.csv` | `figures/Fig4_rest_stop.jpg`, `figures/Fig6_downstream_occupancy.jpg` |
| `13_fw_exit_size_plot.R` | Creates figure showing fork length distribution at freshwater exit for different cohorts/habitats. | `outputs/oto_sr8786_dat_with_brood_year.csv` | `figures/FigS4_fw_exit_size.jpg` |

---

### 📁 **data/** — Input Data Files (14 files)

#### Otolith Microchemistry Data

**`oto_sr8786_dat_all_yrs.csv`** (1.5 MB, 14,865 rows, 15 columns)  
Raw strontium isotope measurements from otolith microchemistry analysis. One row per laser ablation spot. (Note: file includes an index column `X` that may be present depending on how the file is read.)

| Column | Data Type | Description | Units / Interpretation |
|--------|-----------|-------------|------------------------|
| Sample_ID | character | Unique identifier for each fish | Format: WR[yy]-[###] or WR[yy].[5/7][###], where yy = escapement year, ### = fish number; 5/7 = basin location code |
| collect_date | character | Date sample collected (field not populated in this version) | NA |
| Escap_yr | numeric | Year adult fish escaped (returned to spawning grounds) | Year; range 2007–2017 |
| sex | character | Sex of fish at collection | "Unknown", "Male", "Female" |
| Exog_dist | numeric | Distance from otolith core to first confirmed exogenous food (yolk transition point) | micrometers (µm); indicates start of record analyzable for habitat inference |
| Spot_no | numeric | Sequential number of laser ablation spot | Integer; incrementing from 1 onward |
| Distance_um | numeric | Distance of spot from otolith core | micrometers (µm); cumulative distance along otolith profile |
| Sr8786_norm | numeric | Measured ⁸⁷Sr/⁸⁶Sr isotope ratio | Dimensionless ratio; baseline ~0.7045 (pure freshwater) to ~0.708+ (ocean water) |
| SE2 | numeric | Standard error of Sr⁸⁷/⁸⁶Sr measurement | Dimensionless; typical values 0.0001–0.0005 |
| SrV | numeric | Relative strontium concentration proxy | Dimensionless; normalized within-fish; used to detect FW exit (ocean entry threshold ~66%) |
| respot | character | Whether spot was re-measured | "y" or "n"; respots excluded from some analyses due to instrument drift |
| Spot_size_um | numeric | Laser ablation spot diameter | micrometers (µm); typically 60 µm |
| Readage | numeric | Whether scale age was read (field not populated in this version) | NA |
| fork_length | numeric | Fork length measurement (field not populated in this version) | NA |

#### Habitat Reference Data

**`otos_to_use_SrV_exit_dist.csv`** (1.2 KB, 65 rows, 2 columns)  
Quality control specification: fish for which SrV method is the preferred FW exit distance estimate due to missing or unreliable Sr⁸⁷/⁸⁶Sr data.

| Column | Data Type | Description |
|--------|-----------|-------------|
| Sample_ID | character | Fish ID |
| Rule | character | Rule applied to determine the preferred FW exit distance estimation method for this fish |

#### Growth Calibration Data

**`juv_fall_run_oto_radius_total_weight.csv`** (58.3 KB, 482 rows, 14 columns)  
Calibration dataset from juvenile fall-run Chinook salmon relating otolith radius to body mass. Used to back-calculate mass at FW exit and subsequent growth intervals. (Note: file includes an unnamed row index column, similar to the raw otolith file.)

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Sample_ID | character | Unique identifier (fall-run fish) | — |
| OR | numeric | Otolith radius | micrometers (µm) |
| Project | character | Data source project | — |
| Date_sampled | character | Collection date | — |
| WY | numeric | Water year | Year |
| FL | numeric | Fork length | millimeters (mm) |
| TW | numeric | Total weight | grams (g) |
| Station_code | numeric | Collection location code | — |
| Site.Name | character | Collection site name | — |
| Region_USE | character | Region code | — |
| Lat | numeric | Latitude | decimal degrees |
| Long | numeric | Longitude | decimal degrees |
| Region_subdivided5 | character | Subdivision region | — |

#### Cohort Data

**`winter_run_2005-2018_scale_reads.csv`** (236.4 KB, 8,116 rows, 8 columns)  
Scale-based age and coded wire tag (CWT) data for winter-run Chinook salmon. Used to assign brood year and age to otolith samples.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Year | numeric | Year specimen was collected/escaped | Year |
| Sample Number | numeric | Sequential number within sample set | — |
| Age | numeric | Age from scale read | years (age 3, 4, etc.) |
| Readage | numeric | Age class for reproducibility checking | — |
| Recovery Date | character | Date recovered/collected | — |
| Fork Length (mm) | numeric | Fork length at collection | millimeters (mm) |
| Sex | character | Sex of fish | "Unknown", "Male", "Female", "Spawned Male", "Spawned Female" |
| CWT Code | character | Coded wire tag identifier if present | — |

#### Environmental Data

**`Sac_flow_temp_USGS11390500_2000to21.xlsx`** (616.9 KB)  
Daily hydrological data from USGS gauge 11390500 (Sacramento River below Wilkins Slough, CA). Data downloaded from USGS National Water Information System (NWIS). Raw file contains USGS metadata header rows and coded parameter columns; the script (`10_flow_temp_summaries_plots.R`) renames and selects the following usable fields:

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| date | — | Date of measurement | YYYY-MM-DD |
| flow_cms | numeric | Mean daily stream flow | cubic meters per second (m³/s) |
| max_daily_temp_C | numeric | Maximum daily water temperature | degrees Celsius (°C) |
| median_daily_temp_C | numeric | Median daily water temperature | degrees Celsius (°C) |

**Red Bluff Diversion Dam Daily Passage Files** (8 files, ~275 KB total)  
Daily juvenile fish passage estimates from rotary screw trap (RST) at Red Bluff Diversion Dam, CA (CDFW). File names: `redbluffdaily_[timestamp]-[year].csv`. Years covered: 2004–2007, 2012–2015.

| Column | Data Type | Description | Units / Values |
|--------|-----------|-------------|-----------------|
| Project | character | Data source | "Red Bluff Diversion Dam" |
| Date | character | Date of observation | YYYY-MM-DD |
| DataType | character | Type of measurement | "RST" or "Manual count" |
| Winter Chinook BY | numeric | Brood year of winter Chinook | Year |
| Winter Chinook Passage Estimate | numeric | Estimated daily passage count | number of fish; NA if not calculated |
| Winter Chinook Length | numeric | Mean fork length of captured winter Chinook | millimeters (mm) |
| Spring Chinook BY | numeric | Brood year of spring Chinook | Year |
| Spring Chinook Passage Estimate | numeric | Estimated daily passage count (spring) | number of fish |
| Spring Chinook Length | numeric | Mean fork length (spring) | millimeters (mm) |
| Fall Chinook BY | numeric | Brood year of fall Chinook | Year |
| Fall Chinook Passage Estimate | numeric | Estimated daily passage count (fall) | number of fish |
| Fall Chinook Length | numeric | Mean fork length (fall) | millimeters (mm) |
| Late-Fall Chinook BY | numeric | Brood year of late-fall Chinook | Year |
| Late-Fall Chinook Passage Estimate | numeric | Estimated daily passage count (late-fall) | number of fish |
| Late-Fall Chinook Length | numeric | Mean fork length (late-fall) | millimeters (mm) |
| Steelhead BY | numeric | Brood year of steelhead | Year |
| Steelhead Passage Estimate | numeric | Estimated daily passage count (steelhead) | number of fish |
| Steelhead Length | numeric | Mean fork length (steelhead) | millimeters (mm) |
| Bend Bridge Peak Flow (CFS) | numeric | Peak flow during day at Bend Bridge location | cubic feet per second (ft³/s) |
| Water Temperature (C) | numeric | Water temperature at dam | degrees Celsius (°C) |
| Turbidity (NTU) | numeric | Water turbidity | Nephelometric Turbidity Units (NTU) |

**`RBDD_RST_Juv_Production.csv`** (237 B, 16 rows, 2 columns)  
Summary of juvenile winter-run Chinook salmon production from Red Bluff RST. One row per brood year (2002–2017).

| Column | Data Type | Description |
|--------|-----------|-------------|
| BY | numeric | Brood year |
| Fry_equiv_JPI | numeric | Estimated number of juvenile fry equivalent from juvenile production index |

---

### 📁 **outputs/** — Processed Data & Summary Tables (14 files)

Output files are generated sequentially by the R scripts. Files prefixed with `oto_sr8786_dat_with_*` represent progressively enriched versions of the raw otolith data, adding new columns at each pipeline step.

#### Enriched Otolith Datasets

**`sr8786_dat_with_fw_exit_dist.csv`** (2.0 MB, 14,865 rows, 17 columns)  
Raw otolith data with estimated freshwater exit distances added.  
*Columns beyond the original 14 (index column dropped):*

| Column | Data Type | Description |
|--------|-----------|-------------|
| SrV2 | numeric | Cleaned SrV values (respots and maternal spots set to NA) |
| SrVpercent | numeric | Normalized SrV as percentage (0–1 range) within each fish |
| FWExit_dist | numeric | Distance from core where fish exited freshwater (in µm). Estimated using SrV method (preferred if available) or Sr⁸⁷/⁸⁶Sr threshold method (0.70785). |

**`oto_sr8786_dat_with_assignments.csv`** (2.1 MB, 14,865 rows, 20 columns)  
Otolith data with habitat assignments for each spot.  
*Additional columns:*

| Column | Data Type | Description | Values |
|--------|-----------|-------------|--------|
| ...1 | numeric | Row index artifact from `write.csv()` (safe to ignore) | — |
| X | numeric | Row index artifact carried from original data file (safe to ignore) | — |
| Habitat | character | Assigned rearing habitat based on Sr⁸⁷/⁸⁶Sr ratio | "LAS" (Lassen/Upper Sacramento, Sr ≤ 0.70467), "SAC" (Sacramento mainstem, 0.70467 < Sr < 0.7061), "DEL" (Delta, 0.7061 < Sr ≤ 0.70785), "AME" (American River, Sr > 0.70785) |

**`oto_sr8786_dat_with_brood_year.csv`** (2.0 MB, 14,865 rows, 20 columns)  
Otolith data with assigned brood year and age.  
*Additional columns:*

| Column | Data Type | Description |
|--------|-----------|-------------|
| final_age | numeric | Estimated age of fish at collection (years) |
| Brood_year | numeric | Year of hatching/rearing (escapement year minus age) |

**`oto_sr8786_dat_with_rearing_types.csv`** (2.1 MB, 14,865 rows, 21 columns)  
Complete enriched otolith dataset with rearing type classification.  
*Additional columns:*

| Column | Data Type | Description | Values |
|--------|-----------|-------------|--------|
| rearing_type | character | Primary habitat used during freshwater residence (assigned to each fish once; all spots for same fish have identical value) | "LAS", "SAC", "DEL", "AME", "X" (unassigned—insufficient clarity in profile) |

#### Fish-Level Summaries

**`rearing_type_by_fish.csv`** (25.5 KB, 705 rows, 8 columns)  
One row per individual fish; summarizes rearing habitat use and assigned type.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Sample_ID | character | Fish ID | — |
| Brood_year | numeric | Year of hatching | Year |
| fork_length | numeric | Fork length at collection (escapement) | millimeters (mm); 0 = missing |
| las_dist | numeric | Total distance (otolith radius) accumulated in Lassen/Upper Sacramento habitat during FW residence | micrometers (µm); 0 = no use |
| ame_dist | numeric | Total distance accumulated in American River habitat during FW residence | micrometers (µm); 0 = no use |
| x_dist | numeric | Total distance from unassigned spots (ambiguous habitat) | micrometers (µm); 0 = none |
| del_dist | numeric | Total distance accumulated in Delta habitat during FW residence | micrometers (µm); 0 = no use |
| rearing_type | character | Primary rearing type assigned to this fish (dominant habitat) | "LAS", "SAC", "DEL", "AME", "X" |

**`rearing_type_by_BY.csv`** (1.2 KB, 33 rows, 5 columns)  
Summary table: rearing type frequencies grouped by brood year.

| Column | Data Type | Description |
|--------|-----------|-------------|
| Brood_year | numeric | Year of hatching |
| rearing_type | character | Assigned rearing type |
| n | numeric | Number of fish in this rearing type for this brood year |
| tot | numeric | Total number of fish in this brood year (all types combined) |
| freq | numeric | Frequency (proportion) of fish in this rearing type for this brood year |

#### Freshwater Exit Data

**`WR_FW_Exit_allyrs.csv`** (43.4 KB, 705 rows, 4 columns)  
Freshwater exit distance summary for all fish and years; three different estimates provided (SrV method and Sr⁸⁷/⁸⁶Sr method, plus consensus estimate).

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Sample_ID | character | Fish ID | — |
| FWDist_8786 | numeric | FW exit distance estimated using Sr⁸⁷/⁸⁶Sr threshold method | micrometers (µm) |
| FWDist_SrV | numeric | FW exit distance estimated using SrV (Sr concentration) method | micrometers (µm) |
| FWExit_dist | numeric | Final consensus FW exit distance (SrV preferred when available; Sr⁸⁷/⁸⁶Sr otherwise) | micrometers (µm) |

#### Environmental Data

**`daily_flow_temp.csv`** (427.6 KB, 7,879 rows, 10 columns)  
Daily hydrological data from USGS gauge with brood year alignment and emigration period flags.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| datetime | character | Date of measurement | YYYY-MM-DD |
| julian_day | numeric | Day of year (1–365 or 366) | integer |
| broodyr_day | numeric | Days since start of water year (assigned brood year) | integer |
| month | numeric | Month of year | 1–12 |
| year | numeric | Calendar year | Year |
| BY | numeric | Associated brood year (for analysis alignment) | Year |
| flow_cms | numeric | Mean daily stream flow | cubic meters per second (m³/s) |
| max_daily_temp_C | numeric | Maximum daily water temperature | degrees Celsius (°C) |
| median_daily_temp_C | numeric | Median daily water temperature | degrees Celsius (°C) |
| emigration_period | logical | TRUE if date falls within Aug–Jan (typical juvenile emigration window) | — |

**`mean_aug_jan_flows.csv`** (195 B, 6 rows, 3 columns)  
Summary of mean discharge and first freshet timing during August–January emigration period, for brood years used in regression analyses.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| BY | numeric | Brood year | Year |
| av_flow8.1 | numeric | Mean daily flow during Aug–Jan of that brood year | cubic meters per second (m³/s) |
| broodyr_day | numeric | Day of brood year when flow first exceeded 400 m³/s (first freshet cue) | integer |

**`flow_temp_stats_AugtoJan_only.csv`** (964 B, 8 rows, 7 columns)  
Statistical summary of flow and temperature during the Aug–Jan emigration period, by brood year.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| BY | numeric | Brood year | Year |
| mean_max_daily_temp_AugtoJan | numeric | Mean of daily maximum temperatures during Aug–Jan | degrees Celsius (°C) |
| median_max_daily_temp_AugtoJan | numeric | Median of daily maximum temperatures during Aug–Jan | degrees Celsius (°C) |
| sd_max_daily_temp_AugtoJan | numeric | Standard deviation of daily maximum temperatures during Aug–Jan | degrees Celsius (°C) |
| mean_mean_daily_flow_AugtoJan | numeric | Mean of mean daily flows during Aug–Jan | cubic meters per second (m³/s) |
| median_mean_daily_flow_AugtoJan | numeric | Median of mean daily flows during Aug–Jan | cubic meters per second (m³/s) |
| sd_mean_daily_flow_AugtoJan | numeric | Standard deviation of mean daily flows during Aug–Jan | cubic meters per second (m³/s) |

#### Growth & Proportion Analysis

**`summary_stats_fw_growth.csv`** (2.8 KB, 40 rows, 7 columns)  
Summary statistics: mean, SE, min, max of proportion freshwater growth by habitat and brood year.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Brood_year | numeric | Year of hatching | Year |
| Habitat | character | Rearing habitat | "LAS", "SAC", "DEL", "AME" |
| mean_prop_fw_growth | numeric | Mean proportion of growth in FW vs. ocean | 0–1 |
| min_prop_fw_growth | numeric | Minimum | 0–1 |
| max_prop_fw_growth | numeric | Maximum | 0–1 |
| se_prop_fw_growth | numeric | Standard error | 0–1 |
| n_fish | numeric | Number of fish in group | count |

**`prop_fw_growth_by_fish.csv`** (135.8 KB, 1,391 rows, 9 columns)  
Per-fish estimates of proportion freshwater growth for each habitat use period.

| Column | Data Type | Description | Units |
|--------|-----------|-------------|-------|
| Sample_ID | character | Fish ID | — |
| Habitat | character | Assigned habitat for this growth interval | "LAS", "SAC", "DEL", "AME" |
| Escap_yr | numeric | Escapement year | Year |
| Brood_year | numeric | Brood year | Year |
| fork_length | numeric | Fork length at collection | millimeters (mm) |
| mass_change_sum | numeric | Cumulative mass gain during this habitat period | grams (g) |
| tot_mass_change | numeric | Total mass gain from FW exit to collection | grams (g) |
| prop_fw_growth | numeric | Proportion of total growth (FW + ocean) that occurred in freshwater during this habitat use period | 0–1 |
| prop_sac | numeric | Proportion of Sacramento River-specific growth (if applicable) | 0–1 |

**`by_fish_prop_growth_wide_format.csv`** (39.2 KB, 705 rows, 7 columns)  
Wide-format version of proportion freshwater growth: one row per fish, columns for each habitat.

| Column | Data Type | Description |
|--------|-----------|-------------|
| Sample_ID | character | Fish ID |
| Brood_year | numeric | Year of hatching |
| DEL | numeric | Proportion of FW growth in Delta habitat |
| LAS | numeric | Proportion of FW growth in Lassen/Upper Sacramento habitat |
| SAC | numeric | Proportion of FW growth in Sacramento mainstem habitat |
| AME | numeric | Proportion of FW growth in American River habitat |
| Unassigned | numeric | Proportion of FW growth in unassigned habitat |

#### Quality Control & Classification Documentation

**`SAC_classification_criteria.txt`** (12.6 KB)  
Generated by script `08_upper_lower_sac_profiles.R`.  
Detailed reference document describing the three-level classification scheme applied to Sacramento River (SAC) rearers to distinguish between Exclusively Upper Sacramento vs. Upper & Lower Sacramento habitat use. Includes:
- Definition of classification levels (isotopic threshold, visual review criteria)
- Rationale for chosen thresholds (reference fish, maximum range values)
- Complete sample ID lists for:
  - Exclusively Upper SAC (n=63)
  - Borderline Exclusively Upper SAC (n=21)
  - Upper and Lower SAC (n=397)
  - All SAC rearers (n=481)
- Summary statistics table

---

### 📁 **figures/** — Publication Figures & Visualizations (15 files)

This directory contains publication-ready figures and supplementary visualizations generated during analysis. All files are organized below with file names, sizes, formats, and descriptions.

#### Main Publication Figures & Supplementary Figures

| File Name | Size | Format | Script | Description |
|-----------|------|--------|--------|-------------|
| `Fig1_rearing_type_props.jpg` | 660.5 KB | JPG | **07** (`07_rearing_types.R`) | Bar/stacked bar plot showing proportions of fish assigned to each rearing type (LAS, SAC, DEL, AME) by brood year. Intended for main manuscript. |
| `Fig2_example_profiles.tiff` | 269.1 KB | TIFF | **06** (`06_profile_plots.R`) | Example otolith Sr⁸⁷/⁸⁶Sr profiles for reference fish representing each rearing type. High-resolution figure for publication. |
| `Fig3_prop_fw_growth_by_fish.jpg` | 2.6 MB | JPG | **09** (`09_mass_assimilated.R`) | Bar plots showing proportion of freshwater vs. ocean growth for each rearing habitat and cohort. Main manuscript figure. |
| `Fig4_rest_stop.jpg` | 321.3 KB | JPG | **12** (`12_regression_plots.R`) | Visualization of "rest stop hypothesis" showing non-natal habitat use patterns over time. Main manuscript figure. |
| `Fig5_fw_growth_passage_flow.jpg` | 1.1 MB | JPG | **11** (`11_hydrographs_vs_mean_fw_growth_by_yr.R`) | Hydrographs vs. mean freshwater growth by brood year with juvenile passage data overlay. Main manuscript figure. |
| `Fig6_downstream_occupancy.jpg` | 297.3 KB | JPG | **12** (`12_regression_plots.R`) | Downstream habitat occupancy patterns across brood years. Main manuscript figure. |
| `FigS1_FL_distributions.jpg` | 595.1 KB | JPG | **04** (`04_cohort_reconstruction.R`) | Fork length distributions by brood year. Supplementary figure for appendix. |
| `FigS2_Individual_otolith_profiles.pdf` | 2.4 MB | PDF | **05** (`05_individual_profile_plots.R`) | All 705 individual otolith Sr⁸⁷/⁸⁶Sr profiles displayed on 4×3 grids (4×3 plots per page, multiple pages). Reference appendix for data validation. |
| `FigS3_flows_per_year.jpg` | 4.4 MB | JPG | **10** (`10_flow_temp_summaries_plots.R`) | Annual Sacramento River hydrographs (2000–2021) with brood year alignment. Supplementary environmental data figure. |
| `FigS4_fw_exit_size.jpg` | 311.8 KB | JPG | **13** (`13_fw_exit_size_plot.R`) | Fork length (otolith radius) distributions at freshwater exit by brood year. Supplementary figure. |

#### Classification Reference PDFs (Generated by Script 08 — SAC Reclassification)

| File Name | Size | Format | Script | Description |
|-----------|------|--------|--------|-------------|
| `SAC_all_rearers.pdf` | 1.7 MB | PDF | **08** (`08_upper_lower_sac_profiles.R`) | All fish classified as Sacramento River (SAC) rearers (n=481). Level 1 classification: baseline group before SAC sub-classification. Reference document. |
| `Exclusively_Upper_Sac_Sr_threshold_only.pdf` | 321.5 KB | PDF | **08** (`08_upper_lower_sac_profiles.R`) | Level 2 classification: SAC rearers passing Sr⁸⁷/⁸⁶Sr threshold test (≤ 0.7053813), indicating exclusive upper Sacramento habitat use (n=87). Reference document. |
| `Exclusively_Upper_Sac_final.pdf` | 231.5 KB | PDF | **08** (`08_upper_lower_sac_profiles.R`) | Level 3 classification: final conservative group of exclusively upper Sacramento rearers after Sr⁸⁷/⁸⁶Sr threshold + isotopic stability criterion + visual review (n=63). Reference document. |
| `Borderline_Exclusively_Upper_Sac.pdf` | 84.1 KB | PDF | **08** (`08_upper_lower_sac_profiles.R`) | Ambiguous otolith profiles (n=21) that passed Sr⁸⁷/⁸⁶Sr threshold but showed marginal isotopic stability. Could represent either exclusive or mixed habitat use. |
| `Lower_Sacramento_rearers.pdf` | 1.4 MB | PDF | **08** (`08_upper_lower_sac_profiles.R`) | Otolith profiles for fish classified as using lower Sacramento River habitat (upper and lower Sacramento rearers). Reference document. |

*Note:* Classification reference PDFs are visual quality-control documents showing individual otolith profiles for each classification group. These are intended for validation and transparency; detailed criteria are documented in `outputs/SAC_classification_criteria.txt`.

---

## Data Provenance & Citations

### Original Data Sources

**Otolith Microchemistry Data:**  
Primary data collection: Morais et al. (2026), Ecosphere.  
Methodological foundation: Phillis et al. (2018). "A baseline characterization of strontium isotope ratios (⁸⁷Sr/⁸⁶Sr) in the Sacramento River system for tracking salmon origin and habitat use." *Journal of Fish Biology*, 93(4), 655–664. [https://doi.org/10.1111/jfb.13804](https://doi.org/10.1111/jfb.13804)

**Scale Age & CWT Data:**  
California Department of Fish and Wildlife (CDFW), Comprehensive Assessment and Review for Exotic Enhancement program.

**USGS Hydrological Data:**  
U.S. Geological Survey National Water Information System (NWIS). Station 11390500 (Sacramento River below Wilkins Slough).  
Data retrieved from: [https://waterdata.usgs.gov/ca/nwis/qw](https://waterdata.usgs.gov/ca/nwis/qw)

**Red Bluff Diversion Dam Juvenile Passage Data:**  
California Department of Fish and Wildlife, Red Bluff Diversion Dam Rotary Screw Trap program.  
Available: [https://www.cdfw.ca.gov/](https://www.cdfw.ca.gov/)

---

## Analysis Notes

### Habitat Isotopic Thresholds

Fish were classified into four natal/non-natal freshwater habitats based on Sr⁸⁷/⁸⁶Sr reference values from water samples:

| Habitat | Sr⁸⁷/⁸⁶Sr Range | Description |
|---------|----------------|-------------|
| LAS (Lassen) | ≤ 0.70467 | Upper Sacramento River above Keswick Dam (maternal natal habitat) |
| SAC (Sacramento) | 0.70467–0.7061 | Sacramento River mainstem (non-natal) |
| DEL (Delta) | 0.7061–0.70785 | Sacramento–San Joaquin Delta (non-natal) |
| AME (American) | > 0.70785 | American River tributary (non-natal) |

Thresholds derived from water samples analyzed by Phillis et al. (2018) and extended with new data from this study.

### Missing Data & Special Codes

**In otolith files:**
- `NA` values in Sr8786_norm, SrV, or other spot measurements indicate missing or unreliable spot data (e.g., instrumental artifact, vaterite, respot exclusions).
- **Unassigned habitat spots (code "X"):** Rare measurement spots that could not be confidently assigned to any habitat due to ambiguous isotopic values.

**In scale/cohort data:**
- Missing fork length values (blank cells) indicate fish not measured at capture.
- Missing CWT values indicate fish without coded wire tags.

**In environmental files:**
- `NA` for Red Bluff passage estimates indicates days when trap was not operative or data were not recorded.

---

## Project Structure & Reproducibility

This analysis was conducted in **R** (version 4.5.1 or later). All code is contained in the `scripts/` directory, numbered in the order of execution.

**To reproduce analyses:**
1. Start with `01_fw_exit_dist.R` and proceed sequentially through `13_fw_exit_size_plot.R`.
2. All relative file paths assume working directory is the project root.
3. Required R packages: `tidyverse` (includes `dplyr`, `ggplot2`, `readr`), `readxl`, and others as noted in individual script headers.
4. Output tables are generated to `outputs/` directory; figures to `figures/` directory.

**RStudio Project Files:**
- `Morais_et_al_winter_run_Chinook_salmon.Rproj` — Main project configuration.

---

## Contact & Questions

For questions about this dataset or analysis, contact the corresponding author of the Morais et al. (2026) Ecosphere publication.

**Repository Generated:** June 2026

---

*Last updated: June 20, 2026*
