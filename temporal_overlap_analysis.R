# ============================================================
# TEMPORAL OVERLAP ANALYSIS
# ============================================================
# Purpose:
# Estimate diel activity overlap from camera trap detection times.
#
# This script calculates:
# 1. 24-hour temporal overlap among wildlife species
# 2. Daytime temporal overlap between wildlife species and humans
# ============================================================

# ============================================================
# ANALYSIS CONCEPTS
# ============================================================

# Temporal Overlap (D-hat)

# D-hat measures how similar two species are in their daily activity patterns.

# Values range from:
# D-hat = 0.00 No overlap in activity.
# D-hat = 1.00 Complete overlap in activity.

# Larger D-hat values indicate greater similarity in activity times.

# Dhat1 vs Dhat4:
# The overlap package recommends different estimators depending on sample size.

# Dhat1 Used when one or both species have fewer than 50 detections.
#
# Dhat4 Used when both species have at least 50 detections.

# Kernel Density Estimation:
# A statistical method that creates a smooth activity curve from detection times.

# Radians:
# Detection times are converted from hours into radians because
# the overlap package treats time as a circular variable.

# Midnight and midnight represent the same point on a circle.

# ============================================================
# BEFORE YOU START
# ============================================================
# Required packages:
# - overlap
# - dplyr
# - lubridate

# Required columns in your dataset:
# - species name column
# - date column
# - time column
# ============================================================

# ============================================================
# STEP 1: LOAD PACKAGES
# ============================================================

library(overlap)    # Calculates temporal overlap
library(dplyr)      # Filters and organizes data
library(lubridate)  # Works with dates and times

# ============================================================
# STEP 2: USER INPUTS - CHANGE THIS SECTION ONLY
# ============================================================

camera_data <- your_file_name 

# These must match the column names in your dataset.
species_column <- "species_common_name"
date_column    <- "date"
time_column    <- "time"

# These must match the species names exactly as they appear in your dataset.
wildlife_species <- c( "Bobcat","Coyote","MuleDeer")

# Human label in your dataset.
human_label <- "Human"

# Daytime window for human-wildlife comparisons.
daytime_start_hour <- 6
daytime_end_hour   <- 18

# Sample size cutoff for choosing the overlap estimator.
minimum_sample_size_for_Dhat4 <- 50

# ============================================================
# DO NOT EDIT BELOW UNLESS MODIFYING THE ANALYSIS METHOD
# ============================================================

# ============================================================
# STEP 3: PREPARE DETECTION TIMES
# ============================================================

prepare_detection_times <- function(input_data) {
  input_data %>%
    mutate(datetime = as.POSIXct(paste(.data[[date_column]], .data[[time_column]]),
        format = "%Y-%m-%d %H:%M:%S"),
      decimal_hour = hour(datetime) + minute(datetime) / 60,
      radians = (decimal_hour / 24) * 2 * pi)}

camera_data_24hr <- prepare_detection_times(camera_data)

# ============================================================
# STEP 4: CHOOSE OVERLAP ESTIMATOR
# ============================================================

choose_overlap_method <- function(species_1_sample_size, species_2_sample_size) {
  if (species_1_sample_size >= minimum_sample_size_for_Dhat4 &&
    species_2_sample_size >= minimum_sample_size_for_Dhat4) {
    return("Dhat4")} else {return("Dhat1")}}

# ============================================================
# STEP 5: CALCULATE 24-HOUR WILDLIFE-WILDLIFE OVERLAP
# ============================================================

wildlife_pairs <- combn(wildlife_species, 2, simplify = FALSE)

wildlife_overlap_results <- lapply(wildlife_pairs, function(species_pair) {
  
  species_1 <- species_pair[1]
  species_2 <- species_pair[2]
  
  species_1_times <- camera_data_24hr %>%
    filter(.data[[species_column]] == species_1) %>%
    pull(radians)
  
  species_2_times <- camera_data_24hr %>%
    filter(.data[[species_column]] == species_2) %>%
    pull(radians)
  
  overlap_method <- choose_overlap_method(
    length(species_1_times),
    length(species_2_times))
  
  data.frame(Species1 = species_1,Species2 = species_2,
    TimeWindow = "24-hour",
    Method = overlap_method,
    Overlap = overlapEst(species_1_times, species_2_times, type = overlap_method),
    N1 = length(species_1_times),
    N2 = length(species_2_times))})

wildlife_overlap_results <- bind_rows(wildlife_overlap_results)


# ============================================================
# STEP 6: FILTER DATA TO DAYTIME HOURS
# ============================================================

camera_data_daytime <- camera_data_24hr %>%
  filter(decimal_hour >= daytime_start_hour & decimal_hour < daytime_end_hour)

# ============================================================
# STEP 7: CALCULATE DAYTIME WILDLIFE-HUMAN OVERLAP
# ============================================================

wildlife_human_pairs <- lapply(wildlife_species, function(wildlife_name) {
  c(wildlife_name, human_label)})

human_overlap_results <- lapply(wildlife_human_pairs, function(species_pair) {
  
  species_1 <- species_pair[1]
  species_2 <- species_pair[2]
  
  species_1_times <- camera_data_daytime %>%
    filter(.data[[species_column]] == species_1) %>%
    pull(radians)
  
  species_2_times <- camera_data_daytime %>%
    filter(.data[[species_column]] == species_2) %>%
    pull(radians)
  
  overlap_method <- choose_overlap_method(
    length(species_1_times),
    length(species_2_times))
  
  data.frame(Species1 = species_1,Species2 = species_2,
    TimeWindow = paste0("Daytime ", daytime_start_hour, ":00-", daytime_end_hour, ":00"),
    Method = overlap_method,
    Overlap = overlapEst(species_1_times, species_2_times, type = overlap_method),
    N1 = length(species_1_times),
    N2 = length(species_2_times))})

human_overlap_results <- bind_rows(human_overlap_results)

# ============================================================
# STEP 8: COMBINE AND VIEW RESULTS
# ============================================================

temporal_overlap_results <- bind_rows(
  wildlife_overlap_results,
  human_overlap_results)

print(temporal_overlap_results)

# ============================================================
# STEP 9: OPTIONAL - SAVE RESULTS
# ============================================================
# Remove the # below if you want to save the results as a CSV file.

# write.csv(temporal_overlap_results, "temporal_overlap_results.csv", row.names = FALSE)
