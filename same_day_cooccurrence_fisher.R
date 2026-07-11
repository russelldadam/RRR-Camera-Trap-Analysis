# ============================================================
# 05 SAME-DAY CO-OCCURRENCE ANALYSIS
# ============================================================
# Purpose:
# Test whether species are more or less likely to be detected on the
# same calendar day as humans at the same camera location using Fisher's Exact Test.

# This script calculates:
# 1. Camera-day presence/absence for each species
# 2. Pairwise same-day co-occurrence
# 3. Fisher's Exact Test
# 4. Odds ratios and p-values
# ============================================================

# ============================================================
# ANALYSIS CONCEPTS
# ============================================================

# Co-occurrence:
# Two species are considered to co-occur when they are detected at
# the same camera on the same calendar day.

# Fisher's Exact Test:
# A statistical test used to evaluate whether two species occur
# together more or less often than expected by chance.

# Odds ratio:
# A value describing the strength and direction of association.

# OR < 1 indicates lower-than-expected co-occurrence.
# OR = 1 indicates no association.
# OR > 1 indicates higher-than-expected co-occurrence.

# Camera-day:
# One camera location on one calendar date.

# ============================================================
# BEFORE YOU START
# ============================================================

# Required packages:
# - dplyr
# - tidyr
# - lubridate

# Required columns in your dataset:
# - species name column
# - date column
# - camera/location column

# ============================================================
# STEP 1: LOAD PACKAGES
# ============================================================

library(dplyr)      # Filters and organizes data
library(tidyr)      # Reshapes data to wide format
library(lubridate)  # Works with dates and time

# ============================================================
# STEP 2: USER INPUTS - CHANGE THIS SECTION ONLY
# ============================================================

camera_data <- your_dataset_name

# These must match the column names in your dataset.

species_column <- "species_common_name"
date_column    <- "date"
camera_column  <- "location"

# Species included in pairwise co-occurrence analysis.
species_to_compare <- c("Human","Bobcat","Coyote","MuleDeer")

# ============================================================
# DO NOT EDIT BELOW UNLESS MODIFYING THE ANALYSIS METHOD
# ============================================================

# ============================================================
# STEP 3: PREPARE CAMERA-DAY DATA
# ============================================================

# Each row represents one species detected at one camera on one day.

camera_day_detections <- camera_data %>%
  mutate(calendar_date = as.Date(.data[[date_column]]),
    camera_id = .data[[camera_column]]) %>%
  filter(.data[[species_column]] %in% species_to_compare) %>%
  distinct(camera_id,calendar_date,.data[[species_column]]) %>%
  mutate(presence = 1)

# ============================================================
# STEP 4: CONVERT TO PRESENCE/ABSENCE MATRIX
# ============================================================

# Each row is one camera-day.
# Each species column is coded as:
# 1 = detected
# 0 = not detected

camera_day_presence_matrix <- camera_day_detections %>%
  pivot_wider(names_from = .data[[species_column]],
    values_from = presence,
    values_fill = 0)

# ============================================================
# STEP 5: CREATE FUNCTION FOR FISHER'S EXACT TEST
# ============================================================

run_fisher_cooccurrence <- function(presence_matrix, species_1, species_2) {
  
  # Add missing species columns if a species was never detected.
  
  if (!species_1 %in% names(presence_matrix)) {presence_matrix[[species_1]] <- 0}
  
  if (!species_2 %in% names(presence_matrix)) {presence_matrix[[species_2]] <- 0}
  
  # Count camera-days where both species were present.
  both_present <- sum(presence_matrix[[species_1]] == 1 &
      presence_matrix[[species_2]] == 1)
  
  # Count camera-days where species 1 was present and species 2 was absent.
  
  species_1_only <- sum(presence_matrix[[species_1]] == 1 &
      presence_matrix[[species_2]] == 0)
  
  # Count camera-days where species 1 was absent and species 2 was present.
  
  species_2_only <- sum(presence_matrix[[species_1]] == 0 &
      presence_matrix[[species_2]] == 1)
  
  # Count camera-days where neither species was present.
  neither_present <- sum(presence_matrix[[species_1]] == 0 &
      presence_matrix[[species_2]] == 0)
  
  # Build 2 x 2 contingency table.
  
  cooccurrence_table <- matrix(
    c(both_present,species_1_only,species_2_only,neither_present),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(Species_1 = c("Present", "Absent"),
      Species_2 = c("Present", "Absent")))
  
  # Run Fisher's Exact Test.
  
  fisher_result <- fisher.test(cooccurrence_table)
  
  # Return one row of results.
  
  data.frame(Species1 = species_1,Species2 = species_2,
    OddsRatio = unname(fisher_result$estimate),
    PValue = fisher_result$p.value,
    BothPresent = both_present,
    Species1Only = species_1_only,
    Species2Only = species_2_only,
    NeitherPresent = neither_present)}

# ============================================================
# STEP 6: RUN ALL PAIRWISE CO-OCCURRENCE TESTS
# ============================================================

species_pairs <- combn(species_to_compare, 2, simplify = FALSE)

cooccurrence_results <- bind_rows(lapply(species_pairs, function(species_pair) {
    run_fisher_cooccurrence(presence_matrix = camera_day_presence_matrix,
      species_1 = species_pair[1],
      species_2 = species_pair[2])}))

# ============================================================
# STEP 7: VIEW RESULTS
# ============================================================

print(cooccurrence_results)

# ============================================================
# STEP 8: OPTIONAL - SAVE RESULTS
# ============================================================
# Remove the # below if you want to save the results as a CSV file.

# write.csv(cooccurrence_results, "same_day_cooccurrence_results.csv", row.names = FALSE)
