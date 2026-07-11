# ============================================================
# 02 OCCUPANCY COVARIATE MODELS
# ============================================================
# Purpose:
# Fit single-season occupancy models with site covariates.

# This script compares:
# 1. Null model
# 2. Elevation model
# 3. Human activity model
# 4. Elevation + human activity model
# ============================================================


# ============================================================
# ANALYSIS CONCEPTS
# ============================================================

# Occupancy probability (psi):
# The probability that a species used a camera site during the study period.

# Detection probability (p):
# The probability of detecting a species during a survey interval,
# given that the species used the site.

# Site covariate:
# A site-level variable that may explain differences in occupancy.
# In this script, elevation and human activity are used as covariates.

# AIC:
# Akaike's Information Criterion. A lower AIC value indicates a model
# that is better supported by the data, while accounting for complexity.

# Scaling:
# Covariates are scaled so they have a mean of 0 and standard deviation of 1.
# This helps model performance and makes covariates easier to compare.

# ============================================================
# BEFORE YOU START
# ============================================================
# Required packages:
# - readxl
# - unmarked
# - dplyr

# Required input:
# - Excel workbook containing detection histories

# Required columns:
# - Camera/site ID column
# - Elevation column
# - Detection interval columns coded as 0 or 1

# Required sheets:
# - One wildlife detection sheet
# - One human detection sheet

# ============================================================
# STEP 1: LOAD PACKAGES
# ============================================================

library(readxl)    # Reads Excel files
library(unmarked)  # Fits occupancy models
library(dplyr)     # Helps organize data

# ============================================================
# STEP 2: USER INPUTS - CHANGE THIS SECTION ONLY
# ============================================================

# Excel file containing detection histories.

detection_matrix_file <- "DetectionMatrix.xlsx"

# Sheet for the wildlife species being modeled.
# Examples: "7-day_deer", "7-day_coyote", "7-day_bobcat"

wildlife_sheet <- "7-day_bobcat"

# Sheet containing human detection history.

human_sheet <- "7-day_human"

# Name of the camera/site ID column.

camera_id_column <- "Camera_ID"

# Name of the elevation column.

elevation_column <- "elevation"

# Name used for the human activity covariate.

human_activity_column <- "human_activity"

# ============================================================
# DO NOT EDIT BELOW UNLESS MODIFYING THE ANALYSIS METHOD
# ============================================================

# ============================================================
# STEP 3: LOAD WILDLIFE AND HUMAN DETECTION MATRICES
# ============================================================

wildlife_data <- read_excel(detection_matrix_file,
  sheet = wildlife_sheet)

human_data <- read_excel(detection_matrix_file,
  sheet = human_sheet)

# ============================================================
# STEP 4: SUMMARIZE HUMAN ACTIVITY BY CAMERA
# ============================================================
# Human activity is summarized as the total number of human detections
# across all survey intervals for each camera.

human_detection_columns <- setdiff(names(human_data),
  c(camera_id_column, elevation_column))

human_activity <- rowSums(human_data[, human_detection_columns],
  na.rm = TRUE)

human_activity_data <- data.frame(Camera_ID = human_data[[camera_id_column]],
  human_activity = human_activity)

# ============================================================
# STEP 5: MERGE HUMAN ACTIVITY WITH WILDLIFE DATA
# ============================================================

merged_data <- merge(wildlife_data,
  human_activity_data,
  by.x = camera_id_column,
  by.y = "Camera_ID")

# ============================================================
# STEP 6: PREPARE WILDLIFE DETECTION HISTORY
# ============================================================
# Remove site-level columns so only wildlife detection intervals remain.

wildlife_detection_columns <- setdiff(names(merged_data),
  c(camera_id_column, elevation_column, human_activity_column))

wildlife_detection_history <- merged_data[, wildlife_detection_columns]

wildlife_detection_matrix <- as.matrix(wildlife_detection_history)

# ============================================================
# STEP 7: PREPARE SITE COVARIATES
# ============================================================
# Elevation and human activity are scaled before modeling.

site_covariates <- data.frame(elevation_scaled = as.numeric(scale(merged_data[[elevation_column]])),
  human_activity_scaled = as.numeric(scale(merged_data[[human_activity_column]])))

# ============================================================
# STEP 8: CREATE UNMARKED DATA OBJECT
# ============================================================

occupancy_data <- unmarkedFrameOccu(y = wildlife_detection_matrix,
  siteCovs = site_covariates)

# ============================================================
# STEP 9: FIT OCCUPANCY MODELS
# ============================================================
# Model format in unmarked:
# occu(~ detection_formula ~ occupancy_formula)

null_model <- occu(~ 1 ~ 1,
  data = occupancy_data)

elevation_model <- occu(~ 1 ~ elevation_scaled,
  data = occupancy_data)

human_activity_model <- occu(~ 1 ~ human_activity_scaled,
  data = occupancy_data)

elevation_human_model <- occu(~ 1 ~ elevation_scaled + human_activity_scaled,
  data = occupancy_data)

# ============================================================
# STEP 10: COMPARE MODELS USING AIC
# ============================================================

model_list <- fitList(Null = null_model,
  Elevation = elevation_model,
  Human_Activity = human_activity_model,
  Elevation_Human = elevation_human_model)

model_comparison <- modSel(model_list)

print(model_comparison)

# ============================================================
# STEP 11: VIEW MODEL SUMMARIES
# ============================================================

summary(null_model)
summary(elevation_model)
summary(human_activity_model)
summary(elevation_human_model)

# ============================================================
# STEP 12: OPTIONAL - SAVE MODEL COMPARISON
# ============================================================
# Remove the # below if you want to save the AIC comparison.

# write.csv(as.data.frame(model_comparison), "occupancy_model_comparison.csv", row.names = TRUE)
