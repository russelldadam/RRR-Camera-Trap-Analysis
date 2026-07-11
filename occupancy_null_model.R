```r
# ============================================================
# OCCUPANCY NULL MODEL
# ============================================================
# Purpose:
# Fit a single-season occupancy model without covariates.

# This model estimates:
# 1. Occupancy probability (psi)
# 2. Detection probability (p)

# Model structure:
# psi ~ 1
# p   ~ 1
# ============================================================

# ============================================================
# ANALYSIS CONCEPTS
# ============================================================

# Occupancy probability (psi):
# The probability that a species used a camera site during the study period.

# Detection probability (p):
# The probability of detecting a species during a survey interval,
# given that the species used the site.

# Null model:
# A model with no covariates. This is useful as a baseline model.

# Detection history:
# A matrix of 0s and 1s showing whether a species was detected
# or not detected at each camera during each survey interval.

# 1 = species detected
# 0 = species not detected

# ============================================================
# BEFORE YOU START
# ============================================================
# Required packages:
# - readxl
# - unmarked

# Required input:
# - Excel workbook containing detection histories

# Required columns:
# - One camera/site ID column
# - Multiple detection interval columns coded as 0 or 1

# ============================================================
# STEP 1: LOAD PACKAGES
# ============================================================

library(readxl)    # Reads Excel files
library(unmarked)  # Fits occupancy models

# ============================================================
# STEP 2: USER INPUTS - CHANGE THIS SECTION ONLY
# ============================================================

# Excel file containing detection histories.
# Replace this with your own file name if needed.

detection_matrix_file <- "DetectionMatrix.xlsx"

# Species name used in the Excel sheet name.
# Example sheet names: "7-day_deer", "7-day_coyote", "7-day_bobcat"

species_name <- "deer"

# Survey interval used in the detection matrix.
# Change if using a different interval.

survey_interval <- "7-day"

# Name of the camera/site ID column.

camera_id_column <- "Camera_ID"

# ============================================================
# DO NOT EDIT BELOW UNLESS MODIFYING THE ANALYSIS METHOD
# ============================================================

# ============================================================
# STEP 3: LOAD DETECTION MATRIX
# ============================================================

# Create the sheet name from the survey interval and species name.

sheet_name <- paste0(survey_interval, "_", species_name)

# Read the selected sheet from the Excel workbook.

detection_data <- read_excel(detection_matrix_file,
  sheet = sheet_name)

# ============================================================
# STEP 4: PREPARE DETECTION HISTORY
# ============================================================

# Remove the camera ID column so only detection/non-detection data remain.

detection_history <- detection_data[, names(detection_data) != camera_id_column]

# Convert the detection history into a matrix for unmarked.

detection_matrix <- as.matrix(detection_history)

# ============================================================
# STEP 5: CREATE UNMARKED DATA OBJECT
# ============================================================

# unmarkedFrameOccu stores the detection matrix in the format required
# by the unmarked package.

occupancy_data <- unmarkedFrameOccu(
  y = detection_matrix)

# ============================================================
# STEP 6: FIT NULL OCCUPANCY MODEL
# ============================================================

# Null occupancy model:
# Detection probability is constant.
# Occupancy probability is constant.

occupancy_null_model <- occu(~ 1 ~ 1,data = occupancy_data)

# ============================================================
# STEP 7: VIEW RESULTS
# ============================================================

summary(occupancy_null_model)

# ============================================================
# STEP 8: OPTIONAL - SAVE MODEL OUTPUT
# ============================================================
# The model summary prints to the console by default.
# If you want to save model results, you can add export code here later.
```
