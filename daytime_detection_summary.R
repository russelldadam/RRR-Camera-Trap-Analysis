# ============================================================
# DAYTIME DETECTION SUMMARY
# ============================================================
# Purpose:
# Summarize daytime wildlife detections on days with and without
# human detections.

# This script calculates:
# 1. Days when humans were detected
# 2. Wildlife detections on human-present days
# 3. Wildlife detections on human-absent days
# 4. Percent of daytime wildlife detections occurring on human-present days

# ============================================================
# ANALYSIS CONCEPTS
# ============================================================

# Human-present day:
# A calendar day when at least one human detection occurred anywhere
# in the camera trap dataset.

# Human-absent day:
# A calendar day when no human detections were recorded.

# Daytime detection:
# A detection occurring within the user-defined daytime window.
# In this thesis, daytime was defined as 06:00-18:00.

# Detection summary:
# A descriptive table showing how many wildlife detections occurred
# on human-present versus human-absent days.

# ============================================================
# BEFORE YOU START
# ============================================================

# Required packages:
# - dplyr
# - lubridate
# - tidyr

# Required columns in your dataset:
# - species name column
# - date column
# - time column

# ============================================================
# STEP 1: LOAD PACKAGES
# ============================================================

library(dplyr)      # Filters and summarizes data
library(lubridate)  # Works with dates and times
library(tidyr)      # Reshapes summary tables

# ============================================================
# STEP 2: USER INPUTS - CHANGE THIS SECTION ONLY
# ============================================================

camera_data <- your_dataset

# These must match the column names in your dataset.

species_column <- "species_common_name"
date_column    <- "date"
time_column    <- "time"

# Wildlife species to summarize.

wildlife_species <- c("Bobcat","Coyote","MuleDeer")

# Human label in your dataset.

human_label <- "Human"

# Daytime window.

daytime_start_hour <- 6
daytime_end_hour   <- 18

# ============================================================
# DO NOT EDIT BELOW UNLESS MODIFYING THE ANALYSIS METHOD
# ============================================================

# ============================================================
# STEP 3: PREPARE DATE AND TIME COLUMNS
# ============================================================

camera_data_daytime <- camera_data %>%
  mutate(datetime = ymd_hms(paste(.data[[date_column]],.data[[time_column]])),
    calendar_date = as.Date(datetime),
    detection_hour = hour(datetime)) %>%
  filter(detection_hour >= daytime_start_hour,
    detection_hour < daytime_end_hour)

# ============================================================
# STEP 4: IDENTIFY HUMAN-PRESENT DAYS
# ============================================================

human_present_days <- camera_data_daytime %>%
  filter(.data[[species_column]] == human_label) %>%
  distinct(calendar_date) %>%
  mutate(human_present_day = TRUE)

# ============================================================
# STEP 5: CLASSIFY WILDLIFE DETECTIONS
# ============================================================

wildlife_daytime_detections <- camera_data_daytime %>%
  filter(.data[[species_column]] %in% wildlife_species) %>%
  left_join(human_present_days, by = "calendar_date") %>%
  mutate(human_present_day = ifelse(is.na(human_present_day),
      FALSE,
      human_present_day))

# ============================================================
# STEP 6: CREATE SUMMARY TABLE
# ============================================================

daytime_detection_summary <- wildlife_daytime_detections %>%
  count(.data[[species_column]], human_present_day) %>%
  pivot_wider(names_from = human_present_day,
    values_from = n,
    values_fill = 0) %>%
  rename(HumanAbsent = `FALSE`,
    HumanPresent = `TRUE`) %>%
  mutate(Total = HumanAbsent + HumanPresent,
    Percent_on_HumanPresent_Days = round(HumanPresent / Total * 100, 1))

# ============================================================
# STEP 7: VIEW RESULTS
# ============================================================

print(daytime_detection_summary)

# ============================================================
# STEP 8: OPTIONAL - SAVE RESULTS
# ============================================================
# Remove the # below if you want to save the summary table.

# write.csv(daytime_detection_summary, "daytime_detection_summary.csv", row.names = FALSE)
