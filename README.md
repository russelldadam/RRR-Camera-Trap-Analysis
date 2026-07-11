# Human Influence on the Spatiotemporal Patterns of Bobcats, Coyotes, and Mule Deer on a Working Landscape in the Sierra Nevada Foothills

## Research Question

How does human activity influence the spatial and temporal behavior of
bobcats, coyotes, and mule deer on a working landscape under a
conservation easement?

Analysis Workflow

Camera Trap Data -> Occupancy Null Model -> Occupancy Covariate Models -> Temporal Overlap Analysis -> Daytime Detection Summary -> Same-Day Co-occurrence Analysis

Master's Thesis  
California State University, Long Beach

Author: Adam Russell

## Description

This repository contains the R scripts used to analyze camera trap data collected at River Ridge Ranch, California.

Analyses include:

- Occupancy modeling
- Temporal overlap analysis
- Daytime detection summaries
- Same-day co-occurrence analysis

## Repository Contents

**occupancy_null_model.R**

Fits a single-season occupancy model without covariates.

---

**occupancy_covariate_models.R**

Fits occupancy models using elevation and human activity as site covariates and compares models using AIC.

---

**temporal_overlap_analysis.R**

Calculates diel activity overlap (D̂) among wildlife species and between wildlife and humans.

---

**daytime_detection_summary.R**

Summarizes daytime wildlife detections on human-present and human-absent days.

---

**same_day_cooccurrence_fisher.R**

Evaluates same-day co-occurrence using Fisher's Exact Test and calculates odds ratios.

## Required R Packages

- dplyr
- tidyr
- lubridate
- overlap
- readxl
- unmarked

## Contact

Adam Russell 
Email:russell.d.adam@gmail.com
California State University, Long Beach
