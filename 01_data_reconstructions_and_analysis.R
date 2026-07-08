# ==============================================================================
# PROJECT: Menstrual Hygiene & Educational Equity Pipeline
# FILE: 01_data_reconstructions_and_analysis.R
# PURPOSE: Full programmatic cleanup, index reconstruction and analysis starting 
#          from raw survey inputs, bypassing all original SPSS calculated fields.
# ==============================================================================

# ------------------------------------------------------------------------------
# PHASE 1: Environment Setup & Raw Data Ingestion
# ------------------------------------------------------------------------------

# Load core data science and biostatistics ecosystems
library(tidyverse)  # For data wrangling, transformation, and modern piping
library(haven)      # For robust ingestion of native SPSS (.sav) files and attributes
library(psych)      # For scale reliability engineering (Cronbach's Alpha)

# Define file path
spss_data_path <- "menstrual_hygiene/change.sav"

# Ingest raw SPSS data while maintaining descriptive variable labels
raw_survey_data <- read_sav(spss_data_path)

# Isolate raw data by dropping all pre-computed indices, filters, and indicators 
# to prevent data contamination from the legacy file.
purged_raw_data <- raw_survey_data %>%
  select(
    # Demographics & socioeconomic structural variables
    respondent, school, age, religion, education, mothers_edu, mothers_occupation, family, menses_start_age,
    
    # Information & communication exposure items
    heard_menstruation, information, who_taught, teach_properly,
    
    # Primary knowledge items
    menstruation_k1, normalage_k2, normalcycle_k3, cookfood_k4, causes_k5, bleedingoccurs_k6, infection_k7,
    
    # Primary practice items
    padperday_p1, reuse_material_p2, material_p3, drycloth_skip_p4, dispose_p5, changepad_p6, cleangenital_p7, use_clean_skip_p8, eat_food_p9, pad_last_used_p10,
    
    # Socio-cultural restriction components
    religious_function_R1, household_work_R2, sleep_R3, touch_others_R4, touch_parents_R5,
    
    # Educational equity & health outcome metrics
    Missed_school_S1, Reason_skip_s2, feeling_menses_s3, reasin_using_cloth, complication_s3, Moreinfo_s4
  )

# ------------------------------------------------------------------------------
# PHASE 2: Automated Structural Data Cleaning & Factor Wrangling
# ------------------------------------------------------------------------------

cleaned_base_data <- purged_raw_data %>%
  # Standardize empty strings or unmapped white spaces to true NA objects
  mutate(across(where(is.character), ~na_if(trimws(.), ""))) %>%
  
  # Explicitly map numbers to meaningful categorical labels based on codebook attributes
  mutate(
    # Structural institutional environment factor
    School_Type = factor(school, levels = c(0,1), labels = c("Private", "Public")),
    
    # Socio-demographic factors
    Religion = factor(religion, levels = c(1, 2, 3, 4, 5),
                      labels = c("Hindu", "Buddhist", "Christian", "Muslim", "Kirat")),
    
    Education_Grade = factor(education, levels = c(1,2,3),
                             labels = c("Grade 7", "Grade 8", "Grade 9"), ordered = TRUE),
    
    Mothers_Education = factor(mothers_edu, levels = c(1, 2, 3, 4, 5),
                               labels = c("Can't read/write", "Can read/write", "Preschool-5th", "6th-12th Grade", "Graduation+"),
                               ordered = TRUE),
    Family_Structure = factor(family, levels = c(1,2), labels = c("Nuclear", "Joint")),
    
    # Explicitly fix the "Coded Category Mean" flaw for age at first period.
    # Instead of calculating the mean using categorical codes (which yielded an invalid '3.17')
    # we treat it as a properly ordered factor for structural groupings.
    Menarche_Age = factor(menses_start_age, levels = c(1, 2, 3, 4, 5),
                          labels = c("<=10", "11", "12", "13", ">=14"), ordered = TRUE),
    
    # Target outcomes variable: School Absenteeism Profile
    School_Absenteeism = factor(Missed_school_S1, levels = c(1, 2, 3),
                                labels = c("Always", "Sometimes", "Never"), ordered = TRUE),
    
    # Create an intuitive binary target indicator for epidemiological logistic modeling
    Has_Missed_School = if_else(Missed_school_S1 %in% c(1, 2), 1, 0)
  ) %>%
  
  # Account for structural skip-logic dependencies built into the survey design
  mutate(
    # If a girl uses commercial pads, the question about where she dries cloth is naturally skipped
    # We recode this from standard missing 'NA' to an explicit structural label
    Cloth_Drying_Location = case_when(
      material_p3 == 3 ~ "Not Applicable (Pad User)",
      is.na(drycloth_skip_p4) ~ "Unreported/Missing",
      drycloth_skip_p4 == 1 ~ "In the Bathroom",
      drycloth_skip_p4 == 2 ~ "In Sunlight",
      drycloth_skip_p4 == 3 ~ "Hidden Places (No Observation)",
      drycloth_skip_p4 == 4 ~ "Others"
    ),
    
    # If a girl never misses school, her reason for missing school defaults to a non-skipped state
    Absenteeism_Primary_Reason = case_when(
      Missed_school_S1 == 3 ~ "School Never Missed",
      is.na(Reason_skip_s2) ~ "Unreported/Missing",
      Reason_skip_s2 == 1 ~ "Lack of clean toilet",
      Reason_skip_s2 == 2 ~ "Lack of privacy to change",
      Reason_skip_s2 == 3 ~ "No water infrastructure",
      Reason_skip_s2 == 4 ~ "Physical pain/discomfort",
      Reason_skip_s2 == 5 ~ "Fear of accidental leakage",
      Reason_skip_s2 == 6 ~ "Others"
    )
  )

# ------------------------------------------------------------------------------
# PHASE 3: Public Health Index Reconstruction (Knowledge & Practice Metrics)
# ------------------------------------------------------------------------------

# Rebuilding index scores using robust public health logic boundaries.
engineered_dataset <- cleaned_base_data %>%
  # Reconstruct Anatomical Knowledge Index Components (1 = Correct, 0 = Incorrect)
  mutate(
    k1_process   = if_else(menstruation_k1 == 1, 1, 0, missing = 0), 
    k2_norm_age  = if_else(normalage_k2 == 2, 1, 0, missing = 0),    
    k3_cycle     = if_else(normalcycle_k3 == 1, 1, 0, missing = 0),  
    k4_cook      = if_else(cookfood_k4 == 1, 1, 0, missing = 0),     
    k5_cause     = if_else(causes_k5 == 1, 1, 0, missing = 0),       
    k6_source    = if_else(bleedingoccurs_k6 == 1, 1, 0, missing = 0),
    k7_infection = if_else(infection_k7 == 1, 1, 0, missing = 0)     
  ) %>%
  
  # Reconstruct Hygienic Practice Index Components (1 = Safe, 0 = Unsafe)
  # Added 'missing = 0' to handle conditional skip logic gracefully
  mutate(
    p1_frequency  = if_else(padperday_p1 %in% c(2, 3), 1, 0, missing = 0), 
    p2_reuse      = if_else(reuse_material_p2 == 2, 1, 0, missing = 0),    
    p3_material   = if_else(material_p3 == 3, 1, 0, missing = 0),          
    p5_disposal   = if_else(dispose_p5 == 2, 1, 0, missing = 0),           
    p6_school_chg = if_else(changepad_p6 == 1, 1, 0, missing = 0),         
    p7_cleaning   = if_else(cleangenital_p7 == 1, 1, 0, missing = 0),      
    # Safely captures skip patterns: if skipped, no safe agent was used
    p8_agent      = if_else(use_clean_skip_p8 == 2, 1, 0, missing = 0)     
  ) %>%
  
  # Reconstruct the Socio-Cultural Restriction Matrix (1 = Restricted, 0 = Allowed)
  mutate(
    r1_religion  = if_else(religious_function_R1 == 2, 1, 0, missing = 0), 
    r2_household = if_else(household_work_R2 == 2, 1, 0, missing = 0),      
    r3_sleeping  = if_else(sleep_R3 %in% c(2, 3), 1, 0, missing = 0),       
    r4_touching  = if_else(touch_others_R4 %in% c(1, 3), 1, 0, missing = 0),
    r5_parents   = if_else(touch_parents_R5 == 2, 1, 0, missing = 0),       
    r6_kitchen   = if_else(eat_food_p9 == 2, 1, 0, missing = 0)             
  ) %>%
  
  # Calculate final row-wise composite index sums based on our reconstructed indicators 
  rowwise() %>%
  mutate(
    Calc_Knowledge_Total   = sum(c_across(k1_process:k7_infection), na.rm = TRUE),
    Calc_Practice_Total    = sum(c_across(p1_frequency:p8_agent), na.rm = TRUE),
    Calc_Restriction_Total = sum(c_across(r1_religion:r6_kitchen), na.rm = TRUE)
  ) %>%
  ungroup() %>% # <-- FIXED: Kept the pipeline flowing smoothly into the next operation
  
  # Scale and transform metrics into standardized statistical units (Z-scores)
  # Standard Formula: Z = (X - Mean) / SD
  mutate(
    Z_Score_Knowledge   = (Calc_Knowledge_Total - mean(Calc_Knowledge_Total, na.rm = TRUE)) / sd(Calc_Knowledge_Total, na.rm = TRUE),
    Z_Score_Practice    = (Calc_Practice_Total - mean(Calc_Practice_Total, na.rm = TRUE)) / sd(Calc_Practice_Total, na.rm = TRUE),
    Z_Score_Restriction = (Calc_Restriction_Total - mean(Calc_Restriction_Total, na.rm = TRUE)) / sd(Calc_Restriction_Total, na.rm = TRUE) # <-- FIXED: Removed trailing comma
  )

# ------------------------------------------------------------------------------
# PHASE 4: Statistical Testing & Explanatory Modeling
# ------------------------------------------------------------------------------

# --- 1. Scale Reliability Verification ---

# Helper Function: Automatically identifies and removes zero-variance columns
# to safeguard downstream correlation calculations.
remove_zero_variance_items <- function(dataframe) {
  dataframe %>% 
    select(where(~ var(., na.rm = TRUE) > 0))
}

# Extract and safeguard the raw item matrices
knowledge_items_clean <- engineered_dataset %>% 
  select(k1_process:k7_infection) %>% 
  remove_zero_variance_items()

practice_items_clean  <- engineered_dataset %>% 
  select(p1_frequency:p8_agent) %>% 
  remove_zero_variance_items()

# Inform the user if any columns were dropped due to lack of variation
dropped_k <- setdiff(names(engineered_dataset %>% select(k1_process:k7_infection)), names(knowledge_items_clean))
dropped_p <- setdiff(names(engineered_dataset %>% select(p1_frequency:p8_agent)), names(practice_items_clean))

if(length(dropped_k) > 0) cat("Dropped from Knowledge Alpha due to 0 variance:", paste(dropped_k, collapse=", "), "\n")
if(length(dropped_p) > 0) cat("Dropped from Practice Alpha due to 0 variance:", paste(dropped_p, collapse=", "), "\n")

# Compute Alpha using pairwise deletions as a final safety check
cat("\n--- Reconstructed Scale Diagnostics ---\n")

alpha_knowledge <- psych::alpha(knowledge_items_clean, use = "pairwise")
cat("Knowledge Index Cronbach's Alpha:", round(alpha_knowledge$total$raw_alpha, 3), "\n")

alpha_practice  <- psych::alpha(practice_items_clean, use = "pairwise")
cat("Practice Index Cronbach's Alpha: ", round(alpha_practice$total$raw_alpha, 3), "\n\n")

# ------------------------------------------------------------------------------
# PHASE 5: Exporting Clean Data Assets
# ------------------------------------------------------------------------------
# Save the clean dataset to feed into Power BI dashboard
write_csv(engineered_dataset, "menstrual_hygiene_clean_analytical.csv")
cat("\nPipeline Execution Complete. Clean data assets exported successfully. \n")