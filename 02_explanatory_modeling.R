# ==============================================================================
# PROJECT: Menstrual Hygiene & Educational Equity Pipeline
# FILE: 02_explanatory_modeling.R
# PURPOSE: Advanced Biostatistics Engine. Performs bivariate profiling, 
#          multicollinearity diagnostics, and multivariable logistic regression.
# ==============================================================================

# ------------------------------------------------------------------------------
# PHASE 1: Environment Setup & Data Re-Factorization
# ------------------------------------------------------------------------------

library(tidyverse)  # For data manipulation and pipelined operations
library(gtsummary)  # For building publication-grade clinical/epidemiological tables
library(car)        # For running Variance Inflation Factor (VIF) diagnostics

# Guarantee the outputs directory exists before attempting exports
if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
  cat("Created missing 'outputs/' folder on disk.\n")
}

# Ingest the clean analytical dataset
clean_df <- read_csv("menstrual_hygiene_clean_analytical.csv")

# Set baseline reference groups for categorical variables
modeled_df <- clean_df %>%
  mutate(
    School_Type = factor(School_Type, levels = c("Private", "Public")),
    Family_Structure = factor(Family_Structure, levels = c("Nuclear", "Joint")),
    Mothers_Education = factor(Mothers_Education, 
                               levels = c("Can't read/write", "Can read/write", 
                                          "Preschool-5th", "6th-12th Grade", "Graduation+")),
    Has_Missed_School_Label = factor(Has_Missed_School, levels = c(0, 1), 
                                     labels = c("Attended School", "Missed School"))
  )

# ------------------------------------------------------------------------------
# PHASE 2: Bivariate Screening Matrix (Table 1)
# ------------------------------------------------------------------------------

cat("Step 2a: Compiling Bivariate Stratified Cohort Matrix...\n")

bivariate_matrix <- modeled_df %>%
  select(Has_Missed_School_Label, School_Type, Calc_Knowledge_Total, 
         Calc_Practice_Total, Calc_Restriction_Total, Mothers_Education) %>%
  tbl_summary(
    by = Has_Missed_School_Label,
    missing = "no",
    # Explicitly force integer scores to be evaluated as continuous variables
    type = list(
      Calc_Knowledge_Total ~ "continuous",
      Calc_Practice_Total ~ "continuous",
      Calc_Restriction_Total ~ "continuous"
    ),
    label = list(
      School_Type ~ "Institutional Facility Type",
      Calc_Knowledge_Total ~ "Total Reproductive Health Knowledge Score",
      Calc_Practice_Total ~ "Total Hygienic Practice Score",
      Calc_Restriction_Total ~ "Socio-Cultural Restriction Index Count",
      Mothers_Education ~ "Maternal Educational Attainment Baseline"
    )
  ) %>%
  add_p(
    test = list(all_categorical() ~ "chisq.test", all_continuous() ~ "wilcox.test")
  ) %>%
  bold_p(t = 0.05) %>%
  bold_labels()

print(bivariate_matrix)

# ------------------------------------------------------------------------------
# PHASE 3: Regression Diagnostic - Multicollinearity Verification
# ------------------------------------------------------------------------------

cat("\nStep 2b: Running Variance Inflation Factor (VIF) Diagnostic Tests...\n")

vif_check_model <- lm(
  Has_Missed_School ~ School_Type + Calc_Restriction_Total + Calc_Knowledge_Total + Calc_Practice_Total + Mothers_Education, 
  data = modeled_df
)

vif_values <- car::vif(vif_check_model)
print(vif_values)

if(any(vif_values[,1] > 5.0)) {
  warning("Caution: High multicollinearity detected.")
} else {
  cat("Success: All predictor variables have VIF scores well below 5.0. No multicollinearity present.\n")
}

# ------------------------------------------------------------------------------
# PHASE 4: Multivariable Explanatory Logistic Regression Modeling
# ------------------------------------------------------------------------------

cat("\nStep 2c: Fitting Multivariable Explanatory Logistic Engine...\n")

absenteeism_logistic_engine <- glm(
  Has_Missed_School ~ School_Type + Calc_Restriction_Total + Calc_Knowledge_Total + Calc_Practice_Total + Mothers_Education,
  data = modeled_df,
  family = binomial(link = "logit")
)

# ------------------------------------------------------------------------------
# PHASE 5: Generating Executive-Grade Publication Tables
# ------------------------------------------------------------------------------

explanatory_regression_table <- tbl_regression(
  absenteeism_logistic_engine,
  exponentiate = TRUE,
  label = list(
    School_Type ~ "Institutional Infrastructure Class (Ref: Private)",
    Calc_Restriction_Total ~ "Socio-Cultural Restriction Index Count (+1 Unit)",
    Calc_Knowledge_Total ~ "Total Reproductive Knowledge Score (+1 Unit)",
    Calc_Practice_Total ~ "Total Hygienic Practice Score (+1 Unit)",
    Mothers_Education ~ "Maternal Educational Level (Ref: Can't read/write)"
  )
) %>%
  bold_p(t = 0.05) %>%
  italicize_levels() %>%
  add_nevent()

print(explanatory_regression_table)

# Save HTML asset cleanly
gt::gtsave(as_gt(explanatory_regression_table), "outputs/explanatory_model_matrix.html")
cat("\nExplanatory Modeling Phase Complete. Production HTML asset exported successfully.\n")