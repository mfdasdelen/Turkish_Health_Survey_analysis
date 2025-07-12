library(tidyverse)    
library(survey)      
library(janitor)      

# ────────────────────────────────────────────────────────────────
# 1. Read & combine all waves  ───────────────────────────────────

ths <- read_csv('last_analysis/all_years_cleaned_data_calibrated.csv')
# ────────────────────────────────────────────────────────────────
# 2. Conversion of factorial variables
ths <- ths %>%
  mutate(
    year     = factor(year),
    cinsiyet   = factor(cinsiyet, levels = c(0, 1), labels = c("Male", "Female")),
    YAS_GRUBU = factor(YAS_GRUBU, levels = c('15-24','25-34', '35-44', '45-54', '55-64', '65-74', '75+')),
    bmi_cat  = factor(bmi_cat,        
                      levels = c(0, 1, 2, 3, 4),
                      labels = c ("Normal weight","Underweight",
                                 "Overweight", "Obese", "N/A"))
  )

var_convert <- c(
  "asthma", "copd", "mi", "coronary", "htn", "cardiac_disease", "stroke",
  "osteoarthritis", "diabetes", "cirrhosis", "ui", "depression",
  "bmi_cat", "egitim_cat", "marital", "KIR_KENT", "depression_cat",
  "med_depression", "med_anxiety"
)

ths <- ths %>%
  mutate(across(all_of(var_convert), as.factor))

ths <- ths[!is.na(ths$hane),]
# ────────────────────────────────────────────────────────────────
# 3. Survey-design object  ───────────────────────────────────────
des <- svydesign(
  ids      = ~hane,        # household ≈ PSU
  strata   = ~IBBS1_SOZDE,       # NUTS-1 strata
  weights  = ~w_calibrated,  # person-level weight
  data     = ths,
  nest     = TRUE)