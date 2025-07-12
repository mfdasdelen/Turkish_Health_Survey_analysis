library(tidyverse)    # dplyr, readr, stringr, purrr
library(survey)       # complex-survey inference
library(janitor)      # optional tabyl helpers

# ────────────────────────────────────────────────────────────────
# 1. Read data  ──────────────────────────────────────────────────
ths <- read_csv("last_analysis/all_years_cleaned_data_calibrated.csv")

# ────────────────────────────────────────────────────────────────
# 2. Recoding & variable types  ─────────────────────────────────
ths <- ths %>% 
  mutate(
    year      = factor(year),
    cinsiyet  = factor(cinsiyet, levels = c(0, 1), labels = c("Male", "Female")),
    YAS_GRUBU = factor(YAS_GRUBU,
                       levels = c("15-24", "25-34", "35-44",
                                  "45-54", "55-64", "65-74", "75+")),
    bmi_cat   = factor(bmi_cat, levels = 0:4,
                       labels = c("Normal weight", "Underweight",
                                  "Overweight", "Obese", "N/A"))
  )

# Binary disease flags (keep as *numeric* 0/1)
disease_vars <- c("asthma", "copd", "mi", "coronary", "htn", "stroke",
                  "osteoarthritis", "diabetes", "cirrhosis", "ui", "depression")

# Other categorical variables you still want as factors
factor_vars  <- c("cardiac_disease", "egitim_cat", "marital",
                  "KIR_KENT", "depression_cat", "bmi_cat")

ths <- ths %>% 
  mutate(
    across(all_of(disease_vars), ~ as.numeric(as.character(.))), # 0/1 numeric
    across(all_of(factor_vars), as.factor)
  )

# Remove records with missing PSU id
ths <- filter(ths, !is.na(hane))

# ────────────────────────────────────────────────────────────────
# 3. Master survey design  ───────────────────────────────────────
des_all <- svydesign(
  ids     = ~hane,              # primary sampling unit
  strata  = ~IBBS1_SOZDE,       # 26 strata common to all waves
  weights = ~w_calibrated,      # calibrated person weight
  data    = ths,
  nest    = TRUE
)

# ────────────────────────────────────────────────────────────────
# 4. European Standard Population (already in 15–24 … 75+ bands) ─
esp <- read_csv("european_standard_population_grouped.csv") %>% 
  rename(YAS_GRUBU = AgeGroup, cinsiyet = Sex,
         std_weight = EuropeanStandardPopulation) %>% 
  filter(YAS_GRUBU != "0-14") %>% 
  group_by(cinsiyet) %>% 
  mutate(std_prop = std_weight / sum(std_weight)) %>% 
  ungroup() %>% 
  select(cinsiyet, YAS_GRUBU, std_prop)

# ────────────────────────────────────────────────────────────────
# 5. Direct age-standardised prevalence  ─────────────────────────
results <- map_dfr(levels(ths$year), \(yr) {
  map_dfr(c(levels(ths$cinsiyet), "ALL"), \(sex) {
    
    # subset design
    des_sub <- if (sex == "ALL") {
      subset(des_all, year == yr)
    } else {
      subset(des_all, year == yr & cinsiyet == sex)
    }
    
    map_dfr(disease_vars, \(d) {
      
      # age-specific prevalence (proportion, not %)
      age_prev <- svyby(
        as.formula(paste0("~", d)),
        ~YAS_GRUBU,
        design = des_sub,
        svymean,
        na.rm = TRUE
      ) %>% 
        rename(pct = !!d)                      # pct is 0–1 proportion
      
      # choose appropriate standard population
      std_weights <- if (sex == "ALL") {
        esp %>%
          group_by(YAS_GRUBU) %>%
          summarise(std_prop = mean(std_prop), .groups = "drop")
      } else {
        esp %>% filter(cinsiyet == sex)
      }
      
      # ensure all age-bands present
      age_std <- full_join(
        std_weights,
        age_prev,
        by = "YAS_GRUBU"
      ) %>% 
        mutate(pct = replace_na(pct, 0))
      
      # direct standardisation
      std_prev_prop <- sum(age_std$pct * age_std$std_prop)
      
      tibble(
        year      = yr,
        sex       = sex,
        disease   = d,
        prev_pct  = std_prev_prop * 100        # convert to %
      )
    })
  })
})

# ────────────────────────────────────────────────────────────────
# 6. Save  ───────────────────────────────────────────────────────
write_csv(results, "last_analysis/age_standardised_prevalence_by_sex.csv")