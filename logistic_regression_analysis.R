library(tidyverse)    # dplyr, readr, stringr, purrr
library(survey)       # complex-survey estimation
library(janitor)
library(broom) 
library(glue)

ths <- read_csv('last_analysis/all_years_cleaned_data_calibrated.csv')

ths <- ths %>%
  mutate(
    year     = factor(year),
    cinsiyet   = factor(cinsiyet, levels = c(0, 1), labels = c("Male", "Female")),
    YAS_GRUBU = factor(YAS_GRUBU, levels = c('15-24','25-34', '35-44', '45-54', '55-64', '65-74', '75+')),
    bmi_cat  = factor(bmi_cat,        # adjust if your coding differs
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

des <- svydesign(
  ids      = ~hane,        # household ≈ PSU
  strata   = ~IBBS1_SOZDE,       # strata available in all waves
  weights  = ~w_calibrated,  # person-level weight
  data     = ths,
  nest     = TRUE)



fit_or <- function(sub_des, rhs, label) {
  fmla  <- reformulate(rhs, response = "ui")        # ui ~ ...
  mod   <- svyglm(fmla, design = sub_des,
                  family = quasibinomial())
  broom::tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(model = label)                            # men / women / all
}

# ---------------------------------------------------------------
# 1.  loop over survey years  -----------------------------------
# ---------------------------------------------------------------


# formula components (as character vectors)
base_rhs <- c("year","YAS_GRUBU", "depression", "asthma", "copd",
              "cardiac_disease", "stroke", "osteoarthritis",
              "diabetes", "cirrhosis")

fit_or(des, base_rhs, "all")
fit_or(subset(des, cinsiyet == "Male"),   base_rhs, "men")
fit_or(subset(des, cinsiyet == "Female"), base_rhs, "women")

# ---------------------------------------------------------------
# 2.  save or inspect  ------------------------------------------
# ---------------------------------------------------------------
write_csv(all_or,
          "last_analysis/OR_svyglm_table.csv")


base_rhs <- c("AgeGroup", "depression", "asthma", "copd",
              "cardiac_disease", "stroke", "osteoarthritis",
              "diabetes", "cirrhosis")

extra_vars <- c("egitim_cat", "marital", "KIR_KENT")

fit_or <- function(design, rhs, label, addon) {
  fmla <- reformulate(rhs, response = "ui")
  mod  <- survey::svyglm(fmla, design = design, family = quasibinomial())
  
  tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
    filter(!term %in% "(Intercept)") |>
    mutate(model = label,
           addon = addon)
}



# ────────────────────────────────────────────────────────────────
# 1. Loop: year × sex × extra var  ------------------------------
# ────────────────────────────────────────────────────────────────
master_or <- map_dfr(levels(ths$year), function(yr) {
  
  message("Year ", yr)
  
  des_y <- subset(des, year == yr)
  
  # decide whether BMI + pregnancy should be part of *every* model in that year
  rhs_core <- base_rhs
  
  # convenience function to build sex-specific formulas
  make_rhs <- function(extra) paste(c(rhs_core, extra), collapse = " + ")
  
  # ---- iterate over the four extra variables ------------------
  map_dfr(extra_vars, function(addon) {
    if (addon == "KIR_KENT" && !yr %in% c("2008", "2010", "2012"))
      return(NULL)
    
    rhs_men   <- make_rhs(addon)
    rhs_all   <- glue("{rhs_men} + cinsiyet")
    
    # add pregnancy only for women in 2019 & 2022
    rhs_women <- rhs_men
    #if (yr %in% c("2019","2022")) rhs_women <- glue("{rhs_women} + pregnancy")
    
    
    bind_rows(
      fit_or(subset(des_y, cinsiyet == "Male"),   rhs_men,   "men",   addon),
      fit_or(subset(des_y, cinsiyet == "Female"), rhs_women, "women", addon),
      fit_or(des_y,                                rhs_all,  "all",   addon)
    )
  }) |>
    mutate(year = yr)
})

# ────────────────────────────────────────────────────────────────
# 2.  Save outputs  ---------------------------------------------
# ────────────────────────────────────────────────────────────────
# master file with everything
write_csv(master_or, "last_analysis/OR_addon_master_multi.csv")

# one file per addon variable
walk(extra_vars, function(v)
  write_csv(filter(master_or, addon == v),
            glue("last_analysis/OR_addon_{v}_multi.csv")) )




base_rhs <- c("AgeGroup", "depression","asthma", "copd",
              "cardiac_disease", "stroke", "osteoarthritis",
              "diabetes", "cirrhosis")

extra_vars <- c("med_depression", "med_anxiety")

fit_or <- function(design, rhs, label, addon) {
  fmla <- reformulate(rhs, response = "ui")
  mod  <- survey::svyglm(fmla, design = design, family = quasibinomial())
  
  tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
    filter(!term %in% "(Intercept)") |>
    mutate(model = label,
           addon = addon)
}
# ────────────────────────────────────────────────────────────────
# 1. Loop: year × sex × extra var  ------------------------------
# ────────────────────────────────────────────────────────────────
master_or <- map_dfr(levels(ths$year), function(yr) {
  
  message("Year ", yr)
  
  des_y <- subset(des, year == yr)
  
  # decide whether BMI + pregnancy should be part of *every* model in that year
  rhs_core <- base_rhs
  
  # convenience function to build sex-specific formulas
  make_rhs <- function(extra) paste(c(rhs_core, extra), collapse = " + ")
  
  # ---- iterate over the four extra variables ------------------
  map_dfr(extra_vars, function(addon) {
    if (addon == "med_depression" && yr %in% c("2014", "2016", "2019", "2022"))
      return(NULL)
    if (addon == "med_anxiety" && yr %in% c("2014", "2016", "2019", "2022"))
      return(NULL)
    rhs_men   <- make_rhs(addon)
    rhs_all   <- glue("{rhs_men} + cinsiyet")

    # add pregnancy only for women in 2019 & 2022
    rhs_women <- rhs_men
    #if (yr %in% c("2019","2022")) rhs_women <- glue("{rhs_women} + pregnancy")
    
    
    bind_rows(
      fit_or(subset(des_y, cinsiyet == "Male"),   rhs_men,   "men",   addon),
      fit_or(subset(des_y, cinsiyet == "Female"), rhs_women, "women", addon),
      fit_or(des_y,                                rhs_all,  "all",   addon)
    )
  }) |>
    mutate(year = yr)
})

# one file per addon variable
walk(extra_vars, function(v)
  write_csv(filter(master_or, addon == v),
            glue("last_analysis/OR_addon_{v}_multi.csv")) )



base_rhs <- c("AgeGroup", "asthma", "copd",
              "cardiac_disease", "stroke", "osteoarthritis",
              "diabetes", "cirrhosis")

extra_vars <- c("depression_cat")

fit_or <- function(design, rhs, label, addon) {
  fmla <- reformulate(rhs, response = "ui")
  mod  <- survey::svyglm(fmla, design = design, family = quasibinomial())
  
  tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
    filter(!term %in% "(Intercept)") |>
    mutate(model = label,
           addon = addon)
}
# ────────────────────────────────────────────────────────────────
# 1. Loop: year × sex × extra var  ------------------------------
# ────────────────────────────────────────────────────────────────
master_or <- map_dfr(levels(ths$year), function(yr) {
  
  message("Year ", yr)
  
  des_y <- subset(des, year == yr)
  
  # decide whether BMI + pregnancy should be part of *every* model in that year
  rhs_core <- base_rhs
  
  # convenience function to build sex-specific formulas
  make_rhs <- function(extra) paste(c(rhs_core, extra), collapse = " + ")
  
  # ---- iterate over the four extra variables ------------------
  map_dfr(extra_vars, function(addon) {
    if (addon == "depression_cat" && yr %in% c("2008", "2010", "2012"))
      return(NULL)
    rhs_men   <- make_rhs(addon)
    rhs_all   <- glue("{rhs_men} + cinsiyet")
    
    # add pregnancy only for women in 2019 & 2022
    rhs_women <- rhs_men
    #if (yr %in% c("2019","2022")) rhs_women <- glue("{rhs_women} + pregnancy")
    
    
    bind_rows(
      fit_or(subset(des_y, cinsiyet == "Male"),   rhs_men,   "men",   addon),
      fit_or(subset(des_y, cinsiyet == "Female"), rhs_women, "women", addon),
      fit_or(des_y,                                rhs_all,  "all",   addon)
    )
  }) |>
    mutate(year = yr)
})

# one file per addon variable
walk(extra_vars, function(v)
  write_csv(filter(master_or, addon == v),
            glue("last_analysis/OR_addon_{v}_multi.csv")) )



base_rhs <- c()

extra_vars <- c("egitim_cat", "marital", "KIR_KENT", "depression_cat", "med_depression","med_anxiety")

fit_or <- function(design, rhs, label, addon) {
  fmla <- reformulate(rhs, response = "ui")
  mod  <- survey::svyglm(fmla, design = design, family = quasibinomial())
  
  tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
    filter(!term %in% "(Intercept)") |>
    mutate(model = label,
           addon = addon)
}



# ────────────────────────────────────────────────────────────────
# 1. Loop: year × sex × extra var  ------------------------------
# ────────────────────────────────────────────────────────────────
master_or <- map_dfr(levels(ths$year), function(yr) {
  
  message("Year ", yr)
  
  des_y <- subset(des, year == yr)
  
  # decide whether BMI + pregnancy should be part of *every* model in that year
  rhs_core <- base_rhs
  
  # convenience function to build sex-specific formulas
  make_rhs <- function(extra) paste(c(rhs_core, extra), collapse = " + ")
  
  # ---- iterate over the four extra variables ------------------
  map_dfr(extra_vars, function(addon) {
    if (addon == "KIR_KENT" && !yr %in% c("2008", "2010", "2012"))
      return(NULL)
    if (addon == "depression_cat" && yr %in% c("2008", "2010", "2012"))
      return(NULL)
    if (addon == "med_depression" && yr %in% c("2014", "2016", "2019", "2022"))
      return(NULL)
    if (addon == "med_anxiety" && yr %in% c("2014", "2016", "2019", "2022"))
      return(NULL)
    
    rhs_men   <- make_rhs(addon)
    rhs_all   <- glue("{rhs_men} + cinsiyet")
    
    # add pregnancy only for women in 2019 & 2022
    rhs_women <- rhs_men
    
    
    bind_rows(
      fit_or(subset(des_y, cinsiyet == "Male"),   rhs_men,   "men",   addon),
      fit_or(subset(des_y, cinsiyet == "Female"), rhs_women, "women", addon),
      fit_or(des_y,                                rhs_all,  "all",   addon)
    )
  }) |>
    mutate(year = yr)
})

# ────────────────────────────────────────────────────────────────
# 2.  Save outputs  ---------------------------------------------
# ────────────────────────────────────────────────────────────────
# master file with everything
write_csv(master_or, "last_analysis/OR_addon_master_uni.csv")

# one file per addon variable
walk(extra_vars, function(v)
  write_csv(filter(master_or, addon == v),
            glue("last_analysis/OR_addon_{v}_uni.csv")) )



# Income

years <- levels(ths$year)

ths$income <- factor(ths$income)
des <- svydesign(
  ids      = ~hane,        # household ≈ PSU
  strata   = ~IBBS1_SOZDE,       # strata available in all waves
  weights  = ~w_calibrated,  # person-level weight
  data     = ths,
  nest     = TRUE)

results <- map_dfr(years, function(y) {
  # Subset data for the year
  ths_y <- filter(ths, year == y)
  des_y <- subset(des, year == y)
  
  # Weighted prevalence of UI by income level
  prev_tbl <- svyby(~ui, ~income, design = des_y, svymean, na.rm = TRUE) %>%
    rename(prevalence = ui1) %>%
    select(income, prevalence)
  
  # Raw counts of UI by income level
  count_tbl <- ths_y %>%
    filter(ui == 1) %>%
    count(income, name = "ui_cases")
  
  # Combine and add year + formatted output
  left_join(prev_tbl, count_tbl, by = "income") %>%
    mutate(
      year = y,
      prevalence_pct = round(prevalence * 100, 2),
      ui_N_pct = paste0(ui_cases, " (", prevalence_pct, "%)")
    )
})

# ────────────────────────────────────────────────────────────────
# 5. Save to CSV
write_csv(results, "last_analysis/ui_by_income_yearly.csv")




results <- map_dfr(years, function(y) {
  # Subset data for the year
  ths_y <- filter(ths, year == y)
  des_y <- subset(des, year == y)
  
  # Weighted prevalence of UI by income level
  prev_tbl <- svyby(~depression, ~income, design = des_y, svymean, na.rm = TRUE) %>%
    rename(prevalence = depression1) %>%
    select(income, prevalence)
  
  # Raw counts of UI by income level
  count_tbl <- ths_y %>%
    filter(depression == 1) %>%
    count(income, name = "depression_cases")
  
  # Combine and add year + formatted output
  left_join(prev_tbl, count_tbl, by = "income") %>%
    mutate(
      year = y,
      prevalence_pct = round(prevalence * 100, 2),
      ui_N_pct = paste0(depression_cases, " (", prevalence_pct, "%)")
    )
})

# ────────────────────────────────────────────────────────────────
# 5. Save to CSV
write_csv(results, "last_analysis/depression_by_income_yearly.csv")


chi_results <- map_dfr(years, function(y) {
  
  ths_y <- filter(ths, year == y)
  ths_y <- ths_y %>% filter(income!=0)
  ths_y <- ths_y %>% filter(income!=99)
  ths_y$income2 <- factor(ths_y$income)
  
  # Drop NAs in UI or income (already done earlier but ensure again)
  
  
  # Create contingency table
  tab <- table(ths_y$income, ths_y$ui)
  
  # Perform chi-squared test
  test <- chisq.test(tab)
  
  # Extract key results
  tibble(
    year        = y,
    chi_squared = test$statistic,
    df          = test$parameter,
    p_value     = test$p.value
  )
})

# ────────────────────────────────────────────────────────────────
# 7. Save chi-squared results to CSV
write_csv(chi_results, "last_analysis/chi_squared_ui_vs_income_by_year.csv")
