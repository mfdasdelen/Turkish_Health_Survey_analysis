library(tidyverse)    # dplyr, readr, stringr, purrr
library(survey)       # complex-survey estimation
library(janitor)      # tabyl() convenience (optional)
library(readxl)

years <- c("2008", "2010", "2012", "2014", "2016", "2019", "2022")

for (y in years) {
  
  # Load microdata
  ths <- read_csv(paste0(y, "_data_orig_renamed.csv"))
  
  # Recode age group
  ths$YAS_GRUBU <- replace(ths$YAS_GRUBU, ths$YAS < 15, "0-14") 
  ths <- ths[!is.na(ths$hane),]
  ths <- ths %>%
    mutate(
      cinsiyet  = factor(cinsiyet, levels = c(1, 2), labels = c("Male", "Female")),
      YAS_GRUBU = factor(YAS_GRUBU, 
                         levels = c('0-14','15-24','25-34','35-44',
                                    '45-54','55-64','65-74','75+'))
    )
  
  # Load population data
  freq <- read_excel(paste0("yearly_population_csvs/population_", y, ".xlsx")) %>%
    mutate(
      cinsiyet  = factor(cinsiyet, levels = c("Male", "Female")),
      YAS_GRUBU = factor(YAS_GRUBU,
                         levels = c('0-14','15-24','25-34','35-44',
                                    '45-54','55-64','65-74','75+'))
    )
  
  # Design object
  des <- svydesign(
    ids     = ~hane,
    strata  = ~IBBS1_SOZDE,
    weights = ~weight_factor,
    data    = ths,
    nest    = TRUE
  )
  
  # Calibration totals
  pop_tot <- with(freq,
                  c(
                    Intercept      = sum(Freq),
                    cinsiyetFemale = sum(Freq[cinsiyet == "Female"]),
                    tapply(Freq, YAS_GRUBU, sum)[-1]
                  ))
  
  # Calibrate survey design
  des_cal <- survey::calibrate(
    design     = des,
    formula    = ~ cinsiyet + YAS_GRUBU,
    population = pop_tot,
    calfun     = "raking"
  )
  
  # Save calibrated weights
  ths$w_calibrated <- weights(des_cal)
  write_csv(ths, paste0("last_analysis/", y, "_calibrated_orig_microdata.csv"))
}
