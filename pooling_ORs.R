library(metafor)
library(readxl)
library(writexl)

combined_data <- read_csv("last_analysis/OR_svyglm_table.csv") %>% 
  mutate(
    ## log(OR)
    logOR  = log(estimate),
    
    ## 95 %-CI → standard error, then variance
    seLogOR  = (log(conf.high) - log(conf.low)) / (2 * 1.96),
    varLogOR = seLogOR^2,
    
    ## make absolutely sure the sex factor is as expected
    model = factor(model, levels = c("men", "women"))
  )

for (sex in levels(combined_data$model)) {
  
  dat_sex <- filter(combined_data, model == sex)
  unique_vars <- unique(dat_sex$term)
  
  results_list <- lapply(unique_vars, function(v) {
    
    dat_var <- filter(dat_sex, term == v)
    
    ## Need ≥2 estimates to run a meta-analysis; otherwise just pass through
    if (nrow(dat_var) < 2) {
      return(data.frame(
        Variable = v,
        OR       = exp(dat_var$logOR[1]),
        Lower_CI = exp(log(dat_var$conf.low[1])),
        Upper_CI = exp(log(dat_var$conf.high[1])),
        p_value  = NA
      ))
    }
    
    rma_res <- rma(
      yi  = logOR,
      sei = seLogOR,
      data = dat_var,
      method = "REML"
    )
    
    data.frame(
      Variable = v,
      OR       = exp(rma_res$b),       # pooled OR
      Lower_CI = exp(rma_res$ci.lb),   # lower 95 % CI
      Upper_CI = exp(rma_res$ci.ub),   # upper 95 % CI
      p_value  = rma_res$pval          # (two-sided) p-value
    )
  })
  
  results_df <- bind_rows(results_list)
  write_csv(results_df,
            paste0("last_analysis/pooling/", sex, "_pooled_ORs.csv"))
}

library(metafor)
library(tidyverse)
library(readr)
library(writexl)

# Define variables and modalities
vars     <- c('egitim_cat', 'marital', 'KIR_KENT', 'depression_cat', 'med_depression', 'med_anxiety')
modalities <- c('multi', 'uni')

# Loop through each combination
for (var in vars) {
  for (mod in modalities) {
    
    file_path <- paste0("last_analysis/OR_addon_", var, "_", mod, ".csv")
    
    if (!file.exists(file_path)) {
      warning(paste("Missing file:", file_path))
      next
    }
    
    # Read and prepare data
    combined_data <- read_csv(file_path) %>% 
      mutate(
        logOR     = log(estimate),
        seLogOR   = (log(conf.high) - log(conf.low)) / (2 * 1.96),
        varLogOR  = seLogOR^2,
        model     = factor(model, levels = c("men", "women"))
      )
    
    for (sex in levels(combined_data$model)) {
      
      dat_sex <- filter(combined_data, model == sex)
      unique_vars <- unique(dat_sex$term)
      
      results_list <- lapply(unique_vars, function(v) {
        
        dat_var <- filter(dat_sex, term == v)
        
        if (nrow(dat_var) < 2) {
          return(data.frame(
            Variable = v,
            OR       = exp(dat_var$logOR[1]),
            Lower_CI = exp(log(dat_var$conf.low[1])),
            Upper_CI = exp(log(dat_var$conf.high[1])),
            p_value  = NA
          ))
        }
        
        rma_res <- rma(
          yi  = logOR,
          sei = seLogOR,
          data = dat_var,
          method = "REML"
        )
        
        data.frame(
          Variable = v,
          OR       = round(exp(rma_res$b), 2),
          Lower_CI = round(exp(rma_res$ci.lb), 2),
          Upper_CI = round(exp(rma_res$ci.ub), 2),
          p_value  = round(rma_res$pval, 3)
        )
      })
      
      results_df <- bind_rows(results_list)
      
      output_file <- paste0("last_analysis/pooling/", sex, "_pooled_ORs_", var, "_", mod, ".csv")
      write_csv(results_df, output_file)
    }
  }
}


