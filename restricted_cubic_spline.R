# Load required libraries
library(readr)
library(dplyr)
library(rms)
library(ggplot2)

# Define and load the dataset
years <- c("2008", "2010", "2012", "2014", "2016", "2019", "2022")
data_list <- lapply(years, function(y) read_csv(paste0(y, "_cleaned_data.csv")))
data <- bind_rows(data_list)

# Filter extreme BMI values
data <- data %>% filter(bmi >= 15 & bmi <= 45 & bmi_cat < 4)

# Recode variables
data$cinsiyet <- factor(data$cinsiyet, levels = c(0, 1), labels = c("Male", "Female"))
data$ui <- factor(data$ui, levels = c(0, 1))

# Covariates
covariates_male <- c("YAS", "depression", "asthma", "copd", 
                "cardiac_disease", "stroke", "osteoarthritis", 
                "diabetes", "cirrhosis")
covariates_female <- c("YAS", "depression", "asthma", "copd", 
                     "cardiac_disease", "stroke", "osteoarthritis", 
                     "diabetes", "cirrhosis")

# Convert binary covariates to factors (except YAS)
for (var in covariates_female) {
  if (var != "YAS") {
    data[[var]] <- factor(data[[var]])
  }
}

# Loop through genders
for (sex in levels(data$cinsiyet)) {
  cat("\n---", sex, "---\n")
  df <- data %>% filter(cinsiyet == sex)
  if (sex == 'Male') {
    covariates <- covariates_male
  }
  else {
    covariates <- covariates_female
  }
  
  # Set datadist for rms
  dd <- datadist(df)
  options(datadist = "dd")
  
  # Fit logistic regression with restricted cubic splines
  formula <- as.formula(paste("ui ~ rcs(bmi, 5) +", paste(covariates, collapse = " + ")))
  model <- lrm(formula, data = df, x = TRUE, y = TRUE)
  anova_model <- anova(model)
  print(anova_model)
  
  # Predict log odds over a BMI range
  pred <- Predict(model, bmi = seq(15, 45, length.out = 100), fun = NULL)
  
  # Reference BMI
  ref_bmi <- 25
  ref_logodds <- approx(pred$bmi, pred$yhat, xout = ref_bmi)$y
  
  # Convert to odds ratios
  pred$or <- exp(pred$yhat - ref_logodds)
  pred$lower_or <- exp(pred$lower - ref_logodds)
  pred$upper_or <- exp(pred$upper - ref_logodds)
  
  # Convert to clean data frame to prevent ggplot() from using yhat
  pred_df <- as.data.frame(pred)
  
  # Plot
  p <- ggplot(pred_df, aes(x = bmi, y = or)) +
    geom_line(color = ifelse(sex == "Male", "#008080", "#cc5500"), linewidth = 1) +
    geom_ribbon(aes(ymin = lower_or, ymax = upper_or),
                alpha = 0.3,
                fill = ifelse(sex == "Male", "#008080", "#cc5500")) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
         x = "BMI",
         y = paste("Odds Ratio (ref = BMI", ref_bmi, ")")) +
    theme_minimal(base_size = 16) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylim(0,4)
  
  # Save and show
  ggsave(paste0("UI_BMI_OR_last", sex, ".png"), p, dpi = 300, width = 10, height = 7)
  print(p)
}
