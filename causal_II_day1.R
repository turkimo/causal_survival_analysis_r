# Day 1 Practical 

# Load necessary library
library(survival)
library(survminer)
library(KMunicate)

options(scipen=999)

# Read the dataset 
data <- read.csv("dataset.csv", stringsAsFactors =TRUE)

# Convert 'Treatment' to a numeric variable if it is not already
data$TreatmentNumeric <- ifelse(data$Treatment == 'Treatment', 1, 0)

data_split <- survSplit(Surv(eventtime, status) ~ ., data = data, cut = 0:(max(data$eventtime)), episode = "year")



# Fit separate Cox models for each year and extract hazard ratios
hazard_ratios <- list()
for (current_year in unique(data_split$year)) {
  # Subset data for the specific current_year
  year_data <- subset(data_split, year == current_year)
  
  # Fit the Cox model for the specific current_year
  cox_model <- coxph(Surv(eventtime, status) ~ TreatmentNumeric, data = year_data)
  
  # Store the hazard ratio for TreatmentNumeric
  hazard_ratios[[as.character(current_year)]] <- exp(coef(cox_model)["TreatmentNumeric"])
}

# Display the hazard ratios for each year
hazard_ratios

km_fit <- survfit(Surv(eventtime, status) ~ Treatment, data=data)

KMunicate(km_fit, time_scale = seq(0,5,by=1))
