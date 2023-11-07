# Day 3 Practical 
library(survival)
library(survminer)
library(KMunicate)
library(dplyr)
library(autoReg)

# Read the dataset 
data <- read.csv("/Users/turkimohammed/Documents/GitHub/causal_survival_sim/dataset.csv", stringsAsFactors = TRUE)


# summary of dataset
gaze(data) %>% myft()

# summary of dataset by Treatment 
gaze(Treatment~.,data=dataset) %>% myft()

gaze(status~.,data=dataset) %>% myft()


# Kaplan-Meier survival estimate

fit1 <- survfit(Surv(eventtime,status)~ Treatment,data=data)
fit1

# Plot Kaplan-Meier survival curves

ggsurvplot(
  fit1,
  data = data,
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Time (in years)",
  ylab = "Survival probability",
  ggtheme = theme_minimal()
)

# Plot Kaplan-Meier survival curves by KMunicate

KMunicate(fit1, time_scale = seq(0,5,by= 1))

# Assign the reference 
data$Treatment
data$Treatment <- relevel(data$Treatment, ref = "Control")
# fit the cox model 

fit2 <- coxph(Surv(eventtime,status)~Treatment+gregion+ miexhep+alpha_feto+ ecog_perf,data=data)
fit2

# try to fit the cox model with other covariate  


# Testing proportional Hazards assumption

test.ph <- cox.zph(fit2)
test.ph

# scaled Schoenfeld residuals

ggcoxzph(test.ph)

# Testing influential observations

ggcoxdiagnostics(fit2,type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

# Create a new data frame for predictions
# This would typically include different levels of covariates for which you want to plot survival curves
newdata <- with(data, expand.grid(Treatment = unique(Treatment), gregion = unique(gregion), miexhep = unique(miexhep), alpha_feto = unique(alpha_feto), ecog_perf = unique(ecog_perf)))

# Predict survival using the fitted model
surv_fit <- survfit(fit2, newdata = newdata)

fit2 <- coxph(Surv(eventtime,status)~Treatment+gregion+ miexhep+alpha_feto+ ecog_perf,data=data)

# Plotting the survival curves
ggadjustedcurves(
  fit2,
  variable = "Treatment",
  data = data,
  palette = "hue",
  ylab = "Survival probability",
  size = 1,
  ggtheme = theme_survminer(),)

