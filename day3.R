# Day 3 Practical 
library(survival)
library(survminer)
library(KMunicate)
library(dplyr)
library(autoReg)

# Read the dataset 
data <- read.csv("C:/Users/Shaher/Downloads/dataset.csv", stringsAsFactors = TRUE)


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

