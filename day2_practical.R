# Day 3 Practical 
library(survival)
library(survminer)
library(KMunicate)


# Read the dataset 
data <- read.csv("dataset.csv", stringsAsFactors =TRUE)

# How many events occurred? 

table(data$status)

# Now do it for each arm


# What is the median survival time? 

summary(data$time)

# Now do it for each arm


# Kaplan-Meier survival estimate
surv_obj <- Surv(data$eventtime, data$status)
km_fit <- survfit(surv_obj ~ Treatment, data = data)

# Plot Kaplan-Meier survival curves
ggsurvplot(
  km_fit,
  data = data,
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Time (in years)",
  ylab = "Survival probability",
  ggtheme = theme_minimal()
)

# Now use KMunicate 
KMunicate(km_fit, time_scale = seq(0,5,by=1))


# Log-rank test to compare the survival curves
log_rank_test <- survdiff(surv_obj ~ Treatment, data = data)
print(log_rank_test)


# Now compare the survival experience among males and females 
data$ChildPugh
surv_obj1 <- Surv(data$eventtime, data$status)
km_fit1 <- survfit(surv_obj ~ hc_cause, data = data)
KMunicate(km_fit1, time_scale = seq(0,5,by=1))

str(data)


# Now compare the survival experience different causes of Hepatocellular carcinoma 
data$ChildPugh
surv_obj1 <- Surv(data$eventtime, data$status)
km_fit1 <- survfit(surv_obj ~ hc_cause, data = data)
KMunicate(km_fit1, time_scale = seq(0,5,by=1))

# Cumulative hazard 

# Create the survival object
surv_obj <- Surv(data$eventtime, data$status)

# Calculate the Nelson-Aalen estimate
na_fit <- survfit(surv_obj ~ 1, type='fleming-harrington')

# Print the Nelson-Aalen cumulative hazard estimates
na_fit

# Plot the Nelson-Aalen cumulative hazard function
plot(na_fit, fun='cumhaz', xlab='Time', ylab='Cumulative hazard',
     main='Nelson-Aalen Cumulative Hazard Function')

# If you wish to compare groups, make sure the 'group' variable is a factor
na_fit_group <- survfit(surv_obj ~ Treatment, data = data, type='fleming-harrington')

# Print the Nelson-Aalen cumulative hazard estimates for each group
na_fit_group

# Plot the Nelson-Aalen cumulative hazard function for each group
plot(na_fit_group, fun='cumhaz', col=c(1,2), lty=1:2, xlab='Time', ylab='Cumulative hazard',
     main='Nelson-Aalen Cumulative Hazard Function by Group')+
legend('bottomright', legend=levels(data$TreatmentT), col=c(1,2), lty=1:2)
