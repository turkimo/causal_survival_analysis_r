# Day 3 Practical 

# Load necessary library

library(survival)
library(dplyr)
options(scipen=999)

# Read the dataset 
data.recurrent <- read.csv("recurrent_dataset_final.csv", stringsAsFactors =TRUE)

data.recurrent <- data.recurrent %>%
  group_by(id) %>%
  mutate(cumulative_event = cumsum(event)) %>%
  ungroup() 

data.recurrent$gap <- data.recurrent$t.stop-data.recurrent$t.start

data.recurrent$time0 <- 0
# Conventional COX model
model.0 <- coxph(Surv(t.start,t.stop,status) ~ trt + sex+ age+ hospital_admissions,  method= "breslow", data = data.recurrent)
summary(model.0)

# Andersen-Gill (AG) model
model.1 <- coxph(Surv(t.start,t.stop,status) ~ trt + sex+ age+ hospital_admissions+cluster(id),  method= "breslow", robust=TRUE, data = data.recurrent)
summary(model.1)


# When specifying the argument robust = TRUE in the previous syntax, it produces an approximate jacknife estimate of the variance.  When the argument cluster(id) is added to the syntax (as showed for the marginal means model), it identifies the clusters of correlated observations and estimates the variance based on a grouped jacknife


# PWP-TT model
model.2 <- coxph(Surv(t.start,t.stop,status)~ trt + sex+ age+ hospital_admissions+cluster(id)+strata(event), method= "breslow",data=data.recurrent)
summary(model.2)

# PWP-GT model
model.3 <- coxph(Surv(time0, gap, status)~trt + sex+ age+ hospital_admissions+cluster(id)+strata(cumulative_event), method= "breslow",  data =data.recurrent)
summary(model.3)

# For the stratified models, the argument strata(event) identifies stratification variable to obtain their estimates. 

# Frailty models
model.frailty <- coxph(Surv(t.start, t.stop, status) ~ trt + sex+ age+ hospital_admissions + frailty(id), data=data.recurrent)
summary(model.frailty)





