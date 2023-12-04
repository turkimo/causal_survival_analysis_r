# Day 2 Practical 

# Load necessary library
install.packages("splitstackshape")
library(splitstackshape)
library(survival)
library(KMunicate)
library(dplyr)
options(scipen=999)

# Read the dataset 
data <- read.csv("dataset.csv", stringsAsFactors =TRUE)

# Convert 'Treatment' to a numeric variable if it is not already
data$trt <- ifelse(data$Treatment == 'Treatment', 1, 0)

# Transform the time variable from years to weeks 

data$eventtime_weeks <- data$eventtime * 52.177457
data$eventtime_weeks <- ceiling(data$eventtime_weeks)

# Expand data for each day to a row per week 
data.surv <- expandRows(data, "eventtime_weeks", drop=F) 
data.surv$time <- sequence(rle(data.surv$id)$lengths)-1
data.surv$event <- ifelse(data.surv$time==data.surv$eventtime_weeks-1 & 
                             data.surv$status==1, 1, 0)
data.surv$timesq <- data.surv$time^2

# fit of parametric hazards model
hazards.model <- glm(event==0 ~ trt + I(trt*time) + I(trt*timesq) + time + timesq, family=binomial(), data=data.surv)
summary(hazards.model)

#Create datasets with all time points under each treatment level

trt0 <- data.frame(cbind(seq(0, 260),0,(seq(0, 260))^2))
trt1 <- data.frame(cbind(seq(0, 260),1,(seq(0, 260))^2))

colnames(trt0) <- c("time", "trt", "timesq")
colnames(trt1) <- c("time", "trt", "timesq")

# assignment of estimated (1-hazard) to each person-week */
trt0$p.noevent0 <- predict(hazards.model, trt0, type="response")
trt1$p.noevent1 <- predict(hazards.model, trt1, type="response")

# computation of survival for each person-week
trt0$surv0 <- cumprod(trt0$p.noevent0)
trt1$surv1 <- cumprod(trt1$p.noevent1)

#merge both datasets together
hazards.graph <- merge(trt0, trt1, by=c("time", "timesq"))

#create the difference in survival
hazards.graph$survdiff <- hazards.graph$surv1 - hazards.graph$surv0

hazards.graph <- hazards.graph %>%
  arrange(time)

surv0 <- hazards.graph$surv0[260]
surv1 <- hazards.graph$surv1[260]
Y_0 <- 1-surv0
Y_1 <- 1-surv1
riskdiff <- Y_1 - Y_0
riskratio <- Y_1/Y_0

# plot
ggplot(hazards.graph, aes(x=time, y=surv)) + 
  geom_line(aes(y = surv0, colour = "0")) + 
  geom_line(aes(y = surv1, colour = "1")) + 
  xlab("Weeks") + 
  scale_x_continuous(limits = c(0, 261), breaks=seq(0,261,52)) +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  ylab("Survival") + 
  ggtitle("Survival from hazards model") + 
  labs(colour="A:") +
  theme_bw() + 
  theme(legend.position="bottom")

# Bootstrapping 

results <- NULL
set.seed(2023)

# Number of bootstraps 
numboot <- 500

for (i in 1:numboot) {
  index <- sample(1:nrow(data), nrow(data), replace=T)
  bootstrap_dat <- data[index, ]

  # Expand data for each day to a row per week 
  data.surv.boot <- bootstrap_dat %>% expandRows( "eventtime_weeks", drop=F) %>% mutate( time = sequence(rle(id)$lengths)-1) %>% mutate(event = ifelse(time== eventtime_weeks-1 & status==1, 1, 0)) %>% mutate(timesq = time^2) 
  
  # fit of parametric hazards model
  hazards.model.boot <- glm(event==0 ~ trt + I(trt*time) + I(trt*timesq) + time + timesq, family=binomial(), data=data.surv.boot)

  #Create datasets with all time points under each treatment level
  
  trt0.boot <- data.frame(cbind(seq(0, 260),0,(seq(0, 260))^2))
  trt1.boot <- data.frame(cbind(seq(0, 260),1,(seq(0, 260))^2))
  
  colnames(trt0.boot) <- c("time", "trt", "timesq")
  colnames(trt1.boot) <- c("time", "trt", "timesq")
  
  # assignment of estimated (1-hazard) to each person-week */
  trt0.boot$p.noevent0 <- predict(hazards.model.boot, trt0.boot, type="response")
  trt1.boot$p.noevent1 <- predict(hazards.model.boot, trt1.boot, type="response")
  
  # computation of survival for each person-week
  trt0.boot$surv0 <- cumprod(trt0.boot$p.noevent0)
  trt1.boot$surv1 <- cumprod(trt1.boot$p.noevent1)
  
  #merge both datasets together
  hazards.boot0 <- merge(trt0.boot, trt1.boot, by=c("time", "timesq"))

  hazards.boot <- hazards.boot0 %>%
    filter(time==260) %>%
    mutate(risk0 = 1-surv0) %>%
    mutate(risk1 = 1-surv1) %>%
    mutate(riskdiff = risk1-risk0) %>%
    mutate(logriskratio = log(risk1/risk0)) %>%
    select(risk0,risk1,riskdiff, logriskratio)
  
  results <- rbind(results, cbind(hazards.boot$risk0, hazards.boot$risk1, hazards.boot$riskdiff, hazards.boot$logriskratio))
  
}

results

# SE for bootstrap samples  
res_se <- apply(results,2,sd)

lcldiff <- riskdiff - 1.96*res_se[3]
ucldiff <- riskdiff + 1.96*res_se[3]

lclratio <- exp(log(riskratio) - 1.96*res_se[4])
uclratio <- exp(log(riskratio) + 1.96*res_se[4])

result1 <- cbind(paste0("death"), "Risk difference", paste0(format(round(riskdiff*100,1), nsmall=1), 
                                                     " (" , format(round(lcldiff*100,1), nsmall=1), "," ,
                                                     format(round(ucldiff*100,1), nsmall=1), ")" ))
result2 <- cbind(paste0("death"), "Risk ratio", paste0(format(round(riskratio,2), nsmall=2), 
                                                     " (" , format(round(lclratio,2), nsmall=2), "," ,
                                                     format(round(uclratio,2), nsmall=2), ")" ))
results <- rbind(result1, result2)

# Organise final results data frame
results <- results %>%
  rename(outcome=V1, measure=V2, effect=V3) %>%
  spread(measure, effect) %>%
  select(outcome, "RD", "RR") %>%
  rename("Outcome" = "outcome",
         "Risk difference (95% CI)"="RD", 
         "Risk ratio (95% CI)"="RR") 

results
# Compare it to cox estimate 
coxph(Surv(eventtime, status) ~ trt, data = data) %>% summary()
km_fit <- survfit(Surv(eventtime_weeks, status) ~ Treatment, data=data)

# Compare it to the KM graph 
KMunicate(km_fit, time_scale = seq(0,261,by=52))

