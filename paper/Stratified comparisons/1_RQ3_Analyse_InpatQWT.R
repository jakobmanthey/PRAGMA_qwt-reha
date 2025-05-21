######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 16/05/2024           ##
## Date last changed: 11/06/2024    ##
######################################

# Load packages

library(dplyr)
library(tidyverse)
library(janitor)
library(sandwich)
library(Hmisc)
library(pscl)
library(lmtest)
library(spatstat) # weighted median&IQR

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")
DATE <- 20240613

# Load data

match.data <- read.csv("Data/preprocessed data/20240611_RQ3_Inpat-QWT_MatchedData.csv")

# Functions

wtd.lci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  sd   <- sqrt(wtd.var(x, w))
  se   <- sd/sqrt(length(x))
  lci   <- mean + qt(0.025,n-1)*se 
  return(lci)
}

wtd.uci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  sd   <- sqrt(wtd.var(x, w))
  se   <- sd/sqrt(length(x))
  uci   <- mean + qt(0.975,n-1)*se 
  return(uci)
}

# Layout for visualizations
theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=14, color = "black"),
        strip.text = element_text(size=14),
        title = element_text(size=14),
        axis.title = element_text(size=14, color = "black"),
        legend.position = "none")

## ----------------------------------------------------------------
## PREAPRE
## ----------------------------------------------------------------

data <- match.data %>% 
  mutate(age.aud = as.numeric(substr(date.aud, 1, 4)) - yob,
         sex = ifelse(sex == "male", 1,
                      ifelse(sex == "female", 0, NA)),
         aud2017 = ifelse(y.aud == 2017, 1, 0),
         aud2018 = ifelse(y.aud == 2018, 1, 0),
         aud2019 = ifelse(y.aud == 2019, 1, 0),
         aud2020 = ifelse(y.aud == 2020, 1, 0),
         time.aud.treat = case_when(
           qwt == 0 ~ as.numeric(as.Date(date.inpat.start) - as.Date(date.aud)),
           qwt == 1 ~ as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)),
           TRUE ~ NA), 
         time.treat = case_when(
           qwt == 0 ~ as.numeric(as.Date(date.inpat.end) - as.Date(date.inpat.start)),
           qwt == 1 ~ as.numeric(as.Date(date.qwt.end) - as.Date(date.qwt.start)),
           TRUE ~ NA)) 

## ----------------------------------------------------------------
## DESCRIPTIVES
## ----------------------------------------------------------------

desc <- data %>%
  group_by(qwt) %>%
  summarise(age.aud.mean = wtd.mean(age.aud, weights), age.aud.std = sqrt(wtd.var(age.aud, weights)),
            sex.prop = wtd.mean(sex, weights), sex.lci = wtd.lci(sex, weights), sex.uci = wtd.uci(sex, weights),
            aud2017.prop = wtd.mean(aud2017, weights), aud2017.lci = wtd.lci(aud2017, weights), aud2017.uci = wtd.uci(aud2017, weights),
            aud2018.prop = wtd.mean(aud2018, weights), aud2018.lci = wtd.lci(aud2018, weights), aud2018.uci = wtd.uci(aud2018, weights),
            aud2019.prop = wtd.mean(aud2019, weights), aud2019.lci = wtd.lci(aud2019, weights), aud2019.uci = wtd.uci(aud2019, weights),
            aud2020.prop = wtd.mean(aud2020, weights), aud2020.lci = wtd.lci(aud2020, weights), aud2020.uci = wtd.uci(aud2020, weights),
            Elix30PRE.median = weighted.median(Elix30PRE, weights), 
            Elix30PRE.iqr1 = weighted.quantile(Elix30PRE, weights, probs = 0.25),
            Elix30PRE.iqr3 = weighted.quantile(Elix30PRE, weights, probs = 0.75),
            time.aud.treat.median = weighted.median(time.aud.treat, weights), 
            time.aud.treat.iqr1 = weighted.quantile(time.aud.treat, weights, probs = 0.25),
            time.aud.treat.iqr3 = weighted.quantile(time.aud.treat, weights, probs = 0.75),
            time.treat.median = weighted.median(time.treat, weights), 
            time.treat.iqr1 = weighted.quantile(time.treat, weights, probs = 0.25),
            time.treat.iqr3 = weighted.quantile(time.treat, weights, probs = 0.75),
            hosp.FU.times.median = weighted.median(hosp.FU.times, weights), 
            hosp.FU.times.iqr1 = weighted.quantile(hosp.FU.times, weights, probs = 0.25),
            hosp.FU.times.iqr3 = weighted.quantile(hosp.FU.times, weights, probs = 0.75),
            hosp.FU.days.mean = weighted.median(hosp.FU.days, weights), 
            hosp.FU.days.iqr1 = weighted.quantile(hosp.FU.days, weights, probs = 0.25),
            hosp.FU.days.iqr3 = weighted.quantile(hosp.FU.days, weights, probs = 0.75),
            hosp.FU.AUD.times.mean = weighted.median(hosp.FU.AUD.times, weights), 
            hosp.FU.AUD.times.iqr1 = weighted.quantile(hosp.FU.AUD.times, weights, probs = 0.25),
            hosp.FU.AUD.times.iqr3 = weighted.quantile(hosp.FU.AUD.times, weights, probs = 0.75),
            hosp.FU.AUD.days.mean = weighted.median(hosp.FU.AUD.days, weights), 
            hosp.FU.AUD.days.iqr1 = weighted.quantile(hosp.FU.AUD.days, weights, probs = 0.25),
            hosp.FU.AUD.days.iqr3 = weighted.quantile(hosp.FU.AUD.days, weights, probs = 0.75)) %>%
  t %>% as.data.frame() %>% row_to_names(1) %>% `colnames<-`(c("Inpat", "QWT"))
  
# Output Table 1
#write.csv(desc, paste0("Output/", DATE, "_Tab1_Inpat-QWT.csv"))

## ----------------------------------------------------------------
## GROUP COMPARISONS
## ----------------------------------------------------------------

# Outcome 1) days of hospitalizations within 1 year after first treatment 

ggplot(data, aes(hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of all-cause hospital days during 12 months follow up") +
  xlab("Days") + theme_barplot
#ggsave(paste0("Output/", DATE, "_FigA1_Inpat-QWT_HospDays.jpg"), dpi=300, width = 10, height = 8)

mHospDays <- zeroinfl(hosp.FU.days ~ as.factor(qwt) + time.aud.treat + time.treat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

#mHospDays <- glm(hosp.FU.days ~ as.factor(qwt), data = data, weights = weights)
#summary(mHospDays)

out_1 <- coeftest(mHospDays, vcov. = vcovCL, cluster = ~subclass)

# Outcome 2) number of hospitalizations within 1 year after first treatment 

hist(data$hosp.FU.times, breaks = 100) 

mHospTimes <- zeroinfl(hosp.FU.times ~ as.factor(qwt) + time.aud.treat + time.treat, data = data, 
                        dist = "negbin", weights = weights)
summary(mHospTimes)

out_2 <- coeftest(mHospTimes, vcov. = vcovCL, cluster = ~subclass)

# Outcome 3) days of AUD hospitalizations within 1 year after first treatment 

ggplot(data, aes(hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of alcohol-specific hospital days during 12 months follow up") +
  xlab("Days") + theme_barplot
#ggsave(paste0("Output/", DATE, "_FigA1_Inpat-QWT_HospAUDDays.jpg"), dpi=300, width = 10, height = 8)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ as.factor(qwt) + time.aud.treat + time.treat, data = data, 
                         dist = "negbin", weights = weights)
summary(mHospAUDDays)

out_3 <- coeftest(mHospAUDDays, vcov. = vcovCL, cluster = ~subclass)

# Outcome 4) number of AUD hospitalizations within 1 year after first treatment 

hist(data$hosp.FU.AUD.times, breaks = 100) 

mHospAUDTimes <- zeroinfl(hosp.FU.AUD.times ~ as.factor(qwt) + time.aud.treat + time.treat, data = data, 
                          dist = "negbin", weights = weights)
summary(mHospAUDTimes)

out_4 <- coeftest(mHospAUDTimes, vcov. = vcovCL, cluster = ~subclass)

# Output Table 2
out <- rbind(
  cbind(var = "Hosp_AllCause_Days/QWT/count", 
        coef = exp(out_1[2]), lci = exp(confint(out_1)[2]), uci = exp(confint(out_1)[10]), pval = out_1[26]),
  cbind(var = "Hosp_AllCause_Days/AUDToTreatment/count",
        coef = exp(out_1[3]), lci = exp(confint(out_1)[3]), uci = exp(confint(out_1)[11]), pval = out_1[27]),
  cbind(var = "Hosp_AllCause_Days/TimeTreatment/count", 
        coef = exp(out_1[4]), lci = exp(confint(out_1)[4]), uci = exp(confint(out_1)[12]), pval = out_1[28]),
  cbind(var = "Hosp_AllCause_Days/QWT/zero", 
        coef = exp(out_1[6]), lci = exp(confint(out_1)[6]), uci = exp(confint(out_1)[14]), pval = out_1[30]),
  cbind(var = "Hosp_AllCause_Days/AUDToTreatment/zero", 
        coef = exp(out_1[7]), lci = exp(confint(out_1)[7]), uci = exp(confint(out_1)[15]), pval = out_1[31]),
  cbind(var = "Hosp_AllCause_Days/TimeTreatment/zero", 
        coef = exp(out_1[8]), lci = exp(confint(out_1)[8]), uci = exp(confint(out_1)[16]), pval = out_1[32]),
  cbind(var = "Hosp_AUDCause_Days/QWT/count", 
        coef = exp(out_3[2]), lci = exp(confint(out_3)[2]), uci = exp(confint(out_3)[10]), pval = out_3[26]),
  cbind(var = "Hosp_AUDCause_Days/AUDToTreatment/count",
        coef = exp(out_3[3]), lci = exp(confint(out_3)[3]), uci = exp(confint(out_3)[11]), pval = out_3[27]),
  cbind(var = "Hosp_AUDCause_Days/TimeTreatment/count", 
        coef = exp(out_3[4]), lci = exp(confint(out_3)[4]), uci = exp(confint(out_3)[12]), pval = out_3[28]),
  cbind(var = "Hosp_AUDCause_Days/QWT/zero", 
        coef = exp(out_3[6]), lci = exp(confint(out_3)[6]), uci = exp(confint(out_3)[14]), pval = out_3[30]),
  cbind(var = "Hosp_AUDCause_Days/AUDToTreatment/zero", 
        coef = exp(out_3[7]), lci = exp(confint(out_3)[7]), uci = exp(confint(out_3)[15]), pval = out_3[31]),
  cbind(var = "Hosp_AUDCause_Days/TimeTreatment/zero", 
        coef = exp(out_3[8]), lci = exp(confint(out_3)[8]), uci = exp(confint(out_3)[16]), pval = out_3[32])) %>% 
  as.data.frame() %>% column_to_rownames(var = "var") %>%
  mutate_at(c("coef", "lci", "uci"), ~round(as.numeric(.),3)) %>% 
  mutate(pval = round(as.numeric(pval), 3))

# Output Table 2
#write.csv(out, paste0("Output/", DATE, "_Tab2_Inpat-QWT.csv"))