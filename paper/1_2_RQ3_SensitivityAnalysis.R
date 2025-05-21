######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 16/05/2024           ##
## Date last changed: 01/10/2024    ##
######################################

## DATA ANALYSIS

# Load packages

library(dplyr)
library(tidyverse)
library(janitor)
library(Hmisc)
library(pscl)
library(nnet)
library(WeightIt)
library(boot)
library(data.table)
library(cobalt)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

data <- readRDS("Data/preprocessed data/2024-10-10_RQ3_AllDat_Preprocessed.RDS")

# Functions

wtd.lci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  sd   <- sqrt(wtd.var(x, w))
  se   <- sd/sqrt(length(x))
  lci   <- mean + qt(0.025,n-1)*se 
  return(lci)
}

lci <- function(x) {
  n    <- length(x)
  mean <- mean(x)
  sd   <- sqrt(var(x))
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

uci <- function(x) {
  n    <- length(x)
  mean <- mean(x)
  sd   <- sqrt(var(x))
  se   <- sd/sqrt(length(x))
  lci   <- mean + qt(0.975,n-1)*se 
  return(lci)
}

# Layout for visualizations

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=14, color = "black"),
        strip.text = element_text(size=14),
        title = element_text(size=14),
        axis.title = element_text(size=14, color = "black"),
        legend.position = "right")

## ----------------------------------------------------------------
## PREPARE
## ----------------------------------------------------------------

# select data
data <- data %>% 
  mutate(sex = ifelse(sex == "male", 1,
                      ifelse(sex == "female", 0, NA)),
         emp.employed = ifelse(emp.type == "employed", 1, 0),
         emp.unemployed = ifelse(emp.type == "unemployed", 1, 0),
         emp.other = ifelse(emp.type == "other", 1, 0),
         aud2017 = ifelse(y.aud == 2017, 1, 0),
         aud2018 = ifelse(y.aud == 2018, 1, 0),
         aud2019 = ifelse(y.aud == 2019, 1, 0),
         aud2020 = ifelse(y.aud == 2020, 1, 0),
         treat = factor(treat, levels=c("qwt","inpat","reha")),
         hosp.FU.AUD.days = as.integer(hosp.FU.AUD.days),
         hosp.FU.days = as.integer(hosp.FU.days),
         inpat = factor(ifelse(inpat_num <= 1, "0/1 inpat", 
                               ifelse(inpat_num > 1, "2+ inpat", NA))),
         inpat2 = as.numeric(ifelse(inpat_num <= 1, 0, 
                                    ifelse(inpat_num > 1, 1, NA))),
         qwt_entl = factor(qwt_entl, labels = c("1" = "regulär",
                                                "2" = "beendet, nachstat. Beh. vorgesehen",
                                                "3" = "aus sonstigen Gründen beendet",
                                                "4" = "gegen ärztlichen Rat beendet",
                                                "6" = "Verlegung in ein anderes Krankenhaus",
                                                "9" = "Rehaeinrichtung",
                                                "10" = "Pflegeeinrichtung",
                                                "15" = "gegen ärztlichen Rat beendet, nachstat. Beh. vorgeseh.",
                                                "17" = "interne Verlegung m. Wechsel zw. D. Gelt.b. BPflV u. KHEntgG")),
         inpat_entl = factor(inpat_entl, labels = c("1" = "regulär",
                                                    "3" = "aus sonstigen Gründen beendet",
                                                    "4" = "gegen ärztlichen Rat beendet",
                                                    "6" = "Verlegung in ein anderes Krankenhaus",
                                                    "9" = "Rehaeinrichtung",
                                                    "14" = "aus sonst. Gründen beend., nachstat. Beh. Vorges.",
                                                    "17" = "interne Verlegung m. Wechsel zw. D. Gelt.b. BPflV u. KHEntgG",
                                                    "22" = "Fallabschl. (int. V.) b. Wechsel zw. Voll- und teilst. Beh."))) %>% 

# exclude cases with irregular discharge 
  mutate(select = case_when(
    treat %like% "qwt|reha" & qwt_entl %like% "regulär|Rehaeinrichtung|Verlegung" ~ 1,
    treat %like% "qwt|reha" & !qwt_entl %like% "regulär|Rehaeinrichtung|Verlegung" & !is.na(qwt_entl) ~ 0,
    treat == "inpat" & inpat_entl %like% "regulär|Rehaeinrichtung|Verlegung|Fallabschl" ~ 1,
    treat == "inpat" & !inpat_entl %like% "regulär|Rehaeinrichtung|Verlegung|Fallabschl" & !is.na(inpat_entl) ~ 0,
    TRUE ~ NA_real_)) 
table(data$treat, data$select) # excluded: INPAT = 83, QWT = 73, REHA = 10

select.data <- data %>% filter(select == 1) #new: INPAT = 223, QWT = 414, REHA = 91
table(select.data$inpat_entl, select.data$treat)
table(select.data$qwt_entl, select.data$treat)

## ----------------------------------------------------------------
## MATCHING WEIGHTS
## ----------------------------------------------------------------

table(select.data$sex, select.data$treat)
table(select.data$yob, select.data$treat)
table(select.data$y.aud, select.data$treat) # n = 0 in 2021, n = 6 in 2020 in reha
table(select.data$q.aud, select.data$treat)
table(select.data$cmb.PRE, select.data$treat)
table(select.data$emp.type, select.data$treat) # no retired in reha group

select.data <- select.data %>% 
  mutate(emp.type = ifelse(emp.type == "retired", "other", emp.type),
         y.aud = factor(y.aud, levels(droplevels(y.aud))))

# explore imbalance and relevance of covariates for matching
bal.tab(treat ~ sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, 
        data = select.data, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) 

# matching using entropy matching

m.data.ebal <- weightit(treat ~ sex + y.aud + q.aud + cmb.PRE + emp.type, data = select.data, 
                        method = "ebal", estimand = "ATE")
summary(m.data.ebal) # max. weight = 2.38, max. Entropy = .07
bal.tab(m.data.ebal, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # balanced

# match data
match.data <- cbind(select.data, weights = m.data.ebal$weights) 

## ----------------------------------------------------------------
## DESCRIPTIVES
## ----------------------------------------------------------------

wdesc <- match.data %>%
  group_by(treat) %>%
  summarise(age.aud.mean = wtd.mean(age.aud, weights), age.aud.std = sqrt(wtd.var(age.aud, weights)),
            sex.prop = wtd.mean(sex, weights), sex.lci = wtd.lci(sex, weights), sex.uci = wtd.uci(sex, weights),
            employed.prop = wtd.mean(emp.employed, weights), employed.lci = wtd.lci(emp.employed, weights), employed.uci = wtd.uci(emp.employed, weights),
            unemployed.prop = wtd.mean(emp.unemployed, weights), unemployed.lci = wtd.lci(emp.unemployed, weights), unemployed.uci = wtd.uci(emp.unemployed, weights),
            emp.oth.prop = wtd.mean(emp.other, weights), emp.oth.lci = wtd.lci(emp.other, weights), emp.oth.uci = wtd.uci(emp.other, weights),
            aud2017.prop = wtd.mean(aud2017, weights), aud2017.lci = wtd.lci(aud2017, weights), aud2017.uci = wtd.uci(aud2017, weights),
            aud2018.prop = wtd.mean(aud2018, weights), aud2018.lci = wtd.lci(aud2018, weights), aud2018.uci = wtd.uci(aud2018, weights),
            aud2019.prop = wtd.mean(aud2019, weights), aud2019.lci = wtd.lci(aud2019, weights), aud2019.uci = wtd.uci(aud2019, weights),
            aud2020.prop = wtd.mean(aud2020, weights), aud2020.lci = wtd.lci(aud2020, weights), aud2020.uci = wtd.uci(aud2020, weights),
            Elix30PRE.median = wtd.quantile(Elix30PRE, weights, probs = 0.5), 
            Elix30PRE.q1 = wtd.quantile(Elix30PRE, weights, probs = 0.25), Elix30PRE.q3 = wtd.quantile(Elix30PRE, weights, probs = 0.75),
            time.aud.treat.median = wtd.quantile(time.aud.treat, weights, probs = 0.5), 
            time.aud.treat.q1 = wtd.quantile(time.aud.treat, weights, probs = 0.25), time.aud.treat.q3 = wtd.quantile(time.aud.treat, weights, probs = 0.75),
            time.qwt.median = wtd.quantile(time.qwt, weights, probs = 0.5), 
            time.qwt.q1 = wtd.quantile(time.qwt, weights, probs = 0.25), time.qwt.q3 = wtd.quantile(time.qwt, weights, probs = 0.75),
            time.treat.median = wtd.quantile(time.treat, weights, probs = 0.5), 
            time.treat.q1 = wtd.quantile(time.treat, weights, probs = 0.25), time.treat.q3 = wtd.quantile(time.treat, weights, probs = 0.75),
            inpat.prop = wtd.mean(inpat2, weights), inpat.lci = wtd.lci(inpat2, weights), inpat.uci = wtd.uci(inpat2, weights),
            hosp.FU.times.median = wtd.quantile(hosp.FU.times, weights, probs = 0.5), 
            hosp.FU.times.q1 = wtd.quantile(hosp.FU.times, weights, probs = 0.25), hosp.FU.times.q3 = wtd.quantile(hosp.FU.times, weights, probs = 0.75),
            hosp.FU.days.median = wtd.quantile(hosp.FU.days, weights, probs = 0.5), 
            hosp.FU.days.q1 = wtd.quantile(hosp.FU.days, weights, probs = 0.25), hosp.FU.days.q3 = wtd.quantile(hosp.FU.days, weights, probs = 0.75),
            hosp.FU.AUD.times.median = wtd.quantile(hosp.FU.AUD.times, weights, probs = 0.5), 
            hosp.FU.AUD.times.q1 = wtd.quantile(hosp.FU.AUD.times, weights, probs = 0.25), hosp.FU.AUD.times.q3 = wtd.quantile(hosp.FU.AUD.times, weights, probs = 0.75),
            hosp.FU.AUD.days.median = wtd.quantile(hosp.FU.AUD.days, weights, probs = 0.5), 
            hosp.FU.AUD.days.q1 = wtd.quantile(hosp.FU.AUD.days, weights, probs = 0.25), hosp.FU.AUD.days.q3 = wtd.quantile(hosp.FU.AUD.days, weights, probs = 0.75)) %>%
  t %>% as.data.frame() %>% row_to_names(1) %>% `colnames<-`(c("QWT", "INPAT", "Reha"))

desc <- match.data %>%
  group_by(treat) %>%
  summarise(age.aud.mean = mean(age.aud), age.aud.std = sqrt(var(age.aud)),
            sex.prop = mean(sex), sex.lci = lci(sex), sex.uci = uci(sex),
            employed.prop = mean(emp.employed), employed.lci = lci(emp.employed), employed.uci = uci(emp.employed),
            unemployed.prop = mean(emp.unemployed), unemployed.lci = lci(emp.unemployed), unemployed.uci = uci(emp.unemployed),
            emp.oth.prop = mean(emp.other), emp.oth.lci = lci(emp.other), emp.oth.uci = uci(emp.other),
            aud2017.prop = mean(aud2017), aud2017.lci = lci(aud2017), aud2017.uci = uci(aud2017),
            aud2018.prop = mean(aud2018), aud2018.lci = lci(aud2018), aud2018.uci = uci(aud2018),
            aud2019.prop = mean(aud2019), aud2019.lci = lci(aud2019), aud2019.uci = uci(aud2019),
            aud2020.prop = mean(aud2020), aud2020.lci = lci(aud2020), aud2020.uci = uci(aud2020),
            Elix30PRE.median = quantile(Elix30PRE, probs = 0.5), 
            Elix30PRE.q1 = quantile(Elix30PRE, probs = 0.25), Elix30PRE.q3 = quantile(Elix30PRE, probs = 0.75),
            time.aud.treat.median = quantile(time.aud.treat, probs = 0.5), 
            time.aud.treat.q1 = quantile(time.aud.treat, probs = 0.25), time.aud.treat.q3 = quantile(time.aud.treat, probs = 0.75),
            time.qwt.median = quantile(time.qwt, probs = 0.5), 
            time.qwt.q1 = quantile(time.qwt, probs = 0.25), time.qwt.q3 = quantile(time.qwt, probs = 0.75),
            time.treat.median = quantile(time.treat, probs = 0.5), 
            time.treat.q1 = quantile(time.treat, probs = 0.25), time.treat.q3 = quantile(time.treat, probs = 0.75),
            inpat.prop = mean(inpat2), inpat.lci = lci(inpat2), inpat.uci = uci(inpat2),
            hosp.FU.times.median = quantile(hosp.FU.times, probs = 0.5), 
            hosp.FU.times.q1 = quantile(hosp.FU.times, probs = 0.25), hosp.FU.times.q3 = quantile(hosp.FU.times, probs = 0.75),
            hosp.FU.days.median = quantile(hosp.FU.days, probs = 0.5), 
            hosp.FU.days.q1 = quantile(hosp.FU.days, probs = 0.25), hosp.FU.days.q3 = quantile(hosp.FU.days, probs = 0.75),
            hosp.FU.AUD.times.median = quantile(hosp.FU.AUD.times, probs = 0.5), 
            hosp.FU.AUD.times.q1 = quantile(hosp.FU.AUD.times, probs = 0.25), hosp.FU.AUD.times.q3 = quantile(hosp.FU.AUD.times, probs = 0.75),
            hosp.FU.AUD.days.median = quantile(hosp.FU.AUD.days, probs = 0.5), 
            hosp.FU.AUD.days.q1 = quantile(hosp.FU.AUD.days, probs = 0.25), hosp.FU.AUD.days.q3 = quantile(hosp.FU.AUD.days, probs = 0.75)) %>%
  t %>% as.data.frame() %>% row_to_names(1) %>% `colnames<-`(c("QWT", "INPAT", "Reha"))

# Output Table 1, Table S1
#write.csv(wdesc, paste0("Output/Leitliniengerechte Versorgung/", Sys.Date(), "_TabS2_SUBSAMPLE_weighted.csv"))
#write.csv(desc, paste0("Output/Leitliniengerechte Versorgung/", Sys.Date(), "_TabS3_SUBSAMPLE_unweighted.csv"))

## ----------------------------------------------------------------
## TREATMENT COMPARISONS
## ----------------------------------------------------------------

# Correlation of treatment condition with covariates 

ggplot(match.data) + geom_boxplot(aes(x = as.factor(treat), y = time.aud.treat)) + theme_barplot
ggplot(match.data) + geom_boxplot(aes(x = as.factor(treat), y = time.treat)) + theme_barplot
ggplot(match.data, aes(x = inpat)) + geom_histogram(stat = "count") + facet_wrap(vars(treat))

# Outcome 1) days of hospitalizations within 1 year after first treatment 

ggplot(match.data, aes(x = hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of all-cause hospital days during 12-months follow-up") +
  xlab("Days") + theme_barplot

mHospDays <- zeroinfl(hosp.FU.days ~ treat + time.aud.treat + inpat, data = match.data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

# Outcome 2) days of AUD hospitalizations within 1 year after first treatment 

ggplot(match.data, aes(hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of alcohol-specific hospital days during 12-months follow-up") +
  xlab("Days") + theme_barplot

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat + time.aud.treat + inpat, data = match.data, 
                         dist = "negbin", weights = weights)
summary(mHospAUDDays)

# Confidence intervals using bootstrapping
# https://stats.oarc.ucla.edu/r/dae/zinb/
# https://www.r-bloggers.com/2019/09/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/

set.seed <- 1234

# Outcome 1) 

dput(round(coef(mHospDays, "count"), 4))
dput(round(coef(mHospDays, "zero"), 4))

# function for bootstrapping using original estimates as starting point
f_zinb <- function(data, i){
  require(pscl)
  m <- zeroinfl(hosp.FU.days ~ treat + time.aud.treat + inpat, data = data[i, ],
                dist = "negbin", weights = weights,
                start = list(count = c(3.4978, -0.2636, -0.1636, -0.0002, 0.2193), 
                             zero = c(0.1152, -0.0332, 0.615, -0.0035, -1.0334)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

# bootstrap
(res <- boot(match.data, f_zinb, R = 1500))

# get confidence intervals using bias-adjusted bootstrap percentile 
out_1 <- t(sapply(c(1, 3, 5, 7, 9, 13, 15, 17, 19, 21), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = "perc", h = exp)
  with(out, c(coef = t0, robust_lci = percent[4], robust_uci = percent[5]))
}))

row.names(out_1) <- names(coef(mHospDays))

# Output 2)

dput(round(coef(mHospAUDDays, "count"), 4))
dput(round(coef(mHospAUDDays, "zero"), 4))

f_zinb <- function(data, i){
  require(pscl)
  m <- zeroinfl(hosp.FU.AUD.days ~ treat + time.aud.treat + inpat, data = data[i, ],
                dist = "negbin", weights = weights,
                start = list(count = c(3.1645, -0.1587, 0.2138, -0.0005, 0.2475), 
                             zero = c(0.8407, 0.264, 0.7057, -0.0023, -1.1968)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

(res <- boot(match.data, f_zinb, R = 1500))

out_2 <- t(sapply(c(1, 3, 5, 7, 9, 13, 15, 17, 19, 21), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = "perc", h = exp)
  with(out, c(coef = t0, robust_lci = percent[4], robust_uci = percent[5]))
}))

row.names(out_2) <- names(coef(mHospAUDDays))

# Combine outputs

out <- rbind(
  cbind(group = "Hosp_AllCause_Days", out_1, 
        lci = exp(confint(mHospDays))[,1], uci = exp(confint(mHospDays)[,2])),
  cbind(group = "Hosp_AUD_Days", out_2,
        lci = exp(confint(mHospAUDDays))[,1], uci = exp(confint(mHospAUDDays)[,2]))) %>% 
  as.data.frame() %>% 
  mutate_at(c("coef", "lci", "uci", "robust_lci", "robust_uci"), ~round(as.numeric(.),3))

# Output Table S4
#write.csv(out, paste0("Output/Leitliniengerechte Versorgung/", Sys.Date(), "_TabS4_SUBSAMPLE_ZINB.csv"))