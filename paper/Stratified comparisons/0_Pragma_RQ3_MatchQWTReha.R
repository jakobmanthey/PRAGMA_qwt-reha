######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 07/05/2024           ##
## Date last changed: 10/06/2024    ##
######################################

# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(RItools)
library(Hmisc)
library(MatchIt)
library(comorbidity)
library(ggplot2)
library(data.table)
library(janitor)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")
DATE <- 20240613

# Load data

datQWT <- read.csv("Data/raw data/data_qwt_2024-05-28.csv")
datREHA <- read.csv("Data/raw data/data_reha_2024-05-28.csv")
datOthCond <- readRDS("Data/raw data/1_data_all diagnoses_2024-05-28.rds") 
datEMP <- readRDS("Data/raw data/1_data_employment periods_2024-05-28.rds")

## ----------------------------------------------------------------
## FILTER ORIGINAL DATA FILES
## ----------------------------------------------------------------

## 1) QWT

# n of people with QWT in same quarter of diagnosis or anytime later 
length(unique(datQWT$pragmaid))

QWT <- datQWT %>% 
  mutate(date.aud = as.Date(date.aud),
         date.qwt.start = as.Date(date.qwt.start),
         date.qwt.end = as.Date(date.qwt.end)) %>%
  # AUD diagnosis before QWT starts 
  filter(date.aud <= date.qwt.start) %>% 
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  filter(date.aud.12mPOST > date.qwt.end) %>% 
  # summarise total number of QWTs within 365 days since AUD diagnosis
  group_by(pragmaid) %>%
  mutate(qwt_num = sum(qwt_id)) %>%
  filter(qwt_id == 1)
length(unique(QWT$pragmaid)) # 672

## 2) REHA

# n of people with REHA in same quarter of diagnosis or anytime later 
length(unique(datREHA$pragmaid))

REHA <- datREHA %>% 
  mutate(date.aud = as.Date(date.aud),
         date.reha.start = as.Date(date.reha.start),
         date.reha.end = as.Date(date.reha.end)) %>%
  #filter(date.aud <= date.reha.start) %>%
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # filter REHA needs to start 12 months since AUD diagnosis 
  filter(date.aud.12mPOST > date.reha.start) %>%
  filter(reha_id == 1)

length(unique(REHA$pragmaid)) # 304

## ----------------------------------------------------------------
## MERGE DATA, INCLUDING EMPLOYMENT, ELIXHAUSER & HOSPITALIZATION
## ----------------------------------------------------------------

# combine REHA and QWT data files 

temp <- merge(REHA, QWT, by = c("gkv", "pragmaid", "date.aud", "sex", "yob", 
                                "date.ins.start", "date.ins.end", 
                                "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  mutate(reha = ifelse(!is.na(date.qwt.start) & !is.na(date.reha.start), 1,
                       ifelse(!is.na(date.qwt.start) & is.na(date.reha.start), 0, NA))) %>%
  filter(!is.na(reha)) %>% # n = 153 IDs with Reha but no QWT
  # get covariate for year and Q of diagnosis
  mutate(y.aud = as.factor(str_sub(date.aud, 1, 4)),
         q.aud = as.factor(ifelse(as.factor(str_sub(date.aud, 6, 7)) == "01", "Q1", 
                                  ifelse(as.factor(str_sub(date.aud, 6, 7)) == "04", "Q2", 
                                         ifelse(as.factor(str_sub(date.aud, 6, 7)) == "07", "Q3", 
                                                ifelse(as.factor(str_sub(date.aud, 6, 7)) == "10", "Q4", NA))))),
         n.qwt = as.factor(ifelse(qwt_num == 1, "1 QWT", 
                                ifelse(qwt_num > 1, "2+ QWTs", NA))),
         sex = as.factor(sex),
         gkv = as.factor(gkv))
table(temp$reha) #527 QWT, 145 Reha

## 3) EMPLOYMENT

# get employment information

EMP <- datEMP %>%
  mutate(date.emp.start = as.Date(date.emp.start),
         date.emp.end = as.Date(date.emp.end)) %>%
  left_join(temp, .) %>%
  # filter for employment overlapping with 12-month period prior to AUD diagnosis 
  filter(date.emp.end > date.aud.12mPRE & date.emp.start < (date.aud + ddays(1))) %>%
  mutate(date.emp.start.new = case_when(
    date.emp.start < date.aud.12mPRE ~ as.Date(date.aud.12mPRE + ddays(1)),
    date.emp.start > date.aud.12mPRE ~ as.Date(date.emp.start), 
    date.emp.start == date.aud.12mPRE ~ as.Date(date.emp.start), 
    TRUE ~ NA), 
    date.emp.end.new = case_when(
      date.emp.end < date.aud ~ as.Date(date.emp.end),
      date.emp.end > date.aud ~ as.Date(date.aud), 
      date.emp.end == date.aud ~ as.Date(date.emp.end), 
      TRUE ~ NA)) %>% 
  # get number of days in each employment group
  group_by(pragmaid, emp.type) %>%
  summarise(emp.days = sum(date.emp.end.new - date.emp.start.new),
            date.aud = mean(date.aud)) %>%
  # select employment status of longest duration 
  group_by(pragmaid) %>%
  filter(emp.days == max(emp.days)) %>%
  mutate(n = n(),
         select = case_when(
           n == 1 ~ 1,
           n > 1 & emp.type == "unemployed" ~ 1,
           n > 1 & emp.type == "retired" ~ 1,
           n > 1 & emp.type == "employed" ~ 0,
           n > 1 & emp.type == "other" ~ 0,
           TRUE ~ NA)) %>%
  filter(select == 1)

# combine with main file

tempEmp <- merge(temp, EMP, all.x = T, all.y = F) 

table(tempEmp$emp.type)
  # employed: 253, unemployed: 245, other: 127, retired: 45
  
## 4) ELIXHAUSER

# get Elixhauser index 

ELIX <- datOthCond %>%
  filter(icd_type %like% "confirmed|primary|secondary|any") %>%
  merge(temp, ., by = c("gkv", "pragmaid"), all.x = T) %>%
  mutate(date.diag.start = as.Date(date.diag.start)) %>%
  filter(date.diag.start > date.aud.12mPRE & date.diag.start < (date.aud + ddays(1))) %>%
  dplyr::select(c("pragmaid", "icd")) %>% 
  comorbidity::comorbidity(x = ., id = "pragmaid", code = "icd",
                           map = "elixhauser_icd10_quan", assign0 = T, tidy.codes = T) %>%
  dplyr::select(-alcohol) %>%
  rowwise() %>% 
  mutate(Elix30PRE = sum(c_across(2:31)),
         Elix27PRE = sum(c_across(2:28))) %>%
  dplyr::select(c("pragmaid", "Elix30PRE", "Elix27PRE"))

# combine with main file

tempEmpElix <- merge(tempEmp, ELIX, all.x = T) %>%
  mutate(Elix30PRE = ifelse(is.na(Elix30PRE), 0, Elix30PRE),
         cmb.PRE = factor(ifelse(Elix30PRE <= 1 | is.na(Elix30PRE), "0/1 comorbidity", 
                             # set comorbidity = 0 for those without recorded comorbidity data
                             ifelse(Elix30PRE >= 2, "2+ comorbidities", NA)),
                      levels = c("0/1 comorbidity", "2+ comorbidities")))

table(tempEmpElix$Elix30PRE) # go with Elix30, differentiate between 0/1 and 2+ comorbidity
table(tempEmpElix$Elix27PRE) 

## 5) HOSPITALIZATION

# get hospitalization

HOSP_temp <- merge(temp, datOthCond, by = c("gkv", "pragmaid"), all.x = T) %>% 
  filter(setting %like% "inpat") %>% 
  # define FU periods
  mutate(date.diag.start = as.Date(date.diag.start),
         date.FU.start = case_when(
           reha == 0 ~ date.qwt.end + ddays(1),
           reha == 1 ~ date.reha.end + ddays(1),
           TRUE ~ NA),
         date.FU.end = case_when(
           reha == 0 ~ date.qwt.end + dyears(1) + ddays(1),
           reha == 1 ~ date.reha.end + dyears(1) + ddays(1),
           TRUE ~ NA)) %>%
  # filter out all with end FU after 12/2021
  filter(date.FU.end < "2022-01-01") %>% 
  # filter out all conditions not occurring during FU
  filter(date.diag.start > date.FU.start & date.diag.start < date.FU.end) 

HOSP_ALL <- HOSP_temp %>% 
  # get number of hospitalizations during FU
  group_by(pragmaid, date.diag.start, date.diag.end) %>%
  summarise(icd.FU = paste0("[", paste0(icd, collapse = ","), "]"),
            hosp.FU.days = mean(as.numeric(as.Date(date.diag.end) - as.Date(date.diag.start)))) %>%
  group_by(pragmaid) %>%
  summarise(hosp.FU.times = n(),
            hosp.FU.days = sum(hosp.FU.days),
            icd.FU = paste0(icd.FU, collapse = ","))

HOSP_AUD <- HOSP_temp %>% 
  #filter(icd.alc == T) %>%
  dplyr::filter(icd %like% "E24.4|F10|G31.2|G62.1|G72.1|I42.6|K29.2|K70.0|K70.1|K70.2|K70.3|K70.4|K70.9|K85.2|K86.0|R78.0|X45|X65|Y15|Y90|Y91") %>%
  # get number of hospitalizations during FU
  group_by(pragmaid, date.diag.start, date.diag.end) %>%
  summarise(icd.FU = paste0("[", paste0(icd, collapse = ","), "]"),
            hosp.FU.AUD.days = mean(as.numeric(as.Date(date.diag.end) - as.Date(date.diag.start)))) %>%
  group_by(pragmaid) %>%
  summarise(hosp.FU.AUD.times = n(),
            hosp.FU.AUD.days = sum(hosp.FU.AUD.days),
            icd.FU.AUD = paste0(icd.FU, collapse = ","))

HOSP <- merge(HOSP_ALL, HOSP_AUD, all = T)

# combine with main file

data <- merge(tempEmpElix, HOSP, by = "pragmaid", all.x = T) %>% 
  # set hospitalisation during FU to 0 for those without record
  mutate(hosp.FU.times = ifelse(is.na(hosp.FU.times), 0, hosp.FU.times),
         hosp.FU.days = ifelse(is.na(hosp.FU.days), 0, hosp.FU.days),
         hosp.FU.AUD.times = ifelse(is.na(hosp.FU.AUD.times), 0, hosp.FU.AUD.times),
         hosp.FU.AUD.days = ifelse(is.na(hosp.FU.AUD.days), 0, hosp.FU.AUD.days),
         date.FU.end = case_when(
           reha == 0 ~ date.qwt.end + dyears(1) + ddays(1),
           reha == 1 ~ date.reha.end + dyears(1) + ddays(1),
           TRUE ~ NA)) %>%
  # filter out all with end FU after 12/2021
  filter(date.FU.end < "2022-01-01") # n = 52 excluded, new n = 563

# complete follow-up
table(data$reha) #478 QWT, 127 Reha

## ----------------------------------------------------------------
## MATCHING THE DATA
## ----------------------------------------------------------------

# explore imbalance and relevance of covariats for matching

# imbalance test: significant p-value indicates imbalance of groups
xBalance(reha ~ gkv + sex + yob + y.aud + q.aud + cmb.PRE + emp.type, 
         data = data, report = c("chisquare.test")) # p = .002

# simple log regression to test covariats
reg <- glm(reha ~ gkv + sex + yob + y.aud + q.aud + cmb.PRE + emp.type, data = data, family = "binomial")
summary(reg) # yob (p = .017), y.aud2020 (p = .015), emp.retired (p = .020)

  # SAVE regression table (Appendix A2, Table A2)
  out <- cbind(as.data.frame(coef(summary(reg))), confint(reg)) %>% 
    dplyr::select(c(Estimate, `2.5 %`, `97.5 %`, `Pr(>|z|)`)) %>%
    `colnames<-`(c("coef", "lci", "uci", "pval")) %>%
    mutate_at(c("coef", "lci", "uci"), ~round(.,2)) %>% 
    mutate(pval = round(pval, 3))
  #write.csv(out, paste0("Output/", DATE, "_TabA2_QWT-Reha.csv"))

# get propensity scores
# keep gkv, sex, yob, y.aud, cmb.PRE, and emp.type based on theoretical considerations

ps <- glm(reha ~ gkv + sex + yob + y.aud + cmb.PRE + emp.type, data = data, family = "binomial")
summary(ps)

data$ps <- predict(ps, type = "response")
tiff(file=paste0("Output/", DATE, "_RQ3_QWT-Reha_SampleDistribution.tiff"), compression = "jpeg")
histbackback(split(data$ps, data$reha),
             main = "Propensity score before matching", xlab = c("qwt", "qwt & reha"))
dev.off()

# matching using full match

m.data <- matchit(reha ~ gkv + sex + yob + y.aud + cmb.PRE + emp.type, data = data, method = "full",
                  min.controls = 1, max.controls = 10, discard = "both")
summary(m.data) # matched: 421 qwt, 127 qwt & reha (57 qwt discarded)
plot(m.data, type = "jitter")
  # not balanced 

# alternative matching: nearest distance matching

m.data <- matchit(reha ~ gkv + sex + yob + y.aud + cmb.PRE + emp.type, data = data, method = "nearest", 
                  caliper = 0.1, ratio = 3)
summary(m.data) # matched: 314 qwt, 127 qwt & reha (164 qwt unmatched)
tiff(file=paste0("Output/", DATE, "_RQ3_QWT-Reha_MatchNearest.tiff"), compression = "jpeg")
plot(m.data, type = "jitter")
dev.off()
  # balanced: p = .706

# alternative matching: optimal matching

m.data <- matchit(reha ~ gkv + sex + yob + y.aud + cmb.PRE + emp.type, data = data, method = "optimal", ratio = 3)
summary(m.data) # matched: 381 qwt, 127 qwt & reha (97 qwt unmatched)
tiff(file=paste0("Output/", DATE, "_RQ3_QWT-Reha_MatchOptimal.tiff"), compression = "jpeg")
plot(m.data, type = "jitter")
dev.off()
  # balanced: p = .432

# check matched data

match.data <-	match.data(m.data)
xBalance(reha ~ gkv + sex + yob + y.aud + cmb.PRE + emp.type, data = match.data, report = c("chisquare.test")) 
tiff(file=paste0("Output/", DATE, "_RQ3_QWT-Reha_MatchedSampleDistribution.tiff"), compression = "jpeg")
histbackback(split(match.data$ps, match.data$reha), main = "Propensity score before matching", xlab = c("qwt", "qwt & reha"))
dev.off()

## ----------------------------------------------------------------
## DESCRIPTIVES OF UNMATCHED PATIENTS
## ----------------------------------------------------------------

desc_excl <- data %>% filter(!pragmaid %in% match.data$pragmaid) %>% 
  mutate(age.aud = as.numeric(substr(date.aud, 1, 4)) - yob,
         sex = ifelse(sex == "male", 1,
                      ifelse(sex == "female", 0, NA)),
         aud2017 = ifelse(y.aud == 2017, 1, 0),
         aud2018 = ifelse(y.aud == 2018, 1, 0),
         aud2019 = ifelse(y.aud == 2019, 1, 0),
         aud2020 = ifelse(y.aud == 2020, 1, 0),
         time.aud.qwt = as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)), 
         time.qwt = as.numeric(as.Date(date.qwt.end) - as.Date(date.qwt.start)),
         time.reha = ifelse(reha == 1, as.numeric(as.Date(date.reha.end) - as.Date(date.reha.start)),
                            ifelse(reha == 0, 0, NA))) %>%
  group_by(reha) %>%
  summarise(age.aud.mean = mean(age.aud ), age.aud.std = sqrt(wtd.var(age.aud )),
            sex.prop = mean(sex), sex.lci = mean(sex) - sd(sex)/sqrt(length(sex)), sex.uci = mean(sex) + sd(sex)/sqrt(length(sex)),
            aud2017.prop = mean(aud2017), aud2017.lci = mean(aud2017) - sd( aud2017)/sqrt(length(aud2017)), aud2017.uci = mean(aud2017) + sd(aud2017)/sqrt(length(aud2017)),
            aud2018.prop = mean(aud2018), aud2018.lci = mean(aud2018) - sd( aud2018)/sqrt(length(aud2018)), aud2018.uci = mean(aud2018) + sd(aud2018)/sqrt(length(aud2018)),
            aud2019.prop = mean(aud2019), aud2019.lci = mean(aud2019) - sd(aud2019)/sqrt(length(aud2019)), aud2019.uci = mean(aud2019) + sd(aud2019)/sqrt(length(aud2019)),
            aud2020.prop = mean(aud2020), aud2020.lci = mean(aud2020) - sd(aud2020)/sqrt(length(aud2020)), aud2020.uci =mean(aud2020) + sd(aud2020)/sqrt(length(aud2020)),
            Elix30PRE.median = median(Elix30PRE), 
            Elix30PRE.iqr1 = quantile(Elix30PRE, probs = 0.25),
            Elix30PRE.iqr3 = quantile(Elix30PRE, probs = 0.75),
            time.aud.qwt.median = median(time.aud.qwt ), 
            time.aud.qwt.iqr1 = quantile(time.aud.qwt, probs = 0.25),
            time.aud.qwt.iqr3 = quantile(time.aud.qwt, probs = 0.75),
            time.qwt.median = median(time.qwt),
            time.qwt.iqr1 = quantile(time.qwt, probs = 0.25),
            time.qwt.iqr3 = quantile(time.qwt, probs = 0.75),
            time.reha.median = median(time.reha),
            time.reha.iqr1 = quantile(time.reha, probs = 0.25),
            time.reha.iqr3 = quantile(time.reha, probs = 0.75),
            hosp.FU.times.median = median(hosp.FU.times ), 
            hosp.FU.times.iqr1 = quantile(hosp.FU.times, probs = 0.25),
            hosp.FU.times.iqr3 = quantile(hosp.FU.times, probs = 0.75),
            hosp.FU.days.mean = median(hosp.FU.days), 
            hosp.FU.days.iqr1 = quantile(hosp.FU.days, probs = 0.25),
            hosp.FU.days.iqr3 = quantile(hosp.FU.days, probs = 0.75),
            hosp.FU.AUD.times.mean = median(hosp.FU.AUD.times), 
            hosp.FU.AUD.times.iqr1 = quantile(hosp.FU.AUD.times, probs = 0.25),
            hosp.FU.AUD.times.iqr3 = quantile(hosp.FU.AUD.times, probs = 0.75),
            hosp.FU.AUD.days.mean = median(hosp.FU.AUD.days), 
            hosp.FU.AUD.days.iqr1 = quantile(hosp.FU.AUD.days, probs = 0.25),
            hosp.FU.AUD.days.iqr3 = quantile(hosp.FU.AUD.days, probs = 0.75)) %>%
  t %>% as.data.frame() %>% row_to_names(1) %>% `colnames<-`(c("unmatched (excluded)"))

## ----------------------------------------------------------------
## SAFE MATCHED DATA
## ----------------------------------------------------------------

write.csv(match.data, paste0("Data/preprocessed data/", DATE, "_RQ3_QWT-Reha_MatchedData.csv"))
#write.csv(desc_excl, paste0("Output/", DATE, "_TabA3_QWT-Reha.csv"))



