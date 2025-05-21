######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 07/05/2024           ##
## Date last changed: 18/06/2024    ##
######################################

## PREPARE DATA FOR MATCHING

# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(comorbidity)
library(data.table)
library(cobalt)
library(Hmisc)
library(nnet)
library(WeightIt)
library(pscl)
library(spatstat)
#library(boot)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

datQWT <- read.csv("Data/raw data/data_qwt_2024-07-09.csv")
datREHA <- read.csv("Data/raw data/data_reha_2024-07-09.csv")
datINPAT <- read.csv("Data/raw data/data_inpat_2024-07-09.csv")
datCONTROL <- read.csv("Data/raw data/data_control_2024-07-09.csv")
datOthCond <- readRDS("Data/raw data/1_data_all diagnoses_2024-07-09.rds") 
datEMP <- readRDS("Data/raw data/1_data_employment periods_2024-07-09.rds")

# Layout for visualizations

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=14, color = "black"),
        strip.text = element_text(size=14),
        title = element_text(size=14),
        axis.title = element_text(size=14, color = "black"),
        legend.position = "right")


## ----------------------------------------------------------------
## PART 1: PREPARE DATA
## ----------------------------------------------------------------

## 1) QWT

# n of people with QWT in same quarter of diagnosis or anytime later 
length(unique(datQWT$pragmaid))

QWT <- datQWT %>% dplyr::select(-KH_FALL_ID) %>%
  mutate(date.aud = as.Date(date.aud),
         date.qwt.start = as.Date(date.qwt.start),
         date.qwt.end = as.Date(date.qwt.end)) %>%
  # AUD diagnosis before QWT starts 
  filter(date.aud <= date.qwt.start) %>% #1,127
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # QWT completed within 365 days of first diagnosis
  filter(date.aud.12mPOST > date.qwt.end) %>%
  filter(qwt_id == 1)

length(unique(QWT$pragmaid)) # 772

## 2) Inpatient / physical withdrawal

# n of people with REHA in same quarter of diagnosis or anytime later 
length(unique(datINPAT$pragmaid))

INPAT <- datINPAT %>% 
  mutate(date.aud = as.Date(date.aud),
         date.inpat.start = as.Date(date.inpat.start),
         date.inpat.end = as.Date(date.inpat.end)) %>%
  filter(date.aud <= date.inpat.start) %>%
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # filter INPAT needs to start 12 months since AUD diagnosis 
  filter(date.aud.12mPOST > date.inpat.start) %>% # 574
  # Inpat completed within 365 days of first diagnosis
  filter(date.aud.12mPOST > date.inpat.end) %>%
  # summarise total number of INPATs within 365 days since AUD diagnosis
  group_by(pragmaid, KH_FALL_ID) %>%
  mutate(inpat_num = sum(inpat_id)) %>%
  filter(inpat_id == 1) %>% dplyr::select(-KH_FALL_ID)

length(unique(INPAT$pragmaid)) # 581

## 3) REHA

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

length(unique(REHA$pragmaid)) # 346

## 3) REHA

CONTROL <- datCONTROL %>% 
  mutate(date.aud = as.Date(date.aud)) %>%
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) 

length(unique(CONTROL$pragmaid)) # 5074

## ----------------------------------------------------------------
## MERGE DATA, INCLUDING EMPLOYMENT, ELIXHAUSER & HOSPITALIZATION
## ----------------------------------------------------------------

# combine data files 

temp <- merge(INPAT, QWT, by = c("gkv", "pragmaid", "date.aud", "sex", "yob", 
                                 "date.ins.start", "date.ins.end", 
                                 "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  merge(., REHA, by = c("gkv", "pragmaid", "date.aud", "sex", "yob", 
                        "date.ins.start", "date.ins.end", 
                        "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  merge(., CONTROL, by = c("gkv", "pragmaid", "date.aud", "sex", "yob",
                           "date.ins.start", "date.ins.end",
                           "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  mutate(qwt = case_when(
           !is.na(date.qwt.start) & !is.na(date.inpat.start) & date.inpat.end <= date.qwt.start ~ 1,
           !is.na(date.qwt.start) & is.na(date.inpat.start) ~ 1,
           !is.na(date.inpat.start) & is.na(date.qwt.start) ~ 0, 
           TRUE ~ NA), 
         reha = case_when(
           !is.na(date.qwt.start) & !is.na(date.reha.start) & date.qwt.end <= date.reha.start ~ 1,
           !is.na(date.qwt.start) & is.na(date.reha.start) ~ 0, 
           !is.na(date.inpat.start) & is.na(date.reha.start) ~ 0, 
           TRUE ~ NA), 
         treat = case_when(
           qwt == 0 & reha == 0 ~ "inpat",
           qwt == 1 & reha == 0 ~ "qwt", 
           qwt == 1 & reha == 1 ~ "reha",
           is.na(date.inpat.start) & is.na(date.qwt.start) & is.na(date.reha.start) ~ "control", 
           TRUE ~ NA), 
         age.aud = as.numeric(substr(date.aud, 1, 4)) - yob) %>%
  
  # filter people with reha but no qwt prior to reha 
  filter(!is.na(treat)) %>% # n = 312 
  
  # get covariate for year and Q of diagnosis
  mutate(y.aud = as.factor(str_sub(date.aud, 1, 4)),
         q.aud = as.factor(ifelse(as.factor(str_sub(date.aud, 6, 7)) == "01", "Q1", 
                                  ifelse(as.factor(str_sub(date.aud, 6, 7)) == "04", "Q2", 
                                         ifelse(as.factor(str_sub(date.aud, 6, 7)) == "07", "Q3", 
                                                ifelse(as.factor(str_sub(date.aud, 6, 7)) == "10", "Q4", NA))))),
         sex = as.factor(sex),
         gkv = as.factor(gkv),
         inpat_num = case_when(
           is.na(inpat_num) ~ 0,
           !is.na(inpat_num) ~ inpat_num,
           TRUE ~ NA)) %>%
  
  #!! drop duplicates but check why they exist first
  distinct()

table(temp$treat) #5074 CONTROL #353 INPAT, 530 QWT, 114 REHA

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

table(tempEmp$emp.type, tempEmp$treat)


## 4) ELIXHAUSER

# get Elixhauser index 12M prior to AUD diagnosis

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
         cmb.PRE = factor(ifelse(Elix30PRE <= 1, "0/1 comorbidity", 
                                 # set comorbidity = 0 for those without recorded comorbidity data
                                 ifelse(Elix30PRE >= 2, "2+ comorbidities", NA)),
                          levels = c("0/1 comorbidity", "2+ comorbidities")))

table(tempEmpElix$Elix30PRE, tempEmpElix$treat) 
table(tempEmpElix$Elix27PRE, tempEmpElix$treat) 
# go with Elix30, differentiate between 0/1 and 2+ comorbidity


## 5) HOSPITALIZATION

# get hospitalization during 12M FU

HOSP_FU <- merge(temp, datOthCond, by = c("gkv", "pragmaid"), all.x = T) %>% 
  filter(setting %like% "inpat") %>% 
  # define FU periods
  mutate(date.diag.start = as.Date(date.diag.start),
         date.FU.start = case_when(
           treat == "inpat" ~ date.inpat.end + ddays(1),
           treat == "qwt" ~ date.qwt.end + ddays(1),
           treat == "reha" ~ date.reha.end + ddays(1),
           treat == "control" ~ date.aud + ddays(1), 
           TRUE ~ NA),
         date.FU.end = case_when(
           treat == "inpat" ~ date.inpat.end + dyears(1) + ddays(1),
           treat == "qwt" ~ date.qwt.end + dyears(1) + ddays(1),
           treat == "reha" ~ date.reha.end + dyears(1) + ddays(1),
           treat == "control" ~ date.aud + dyears(1) + ddays(1), 
           TRUE ~ NA)) %>%
  
  # filter out all with end FU after 12/2021
  filter(date.FU.end < "2022-01-01") %>% 
  
  # filter out all conditions not occurring during FU
  filter(date.diag.start > date.FU.start & date.diag.start < date.FU.end) 

HOSP_ALL <- HOSP_FU %>% 
  # get number of hospitalizations during FU
  group_by(pragmaid, date.diag.start, date.diag.end) %>%
  summarise(icd.FU = paste0("[", paste0(icd, collapse = ","), "]"),
            hosp.FU.days = mean(as.numeric(as.Date(date.diag.end) - as.Date(date.diag.start)))) %>%
  group_by(pragmaid) %>%
  summarise(hosp.FU.times = n(),
            hosp.FU.days = sum(hosp.FU.days),
            icd.FU = paste0(icd.FU, collapse = ","))

HOSP_AUD <- HOSP_FU %>% 
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

data <- merge(tempEmpElix, HOSP, all.x = T) %>% 
  # set hospitalisation during FU to 0 for those without record
  mutate(treat = factor(treat, levels = c("control", "inpat", "qwt", "reha")),
         hosp.FU.times = ifelse(is.na(hosp.FU.times), 0, hosp.FU.times),
         hosp.FU.days = ifelse(is.na(hosp.FU.days), 0, hosp.FU.days),
         hosp.FU.AUD.times = ifelse(is.na(hosp.FU.AUD.times), 0, hosp.FU.AUD.times),
         hosp.FU.AUD.days = ifelse(is.na(hosp.FU.AUD.days), 0, hosp.FU.AUD.days),
         date.FU.end = case_when(
           treat == "inpat" ~ date.inpat.end + dyears(1) + ddays(1),
           treat == "qwt" ~ date.qwt.end + dyears(1) + ddays(1),
           treat == "reha" ~ date.reha.end + dyears(1) + ddays(1),
           treat == "control" ~ date.aud + dyears(1) + ddays(1), 
           TRUE ~ NA),
         time.aud.treat = case_when(
           treat == "inpat" ~ as.numeric(as.Date(date.inpat.start) - as.Date(date.aud)),
           treat == "qwt" ~ as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)),
           treat == "reha" ~ as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)),
           treat == "control" ~ 0, 
           TRUE ~ NA), 
         time.treat = case_when(
           treat == "inpat" ~ as.numeric(as.Date(date.inpat.end) - as.Date(date.inpat.start)),
           treat == "qwt" ~ as.numeric(as.Date(date.qwt.end) - as.Date(date.qwt.start)),
           treat == "reha" ~ (as.numeric(as.Date(date.qwt.end) - as.Date(date.qwt.start))) + 
             as.numeric(as.Date(date.reha.end) - as.Date(date.reha.start)),
           treat == "control" ~ 0, 
           TRUE ~ NA)) %>%
  
  # filter out all with end FU after 12/2021
  filter(date.FU.end < "2022-01-01") # n = 73 excluded, new n = 570

# complete follow-up
table(data$treat) #4819 CONTROL, 301 INPAT, 483 QWT, 101 REHA

## ----------------------------------------------------------------
## PART 2: MATCHING WEIGHTS
## ----------------------------------------------------------------

# CHECK COUNTS

table(data$gkv, data$treat)
table(data$sex, data$treat)
table(data$yob, data$treat)
table(data$y.aud, data$treat) # n = 6 in 2020 in reha
table(data$q.aud, data$treat)
table(data$cmb.PRE, data$treat)
table(data$emp.type, data$treat) # no retired in reha

data <- data %>% 
  mutate(emp.type = ifelse(emp.type == "retired", "other", emp.type))

table(data$emp.type, data$treat)

data$y.aud <- factor(data$y.aud, levels(droplevels(data$y.aud)))

# MATCH DATA 

# explore imbalance and relevance of covariates for matching
bal.tab(treat ~ gkv + sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, 
        data = data, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) 


# matching using entropy matching

m.data.ebal <- weightit(treat ~  gkv + sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, data = data, 
                        method = "ebal", estimand = "ATE")
summary(m.data.ebal) # max. weight = 24.88, max. Entropy = 1.926
bal.tab(m.data.ebal, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # balanced

# alternative matching: using optimization-based weighting

m.data.opt <- weightit(treat ~ gkv + sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, data = data, 
                       method = "optweight", estimand = "ATE")
summary(m.data.opt) # max. weight = 19.4890, max. Entropy = 2.012
bal.tab(m.data.opt, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # balanced, slightly higher sample size

# alternative matching: Propensity score weighting using Bayesian additive regression trees
set.seed(1234)
m.data.bart <- weightit(treat ~ gkv + sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, data = data, 
                        method = "bart", estimand = "ATE", n.threads = 1)
summary(m.data.bart) # max. weight = 14.3, max. Entropy = .069
bal.tab(m.data.bart, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # not balanced

# match data
match.data <- cbind(data, weights = m.data.opt$weights) 

## ----------------------------------------------------------------
## PART 3: ANLAYSIS
## ----------------------------------------------------------------

data <- match.data %>% 
  mutate(sex = ifelse(sex == "male", 1,
                      ifelse(sex == "female", 0, NA)),
         emp.employed = ifelse(emp.type == "employed", 1, 0),
         emp.unemployed = ifelse(emp.type == "unemployed", 1, 0),
         emp.other = ifelse(emp.type == "other", 1, 0),
         aud2017 = ifelse(y.aud == 2017, 1, 0),
         aud2018 = ifelse(y.aud == 2018, 1, 0),
         aud2019 = ifelse(y.aud == 2019, 1, 0),
         aud2020 = ifelse(y.aud == 2020, 1, 0),
         treat = factor(treat, levels=c("control", "qwt","inpat","reha")),
         hosp.FU.AUD.days = as.integer(hosp.FU.AUD.days),
         hosp.FU.days = as.integer(hosp.FU.days),
         inpat = factor(ifelse(inpat_num <= 1, "0/1 inpat", 
                               ifelse(inpat_num > 1, "2+ inpat", NA))),
         inpat2 = as.numeric(ifelse(inpat_num <= 1, 0, 
                                    ifelse(inpat_num > 1, 1, NA)))) 

# Correlation of treatment condition with covariates 

ggplot(data) + geom_boxplot(aes(x = as.factor(treat), y = time.aud.treat)) + theme_barplot
ggplot(data) + geom_boxplot(aes(x = as.factor(treat), y = time.treat)) + theme_barplot
ggplot(data, aes(x = inpat)) + geom_histogram(stat = "count") + facet_wrap(vars(treat))

# Outcome 1) days of hospitalizations within 1 year after first treatment 

ggplot(data, aes(x = hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of all-cause hospital days during 12 months follow up") +
  xlab("Days") + theme_barplot
#ggsave(paste0("Output/", Sys.Date(), "_FigA1_Inpat-QWT_HospDays.jpg"), dpi=300, width = 10, height = 8)

mHospDays <- zeroinfl(hosp.FU.days ~ treat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

# Outcome 2) days of AUD hospitalizations within 1 year after first treatment 

ggplot(data, aes(hosp.FU.days)) + geom_histogram() + 
  labs(title = "Total number of alcohol-specific hospital days during 12 months follow up") +
  xlab("Days") + theme_barplot
#ggsave(paste0("Output/", Sys.Date(), "_FigA1_Inpat-QWT_HospAUDDays.jpg"), dpi=300, width = 10, height = 8)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat, data = data, 
                         dist = "negbin", weights = weights)
summary(mHospAUDDays)

