######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 07/05/2024           ##
## Date last changed: 01/10/2024    ##
######################################

## PREPARE DATA FOR MATCHING

# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(comorbidity)
library(data.table)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

datQWT <- read.csv("Data/raw data/data_qwt_2024-10-10.csv")
datREHA <- read.csv("Data/raw data/data_reha_2024-10-01.csv")
datINPAT <- read.csv("Data/raw data/data_inpat_2024-10-10.csv")
datOthCond <- readRDS("Data/raw data/1_data_all diagnoses_2024-07-09.rds")
datEMP <- readRDS("Data/raw data/1_data_employment periods_2024-07-09.rds")

## ----------------------------------------------------------------
## FILTER ORIGINAL DATA FILES
## ----------------------------------------------------------------

## 1) QWT

# n of people with QWT in same quarter of diagnosis or anytime later 
length(unique(datQWT$pragmaid)) #1,184

QWT <- datQWT %>% dplyr::select(-KH_FALL_ID) %>%
  mutate(date.aud = as.Date(date.aud),
         date.qwt.start = as.Date(date.qwt.start),
         date.qwt.end = as.Date(date.qwt.end)) %>%
  # AUD diagnosis before QWT starts 
  filter(date.aud <= date.qwt.start) %>% #1,163
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # QWT completed within 365 days of first diagnosis
  filter(date.aud.12mPOST > date.qwt.end) %>%
  filter(qwt_id == 1) %>% 
  rename("qwt_entl" = "ENTL301")

length(unique(QWT$pragmaid)) # 764

## 2) Inpatient 

# n of people with REHA in same quarter of diagnosis or anytime later 
length(unique(datINPAT$pragmaid)) # 1,069

INPAT <- datINPAT %>% #select(-KH_FALL_ID) %>%
  mutate(date.aud = as.Date(date.aud),
         date.inpat.start = as.Date(date.inpat.start),
         date.inpat.end = as.Date(date.inpat.end)) %>%
  filter(date.aud <= date.inpat.start) %>%
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # filter INPAT needs to start 12 months since AUD diagnosis 
  filter(date.aud.12mPOST > date.inpat.start) %>% # 595
  # Inpat completed within 365 days of first diagnosis
  filter(date.aud.12mPOST > date.inpat.end) %>%
  # summarise total number of INPATs by patient and KH_FALL_ID within 365 days since AUD diagnosis
  group_by(pragmaid, KH_FALL_ID) %>%
  mutate(inpat_num = sum(inpat_id)) %>%
  filter(inpat_id == 1) %>% ungroup() %>% dplyr::select(-KH_FALL_ID) %>% 
  rename("inpat_entl" = "ENTL301")

length(unique(INPAT$pragmaid)) # 581

## 3) REHA

# n of people with REHA in same quarter of diagnosis or anytime later 
length(unique(datREHA$pragmaid)) #573

REHA <- datREHA %>% 
  mutate(date.aud = as.Date(date.aud),
         date.reha.start = as.Date(date.reha.start),
         date.reha.end = as.Date(date.reha.end)) %>%
  filter(date.aud <= date.reha.start) %>% # 535
  mutate(date.aud.12mPOST = date.aud + dyears(1) + ddays(1),
         date.aud.12mPRE = date.aud - dyears(1) - ddays(1)) %>%
  # filter REHA needs to start 12 months since AUD diagnosis 
  filter(date.aud.12mPOST > date.reha.start) %>%
  filter(reha_id == 1)

length(unique(REHA$pragmaid)) # 292

## ----------------------------------------------------------------
## MERGE DATA, INCLUDING EMPLOYMENT, ELIXHAUSER & HOSPITALIZATION
## ----------------------------------------------------------------

# combine data files 

temp <- merge(INPAT, QWT, by = c("pragmaid", "date.aud", "sex", "yob", 
                                 "date.ins.start", "date.ins.end", 
                                 "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  merge(., REHA, by = c("pragmaid", "date.aud", "sex", "yob", 
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
           TRUE ~ NA), 
         age.aud = as.numeric(substr(date.aud, 1, 4)) - yob) %>%
  
  # filter people with reha but no qwt prior to reha 
  filter(!is.na(treat)) %>% # n = 259 
  
  # get covariate for year and Q of diagnosis
  mutate(y.aud = as.factor(str_sub(date.aud, 1, 4)),
         q.aud = as.factor(ifelse(as.factor(str_sub(date.aud, 6, 7)) == "01", "Q1", 
                                  ifelse(as.factor(str_sub(date.aud, 6, 7)) == "04", "Q2", 
                                         ifelse(as.factor(str_sub(date.aud, 6, 7)) == "07", "Q3", 
                                                ifelse(as.factor(str_sub(date.aud, 6, 7)) == "10", "Q4", NA))))),
         sex = as.factor(sex),
         inpat_num = case_when(
           is.na(inpat_num) ~ 0,
           !is.na(inpat_num) ~ inpat_num,
           TRUE ~ NA))
table(temp$treat) #359 INPAT, 534 QWT, 114 reha

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
  merge(temp, ., by = c("pragmaid"), all.x = T) %>%
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

## 5) HOSPITALIZATION

# get hospitalization during 12M FU

HOSP_FU <- datOthCond %>% 
  merge(temp, by = c("pragmaid"), all.x = T) %>% 
  filter(setting %like% "inpat" & icd_type == "primary") %>% 
  # define FU periods
  mutate(date.diag.start = as.Date(date.diag.start),
         date.FU.start = case_when(
           treat == "inpat" ~ date.inpat.end + ddays(1),
           treat == "qwt" ~ date.qwt.end + ddays(1),
           treat == "reha" ~ date.reha.end + ddays(1),
           TRUE ~ NA),
         date.FU.end = case_when(
           treat == "inpat" ~ date.inpat.end + dyears(1) + ddays(1),
           treat == "qwt" ~ date.qwt.end + dyears(1) + ddays(1),
           treat == "reha" ~ date.reha.end + dyears(1) + ddays(1),
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
  mutate(treat = factor(treat, levels = c("inpat", "qwt", "reha")),
         hosp.FU.times = ifelse(is.na(hosp.FU.times), 0, hosp.FU.times),
         hosp.FU.days = ifelse(is.na(hosp.FU.days), 0, hosp.FU.days),
         hosp.FU.AUD.times = ifelse(is.na(hosp.FU.AUD.times), 0, hosp.FU.AUD.times),
         hosp.FU.AUD.days = ifelse(is.na(hosp.FU.AUD.days), 0, hosp.FU.AUD.days),
         date.FU.end = case_when(
           treat == "inpat" ~ date.inpat.end + dyears(1) + ddays(1),
           treat == "qwt" ~ date.qwt.end + dyears(1) + ddays(1),
           treat == "reha" ~ date.reha.end + dyears(1) + ddays(1),
           TRUE ~ NA),
         time.aud.treat = case_when(
           treat == "inpat" ~ as.numeric(as.Date(date.inpat.start) - as.Date(date.aud)),
           treat == "qwt" ~ as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)),
           treat == "reha" ~ as.numeric(as.Date(date.qwt.start) - as.Date(date.aud)),
           TRUE ~ NA), 
         time.qwt = case_when(
           treat == "inpat" ~ 0,
           treat == "qwt" ~ as.numeric(as.Date(date.qwt.end + ddays(1)) - as.Date(date.qwt.start)),
           treat == "reha" ~ as.numeric(as.Date(date.qwt.end + ddays(1)) - as.Date(date.qwt.start)),
           TRUE ~ NA),
         time.treat = case_when(
           treat == "inpat" ~ as.numeric(as.Date(date.inpat.end + ddays(1)) - as.Date(date.inpat.start)),
           treat == "qwt" ~ as.numeric(as.Date(date.qwt.end + ddays(1)) - as.Date(date.qwt.start)),
           treat == "reha" ~ as.numeric(as.Date(date.reha.end + ddays(1)) - as.Date(date.reha.start)),
           TRUE ~ NA)) %>%
  
  # filter out all with end FU after 12/2021
  filter(date.FU.end < "2022-01-01") # n = 113 excluded, new n = 894

# complete follow-up
table(data$treat) #306 INPAT, 487 QWT, 101 REHA

## ----------------------------------------------------------------
## SAFE PREPARED DATA
## ----------------------------------------------------------------

saveRDS(data, paste0("Data/preprocessed data/", Sys.Date(), "_RQ3_AllDat_Preprocessed.RDS"))

