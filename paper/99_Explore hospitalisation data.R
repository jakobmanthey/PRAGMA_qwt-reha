######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 07/05/2024           ##
## Date last changed: 18/06/2024    ##
######################################

## INVESTIGATE INPATIENT AND QWT DATA

# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")
DATE <- 20240618

# Load data

datQWT <- read.csv("Data/raw data/data_qwt_2024-06-18.csv")
datREHA <- read.csv("Data/raw data/data_reha_2024-06-18.csv")
datINPAT <- read.csv("Data/raw data/data_inpat_2024-06-18.csv")
datOthCond <- readRDS("Data/raw data/1_data_all diagnoses_2024-06-18.rds") 

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
  filter(qwt_id == 1)

length(unique(QWT$pragmaid)) # 672

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
  filter(date.aud.12mPOST > date.inpat.start) %>%
  # summarise total number of INPATs within 365 days since AUD diagnosis
  group_by(pragmaid) %>%
  mutate(inpat_num = sum(inpat_id)) %>%
  filter(inpat_id == 1)  

length(unique(INPAT$pragmaid)) # 538

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

length(unique(REHA$pragmaid)) # 304

# combine data files 

temp <- merge(INPAT, QWT, by = c("gkv", "pragmaid", "date.aud", "sex", "yob", 
                                 "date.ins.start", "date.ins.end", 
                                 "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  merge(., REHA, by = c("gkv", "pragmaid", "date.aud", "sex", "yob", 
                        "date.ins.start", "date.ins.end", 
                        "date.aud.12mPRE", "date.aud.12mPOST"), all = T) %>%
  mutate(qwt = ifelse(!is.na(date.qwt.start), 1,
                      ifelse(!is.na(date.inpat.start) & is.na(date.qwt.start), 0, NA)),
         reha = ifelse(!is.na(date.qwt.start) & !is.na(date.reha.start), 1,
                       ifelse(!is.na(date.qwt.start) & is.na(date.reha.start), 0, NA)),
         treat = ifelse(qwt == 0 & is.na(reha), "inpat", 
                        ifelse(qwt == 1 & reha == 0, "qwt",
                               ifelse(qwt == 1 & reha == 1, "reha", NA)))) %>%
  filter(!is.na(treat)) %>% # n = 121 with reha but without qwt
  # get covariate for year and Q of diagnosis
  mutate(y.aud = as.factor(str_sub(date.aud, 1, 4)),
         q.aud = as.factor(ifelse(as.factor(str_sub(date.aud, 6, 7)) == "01", "Q1", 
                                  ifelse(as.factor(str_sub(date.aud, 6, 7)) == "04", "Q2", 
                                         ifelse(as.factor(str_sub(date.aud, 6, 7)) == "07", "Q3", 
                                                ifelse(as.factor(str_sub(date.aud, 6, 7)) == "10", "Q4", NA))))),
         sex = as.factor(sex),
         gkv = as.factor(gkv))
table(temp$treat) #352 INPAT, 527 QWT, 124 reha

# get hospitalisation data including period before/after treatment of interest

sub <- temp %>% mutate(
  date.treat.start = case_when(    
    treat == "inpat" ~ as.Date(date.inpat.start),
    treat == "qwt" ~ as.Date(date.qwt.start), 
    treat == "reha" ~ as.Date(date.reha.start), 
    TRUE ~ NA),
  date.treat.end = case_when(    
    treat == "inpat" ~ as.Date(date.inpat.end),
    treat == "qwt" ~ as.Date(date.qwt.end), 
    treat == "reha" ~ as.Date(date.reha.end), 
    TRUE ~ NA)) %>%
  left_join(., datOthCond) %>% filter(date.diag.start <= date.treat.start & date.diag.end >= date.treat.end) %>% 
  dplyr::filter(icd %like% "E24.4|F10|G31.2|G62.1|G72.1|I42.6|K29.2|K70.0|K70.1|K70.2|K70.3|K70.4|K70.9|K85.2|K86.0|R78.0|X45|X65|Y15|Y90|Y91") %>%
  mutate(main.treat = ifelse(date.diag.start == date.treat.start & date.diag.end == date.treat.end, 
                             "treatment of interest", "before/after treatment of interest"))

# look at alcohol-specific ICD codes during vs. before/after treatment of interest

sub.icd <- sub %>% group_by(treat) %>%
  mutate(n.treat = n(), icd = icd) %>% 
  group_by(treat, main.treat, icd) %>%
  summarise(prop = n() / mean(n.treat))

ggplot(sub.icd, aes(x = as.factor(icd), fill = as.factor(main.treat))) +
  geom_bar(aes(y = prop), stat = "identity") + scale_y_continuous(labels = percent) +
  facet_grid(rows = vars(as.factor(treat)), scales = "free") +
  theme_bw()

# individual trajectories of hospitalisations 

# inpat sample 

inpat.sample <- sub %>% filter(treat == "inpat") %>% group_by(pragmaid) %>%
  mutate(first.date = min(date.diag.start)) %>% ungroup() %>%
  dplyr::select(c("pragmaid", "first.date")) %>% 
  group_by(pragmaid) %>% slice(1) %>% ungroup() %>%
  sample_n(100) %>% arrange(first.date) %>% mutate(k = seq.int(unique(pragmaid)))

sub.inpat <- sub %>% merge(., inpat.sample, all.x = F, all.y = T) %>% 
  mutate(ymin = k + 1/(length(pragmaid)+1),
         ymax = k + 1 + 1/(length(pragmaid)+1))

ggplot(sub.inpat) +
  ggtitle("Hospital stays with alcohol-specific diagnosis (inpatient sample, random sample of 100)") +
  geom_rect(aes(xmin = date.diag.start, xmax = date.diag.end, ymin = ymin, ymax = ymax, 
                group = pragmaid, fill = (main.treat)), alpha = 0.5) +
  scale_y_continuous("", breaks = c()) + 
  scale_fill_manual("", values = c("#109618","#dc3912")) +
  theme_bw()

# qwt sample

qwt.sample <- sub %>% filter(treat == "qwt") %>% group_by(pragmaid) %>%
  mutate(first.date = min(date.diag.start)) %>% ungroup() %>%
  dplyr::select(c("pragmaid", "first.date")) %>% 
  group_by(pragmaid) %>% slice(1) %>% ungroup() %>%
  sample_n(100) %>% arrange(first.date) %>% mutate(k = seq.int(unique(pragmaid)))

sub.qwt <- sub %>% merge(., qwt.sample, all.x = F, all.y = T) %>% 
  mutate(ymin = k + 1/(length(pragmaid)+1),
         ymax = k + 1 + 1/(length(pragmaid)+1))

ggplot(sub.qwt) +
  ggtitle("Hospital stays with alcohol-specific diagnosis (qwt sample, random sample of 100)") +
  geom_rect(aes(xmin = date.diag.start, xmax = date.diag.end, ymin = ymin, ymax = ymax, 
                group = pragmaid, fill = (main.treat)), alpha = 0.5) +
  scale_y_continuous("", breaks = c()) + 
  scale_fill_manual("", values = c("#109618","#dc3912")) +
  theme_bw()


