######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 07/05/2024           ##
## Date last changed: 01/10/2024    ##
######################################

## DATA MATCHING

# Load packages

library(dplyr)
library(tidyverse)
library(cobalt)
library(Hmisc)
library(nnet)
library(WeightIt)
library(ggplot2)
library(data.table)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

data <- readRDS("Data/preprocessed data/2024-10-10_RQ3_AllDat_Preprocessed.RDS") 

## ----------------------------------------------------------------
## COUNTS
## ----------------------------------------------------------------

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

## ----------------------------------------------------------------
## MATCHING THE DATA
## ----------------------------------------------------------------
# NOTE: using Sample Average Treatment Effect (SATE) design:
# Effect treatment vs. control

# explore imbalance and relevance of covariates for matching
bal.tab(treat ~ sex + age.aud + y.aud + q.aud + cmb.PRE + emp.type, 
        data = data, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) 

# matching using entropy matching

m.data.ebal <- weightit(treat ~ sex + y.aud + q.aud + cmb.PRE + emp.type, data = data, 
                   method = "ebal", estimand = "ATE")
summary(m.data.ebal) # max. weight = 2.93, max. Entropy = .087
bal.tab(m.data.ebal, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # balanced

# match data
match.data <- cbind(data, weights = m.data.ebal$weights) 

## ----------------------------------------------------------------
## SAFE MATCHED DATA
## ----------------------------------------------------------------

saveRDS(match.data, paste0("Data/preprocessed data/", Sys.Date(), "_RQ3_AllDat_Matched.RDS"))
