######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 25/06/2024           ##
## Date last changed: 25/06/2024    ##
######################################

## SENSITIVITY ANALYSIS 1 — alternative definition of QWT/INPAT treatment

# Load packages

library(tidyverse)
library(WeightIt)
library(cobalt)
library(pscl)
library(parameters)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")
DATE <- 20240625

# Load data & redefine treatment groups

data <- readRDS("Data/preprocessed data/20240618_RQ3_AllDat_Preprocessed.RDS") %>% 
  mutate(qwt2 = ifelse(time.treat < 13, 0, ifelse(time.treat >= 14, 1, NA)),
         reha2 = ifelse(qwt2 == 1 & reha == 1, 1, ifelse(reha == 0, 0, NA)),
         treat2 = factor(ifelse(qwt2 == 0, "inpat",
                         ifelse(qwt2 == 1 & reha2 == 0, "qwt",
                                ifelse(qwt2 == 1 & reha2 == 1, "reha", NA))),
                         levels = c("qwt", "inpat", "reha"))) %>% 
  mutate(emp.type = ifelse(emp.type == "retired", "other", emp.type)) %>%
  filter(!is.na(treat2))

table(data$qwt, data$qwt2)
table(data$reha, data$reha2)
table(data$treat, data$treat2) 
  # plus 136 inpat from original qwt, no impact on reha
  # 303 INPAT, 154 QWT, 86 REHA

data$y.aud <- factor(data$y.aud, levels(droplevels(data$y.aud)))

## ----------------------------------------------------------------
## MATCHING
## ----------------------------------------------------------------

bal.tab(treat2 ~ gkv + sex + yob + y.aud + q.aud + cmb.PRE + emp.type, 
        data = data, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) 

# optimization-based weighting

m.data.opt <- weightit(treat2 ~ gkv + y.aud + q.aud + emp.type, data = data, 
                       method = "optweight", estimand = "ATE")
summary(m.data.opt) # max. weight = 2.41, max. Entropy = 100
bal.tab(m.data.opt, s.d.denom = "pooled", thresholds = c(m = 0.05, v = 2)) # balanced

# get matching weights 

match.data <- cbind(data, weights = m.data.opt$weights) %>%
  mutate(hosp.FU.days = as.integer(hosp.FU.days),
         hosp.FU.AUD.days = as.integer(hosp.FU.AUD.days),
         time.aud.treat = as.integer(time.aud.treat),
         time.treat = as.integer(time.treat))

## ----------------------------------------------------------------
## GROUP COMPARISONS
## ----------------------------------------------------------------

mHospDays <- zeroinfl(hosp.FU.days ~ treat2 + time.aud.treat + time.treat, data = match.data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat2 + time.aud.treat + time.treat, data = match.data, 
                         dist = "negbin", weights = weights)
summary(mHospAUDDays)

# Output Table A1

out <- rbind(
  cbind(group = "Hosp_AllCause_Days", var = p_value(mHospDays)[1],
        coef = exp(unlist(mHospDays$coefficients)), lci = exp(confint(mHospDays))[,1], uci = exp(confint(mHospDays)[,2]), p_value(mHospDays)[2]),
  cbind(group = "Hosp_AUD_Days", var = p_value(mHospDays)[1],
        coef = exp(unlist(mHospAUDDays$coefficients)), lci = exp(confint(mHospAUDDays))[,1], uci = exp(confint(mHospAUDDays)[,2]), p_value(mHospAUDDays)[2])) %>% 
  as.data.frame() %>% 
  mutate_at(c("coef", "lci", "uci"), ~round(as.numeric(.),3)) %>% 
  mutate(p = round(as.numeric(p), 3))

#write.csv(out, paste0("Output/", DATE, "_TabA1_SA_AlternativeTreatmentDefinition.csv"))



