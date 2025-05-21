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

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

match.data <- readRDS("Data/preprocessed data/2024-10-10_RQ3_AllDat_Matched.RDS")

## ----------------------------------------------------------------
## PREPARE
## ----------------------------------------------------------------

data <- match.data %>% filter(treat != "inpat") %>%
  mutate(reha = ifelse(treat == "reha", 1, 0))

histogram(data$time.qwt)

data <- data %>% 
  mutate(time.qwt.cat = factor(case_when(
    time.qwt < 21 ~ "<21 days",
    time.qwt == 21 ~ "21 days", 
    time.qwt > 21 ~ ">21 days"),
    levels = c("<21 days", "21 days", ">21 days")),)

table(data$time.qwt, data$time.qwt.cat)
table(data$time.qwt.cat)
table(data$reha, data$time.qwt.cat)

## ----------------------------------------------------------------
## LOGISTIC MODEL
## ----------------------------------------------------------------

model1 <- glm(reha ~ time.qwt.cat, data, weights = weights, family = quasibinomial)
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))
