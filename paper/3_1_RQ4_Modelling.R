######################################
## Project: PRAGMA — RQ5            ##
## Author: Carolin Kilian           ##
## Start date: 03/09/2024           ##
## Date last changed: 06/11/2024    ##
######################################

## DATA ANALYSIS

# Load packages

library(dplyr)
library(tidyverse)
library(data.table)
library(pscl)
library(beepr)
library(writexl)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

match.data <- readRDS("Data/preprocessed data/2024-10-01_RQ3_AllDat_Matched.RDS")

# Set number of repetitions

rep <- 1000

## ----------------------------------------------------------------
## PREPARE
## ----------------------------------------------------------------

# get original data
data <- match.data %>% 
  mutate(treat = factor(treat, levels=c("qwt","inpat","reha")),
         hosp.FU.AUD.days = as.integer(hosp.FU.AUD.days),
         hosp.FU.days = as.integer(hosp.FU.days),
         inpat = factor(ifelse(inpat_num <= 1, "0/1 inpat", 
                               ifelse(inpat_num > 1, "2+ inpat", NA)))) %>% 
  dplyr::select(c("pragmaid", "treat", "inpat", "time.aud.treat", 
                  "hosp.FU.days", "hosp.FU.AUD.days", "weights"))

# get zero-inflated models
mHospDays <- zeroinfl(hosp.FU.days ~ treat + time.aud.treat + inpat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat + time.aud.treat + inpat, data = data, 
                         dist = "negbin", weights = weights)
summary(mHospAUDDays)

## ----------------------------------------------------------------
## MODELLING
## ----------------------------------------------------------------
# https://stackoverflow.com/questions/22314921/no-zeros-predicted-from-zeroinfl-object-in-r

# distribution of weights by treatment group
ggplot(data, aes(x = weights)) + geom_histogram() +
  facet_grid(cols = vars(treat))
ggplot(data[data$treat != "reha",], aes(x = log(weights))) + geom_histogram() 

m.dist <- data %>% filter(treat != "reha") %>% summarise(mean(log(weights))) %>% pull()
sd.dist <- data %>% filter(treat != "reha") %>% summarise(sd(log(weights))) %>% pull()

# Scenario 1: 20% Reha 

data20 <- data.frame()
ids <- c()
i <- 1
k <- 1

while (i<=rep) {
  
  print(i)
  set.seed(i)
  
  wdata <- exp(rnorm(round(0.2 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                     mean = m.dist, sd = sd.dist))
  
  while (k <= length(wdata)) {
    #foreach(k=1:length(wdata)) %do% {
    weight.select <- wdata[k]
    add <- data %>% filter(treat != "reha") %>% 
      mutate(diff = abs(weights - weight.select)) %>% 
      filter(diff == min(diff)) %>% pull(pragmaid)
    ids <- c(ids, add)
    k <- k+1
  } 
  
  sdata <- unique(ids) %>% as.data.frame() %>%
    sample_n(., round(0.2 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
    pull(.)
  
  add <- data %>% filter(pragmaid %in% sdata) %>% mutate(treat = "reha")
  sample20w <- data %>% filter(!pragmaid %in% sdata) %>% rbind(., add) %>% 
    mutate(sample = i)
  data20 <- rbind(data20, sample20w)
  
  i <- i+1
  
}

prop.table(table(data20$treat)) # 20% in Reha

# predict

set.seed(1234)
data20 <- cbind(data20, 
                sim.hosp.FU.days = predict(mHospDays, data20, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data20, type = c("prob"))) %>%
  dplyr::select(c(pragmaid, treat, sample, sim.hosp.FU.days.0, sim.hosp.FU.AUD.days.0)) %>%
  rename("any" = "sim.hosp.FU.days.0", "aud" = "sim.hosp.FU.AUD.days.0") %>%
  as.data.table()

sim.data20 <- data20[rep(seq_len(nrow(data20)), rep)][, nrep := 1:rep, by = .(pragmaid, sample)]
rm(data20, sample20w)

# get random probability
sim.data20[, prob := runif(nrow(sim.data20))]

# compare random probability with simulated probability of 0 days
sim.data20[, ':=' (any.zero = ifelse(as.numeric(any) > as.numeric(prob), 1, 0), aud.zero = ifelse(as.numeric(aud) > as.numeric(prob), 1, 0))]

# summarise across samples and nrep
sim.data20 <- sim.data20 %>%
  group_by(nrep, sample) %>%
  summarise(zero.stays = mean(any.zero),
            zero.AUD.stays = mean(aud.zero)) %>% ungroup() %>%
  mutate(scenario = "20% Reha")

saveRDS(sim.data20, paste0("Data/preprocessed data/", Sys.Date(), "_Reha20_Sim1000runs.RDS"))
rm(sim.data20)

# Scenario 2: 35% in REHA

data35 <- data.frame()
ids <- c()
i <- 1
k <- 1

while (i<=rep) {
  
  print(i)
  set.seed(i)
  
  wdata <- exp(rnorm(round(0.35 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                     mean = m.dist, sd = sd.dist))
  
  while (k <= length(wdata)) {
    #foreach(k=1:length(wdata)) %do% {
    weight.select <- wdata[k]
    add <- data %>% filter(treat != "reha") %>% 
      mutate(diff = abs(weights - weight.select)) %>% 
      filter(diff == min(diff)) %>% pull(pragmaid)
    ids <- c(ids, add)
    k <- k+1
  } 
  
  sdata <- unique(ids) %>% as.data.frame() %>%
    sample_n(., round(0.35 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
    pull(.)
  
  add <- data %>% filter(pragmaid %in% sdata) %>% mutate(treat = "reha")
  sample35w <- data %>% filter(!pragmaid %in% sdata) %>% rbind(., add) %>% 
    mutate(sample = i)
  data35 <- rbind(data35, sample35w)
  
  i <- i+1
  
}

prop.table(table(data35$treat)) # 35% in Reha

# predict

set.seed(1234)
data35 <- cbind(data35, 
                sim.hosp.FU.days = predict(mHospDays, data35, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data35, type = c("prob"))) %>%
  dplyr::select(c(pragmaid, treat, sample, sim.hosp.FU.days.0, sim.hosp.FU.AUD.days.0)) %>%
  rename("any" = "sim.hosp.FU.days.0", "aud" = "sim.hosp.FU.AUD.days.0") %>% 
  as.data.table()

sim.data35 <- data35[rep(seq_len(nrow(data35)), rep)][, nrep := 1:rep, by = .(pragmaid, sample)]
rm(data35, sample35w)

# get random probability
sim.data35[, prob := runif(nrow(sim.data35))]

# compare random probability with simulated probability of 0 days
sim.data35[, ':=' (any.zero = ifelse(as.numeric(any) > as.numeric(prob), 1, 0), aud.zero = ifelse(as.numeric(aud) > as.numeric(prob), 1, 0))]

# summarise
sim.data35 <- sim.data35 %>%
  group_by(nrep, sample) %>%
  summarise(zero.stays = mean(any.zero),
            zero.AUD.stays = mean(aud.zero)) %>% ungroup() %>%
  mutate(scenario = "35% Reha")

saveRDS(sim.data35, paste0("Data/preprocessed data/", Sys.Date(), "_Reha35_Sim1000runs.RDS"))
rm(sim.data35)

# Scenario 3: 50% in REHA

data50 <- data.frame()
ids <- c()
i <- 1
k <- 1

while (i<=rep) {
  
  print(i)
  set.seed(i)
  
  wdata <- exp(rnorm(round(0.5 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                     mean = m.dist, sd = sd.dist))
  
  while (k <= length(wdata)) {
    #foreach(k=1:length(wdata)) %do% {
    weight.select <- wdata[k]
    add <- data %>% filter(treat != "reha") %>% 
      mutate(diff = abs(weights - weight.select)) %>% 
      filter(diff == min(diff)) %>% pull(pragmaid)
    ids <- c(ids, add)
    k <- k+1
  } 
  
  sdata <- unique(ids) %>% as.data.frame() %>%
    sample_n(., round(0.5 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
    pull(.)
  
  add <- data %>% filter(pragmaid %in% sdata) %>% mutate(treat = "reha")
  sample50w <- data %>% filter(!pragmaid %in% sdata) %>% rbind(., add) %>% 
    mutate(sample = i)
  data50 <- rbind(data50, sample50w)
  
  i <- i+1
  
}

prop.table(table(data50$treat)) # 50% in Reha

# predict

set.seed(1234)
data50 <- cbind(data50, 
                sim.hosp.FU.days = predict(mHospDays, data50, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data50, type = c("prob"))) %>%
  dplyr::select(c(pragmaid, treat, sample, sim.hosp.FU.days.0, sim.hosp.FU.AUD.days.0)) %>%
  rename("any" = "sim.hosp.FU.days.0", "aud" = "sim.hosp.FU.AUD.days.0") %>%
  as.data.table()

sim.data50 <- data50[rep(seq_len(nrow(data50)), rep)][, nrep := 1:rep, by = .(pragmaid, sample)]
rm(data50, sample50w)

# get random probability
sim.data50[, prob := runif(nrow(sim.data50))]

# compare random probability with simulated probability of 0 days
sim.data50[, ':=' (any.zero = ifelse(as.numeric(any) > as.numeric(prob), 1, 0), aud.zero = ifelse(as.numeric(aud) > as.numeric(prob), 1, 0))]

# summarise
sim.data50 <- sim.data50 %>%
  group_by(nrep, sample) %>%
  summarise(zero.stays = mean(any.zero),
            zero.AUD.stays = mean(aud.zero)) %>% ungroup() %>%
  mutate(scenario = "50% Reha")

saveRDS(sim.data50, paste0("Data/preprocessed data/", Sys.Date(), "_Reha50_Sim1000runs.RDS"))

