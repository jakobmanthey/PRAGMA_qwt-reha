######################################
## Project: PRAGMA — RQ5            ##
## Author: Carolin Kilian           ##
## Start date: 03/09/2024           ##
## Date last changed: 01/10/2024    ##
######################################

## DATA ANALYSIS

# Load packages

library(dplyr)
library(tidyverse)
library(data.table)
library(pscl)
library(beepr)
library(doParallel)
library(writexl)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

match.data <- readRDS("Data/preprocessed data/2024-10-01_RQ3_AllDat_Matched.RDS")
sim.data20 <- readRDS("Data/preprocessed data/2024-11-06_Reha20_Sim1000runs.RDS")
sim.data35 <- readRDS("Data/preprocessed data/2024-11-06_Reha35_Sim1000runs.RDS")
sim.data50 <- readRDS("Data/preprocessed data/2024-11-06_Reha50_Sim1000runs.RDS")

# Set number of repetitions

rep <- 1000
set.seed(1234)

# Ggplot design

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=14, color = "black"),
        strip.text = element_text(size=14),
        title = element_text(size=14),
        axis.title = element_text(size=14, color = "black"),
        legend.position = "none")
col <- colorRampPalette(c("#DEF0D2", "#357052"))
c4 <- col(4)
c3 <- c4[2:4]
  
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

# get average number of days in hospital for those being re-hospitalised during follow-up
data %>% filter(hosp.FU.days > 0) %>% 
  summarise(any = wtd.mean(hosp.FU.days, weights))
data %>% filter(hosp.FU.AUD.days > 0) %>% 
  summarise(any = wtd.mean(hosp.FU.AUD.days, weights))

# get zero-inflated models
mHospDays <- zeroinfl(hosp.FU.days ~ treat + time.aud.treat + inpat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat + time.aud.treat + inpat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospAUDDays)

# Reference scenario / Original model
data <- cbind(data, 
                sim.hosp.FU.days = predict(mHospDays, data, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data, type = c("prob"))) %>%
  dplyr::select(c(pragmaid, treat, sim.hosp.FU.days.0, sim.hosp.FU.AUD.days.0)) %>%
  rename("any" = "sim.hosp.FU.days.0", "aud" = "sim.hosp.FU.AUD.days.0")

sim.data <- data %>% expand(data, nrep=1:rep) %>% 
  
  # get random probability
  mutate(prob = runif(nrow(.)), 
         
         # compare random probability with simulated probability of 0 days
         any.zero = ifelse(as.numeric(any) > as.numeric(prob), 1, 0),
         aud.zero = ifelse(as.numeric(aud) > as.numeric(prob), 1, 0)) %>%
  
  # summarise
  group_by(nrep) %>%
  summarise(zero.stays = mean(any.zero),
            zero.AUD.stays = mean(aud.zero)) %>% ungroup() %>%
  mutate(scenario = "Reference", sample = 0)

# get the new data
prop.table(table(data$treat))

## ----------------------------------------------------------------
## SUMMARY ZERO COMPONENT
## ----------------------------------------------------------------

# reference scenario
pdat.zero <- sim.data %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "Reference")

# extract data 20% scenario
pdat.zero20 <- sim.data20 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "20% Reha")

# extract data 35% scenario
pdat.zero35 <- sim.data35 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "35% Reha")

# extract data 50% scenario
pdat.zero50 <- sim.data50 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "50% Reha")

# test for difference

pdat <- rbind(sim.data, sim.data20, sim.data35, sim.data50) %>%
  mutate(scenario = factor(scenario, levels = c("Reference", "20% Reha", "35% Reha", "50% Reha")))

m1 <- glm(zero.stays ~ scenario, data = pdat, family = gaussian)
summary(m1)

m2 <- glm(zero.AUD.stays ~ scenario, data = pdat, family = gaussian)
summary(m2)

out_reg <- rbind(cbind(beta = coef(m1), confint(m1), 
                       pval = coef(summary(m1))[,'Pr(>|t|)'], "all-cause"),
                 cbind(beta = coef(m2), confint(m2), 
                       pval = coef(summary(m2))[,'Pr(>|t|)'], "alcohol-specific")) %>% as.data.frame()

# output: absolute proportion of patients with zero hospital days

pdat <- rbind(pdat.zero, pdat.zero20, pdat.zero35, pdat.zero50) %>%
  pivot_longer(cols = c(zero.stays_mean, zero.stays_lci, zero.stays_uci,
                        zero.AUD.stays_mean, zero.AUD.stays_lci, zero.AUD.stays_uci),
               names_to = "var", values_to = "value") %>% 
  separate(var, c("var", "measure"), sep = "_") %>%
  pivot_wider(names_from = "measure", values_from = "value") %>%
  mutate(scenario = factor(scenario, 
                           levels = c("Reference", "20% Reha", "35% Reha", "50% Reha"),
                           labels = c("Reference", "20%", "35%", "50%")),
         var = factor(var, levels = c("zero.stays", "zero.AUD.stays"),
                      labels = c("All cause", "Alcohol-specific")))
  
ggplot(pdat, aes(x = scenario, y = mean)) + 
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = lci, ymax = uci), position = "dodge2") +
  facet_grid(cols = vars(var)) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "Proportion of patients with zero hospital days\n", fill = "") +
  scale_fill_manual(values = c4) + theme_barplot
ggsave(paste0("Output/Modelling/", Sys.Date(), "/Fig1_ZeroHosp.jpg"), height = 5, width = 10)

# output: total number without hospital days / total number of hospital submissions avoided

out_abs <- pdat %>% 
  mutate(n_aud = length(unique(data$pragmaid)),
         n_ZeroHosp = mean * n_aud,
         n_ZeroHosp_lci = lci * n_aud,
         n_ZeroHosp_uci = uci * n_aud) %>% as.data.frame()

# output: PP difference in proportion of patients with zero hospital days

pdat <- rbind(sim.data, sim.data20, sim.data35, sim.data50) %>%
  pivot_longer(cols = c(zero.stays, zero.AUD.stays),
               names_to = "var", values_to = "value") 

ref <- pdat %>% filter(scenario == "Reference") %>% 
  rename("valueref" = "value") %>% dplyr::select(c(nrep, var, valueref))

out_diff <- pdat %>% filter(scenario != "Reference") %>% 
  left_join(., ref) %>% 
  mutate(diff = value - valueref,
         perc = (value - valueref) / value) %>% 
  group_by(scenario, var) %>% 
  summarise(diff_mean = mean(diff),
            diff_lci = quantile(diff, 0.025),
            diff_uci = quantile(diff, 0.975),
            perc_mean = mean(perc),
            perc_lci = quantile(perc, 0.025),
            perc_uci = quantile(perc, 0.975)) %>% as.data.frame()

# save output

list <- list(out_abs, out_diff, out_reg)
write_xlsx(list, paste0("Output/Modelling/", Sys.Date(), "/Tab3_HospAvoid.xlsx"))
