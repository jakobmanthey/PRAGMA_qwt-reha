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
## PREAPRE
## ----------------------------------------------------------------

# get original data
data <- match.data %>% 
  mutate(treat = factor(treat, levels=c("qwt","inpat","reha")),
         hosp.FU.AUD.days = as.integer(hosp.FU.AUD.days),
         hosp.FU.days = as.integer(hosp.FU.days),
         inpat = factor(ifelse(inpat_num <= 1, "0/1 inpat", 
                               ifelse(inpat_num > 1, "2+ inpat", NA)))) %>% 
  dplyr::select(c("pragmaid", "date.aud", "sex", "yob", 
           "treat", "inpat", "time.aud.treat", "time.treat",
           "hosp.FU.times", "hosp.FU.days", "hosp.FU.AUD.times", "hosp.FU.AUD.days",
           "weights"))

# get zero-inflated models
mHospDays <- zeroinfl(hosp.FU.days ~ treat + time.aud.treat + inpat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospDays)

mHospAUDDays <- zeroinfl(hosp.FU.AUD.days ~ treat + time.aud.treat + inpat, data = data, 
                      dist = "negbin", weights = weights)
summary(mHospAUDDays)

# get new data
prop.table(table(data$treat))

# distribution of weights by treatment group
ggplot(data, aes(x = weights)) + geom_histogram() +
  facet_grid(cols = vars(treat))
ggplot(data[data$treat != "reha",], aes(x = log(weights))) + geom_histogram() 

m.dist <- data %>% filter(treat != "reha") %>% summarise(mean(log(weights))) %>% pull()
sd.dist <- data %>% filter(treat != "reha") %>% summarise(sd(log(weights))) %>% pull()

# Scenario 1: 20% in REHA
sample20w <- foreach(i=1:100, .inorder=TRUE) %do% {   
  
  print(i)
  set.seed(i)
  wdata <- exp(rnorm(round(0.2 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                      mean = m.dist, sd = sd.dist))
  
  ids <- foreach(k=1:length(wdata)) %do% {
    weight.select <- wdata[k]
    data %>% filter(treat != "reha") %>% 
      mutate(diff = abs(weights - weight.select)) %>% 
      filter(diff == min(diff)) %>% pull(pragmaid)
    } 
  
  sdata <- unique(unlist(ids)) %>% as.data.frame() %>%
    sample_n(., round(0.2 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
    pull(.)
  
  add <- data %>% filter(pragmaid %in% sdata) %>% mutate(treat = "reha")
  data20 <- data %>% filter(!pragmaid %in% sdata) %>% rbind(., add) %>% 
    mutate(sample = i)

}

data20 <- do.call(rbind, sample20w)
prop.table(table(data20$treat)) # 20% in Reha

# Scenario 2: 35% in REHA
sample35w <- exp(rnorm(round(0.35 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                       mean = m.dist, sd = sd.dist))

sample35id <- foreach(i=1:length(sample35w)) %do% {
  weight.select <- sample35w[i]
  data %>% filter(treat != "reha") %>% 
    mutate(diff = abs(weights - weight.select)) %>% 
    filter(diff == min(diff)) %>% pull(pragmaid)
} 

sample35 <- unique(unlist(sample35id)) %>% as.data.frame() %>%
  sample_n(., round(0.35 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
  pull(.)

add35 <- data %>% filter(pragmaid %in% sample35) %>% mutate(treat = "reha")
data35 <- data %>% filter(!pragmaid %in% sample35) %>% rbind(., add35)
prop.table(table(data35$treat)) # 35% in Reha

# Scenario 3: 50% in REHA
sample50w <- exp(rnorm(round(0.5 * nrow(data) - nrow(data[data$treat == "reha",]), 0),
                       mean = m.dist, sd = sd.dist))

sample50id <- foreach(i=1:length(sample50w)) %do% {
  weight.select <- sample50w[i]
  data %>% filter(treat != "reha") %>% 
    mutate(diff = abs(weights - weight.select)) %>% 
    filter(diff == min(diff)) %>% pull(pragmaid)
} 

sample50 <- unique(unlist(sample50id)) %>% as.data.frame() %>%
  sample_n(., round(0.5 * nrow(data) - nrow(data[data$treat == "reha",]), 0)) %>% 
  pull(.)

add50 <- data %>% filter(pragmaid %in% sample50) %>% mutate(treat = "reha")
data50 <- data %>% filter(!pragmaid %in% sample50) %>% rbind(., add50)
prop.table(table(data50$treat)) # 50% in Reha

## ----------------------------------------------------------------
## MODELLING
## ----------------------------------------------------------------
# https://stackoverflow.com/questions/22314921/no-zeros-predicted-from-zeroinfl-object-in-r

# Scenario 0 / Original model
data <- cbind(data, 
              sim.hosp.FU.days = predict(mHospDays, data, type = c("prob")),
              sim.hosp.FU.AUD.days = predict(mHospAUDDays, data, type = c("prob"))) 

sim.data <- foreach(i=1:1000, .inorder=TRUE) %do% {
  
  print(paste0("Reference: ", i))
  set.seed(i)
  
  data %>% 
    
    # get random probability
    mutate(prob = runif(nrow(.))) %>% 
    pivot_longer(cols = 14:(ncol(.)-1), names_to = "sim.days", values_to = "sim.prob") %>%
    mutate(var = ifelse(sim.days %like% "sim.hosp.FU.days", "any", 
                        ifelse(sim.days %like% "sim.hosp.FU.AUD.days", "AUD", NA)),
           sim.days = ifelse(var == "any", gsub("sim.hosp.FU.days.", "", sim.days),
                             ifelse(var == "AUD", gsub("sim.hosp.FU.AUD.days.", "", sim.days), NA))) %>% 
    pivot_wider(names_from = "var", values_from = "sim.prob") %>% 
    
    # get cumulative sum of days in hospital
    group_by(pragmaid, treat) %>%
    mutate(any.csum = cumsum(any),
           aud.csum = cumsum(AUD),
           
           # compare random probability with cumulative sum of days in hospital
           any.select = ifelse(as.numeric(any.csum) > as.numeric(prob), 0, 1),
           aud.select = ifelse(as.numeric(aud.csum) > as.numeric(prob), 0, 1)) %>% 
    
    # count days in hospital 
    summarise(nrep = i,
              sim.hosp.FU.days = sum(any.select, na.rm = T) - 1,
              sim.hosp.FU.days = ifelse(sim.hosp.FU.days < 0, 0, sim.hosp.FU.days),
              sim.hosp.FU.AUD.days = sum(aud.select, na.rm = T) - 1,
              sim.hosp.FU.AUD.days = ifelse(sim.hosp.FU.AUD.days < 0, 0, sim.hosp.FU.AUD.days))
  
}

# Scenario 1: 20% in REHA
data20 <- cbind(data20, 
                sim.hosp.FU.days = predict(mHospDays, data20, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data20, type = c("prob"))) 

# drop foreach loop and generate large file to run across
# get large dataframe with 1000 seeds
sim.data20 <- foreach(i=1:100, .inorder=TRUE) %do% {
  
  print(paste0("Modelling 20%: ", i))
  set.seed(i)
  
  data20 %>% 
    
    # get random probability
    mutate(prob = runif(nrow(.))) %>% 
    pivot_longer(cols = 15:(ncol(.)-1), names_to = "sim.days", values_to = "sim.prob") %>%
    mutate(var = ifelse(sim.days %like% "sim.hosp.FU.days", "any", 
                        ifelse(sim.days %like% "sim.hosp.FU.AUD.days", "AUD", NA)),
           sim.days = ifelse(var == "any", gsub("sim.hosp.FU.days.", "", sim.days),
                             ifelse(var == "AUD", gsub("sim.hosp.FU.AUD.days.", "", sim.days), NA))) %>% 
    pivot_wider(names_from = "var", values_from = "sim.prob") %>% 
    
    # get cumulative sum of days in hospital
    group_by(pragmaid, treat, sample) %>%
    mutate(any.csum = cumsum(any),
           aud.csum = cumsum(AUD),
           
           # compare random probability with cumulative sum of days in hospital
           any.select = ifelse(as.numeric(any.csum) > as.numeric(prob), 0, 1),
           aud.select = ifelse(as.numeric(aud.csum) > as.numeric(prob), 0, 1)) %>% 
                      
           # count days in hospital 
           summarise(nrep = i,
                     sample = mean(sample), 
                     sim.hosp.FU.days = sum(any.select, na.rm = T) - 1,
                     sim.hosp.FU.days = ifelse(sim.hosp.FU.days < 0, 0, sim.hosp.FU.days),
                     sim.hosp.FU.AUD.days = sum(aud.select, na.rm = T) - 1,
                     sim.hosp.FU.AUD.days = ifelse(sim.hosp.FU.AUD.days < 0, 0, sim.hosp.FU.AUD.days))
  
}

# Scenario 2: 35% in REHA
data35 <- cbind(data35, 
                sim.hosp.FU.days = predict(mHospDays, data35, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data35, type = c("prob"))) 

sim.data35 <- foreach(i=1:1000, .inorder=TRUE) %do% {
  
  print(paste0("Modelling 35%: ", i))
  set.seed(i)
  
  data35 %>% 
    
    # get random probability
    mutate(prob = runif(nrow(.))) %>% 
    pivot_longer(cols = 14:(ncol(.)-1), names_to = "sim.days", values_to = "sim.prob") %>%
    mutate(var = ifelse(sim.days %like% "sim.hosp.FU.days", "any", 
                        ifelse(sim.days %like% "sim.hosp.FU.AUD.days", "AUD", NA)),
           sim.days = ifelse(var == "any", gsub("sim.hosp.FU.days.", "", sim.days),
                             ifelse(var == "AUD", gsub("sim.hosp.FU.AUD.days.", "", sim.days), NA))) %>% 
    pivot_wider(names_from = "var", values_from = "sim.prob") %>% 
    
    # get cumulative sum of days in hospital
    group_by(pragmaid, treat) %>%
    mutate(any.csum = cumsum(any),
           aud.csum = cumsum(AUD),
           
           # compare random probability with cumulative sum of days in hospital
           any.select = ifelse(as.numeric(any.csum) > as.numeric(prob), 0, 1),
           aud.select = ifelse(as.numeric(aud.csum) > as.numeric(prob), 0, 1)) %>% 
    
    # count days in hospital 
    summarise(nrep = i,
              sim.hosp.FU.days = sum(any.select, na.rm = T) - 1,
              sim.hosp.FU.days = ifelse(sim.hosp.FU.days < 0, 0, sim.hosp.FU.days),
              sim.hosp.FU.AUD.days = sum(aud.select, na.rm = T) - 1,
              sim.hosp.FU.AUD.days = ifelse(sim.hosp.FU.AUD.days < 0, 0, sim.hosp.FU.AUD.days))
  
}

# Scenario 3: 50% in REHA
data50 <- cbind(data50, 
                sim.hosp.FU.days = predict(mHospDays, data50, type = c("prob")),
                sim.hosp.FU.AUD.days = predict(mHospAUDDays, data50, type = c("prob"))) 

sim.data50 <- foreach(i=1:1000, .inorder=TRUE) %do% {
  
  print(paste0("Modelling 50%: ", i))
  set.seed(i)
  
  data50 %>% 
    
    # get random probability
    mutate(prob = runif(nrow(.))) %>% 
    pivot_longer(cols = 14:(ncol(.)-1), names_to = "sim.days", values_to = "sim.prob") %>%
    mutate(var = ifelse(sim.days %like% "sim.hosp.FU.days", "any", 
                        ifelse(sim.days %like% "sim.hosp.FU.AUD.days", "AUD", NA)),
           sim.days = ifelse(var == "any", gsub("sim.hosp.FU.days.", "", sim.days),
                             ifelse(var == "AUD", gsub("sim.hosp.FU.AUD.days.", "", sim.days), NA))) %>% 
    pivot_wider(names_from = "var", values_from = "sim.prob") %>% 
    
    # get cumulative sum of days in hospital
    group_by(pragmaid, treat) %>%
    mutate(any.csum = cumsum(any),
           aud.csum = cumsum(AUD),
           
           # compare random probability with cumulative sum of days in hospital
           any.select = ifelse(as.numeric(any.csum) > as.numeric(prob), 0, 1),
           aud.select = ifelse(as.numeric(aud.csum) > as.numeric(prob), 0, 1)) %>% 
    
    # count days in hospital 
    summarise(nrep = i,
              sim.hosp.FU.days = sum(any.select, na.rm = T) - 1,
              sim.hosp.FU.days = ifelse(sim.hosp.FU.days < 0, 0, sim.hosp.FU.days),
              sim.hosp.FU.AUD.days = sum(aud.select, na.rm = T) - 1,
              sim.hosp.FU.AUD.days = ifelse(sim.hosp.FU.AUD.days < 0, 0, sim.hosp.FU.AUD.days))
  
}

list <- list(sim.data, sim.data20, sim.data35, sim.data50)
saveRDS(list, paste0("Output/Modelling/", Sys.Date(), "/Sim1000runs.RDS"))

## ----------------------------------------------------------------
## SUMMARY ZERO COMPONENT
## ----------------------------------------------------------------

# reference scenario
zero.sim.data <- do.call(rbind, sim.data) %>% as.data.frame() %>% 
  mutate(zero.stays = ifelse(sim.hosp.FU.days == 0, 1, 0),
         zero.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, 1, 0)) %>% 
  group_by(nrep) %>%
  summarise(zero.stays = mean(zero.stays),
            zero.AUD.stays = mean(zero.AUD.stays)) %>% ungroup() %>%
  mutate(scenario = "Reference")

pdat.zero <- zero.sim.data %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "Reference")

# extract data 20% scenario
zero.sim.data20 <- do.call(rbind, sim.data20) %>% as.data.frame() %>% 
  mutate(zero.stays = ifelse(sim.hosp.FU.days == 0, 1, 0),
         zero.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, 1, 0)) %>% 
  group_by(sample, nrep) %>%
  summarise(zero.stays = mean(zero.stays),
            zero.AUD.stays = mean(zero.AUD.stays)) %>% ungroup() %>%
  mutate(scenario = "20% Reha")

pdat.zero20 <- zero.sim.data20 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "20% Reha")

# extract data 35% scenario
zero.sim.data35 <- do.call(rbind, sim.data35) %>% as.data.frame() %>% 
  mutate(zero.stays = ifelse(sim.hosp.FU.days == 0, 1, 0),
         zero.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, 1, 0)) %>% 
  group_by(nrep) %>%
  summarise(zero.stays = mean(zero.stays),
            zero.AUD.stays = mean(zero.AUD.stays)) %>% ungroup() %>%
  mutate(scenario = "35% Reha")

pdat.zero35 <- zero.sim.data35 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "35% Reha")

# extract data 50% scenario
zero.sim.data50 <- do.call(rbind, sim.data50) %>% as.data.frame() %>% 
  mutate(zero.stays = ifelse(sim.hosp.FU.days == 0, 1, 0),
         zero.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, 1, 0)) %>% 
  group_by(nrep) %>%
  summarise(zero.stays = mean(zero.stays),
            zero.AUD.stays = mean(zero.AUD.stays)) %>% ungroup() %>%
  mutate(scenario = "50% Reha")

pdat.zero50 <- zero.sim.data50 %>%
  summarise(zero.stays_mean = mean(zero.stays),
            zero.stays_lci = quantile(zero.stays, 0.025),
            zero.stays_uci = quantile(zero.stays, 0.975),
            zero.AUD.stays_mean = mean(zero.AUD.stays),
            zero.AUD.stays_lci = quantile(zero.AUD.stays, 0.025),
            zero.AUD.stays_uci = quantile(zero.AUD.stays, 0.975),
            scenario = "50% Reha")

# test for difference

pdat <- rbind(zero.sim.data, zero.sim.data20, zero.sim.data35, zero.sim.data50) %>%
  mutate(scenario = factor(scenario, levels = c("Reference", "20% Reha", "35% Reha", "50% Reha")))

m1 <- glm(zero.stays ~ scenario, data = pdat, family = gaussian)
summary(m1)

m2 <- glm(zero.AUD.stays ~ scenario, data = pdat, family = gaussian)
summary(m2)

out_reg <- rbind(cbind(beta = coef(m1), confint(m1), 
                       pval = coef(summary(m1))[,'Pr(>|t|)'], "all-cause"),
                 cbind(beta = coef(m2), confint(m2), 
                       pval = coef(summary(m2))[,'Pr(>|t|)'], "alcohol-specific")) %>% as.data.frame()

# plot 1) absolute % with zero hospital days

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

ref <- out_abs %>% filter(scenario == "Reference") %>% 
  rename("n_ZeroHospRef" = "n_ZeroHosp") %>% dplyr::select(c(var, n_ZeroHospRef))

pdat <- out_abs %>% filter(scenario != "Reference") %>% 
  left_join(., ref) %>%
  mutate(n_HospAvoid = round((n_aud - n_ZeroHospRef) - (n_aud - n_ZeroHosp), 0),
         n_HospAvoid_lci = round((n_aud - n_ZeroHospRef) - (n_aud - n_ZeroHosp_lci), 0),
         n_HospAvoid_uci = round((n_aud - n_ZeroHospRef) - (n_aud - n_ZeroHosp_uci), 0))

ggplot(pdat, aes(x = scenario, y = n_HospAvoid)) + 
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = n_HospAvoid_lci, ymax = n_HospAvoid_uci), position = "dodge2") +
  facet_grid(cols = vars(var)) + 
  labs(x = "", y = "Total number of AUD patients without any hospitalisation (12-months)\n", fill = "") +
  scale_fill_manual(values = c3) + theme_barplot
#ggsave(paste0("Output/Modelling/", Sys.Date(), "/Fig1_HospAvoided.jpg"), height = 6, width = 10)

# plot 2) PP difference in % with zero hospital days

pdat <- rbind(zero.sim.data, zero.sim.data20, zero.sim.data35, zero.sim.data50) %>%
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

ggplot(out_diff, aes(x = scenario, y = diff_mean)) + 
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = diff_lci, ymax = diff_uci), position = "dodge2") +
  facet_grid(cols = vars(var)) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "Percentage point difference in patients with zero hospital days\n", fill = "") +
  scale_fill_manual(values = c3) + theme_barplot
#ggsave(paste0("Output/Modelling/", Sys.Date(), "/Fig1_ZeroHosp_PPDiff.jpg"), height = 6, width = 10)

ggplot(out_diff, aes(x = scenario, y = perc_mean)) + 
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = perc_lci, ymax = perc_uci), position = "dodge2") +
  facet_grid(cols = vars(var)) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "% difference in patients with zero hospital days\n", fill = "") +
  scale_fill_manual(values = c3) + theme_barplot
#ggsave(paste0("Output/Modelling/", Sys.Date(), "/Fig1_ZeroHosp_Perc.jpg"), height = 6, width = 10)

## ----------------------------------------------------------------
## SUMMARY COUNT COMPONENT
## ----------------------------------------------------------------

# reference scenario
count.sim.data <- do.call(rbind, sim.data) %>% as.data.frame() %>% 
  mutate(count.stays = ifelse(sim.hosp.FU.days == 0, NA, sim.hosp.FU.days),
         count.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, NA, sim.hosp.FU.AUD.days)) %>% 
  group_by(nrep) %>%
  summarise(count.stays = mean(count.stays, na.rm = T),
            count.AUD.stays = mean(count.AUD.stays, na.rm = T)) %>% 
  mutate(scenario = "Reference")

pdat.count <- count.sim.data %>% ungroup() %>%
  summarise(count.stays_mean = mean(count.stays),
            count.stays_lci = quantile(count.stays, 0.025),
            count.stays_uci = quantile(count.stays, 0.975),
            count.AUD.stays_mean = mean(count.AUD.stays),
            count.AUD.stays_lci = quantile(count.AUD.stays, 0.025),
            count.AUD.stays_uci = quantile(count.AUD.stays, 0.975),
            scenario = "Reference")

# extract data 20% scenario
count.sim.data20 <- do.call(rbind, sim.data20) %>% as.data.frame() %>% 
  mutate(count.stays = ifelse(sim.hosp.FU.days == 0, NA, sim.hosp.FU.days),
         count.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, NA, sim.hosp.FU.AUD.days)) %>% 
  group_by(nrep) %>%
  summarise(count.stays = mean(count.stays, na.rm = T),
            count.AUD.stays = mean(count.AUD.stays, na.rm = T)) %>%
  mutate(scenario = "20% Reha")

pdat.count20 <- count.sim.data20 %>% ungroup() %>%
  summarise(count.stays_mean = mean(count.stays),
            count.stays_lci = quantile(count.stays, 0.025),
            count.stays_uci = quantile(count.stays, 0.975),
            count.AUD.stays_mean = mean(count.AUD.stays),
            count.AUD.stays_lci = quantile(count.AUD.stays, 0.025),
            count.AUD.stays_uci = quantile(count.AUD.stays, 0.975),
            scenario = "20% Reha")

# extract data 35% scenario
count.sim.data35 <- do.call(rbind, sim.data35) %>% as.data.frame() %>% 
  mutate(count.stays = ifelse(sim.hosp.FU.days == 0, NA, sim.hosp.FU.days),
         count.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, NA, sim.hosp.FU.AUD.days)) %>% 
  group_by(nrep) %>%
  summarise(count.stays = mean(count.stays, na.rm = T),
            count.AUD.stays = mean(count.AUD.stays, na.rm = T)) %>% 
  mutate(scenario = "35% Reha")

pdat.count35 <- count.sim.data35 %>% ungroup() %>%
  summarise(count.stays_mean = mean(count.stays),
            count.stays_lci = quantile(count.stays, 0.025),
            count.stays_uci = quantile(count.stays, 0.975),
            count.AUD.stays_mean = mean(count.AUD.stays),
            count.AUD.stays_lci = quantile(count.AUD.stays, 0.025),
            count.AUD.stays_uci = quantile(count.AUD.stays, 0.975),
            scenario = "35% Reha")

# extract data 50% scenario
count.sim.data50 <- do.call(rbind, sim.data50) %>% as.data.frame() %>% 
  mutate(count.stays = ifelse(sim.hosp.FU.days == 0, NA, sim.hosp.FU.days),
         count.AUD.stays = ifelse(sim.hosp.FU.AUD.days == 0, NA, sim.hosp.FU.AUD.days)) %>% 
  group_by(nrep) %>%
  summarise(count.stays = mean(count.stays, na.rm = T),
            count.AUD.stays = mean(count.AUD.stays, na.rm = T)) %>% 
  mutate(scenario = "50% Reha")

pdat.count50 <- count.sim.data50 %>% ungroup() %>%
  summarise(count.stays_mean = mean(count.stays),
            count.stays_lci = quantile(count.stays, 0.025),
            count.stays_uci = quantile(count.stays, 0.975),
            count.AUD.stays_mean = mean(count.AUD.stays),
            count.AUD.stays_lci = quantile(count.AUD.stays, 0.025),
            count.AUD.stays_uci = quantile(count.AUD.stays, 0.975),
            scenario = "50% Reha")

# plot 1) 

pdat <- rbind(pdat.count, pdat.count20, pdat.count35, pdat.count50) %>% 
  pivot_longer(cols = c(count.stays_mean, count.stays_lci, count.stays_uci,
                        count.AUD.stays_mean, count.AUD.stays_lci, count.AUD.stays_uci),
               names_to = "var", values_to = "value") %>% 
  separate(var, c("var", "measure"), sep = "_") %>% 
  pivot_wider(names_from = "measure", values_from = "value") %>%
  mutate(scenario = factor(scenario, 
                           levels = c("Reference", "20% Reha", "35% Reha", "50% Reha"),
                           labels = c("Reference", "20%", "35%", "50%")),
         var = factor(var, levels = c("count.stays", "count.AUD.stays"),
                      labels = c("all-cause", "alcohol-specific")))

ggplot(pdat, aes(x = scenario, y = mean)) + 
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = lci, ymax = uci), position = "dodge2") +
  facet_grid(cols = vars(var)) +
  labs(x = "", y = "Average number of days in hospital\n", fill = "") +
  scale_fill_manual(values = c4) + theme_barplot
#ggsave(paste0("Output/Modelling/", Sys.Date(), "/Fig2_HospDays.jpg"), height = 6, width = 12)

# save output
list <- list(out_abs, out_diff, out_reg)
#write_xlsx(list, paste0("Output/Modelling/", Sys.Date(), "/Tab3_HospAvoid.xlsx"))
