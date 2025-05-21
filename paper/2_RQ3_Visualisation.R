######################################
## Project: PRAGMA — RQ3            ##
## Author: Carolin Kilian           ##
## Start date: 25/06/2024           ##
## Date last changed: 01/10/2024    ##
######################################

## VISUALISATION

# Libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpattern)
library(janitor)
library(Hmisc)
library(lubridate)

# Load settings

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Pragma/")

# Load data

match.data <- readRDS("Data/preprocessed data/2024-10-01_RQ3_AllDat_Matched.RDS")

# Layout for visualizations

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=14, color = "black"),
        strip.text = element_text(size=14),
        title = element_text(size=14),
        axis.title = element_text(size=14, color = "black"),
        legend.position = "right")

# Functions

wtd.lci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  sd   <- sqrt(wtd.var(x, w))
  se   <- sd/sqrt(length(x))
  lci   <- mean + qt(0.025,n-1)*se 
  return(lci)
}

wtd.uci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  sd   <- sqrt(wtd.var(x, w))
  se   <- sd/sqrt(length(x))
  uci   <- mean + qt(0.975,n-1)*se 
  return(uci)
}

## ----------------------------------------------------------------
## EXAMPLE TREATMENT HISTORY
## ----------------------------------------------------------------

# select one patient per group with hightest weight 

pdat <- match.data %>% group_by(treat) %>% filter(sex == "male" & age.aud > 45 & age.aud < 49 & y.aud == 2017) %>%  
  mutate(max.weight = max(weights)) %>% filter(weights == max.weight) %>% 
  mutate(date.aud = date.aud, 
            date.aud.12mPRE = date.aud.12mPRE,
            date.FU.start = case_when(
              treat == "inpat" ~ date.inpat.end + ddays(1),
              treat == "qwt" ~ date.qwt.end + ddays(1),
              treat == "reha" ~ date.reha.end + ddays(1),
              TRUE ~ NA),
            date.FU.end = case_when(
              treat == "inpat" ~ date.inpat.end + dyears(1) + ddays(1),
              treat == "qwt" ~ date.qwt.end + dyears(1) + ddays(1),
              treat == "reha" ~ date.reha.end + dyears(1) + ddays(1),
              TRUE ~ NA),
            ymin = case_when(
              treat == "inpat" ~ 0.7,
              treat == "qwt" ~ 0.4,
              treat == "reha" ~ 0.1,
              TRUE ~ NA),
            ymax = case_when(
              treat == "inpat" ~ 0.9,
              treat == "qwt" ~ 0.6,
              treat == "reha" ~ 0.3,
              TRUE ~ NA)) %>% 
  mutate_at(c("date.aud", "date.aud.12mPRE", "date.FU.start", "date.FU.end", 
              "date.inpat.start", "date.inpat.end", "date.qwt.start", "date.qwt.end",
              "date.reha.start", "date.reha.end"), as.Date)


ggplot(pdat) + 
  ggtitle("Exemplary study periods for three patients in each treatment group",
          "Patients are male, 45-49 years old, and were first diagnosed with AUD in 2017") +
  geom_rect(aes(xmin = date.aud.12mPRE, xmax = date.aud, ymin = ymin, ymax = ymax, 
                group = pragmaid), fill = "#E7C9B7", color = "white", alpha = 0.5, show.legend = F) +
  geom_rect(aes(xmin = date.aud, xmax = date.aud + ddays(5), ymin = ymin, ymax = ymax, 
                group = pragmaid), fill = "#A21100", color = "white", alpha = 1, show.legend = F) +
  geom_rect(aes(xmin = date.inpat.start, xmax = date.inpat.end, ymin = ymin, ymax = ymax, 
                group = pragmaid), fill = "#0F9ED5", color = "white", alpha = 1, show.legend = F) +
  geom_rect(aes(xmin = date.qwt.start, xmax = date.qwt.end, ymin = ymin, ymax = ymax, 
                group = pragmaid), fill = "#196B24", color = "white", alpha = 1, show.legend = F) +
  geom_rect(aes(xmin = date.reha.start, xmax = date.reha.end, ymin = ymin, ymax = ymax, 
                group = pragmaid), fill = "#B4E4A2", color = "white", alpha = 1, show.legend = F) +
  geom_rect_pattern(aes(xmin = date.FU.start, xmax = date.FU.end, ymin = ymin, ymax = ymax, group = pragmaid), 
                    pattern = "stripe", pattern_fill = "grey", pattern_colour = "grey", pattern_spacing = 0.01,
                    fill = "white", color = "white", alpha = 1, show.legend = F) +
  scale_y_continuous(breaks=c(0.2, 0.5, 0.8), 
                     labels=c("Rehabilitation\ntreatment (with\nprior qualified\nwithdrawal\ntreatment)", "Qualified\nwithdrawal\ntreatment", "Inpatient\nalcohol\ntreatment")) + 
  scale_x_date(limits = as.Date(c("2015-12-31", "2020-01-01"))) + 
  theme_barplot 

#ggsave(paste0("Output/Leitliniengerechte Versorgung/", Sys.Date(), "_Fig2_TreatPaths.jpg"), dpi=300, width = 12, height = 4)

## ----------------------------------------------------------------
## COMPARE OUTCOMES BY TREATMENT GROUPS 
## ----------------------------------------------------------------

pdat <- match.data %>% 
  dplyr::select(c("treat", "hosp.FU.days", "hosp.FU.AUD.days", "weights")) %>%
  pivot_longer(cols = c("hosp.FU.days", "hosp.FU.AUD.days"), 
               names_to = "outcome", values_to = "days") %>% 
  mutate(time_0 = ifelse(days == 0, 1, 0), 
         time_1to7 = ifelse(days > 0 & days <= 7, 1, 0),
         time_8to21 = ifelse(days > 7 & days <= 21, 1, 0),
         time_22to42 = ifelse(days > 21 & days <= 42, 1, 0), 
         time_43to84 = ifelse(days > 42 & days <= 84, 1, 0),
         time_85to182 = ifelse(days > 85 & days <= 182, 1, 0),
         outcome = factor(ifelse(outcome == "hosp.FU.days", "All cause", 
                                 ifelse(outcome == "hosp.FU.AUD.days", "Alcohol-specific", NA)), 
                          levels = c("All cause", "Alcohol-specific"))) %>%
  group_by(treat, outcome) %>%
  summarise(prop_0 = wtd.mean(time_0, weights), lci_0 = wtd.lci(time_0, weights), uci_0 = wtd.uci(time_0, weights),
            prop_1to7 = wtd.mean(time_1to7, weights), lci_1to7 = wtd.lci(time_1to7, weights), uci_1to7 = wtd.uci(time_1to7, weights),
            prop_8to21 = wtd.mean(time_8to21, weights), lci_8to21 = wtd.lci(time_8to21, weights), uci_8to21 = wtd.uci(time_8to21, weights),
            prop_22to42 = wtd.mean(time_22to42, weights), lci_22to42 = wtd.lci(time_22to42, weights), uci_22to42 = wtd.uci(time_22to42, weights),
            prop_43to84 = wtd.mean(time_43to84, weights), lci_43to84 = wtd.lci(time_43to84, weights), uci_43to84 = wtd.uci(time_43to84, weights),
            prop_85to182 = wtd.mean(time_85to182, weights), lci_85to182 = wtd.lci(time_85to182, weights), uci_85to182 = wtd.uci(time_85to182, weights)) %>%
  pivot_longer(cols = -c("treat", "outcome"), 
               names_to = "measure", values_to = "estimate") %>%
  separate(measure, sep = "_", into = c("ind", "time")) %>%
  pivot_wider(id_cols = c("treat", "outcome", "time"), names_from = "ind", values_from = "estimate") %>%
  mutate(time = factor(time, levels = c("0", "1to7", "8to21", "22to42", "43to84", "85to182"),
                       labels = c("0", "1-7", "8-21", "22-42", "43-84", "85-182")),
         lci = ifelse(lci < 0, 0, lci))


ggplot(pdat, aes(x = time, y = prop)) + 
  geom_bar(aes(group = as.factor(treat), fill = as.factor(treat)), stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = lci, ymax = uci, group = as.factor(treat)), width = .2, position = position_dodge(.9)) +
  facet_grid(cols = vars(outcome)) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "\nDays in hospital during follow-up", y = "Weighted proportion of patients\n", fill = "") +
  scale_fill_manual(values = c("#0E4D64", "#1D9A6C", "#99D492"),
                    labels = c("IAT", "QWT", "Reha")) +
  theme_barplot

#ggsave(paste0("Output/Leitliniengerechte Versorgung/", Sys.Date(), "_Fig3_OutcomeDist.jpg"), dpi=300, width = 12, height = 6)

