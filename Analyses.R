##############################################
# Program: Analyses.R
# Programmer: Chase Latour
# Date Last Modified:
#
# Purpose: Run the analyses for the different
# scenarios.
##############################################

## Load in the Libraries that need
library(tidyverse)
library(survival)
library(Epi)
library(kableExtra)

## Load in the functions that need.
source('analysis functions.R')

#############################################
## SCENARIO 1
#############################################

## Load in the data
data <- readRDS('scenario1.rds')
# For testing: data <- readRDS('test.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario1.rds")