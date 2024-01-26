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





#############################################
## SCENARIO 2
#############################################

## Load in the data
data <- readRDS('scenario2.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario2.rds")



#############################################
## SCENARIO 3
#############################################

## Load in the data
data <- readRDS('scenario3.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario3.rds")




#############################################
## SCENARIO 4
#############################################

## Load in the data
data <- readRDS('scenario4.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario4.rds")





#############################################
## SCENARIO 5
#############################################

## Load in the data
data <- readRDS('scenario5.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario5.rds")





#############################################
## SCENARIO 6
#############################################

## Load in the data
data <- readRDS('scenario6.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario6.rds")




#############################################
## SCENARIO 7
#############################################

## Load in the data
data <- readRDS('scenario7.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario7.rds")





#############################################
## SCENARIO 8
#############################################

## Load in the data
data <- readRDS('scenario8.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario8.rds")





#############################################
## SCENARIO 9
#############################################

## Load in the data
data <- readRDS('scenario9.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario9.rds")





#############################################
## SCENARIO 10
#############################################

## Load in the data
data <- readRDS('scenario10.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario10.rds")





#############################################
## SCENARIO 11
#############################################

## Load in the data
data <- readRDS('scenario6.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario11.rds")






#############################################
## SCENARIO 12
#############################################

## Load in the data
data <- readRDS('scenario6.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario12.rds")











