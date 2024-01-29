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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)





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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)






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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)





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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)






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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)







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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)




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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)






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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)





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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)





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

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)





#############################################
## SCENARIO 11
#############################################

## Load in the data
data <- readRDS('scenario11.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario11.rds")

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)






#############################################
## SCENARIO 12
#############################################

## Load in the data
data <- readRDS('scenario12.rds')

# Clean the data for analyses
trial <- trial_cohort(data)

## Analyze the cleaned data
analyses <- clean_analyze(trial)

## Select the information of-interest
select_save <- analyses %>% 
  select(-c(data))

## Save the RDS
saveRDS(select_save, "analyses_scenario12.rds")

## Remove datasets from the environment to ensure no issues

data_delete <- c("data", "trial", "analyses", "select_save")

rm(list = data_delete)




#############################################
## GET SAMPLE SIZES OF TRIAL COHORTS FROM 
## EACH SCENARIO
#############################################

## Scenario1
data <- readRDS('scenario1.rds')
trial <- trial_cohort(data)
n_s1 <- nrow(trial)
n_s1 # 518,444

## Scenario2
data <- readRDS('scenario2.rds')
trial <- trial_cohort(data)
n_s2 <- nrow(trial)
n_s2 # 518,444

## ALL HAVE THE SAME AMOUNT
## -- As to be expected because the fetal death probabilities
## -- are the same in the first 4 weeks of gestation and
## -- we used the same seed for all simulations.




