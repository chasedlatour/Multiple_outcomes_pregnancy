###################################################
# PROGRAM: Analyze resample.R
# PURPOSE: The goal of this program is to run the
# primary analyses in the resampled data.
#
# PROGRAMMER: Chase Latour
###################################################


###################################################
#Load in libraries
###################################################
library(tidyverse)
library(dplyr)
library(purrr)
library(survival)
library(Epi)


###################################################
# Call in the analysis functions
###################################################

source('analysis functions.R')











###################################################
# Loop through scenarios 1 to 12
###################################################
for (i in 1:12) {
  
  scenario_name <- paste0("resample_scenario", i)
  resample_name <- paste0("resample_", scenario_name, "_anal.rds")
  
  # Pull in the generated data
  scenario <- readRDS(paste0(scenario_name, ".rds"))
  
  # Set seed so that it's the same for all. Important now for assigning treatment
  set.seed(1234)
  
  # Split the dataset by resample_id
  split_data <- split(scenario, scenario$resample_id)
  
  # Apply the trial_cohort function to each subset and store the results
  trial_data <- lapply(split_data, trial_cohort)
  
  # Delete datasets so that local environment doesn't get too large
  rm(split_data)
  gc() # Call garbage collection to free up memory
  
  # Apply the clean_analyze function to each subset and store the results
  analyzed_data <- lapply(trial_data, function(subset){
    analyzed_data <- clean_analyze(subset)
    analyzed_data$resample_id <- unique(subset$resample_id)
    return(analyzed_data)
  })
  
  # Combine all processed subsets back into a single data frame
  final_data <- do.call(rbind, analyzed_data)
  
  saveRDS(final_data, resample_name)
  
  # Delete datasets so that local environment doesn't get too large
  rm(scenario, split_data, trial_data, analyzed_data, final_data)
  gc() # Call garbage collection to free up memory
}




# 
# 
# # Call in the data that need to run the analyses on.
# scenario <- readRDS('resample_scenario1.rds')
# 
# test <- subset(scenario, resample_id %in% c('1','2','3','4','5'))
# 

