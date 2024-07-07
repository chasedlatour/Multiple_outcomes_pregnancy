###################################################
# PROGRAM: Resample.R
# PURPOSE: The goal of this program is to resample
# the generated datasets, with replacement, so that
# we can calculate the standard errors.
#
# PROGRAMMER: Chase Latour
###################################################





###################################################
#Load in libraries
###################################################
library(tidyverse)
library(dplyr)
library(purrr)





###################################################
# Set the number of samples
###################################################

# Number of resampled datasets we want to create
num_resamples <- 2000

# Number of sampled individuals in each resample
n_row <- 5000




###################################################
# Function to resample data
###################################################
# Function to resample the dataset with replacement
resample_data <- function(data) {
  data %>% 
    slice_sample(n = n_row, replace = TRUE)
}







###################################################
# Loop through scenarios 1 to 12
###################################################
for (i in 1:12) {
  scenario_name <- paste0("scenario", i)
  resample_name <- paste0("resample_", scenario_name, ".rds")
  
  # Pull in the generated data
  scenario <- readRDS(paste0(scenario_name, ".rds"))
  
  # Use purrr::map to create a list of resampled datasets
  resampled_datasets <- map(1:num_resamples, ~resample_data(subset(scenario[[1]], 
                                                                   trial_participant == TRUE)))
  # Combine all resampled datasets into one
  combined_resampled_data <- bind_rows(resampled_datasets, .id = "resample_id")
  
  # Save the dataset for the current scenario
  saveRDS(combined_resampled_data, resample_name)
  
  # Delete datasets so that local environment doesn't get too large
  rm(scenario, resampled_datasets, combined_resampled_data)
  gc() # Call garbage collection to free up memory
}























###################################################
# SCENARIO 1 -- OLD
###################################################

# # Pull in the generated data
# scenario <- readRDS('scenario1.rds')
# 
# # Use purrr::map to create a list of resampled datasets
# resampled_datasets <- map(1:num_resamples, ~resample_data(subset(scenario[[1]], 
#                                                                  trial_participant == TRUE)))
# # Combine all resampled datasets into one
# combined_resampled_data <- bind_rows(resampled_datasets, .id = "resample_id")
# 
# # Save the dataset for scenario 1
# saveRDS(combined_resampled_data, "resample_scenario1.rds")
# 
# # Delete datasets so that local environment doesn't get too large
# rm(scenario, resampled_datasets, combined_resampled_data)



