########################################
# Program: DGM Scenario 1.R
# Programmer: Chase Latour
# Date last Modified: 01/22/2024
#
# Purpose: Generate the cohort for 
# scenario 1.
########################################

# Pull in the necessary libraries
library(tidyverse)
library(readxl)

# Pull in the data generation functions.
source("Data generation functions.R")





## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase5")





###########################
# Generate the Data
###########################

# Specify the required values for the simulation
n_sim <- 1
settings <- list(
  n = 5000000 # 5 million
)


#data <- each_sim(n_sim, n)
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginnign of the simulation, not at
# -- the beginning of each repetition of the DGM, as was done previously.
set.seed(1234)
all_sims <- do.call(purrr::map, args = for_sim)


### Save the file as an RDS file

saveRDS(all_sims, file = "scenario1.rds")

# Eventually - want to write in code that will save these data. 
# -- For other simulations, will want to incorporate parallel processing. This currently takes a while
# -- Maybe should get rid of some of the if/then options if possible.

