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

# Specify the required values for the simulation
# These are the same across scenarios.
n_sim <- 1
settings <- list(
  n = 5000000 # 5 million
)




################################
# SCENARIO 1
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 1.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario1.rds")



################################
# SCENARIO 2
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 2.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 2.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 2.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 2.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 2.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario2.rds")



################################
# SCENARIO 3
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 3.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 3.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 3.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 3.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 3.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario3.rds")




################################
# SCENARIO 4
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 4.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 4.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 4.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 4.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 4.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario4.rds")



################################
# SCENARIO 5
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 5.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 5.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 5.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 5.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 5.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario5.rds")



################################
# SCENARIO 6
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 6.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 6.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 6.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 6.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 6.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario6.rds")



################################
# SCENARIO 7
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 7.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 7.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 7.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 7.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 7.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario7.rds")



################################
# SCENARIO 8
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 8.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 8.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 8.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 8.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 8.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario8.rds")



################################
# SCENARIO 9
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 9.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 9.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 9.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 9.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 9.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario9.rds")





################################
# SCENARIO 10
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 10.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 10.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 10.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 10.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 10.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario10.rds")







################################
# SCENARIO 11
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 11.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 11.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 11.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 11.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 11.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario11.rds")








################################
# SCENARIO 12
################################

## Upload the parameter Excel files.
phase1 <- read_xlsx("Scenario 12.xlsx", sheet = "Phase1")
phase2 <- read_xlsx("Scenario 12.xlsx", sheet = "Phase2")
phase3 <- read_xlsx("Scenario 12.xlsx", sheet = "Phase3")
phase4 <- read_xlsx("Scenario 12.xlsx", sheet = "Phase4")
phase5 <- read_xlsx("Scenario 12.xlsx", sheet = "Phase5")

# Generate the Data

# Specify the simulation settings
for_sim <- c(list(.x = 1:n_sim, .f=each_sim), settings)
# Setting seed once at the beginning of the simulation
set.seed(1234)
# Simulate the data
all_sims <- do.call(purrr::map, args = for_sim)

### Save the file as an RDS file

saveRDS(all_sims, file = "scenario12.rds")
