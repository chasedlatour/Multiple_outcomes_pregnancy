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
library(purrr)





###################################################
# SCENARIO 1
###################################################

# Pull in the generated data
scenario <- readRDS('scenario1.rds')


