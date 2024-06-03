###################################################
# PROGRAM: Create resampled dataset.R
# PURPOSE: The goal of this program is to create 
# another version of the resampled dataset that
# mirrors the structure of that created in the
# super population. Eventually, we will input this
# dataset into the review results.RMD file.
#
# PROGRAMMER: Chase Latour
###################################################



###################################################
# Load in libraries
###################################################
library(tidyverse)



###################################################
# Helper functions
###################################################

# Function to replace the prefix for the AJ estimates for the composite outcome
replace_prefix <- function(df, old_prefix, new_prefix) {
  names(df) <- gsub(paste0("^", old_prefix), new_prefix, names(df))
  return(df)
}




###################################################
# Re-calculate CIs based on the resampled data's
# standard errors over all 12 scenarios
###################################################

for (i in 1:12){
  
  readResampData <- paste0("resample_resample_scenario",i,"_anal.rds")
  readOrigData <- paste0("analyses_scenario", i, ".rds")
  saveData <- paste0("resampled_analyses_scenario", i, ".rds")
  
  # Read in the data
  scenario <- readRDS(readResampData) %>% 
    replace_prefix("composite_multi_gw","aj_composite_")
  
  # Calculate the standard errors
  stderrs <- scenario %>% 
    summarize(
      km_composite_rd_se = sd(`km_composite_ rd`)/sqrt(n()),
      composite_multi_gw_se = sd(`aj_composite_ rd`)/sqrt(n()),
      km_sga_fd_se = sd(`km_sga_all_ rd`)/sqrt(n()),
      aj_sga_fd_se = sd(`aj_sga_fd_ rd`)/sqrt(n()),
      km_sga_all_se = sd(`km_sga_all_ rd`)/sqrt(n()),
      aj_sga_all_se = sd(`aj_sga_all_ rd`)/sqrt(n())
    )
  
  z <- qnorm(0.975)
  
  # Revise the CI values
  
  analyses <- readRDS(readOrigData) %>% 
    mutate(
      # Composite Outcome
      `km_composite_ rd_lcl` = `km_composite_ rd` - z*stderrs$km_composite_rd_se[[1]],
      `km_composite_ rd_ucl` = `km_composite_ rd` + z*stderrs$km_composite_rd_se[[1]],
      `composite_multi_gw rd_lcl` = `composite_multi_gw rd` - z*stderrs$composite_multi_gw_se[[1]],
      `composite_multi_gw rd_ucl` = `composite_multi_gw rd` + z*stderrs$composite_multi_gw_se[[1]],
      # SGA -- safety outcome
      `km_sga_fd_ rd_lcl` = `km_sga_fd_ rd` - z*stderrs$km_sga_fd_se[[1]],
      `km_sga_fd_ rd_ucl` = `km_sga_fd_ rd` + z*stderrs$km_sga_fd_se[[1]],
      `km_sga_all_ rd_lcl` = `km_sga_all_ rd` - z*stderrs$km_sga_all_se[[1]],
      `km_sga_all_ rd_ucl` = `km_sga_all_ rd` + z*stderrs$km_sga_all_se[[1]],
      `aj_sga_fd_ rd_lcl` = `aj_sga_fd_ rd` - z*stderrs$aj_sga_fd_se[[1]],
      `aj_sga_fd_ rd_ucl` = `aj_sga_fd_ rd` + z*stderrs$aj_sga_fd_se[[1]],
      `aj_sga_all_ rd_lcl` = `aj_sga_all_ rd` - z*stderrs$aj_sga_all_se[[1]],
      `aj_sga_all_ rd_ucl` = `aj_sga_all_ rd` + z*stderrs$aj_sga_all_se[[1]]
    )
  
  saveRDS(analyses, saveData)
  
}




###################################################
# Read in the data
###################################################

scenario1 <- readRDS("resample_resample_scenario1_anal.rds") %>% 
  replace_prefix("composite_multi_gw","aj_composite_")




###################################################
# Calculate the standard errors
###################################################

stderrs <- scenario1 %>% 
  summarize(
    km_composite_rd_se = sd(`km_composite_ rd`)/sqrt(n()),
    composite_multi_gw_se = sd(`aj_composite_ rd`)/sqrt(n()),
    km_sga_fd_se = sd(`km_sga_all_ rd`)/sqrt(n()),
    aj_sga_fd_se = sd(`aj_sga_fd_ rd`)/sqrt(n()),
    km_sga_all_se = sd(`km_sga_all_ rd`)/sqrt(n()),
    aj_sga_all_se = sd(`aj_sga_all_ rd`)/sqrt(n())
    )

z <- qnorm(0.975)

# Revise the CI values

analyses_scenario1 <- readRDS("analyses_scenario1.rds") %>% 
  mutate(
    # Composite Outcome
    `km_composite_ rd_lcl` = `km_composite_ rd` - z*stderrs$km_composite_rd_se[[1]],
    `km_composite_ rd_ucl` = `km_composite_ rd` + z*stderrs$km_composite_rd_se[[1]],
    `composite_multi_gw rd_lcl` = `composite_multi_gw rd` - z*stderrs$composite_multi_gw_se[[1]],
    `composite_multi_gw rd_ucl` = `composite_multi_gw rd` + z*stderrs$composite_multi_gw_se[[1]],
    # SGA -- safety outcome
    `km_sga_fd_ rd_lcl` = `km_sga_fd_ rd` - z*stderrs$km_sga_fd_se[[1]],
    `km_sga_fd_ rd_ucl` = `km_sga_fd_ rd` + z*stderrs$km_sga_fd_se[[1]],
    `km_sga_all_ rd_lcl` = `km_sga_all_ rd` - z*stderrs$km_sga_all_se[[1]],
    `km_sga_all_ rd_ucl` = `km_sga_all_ rd` + z*stderrs$km_sga_all_se[[1]],
    `aj_sga_fd_ rd_lcl` = `aj_sga_fd_ rd` - z*stderrs$aj_sga_fd_se[[1]],
    `aj_sga_fd_ rd_ucl` = `aj_sga_fd_ rd` + z*stderrs$aj_sga_fd_se[[1]],
    `aj_sga_all_ rd_lcl` = `aj_sga_all_ rd` - z*stderrs$aj_sga_all_se[[1]],
    `aj_sga_all_ rd_ucl` = `aj_sga_all_ rd` + z*stderrs$aj_sga_all_se[[1]]
    )

saveRDS(analyses_scenario1, "resampled_analyses_scenario1.rds")
