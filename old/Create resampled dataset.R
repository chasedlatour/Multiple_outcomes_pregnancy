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

# Testing: i <- 1

for (i in 1:12){
  
  readResampData <- paste0("resample_resample_scenario",i,"_anal.rds")
  readOrigData <- paste0("analyses_scenario", i, ".rds")
  saveData <- paste0("resampled_analyses_scenario", i, ".rds")
  
  # Get val from normal dist for CIs
  z <- qnorm(0.975)
  
  # Read in the data
  scenario <- readRDS(readResampData) %>% 
    replace_prefix("composite_multi_gw","aj_composite_") %>% 
    # Calc standard errors for each resampled dataset.
    mutate(km_composite_se = (`km_composite_ rd_ucl` - `km_composite_ rd_lcl`)/(2*z),
           aj_composite_se = (`aj_composite_ rd_ucl` - `aj_composite_ rd_lcl`) / (2*z),
           km_sga_se = (`aj_composite_ rd_ucl` - `km_composite_ rd_lcl`)/(2*z),
           aj_sga_fd_se = (`aj_sga_fd_ rd_ucl` - `aj_sga_fd_ rd_lcl`)/(2*z),
           aj_sga_all_se = (`aj_sga_all_ rd_ucl` - `aj_sga_all_ rd_lcl`)/(2*z))
    
  
  
  # Calculate the standard errors and means
  stderrs <- scenario %>% 
    summarize(
      
      # Determine the number where LCL or UCL is INF
      km_composite_inf = sum(!is.finite(`km_composite_ rd_lcl`) | !is.finite(`km_composite_ rd_ucl`)),
      aj_composite_inf = sum(!is.finite(`aj_composite_ rd_lcl`) | !is.finite(`aj_composite_ rd_ucl`)),
      aj_sga_fd_inf = sum(!is.finite(`aj_sga_fd_ rd_lcl`) | !is.finite(`aj_sga_fd_ rd_ucl`)),
      aj_sga_all_inf = sum(!is.finite(`aj_sga_all_ rd_lcl`) | !is.finite(`aj_sga_all_ rd_ucl`)),
      km_sga_inf = sum(!is.finite(`km_sga_all_ rd_lcl`) | !is.finite(`km_sga_all_ rd_ucl`)),
      
      #Means
      km_composite_mean = mean(`km_composite_ rd`),
      composite_mean = mean(`aj_composite_ rd`),
      km_sga_fd_mean = mean(`km_sga_all_ rd`),
      aj_sga_fd_mean = mean(`aj_sga_fd_ rd`),
      km_sga_all_mean = mean(`km_sga_all_ rd`),
      aj_sga_all_mean = mean(`aj_sga_all_ rd`),
      
      # Standard Errors - ESE and ASE
      km_composite_rd_se2 = mean(km_composite_se[is.finite(km_composite_se)]),
      km_composite_rd_se = sd(`km_composite_ rd`),
      composite_multi_gw_se2 = mean(aj_composite_se[is.finite(aj_composite_se)]),
      composite_multi_gw_se = sd(`aj_composite_ rd`),
      km_sga_fd_se2 = mean(km_sga_se[is.finite(km_sga_se)]),
      km_sga_fd_se = sd(`km_sga_all_ rd`),
      aj_sga_fd_se2 = mean(aj_sga_fd_se[is.finite(aj_sga_fd_se)]),
      aj_sga_fd_se = sd(`aj_sga_fd_ rd`),
      km_sga_all_se = sd(`km_sga_all_ rd`),
      aj_sga_all_se2 = mean(aj_sga_all_se[is.finite(aj_sga_all_se)]),
      aj_sga_all_se = sd(`aj_sga_all_ rd`),
      
      # Risks
      km_composite_r0 = mean(`km_composite_ r_0`),
      km_composite_r1 = mean(`km_composite_ r_1`),
      aj_composite_r0 = mean(`aj_composite_ r_0`),
      aj_composite_r1 = mean(`aj_composite_ r_1`),
      km_sga_fd_r0 = mean(`km_sga_fd_ r_0`),
      km_sga_fd_r1 = mean(`km_sga_fd_ r_1`),
      aj_sga_fd_r0 = mean(`aj_sga_fd_ r_0`),
      aj_sga_fd_r1 = mean(`aj_sga_fd_ r_1`),
      km_sga_all_r0 = mean(`km_sga_all_ r_0`),
      km_sga_all_r1 = mean(`km_sga_all_ r_1`),
      aj_sga_all_r0 = mean(`aj_sga_all_ r_0`),
      aj_sga_all_r1 = mean(`aj_sga_all_ r_1`)
      
    )
  
  
  
  # Revise the CI values
  
  analyses <- readRDS(readOrigData) %>% 
    mutate(
      
      # Risks
      
      `km_composite_ r_0` = stderrs$km_composite_r0,
      `km_composite_ r_1` = stderrs$km_composite_r1,
      `composite_multi_gw r_0` = stderrs$aj_composite_r0,
      `composite_multi_gw r_1` = stderrs$aj_composite_r1,
      `km_sga_fd_ r_0` = stderrs$km_sga_all_r0,
      `km_sga_fd_ r_1` = stderrs$km_sga_all_r1,
      `aj_sga_fd_ r_0` = stderrs$aj_sga_fd_r0,
      `aj_sga_fd_ r_1` = stderrs$aj_sga_fd_r1,
      `km_sga_all_ r_0` = stderrs$km_sga_all_r0,
      `km_sga_all_ r_1` = stderrs$km_sga_all_r1,
      `aj_sga_all_ r_0` = stderrs$aj_sga_all_r0,
      `aj_sga_all_ r_1` = stderrs$aj_sga_all_r1,
      
      #RDs
      ## Estimates
      `km_composite_ rd` = stderrs$km_composite_mean[[1]],
      `composite_multi_gw rd` = stderrs$composite_mean[[1]],
      `km_sga_fd_ rd` = stderrs$km_sga_fd_mean[[1]],
      `km_sga_all_ rd` = stderrs$km_sga_all_mean[[1]],
      `aj_sga_fd_ rd` = stderrs$aj_sga_fd_mean[[1]],
      `aj_sga_all_ rd` = stderrs$aj_sga_all_mean[[1]],
      ## CIs
      `km_composite_ rd_lcl` = stderrs$km_composite_mean[[1]] - z*stderrs$km_composite_rd_se[[1]],
      `km_composite_ rd_ucl` = stderrs$km_composite_mean[[1]] + z*stderrs$km_composite_rd_se[[1]],
      `composite_multi_gw rd_lcl` = stderrs$composite_mean[[1]] - z*stderrs$composite_multi_gw_se[[1]],
      `composite_multi_gw rd_ucl` = stderrs$composite_mean[[1]] + z*stderrs$composite_multi_gw_se[[1]],
      `km_sga_fd_ rd_lcl` = stderrs$km_sga_fd_mean[[1]] - z*stderrs$km_sga_fd_se[[1]],
      `km_sga_fd_ rd_ucl` = stderrs$km_sga_fd_mean[[1]] + z*stderrs$km_sga_fd_se[[1]],
      `km_sga_all_ rd_lcl` = stderrs$km_sga_all_mean[[1]] - z*stderrs$km_sga_all_se[[1]],
      `km_sga_all_ rd_ucl` = stderrs$km_sga_all_mean[[1]] + z*stderrs$km_sga_all_se[[1]],
      `aj_sga_fd_ rd_lcl` = stderrs$aj_sga_fd_mean[[1]] - z*stderrs$aj_sga_fd_se[[1]],
      `aj_sga_fd_ rd_ucl` = stderrs$aj_sga_fd_mean[[1]] + z*stderrs$aj_sga_fd_se[[1]],
      `aj_sga_all_ rd_lcl` = stderrs$aj_sga_all_mean[[1]] - z*stderrs$aj_sga_all_se[[1]],
      `aj_sga_all_ rd_ucl` = stderrs$aj_sga_all_mean[[1]] + z*stderrs$aj_sga_all_se[[1]]
      
    )
  
  saveRDS(analyses, saveData)
  
  saveRDS(stderrs, paste0('stderrs_', saveData))
  
}

# 
# stacked_se <- rbind(readRDS('stderrs_resampled_analyses_scenario1.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario2.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario3.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario4.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario5.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario6.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario7.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario8.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario9.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario10.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario11.rds'),
#                     readRDS('stderrs_resampled_analyses_scenario12.rds'))
                    