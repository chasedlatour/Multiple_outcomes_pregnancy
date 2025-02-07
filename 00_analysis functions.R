#####################################################
# Program: analysis functions.R
# Programmer: Chase
# Date last modified: 01.22.2024
#
# Purpose: The program contains all the R functions
# for analyzing the data. The goal of doing this
# is to ensure that these functions only need to be
# editing one time and will apply throughout all
# simulations easily.
#####################################################




## Identify the cohort that entered the trial

# TO DO: Determine if this trt variable should be remade in the trial participants

trial_cohort <- function(dataset){
  
  trial1 <- dataset %>% 
    bind_rows() %>%
    subset(trial_participant == TRUE)
  
  n <- nrow(trial1)
  
  trial <- trial1 %>% 
    # Create a new treatment indicator in the trial cohort only.
    mutate(trt = rbinom(n=n, size=1, prob=0.5)) %>% 
    # Identify the observed outcomes
    mutate(preg_outcome_final = ifelse(trt == 1,
                                       preg_outcome_final_trt,
                                       preg_outcome_final_untrt),
           preg_outcome_final_gw = ifelse(trt == 1,
                                          preg_outcome_final_gw_trt,
                                          preg_outcome_final_gw_untrt),
           preeclampsia = ifelse(trt == 1,
                                 preeclampsia_trt,
                                 preeclampsia_untrt),
           sga = ifelse(trt == 1,
                        sga_trt,
                        sga_untrt)) %>% 
    rowwise() %>% 
    mutate(censor = ifelse(first_censoring_gw < preg_outcome_final_gw & !is.na(first_censoring_gw),
                           1,
                           0),
           time = ifelse(censor == 1,
                         first_censoring_gw,
                         preg_outcome_final_gw)
    ) %>% 
    ungroup() %>% 
    mutate(
      
      ### Composite Outcomes
      
      # Composite outcome for the KM estimator (i.e., censor at anything other than composite outcome)
      composite_km = case_when(censor == 1 ~ 0,
                               preg_outcome_final == "fetaldeath" & censor == 0 ~ 1,
                               preg_outcome_final == "livebirth" & censor == 0 & preeclampsia == 1 ~ 1,
                               preg_outcome_final == "livebirth" & censor == 0 & preeclampsia == 0 ~ 0),
      # Multinomial composite outcome for AJ estimator
      composite_multi = case_when(censor == 1 ~ 0,
                                  censor == 0 & preg_outcome_final == "fetaldeath" ~ 1,
                                  censor == 0 & preeclampsia == 1 ~ 1,
                                  censor == 0 & preg_outcome_final == "livebirth" & preeclampsia == 0 ~ 2
      ),
      # Make a gestational week variable for the composite_multi variable
      composite_multi_gw = as.double(ifelse(censor == 1,
                                  first_censoring_gw,
                                  preg_outcome_final_gw)),
      # Make composite variable that ignores censoring
      composite_no_censor = ifelse(preg_outcome_final == "fetaldeath" | 
                                     (preg_outcome_final == "livebirth" & preeclampsia == 1),
                                   1,
                                   0),
      # Potential outcomes for composite outcome
      # -- Ignore censoring for the potential outcomes because this is the full outcome
      # -- GW will be the same as those for composite_no_censor but only for _trt or _untrt
      composite_trt = ifelse(preg_outcome_final_trt == "fetaldeath" | 
                               (preg_outcome_final_trt == "livebirth" & preeclampsia_trt == 1),
                             1,
                             0),
      composite_untrt = ifelse(preg_outcome_final_untrt == "fetaldeath" | 
                                 (preg_outcome_final_untrt == "livebirth" & preeclampsia_untrt == 1),
                               1,
                               0),
      ### END Composite Outcomes
      
      
      ### SGA Outcomes
      
      # Make a SGA variable for the KM estimator
      sga_km = ifelse(sga == 1 & censor == 0,
                      1,
                      0),
      # Make SGA variables with fetal death only as a competing event
      sga_multi_do = case_when(censor == 1 ~ 0,
                               censor == 0 & sga == 1 & preg_outcome_final == "livebirth" ~ 1,
                               censor == 0 & preg_outcome_final == "fetaldeath" ~ 2,
                               censor == 0 & sga == 0 & preg_outcome_final == "livebirth" ~ 0), 
      # Make a gestational week variable for the sga_multi variable
      sga_multi_do_gw = as.double(ifelse(censor == 1,
                               first_censoring_gw,
                               preg_outcome_final_gw)),
      # Make SGA variables with all competing events - 3 level variable
      sga_multi_all = case_when(censor == 1 ~ 0,
                                censor == 0 & sga == 1 ~ 1,
                                censor == 0 & preg_outcome_final == "fetaldeath" ~ 2,
                                censor == 0 & sga == 0 & preg_outcome_final == "livebirth" ~ 3), 
      # Make a gestational week variable for the sga_multi variable
      sga_multi_all_gw = as.double(ifelse(censor == 1,
                                first_censoring_gw,
                                preg_outcome_final_gw)),
      # Make a SGA variable that ignores censoring
      sga_no_censor = ifelse(sga == 1, 
                             1, 
                             0) #,
      # Potential Outcomes for SGA
      # Do not need to make these additional to what is already generated
      
      ### END SGA Outcomes
      
    )
  
  return(trial)
  
}



### Describe the trial cohort

describe_trial <- function(dataset){
  
  tib <- dataset %>% 
    group_by(sim_id, trt) %>% 
    summarize(
      
      # Count the number of trial participants & percentage
      n_people = n(),
      percent_n_by_trt = round(n_people / nrow(dataset) * 100, 2),
      
      # Count the number of trial participants that are censored
      n_censor = sum(censor == 1),
      percent_censor = round(n_censor / n() * 100, 1),
      
      # Describe the indexing into the cohort - should all be the same, so a check
      min_index = min(first_index),
      max_index = max(first_index),
      
      ## Number of live births and fetal deaths across treatment groups
      ## - ignoring censoring
      n_fd_no_censor = sum(preg_outcome_final == 'fetaldeath'),
      fd_percent_no_censor = round(n_fd_no_censor / n() * 100, 1),
      n_lb_no_censor = sum(preg_outcome_final == 'livebirth'),
      lb_percent_no_censor = round(n_lb_no_censor / n() * 100, 1),
      
      ## Number of live births and fetal deaths across treatment groups
      ## - ignoring censoring
      n_fd_censor = sum(preg_outcome_final == "fetaldeath" & censor == 0),
      fd_percent_censor = round(n_fd_censor / n() * 100, 1),
      n_lb_censor = sum(preg_outcome_final == 'livebirth' & censor == 0),
      lb_percent_censor = round(n_lb_censor / n() * 100, 1),
      
      ## Number with preeclampsia and SGA, ignoring censoring
      n_preeclampsia_no_cen = sum(preeclampsia == 1),
      preec_perc_no_cen = round(n_preeclampsia_no_cen / n() * 100, 1),
      n_sga_no_cen = sum(sga == 1),
      sga_perc_no_cen = round(n_sga_no_cen / n() * 100, 1),
      
      ## Number with preeclampsia and SGA, ignoring censored events
      n_preeclampsia_cen = sum(preeclampsia == 1 & censor == 0),
      preec_perc_cen = round(n_preeclampsia_no_cen / n() * 100, 1),
      n_sga_cen = sum(sga == 1 & censor == 0),
      sga_perc_cen = round(n_sga_no_cen / n() * 100, 1),
      .groups = 'keep'
      
      )
  
  return(tib)
    
}



## Describe the GW distribution of the pregnancy outcomes
## -- This looks at the total population and stratified by if
## -- they are censored or not.

describe_outcomes <- function(dataset){
  
  # Look at the distribution by being censored or not
  
  tib_censor_strat <- dataset %>% 
    group_by(sim_id, censor, trt, preg_outcome_final) %>% 
    summarize(
      min = min(preg_outcome_final_gw),
      p25 = quantile(preg_outcome_final_gw, 0.25),
      median = median(preg_outcome_final_gw),
      p75 = quantile (preg_outcome_final_gw, 0.75),
      max = max(preg_outcome_final_gw),
      count = n(),
      .groups = 'keep'
    )
  
  tib_no_strat <- dataset %>% 
    group_by(sim_id, trt, preg_outcome_final) %>% 
    summarize(
      min = min(preg_outcome_final_gw),
      p25 = quantile(preg_outcome_final_gw, 0.25),
      median = median(preg_outcome_final_gw),
      p75 = quantile (preg_outcome_final_gw, 0.75),
      max = max(preg_outcome_final_gw),
      count = n(),
      .groups = 'keep'
    ) %>% 
    mutate(censor = NA)
  
  tib <- rbind(tib_no_strat, tib_censor_strat) %>% 
    mutate(strata = case_when(is.na(censor) ~ "Total",
                              censor == 0 ~ "Not censored",
                              censor == 1 ~ "Censored")
           ) %>% 
    select(-censor) %>% 
    arrange(sim_id)
  
  return(tib) 
  
}





## KM Estimator
## -- Code for function that uses the Kaplan-Meier estimator to estimate risk.

km_estimator <- function(dataset, outcome_var, outcome_var_t, t_val, rename){
  
  z <- qnorm(0.975)
  
  ## Run the KM analysis only considering the composite outcome.
  km <- survfit(Surv(get(outcome_var_t), get(outcome_var)) ~ trt, data = dataset)
  # Testing
  # km <- survfit(Surv(composite_km, composite_km) ~ trt, data = dataset)
  
  wrisk <- data.frame(t = km$time, s = km$surv, r = 1 - km$surv, se = km$std.err,
                      trt = c(rep(0, km$strata["trt=0"]), rep(1, km$strata["trt=1"])))
  
  estimate <- wrisk %>% 
    filter(t < t_val+0.5) %>% # Deal with no additional risk scenarios
    group_by(trt) %>% 
    summarize(r = last(r),
              se = last(se),
              .groups = 'drop') %>% 
    pivot_wider(names_from = trt,
                values_from = c(r, se), 
                names_glue = "{.value}_{trt}") %>% 
    rowwise() %>% 
    mutate(rr = r_1/r_0,
           selnrr = sqrt((1/r_0)^2 * se_0^2 + (1/r_1)^2*se_1^2),
           rr_lcl = exp(log(rr) - z*selnrr),
           rr_ucl = exp(log(rr) + z*selnrr),
           rd = r_1 - r_0,
           se_rd = sqrt(se_1^2 + se_0^2),
           rd_lcl = rd - 1.96*se_rd,
           rd_ucl = rd + 1.96*se_rd,
           Estimator = "Kaplan-Meier") %>%
    select(r_0, r_1, rr, rr_lcl, rr_ucl,
           rd, rd_lcl, rd_ucl
    ) %>% 
    rename_all((~ paste(rename, .)))
  
  return(estimate)
  
}




## AJ Estimator

aj_estimator <- function(dataset, outcome_var, outcome_var_t, t_val, rename){
  
  z <- qnorm(0.975)
  
  #jitter ties
  #set.seed(1234)
  # Not sure if should set seed for jittering outcomes
  dataset2 <- dataset %>% 
    group_by(get(outcome_var_t)) %>% 
    add_tally() %>% 
    ungroup() %>% 
    mutate(
      # Jitter event times
      jitter = runif(nrow(dataset), min = -.01, max = .01), # N in the samp
      time = ifelse(n>1, get(outcome_var_t) + jitter, get(outcome_var_t))
    )
  
  # Run the AJ model
  aj <- survfit(Surv(time, factor(get(outcome_var))) ~ trt, data = dataset2)
  
  mod <- summary(aj)
  
  summod <- data.frame(t = mod$time,
                       r = mod$pstate[,2], 
                       se = mod$std.err[,2],
                       trt = c(rep(0, length(mod[["strata"]][mod[["strata"]] == "trt=0"])), 
                               rep(1, length(mod[["strata"]][mod[["strata"]] == "trt=1"])))
  ) %>%
    filter(t < t_val+0.5) %>%  # Deal with the jittering of outcomes
    group_by(trt) %>% 
    summarize(r = last(r),
              se = last(se),
              .groups = 'drop') %>% 
    pivot_wider(names_from = trt,
                values_from = c(r, se), 
                names_glue = "{.value}_{trt}") %>% 
    rowwise() %>% 
    mutate(rr = r_1/r_0,
           selnrr = sqrt((1/r_0)^2 * se_0^2 + (1/r_1)^2*se_1^2),
           rr_lcl = exp(log(rr) - z*selnrr),
           rr_ucl = exp(log(rr) + z*selnrr),
           rd = r_1 - r_0,
           se_rd = sqrt(se_1^2 + se_0^2),
           rd_lcl = rd - 1.96*se_rd,
           rd_ucl = rd + 1.96*se_rd,
           Estimator = "Aalen-Johanssen") %>%
    select(r_0, r_1, rr, rr_lcl, rr_ucl,
           rd, rd_lcl, rd_ucl
    )  %>% 
    rename_all((~ paste(rename, .)))
  
  return(summod)
  
  
}





## Create a function that calculates the RD and RR that we would have
## -- expected without censoring. This should provide the 
## -- correct estimate for the trial and give a sense of how 
## -- informative the censoring process is.

# Formula for SE for RR: https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals8.html
# Formula for SE for RD: https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals7.html

no_censor_estimator <- function(dataset, outcome_var, rename){
  
  z <- qnorm(0.975)
  
  summary <- dataset %>% 
    group_by(trt) %>% 
    summarize(r = sum(get(outcome_var) == 1) / n(),
              n = n(), # Needed for SE,
              x = sum(get(outcome_var) == 1), # Needed for SE
              .groups = "drop") %>% 
    pivot_wider(names_from = trt,
                values_from = c(r, n, x),
                names_glue = "{.value}_{trt}") %>% 
    rowwise() %>% 
    mutate(rr = r_1 / r_0,
           rr_se = sqrt((((n_1-x_1)/x_1)/n_1) + (((n_0-x_0)/x_0)/n_0)),
           rr_lcl = exp(log(rr) - (z*rr_se)),
           rr_ucl = exp(log(rr) + (z*rr_se)),
           rd = r_1 - r_0,
           rd_se = ((r_1*(1-r_1))/n_1) + ((r_0*(1-r_0))/n_0),
           rd_lcl = rd - (z*rd_se),
           rd_ucl = rd + (z*rd_se)
    ) %>% 
    rename_all((~ paste(rename, .)))
  
  return(summary)
  
}


## Create a function that calculates the RD and RR using the
## -- potential outcomes. This should provide the theoretically correct
## -- estimate for the trial.
## Do not calculate CIs here, as a person contributes twice. There is
## -- no statitical variability because we have their full
## -- set of outcomes.

potential_outcomes_estimator <- function(dataset, rename){
  
  summary <- dataset %>% 
    summarize(risk_composite_trt = sum(composite_trt) / n(),
              risk_composite_untrt = sum(composite_untrt) / n(),
              risk_sga_trt = sum(unlist(sga_trt)) / n(),
              risk_sga_untrt = sum(unlist(sga_untrt)) / n(),
              ) %>% 
    rowwise() %>% 
    mutate(rr_composite = risk_composite_trt / risk_composite_untrt,
           rd_composite = risk_composite_trt - risk_composite_untrt,
           rr_sga = risk_sga_trt / risk_sga_untrt,
           rd_sga = risk_sga_trt - risk_sga_untrt) %>% 
    rename_all((~ paste(rename, .)))
  
  return(summary)
  
}




## Create a function that appliesd all the analysis functions.


clean_analyze <- function(dset){
  
  ## KM estiamtor for composite outcome
  km_composite <- km_estimator(dset, outcome_var = "composite_km", outcome_var_t = "time", t_val = 41, rename = "km_composite_")
  
  ## AJ composite outcome
  aj_composite <- aj_estimator(dset, outcome_var = "composite_multi", outcome_var_t = "composite_multi_gw", 41, "composite_multi_gw")
  
  no_censor_composite <- no_censor_estimator(dset, outcome_var = "composite_no_censor", rename = "no_censor_composite_")
  
  km_sga_fd = km_estimator(dset, outcome_var = "sga_km", outcome_var_t = "time", t_val=41, rename = "km_sga_fd_")
  
  aj_sga_fd = aj_estimator(dset, outcome_var = "sga_multi_do", outcome_var_t = "sga_multi_do_gw", t_val = 41, rename = "aj_sga_fd_")
  
  no_censor_sga_fd = no_censor_estimator(dset, outcome_var = "sga_no_censor", rename = "no_censor_sga_fd_")
  
  km_sga_all = km_estimator(dset, outcome_var = "sga_km", outcome_var_t = "time", t_val = 41, rename = "km_sga_all_")
  
  aj_sga_all = aj_estimator(dset, outcome_var = "sga_multi_all", outcome_var_t = "sga_multi_all_gw", t_val = 41, rename = "aj_sga_all_")
  
  no_censor_sga_all = no_censor_estimator(dset, outcome_var = "sga_no_censor", rename = "no_censor_sga_all_")
  
  potential_outcomes = potential_outcomes_estimator(dset, "potential_")
  
  # Make output dataset
  combined <- cbind(km_composite, aj_composite, no_censor_composite, km_sga_fd, aj_sga_fd, 
                    no_censor_sga_fd, km_sga_all, aj_sga_all, no_censor_sga_all,
                    potential_outcomes)
  
}


### Function that will calculate the bias that we're interested in.

bias_outcome <- function(r1_km, r0_km, r1_nocen, r0_nocen, r1_aj, r0_aj,
                         r1_po, r0_po,
                         rr_km, rr_nocen, rr_aj, rr_po,
                         rd_km, rd_nocen, rd_aj, rd_po, prefix = ""){
  
  dataset <- tibble(
    risk1_km_nocen = r1_km - r1_nocen,
    risk1_km_po = r1_km - r1_po,
    risk0_km_nocen = r0_km - r0_nocen,
    risk0_km_po = r0_km - r0_po,
    risk1_aj_nocen = r1_aj - r1_nocen,
    risk1_aj_po = r1_aj - r1_po,
    risk0_aj_nocen = r0_aj - r0_nocen,
    risk0_aj_po = r0_aj - r0_po,
    rr_km_nocen = log(rr_km) - log(rr_nocen),
    rr_km_po = log(rr_km) - log(rr_po),
    rr_aj_nocen = log(rr_aj) - log(rr_nocen),
    rr_aj_po = log(rr_aj) - log(rr_po),
    rd_km_nocen = rd_km - rd_nocen,
    rd_km_po = rd_km - rd_po,
    rd_aj_nocen = rd_aj - rd_nocen,
    rd_aj_po = rd_aj - rd_po
  )
  
  dataset <- dataset %>% 
    rename_all(~paste0(prefix, .))
  
  return(dataset)
  
}



# STOPPED CHECKING HERE.

#### Apply the bias_outcome() function in our dataset

apply_bias_outcome <- function(dset){
  
  hold <- dset %>% 
    rowwise() %>% 
    mutate(
      composite = list(bias_outcome(`km_composite_ r_1`, `km_composite_ r_0`, `no_censor_composite_ r_1`,
                                    `no_censor_composite_ r_0`, `aj_composite_ r_1`, `aj_composite_ r_0`,
                                    `potential_ risk_composite_trt`, `potential_ risk_composite_untrt`,
                                    `km_composite_ rr`, `no_censor_composite_ rr`, `aj_composite_ rr`, 
                                    `potential_ rr_composite`,
                                    `km_composite_ rd`, `no_censor_composite_ rd`, `aj_composite_ rd`, 
                                    `potential_ rd_composite`, "composite_")),
      sga_fd = list(bias_outcome(`km_sga_fd_ r_1`, `km_sga_fd_ r_0`, `no_censor_sga_fd_ r_1`, 
                                 `no_censor_sga_fd_ r_0`, 
                                 `potential_ risk_sga_trt`, `potential_ risk_sga_untrt`,
                                 `aj_sga_fd_ r_1`, `aj_sga_fd_ r_0`, 
                                 `km_sga_fd_ rr`, `no_censor_sga_fd_ rr`, `aj_sga_fd_ rr`,
                                 `potential_ rr_sga`,
                                 `km_sga_fd_ rd`, `no_censor_sga_fd_ rd`, `aj_sga_fd_ rd`, 
                                 `potential_ rd_sga`, "sga_fd_")),
      sga_all = list(bias_outcome(`km_sga_all_ r_1`, `km_sga_all_ r_0`, `no_censor_sga_all_ r_1`, 
                                  `no_censor_sga_all_ r_0`, `aj_sga_all_ r_1`, `aj_sga_all_ r_0`, 
                                  `potential_ risk_sga_trt`, `potential_ risk_sga_untrt`,
                                  `km_sga_all_ rr`, `no_censor_sga_all_ rr`, `aj_sga_all_ rr`,
                                  `potential_ rr_sga`,
                                  `km_sga_all_ rd`, `no_censor_sga_all_ rd`, `aj_sga_all_ rd`, 
                                  `potential_ rd_sga`, "sga_all_"))
    ) %>% 
    unnest(c(composite, sga_fd, sga_all))
  
  return(hold)
  
}





# Function that outputs the RR and RD estimates for each of the analytic approaches.


outcome_rr_rd <- function(dataset, outcome_var){
  
  z <- qnorm(0.975)
  
  data2 <- dataset %>% 
    ungroup() %>% 
    summarize(
      
      outcome = outcome_var,
      
      ##########
      ## Composite outcome
      ##########
      
      ### KM Estimates
      ### -- RR
      rr_km = exp(mean(log(!!sym(paste0("km_", outcome_var, "_ rr"))))),
      rr_km_var = var(log(!!sym(paste0("km_", outcome_var, "_ rr")))),
      rr_km_lcl = exp(log(rr_km) - z * sqrt(rr_km_var / n())),
      rr_km_ucl = exp(log(rr_km) + z * sqrt(rr_km_var / n())),
      ### -- RD
      rd_km = mean(!!sym(paste0("km_", outcome_var, "_ rd"))),
      rd_km_var := var(!!sym(paste0("km_", outcome_var, "_ rd"))),
      rd_km_lcl := rd_km - z * sqrt(rd_km_var / n()),
      rd_km_ucl := rd_km + z * sqrt(rd_km_var / n()),
      
      ### AJ Estimates
      ### -- RR
      rr_aj := exp(mean(log(!!sym(paste0("aj_", outcome_var, "_ rr"))))),
      rr_aj_var := var(log(!!sym(paste0("aj_", outcome_var, "_ rr")))),
      rr_aj_lcl := exp(log(rr_aj) - z * sqrt(rr_aj_var / n())),
      rr_aj_ucl := exp(log(rr_aj) + z * sqrt(rr_aj_var / n())),
      ### -- RD
      rd_aj := mean(!!sym(paste0("aj_", outcome_var, "_ rd"))),
      rd_aj_var := var(!!sym(paste0("aj_", outcome_var, "_ rd"))),
      rd_aj_lcl := rd_aj - z * sqrt(rd_aj_var / n()),
      rd_aj_ucl := rd_aj + z * sqrt(rd_aj_var / n()),
      
      ### No Censoring Estimates
      ### -- RR
      rr_nocen := exp(mean(log(!!sym(paste0("no_censor_", outcome_var, "_ rr"))))),
      rr_nocen_var := var(log(!!sym(paste0("no_censor_", outcome_var, "_ rr")))),
      rr_nocen_lcl := exp(log(rr_nocen) - z * sqrt(rr_nocen_var / n())),
      rr_nocen_ucl := exp(log(rr_nocen) + z * sqrt(rr_nocen_var / n())),
      ### -- RD
      rd_nocen := mean(!!sym(paste0("no_censor_", outcome_var, "_ rd"))),
      rd_nocen_var := var(!!sym(paste0("no_censor_", outcome_var, "_ rd"))),
      rd_nocen_lcl := rd_nocen - z * sqrt(rd_nocen_var / n()),
      rd_nocen_ucl := rd_nocen + z * sqrt(rd_nocen_var / n())
      
    ) 
  
  return(data2)
  
}



# Function to plot the RRs

plot_rr <- function(dataset, outcome_var, title = "") {
  
  rr_var <- paste0(outcome_var, "_ rr")
  
  plot <- dataset %>% 
    select(
      !!sym(paste0("km_", rr_var)),
      !!sym(paste0("aj_", rr_var)),
      !!sym(paste0("no_censor_", rr_var))
    ) %>% 
    pivot_longer(
      names_to = "Estimator", 
      cols = c(!!sym(paste0("km_", rr_var)), 
               !!sym(paste0("aj_", rr_var)), 
               !!sym(paste0("no_censor_", rr_var))),
      values_to = "Estimate"
    ) %>% 
    mutate(
      Estimator = case_when(
        Estimator == paste0("km_", rr_var) ~ "Kaplan Meier",
        Estimator == paste0("aj_", rr_var) ~ "Aalen-Johansen",
        Estimator == paste0("no_censor_", rr_var) ~ "No Censoring"
      )
    ) %>% 
    ggplot(aes(x = Estimate, y = Estimator, fill = Estimator)) +
    geom_density_ridges() +
    scale_x_continuous(trans = "log10") +
    theme(legend.position = "none") +
    ggtitle(title)
  
  # Suppress messages so that we do not see the binwidth message.
  suppressMessages(print(plot))
  
}


# Plot the RD estimates.

plot_rd <- function(dataset, outcome_var, title = "") {
  
  rd_var <- paste0(outcome_var, "_ rd")
  
  plot <- dataset %>% 
    select(
      !!sym(paste0("km_", rd_var)),
      !!sym(paste0("aj_", rd_var)),
      !!sym(paste0("no_censor_", rd_var))
    ) %>% 
    pivot_longer(
      names_to = "Estimator", 
      cols = c(!!sym(paste0("km_", rd_var)), 
               !!sym(paste0("aj_", rd_var)), 
               !!sym(paste0("no_censor_", rd_var))),
      values_to = "Estimate"
    ) %>% 
    mutate(
      Estimator = case_when(
        Estimator == paste0("km_", rd_var) ~ "Kaplan Meier",
        Estimator == paste0("aj_", rd_var) ~ "Aalen-Johansen",
        Estimator == paste0("no_censor_", rd_var) ~ "No Censoring"
      )
    ) %>% 
    ggplot(aes(x = Estimate, y = Estimator, fill = Estimator)) +
    geom_density_ridges() +
    #scale_x_continuous(trans = "log10") +
    theme(legend.position = "none") +
    ggtitle(title)
  
  # Suppress messages so that we do not see the binwidth message.
  suppressMessages(print(plot))
  
}











