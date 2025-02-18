#####################################################
# Program: Data generation functions. R
# Programmer: Chase
# Date last modified: 01.22.2024
#
# Purpose: The program contains all the R functions
# for generating the data. The goal of doing this
# is to ensure that these functions only need to be
# editing one time and will apply throughout all
# simulations easily.
#####################################################


# This is the baseline function that will be 
# -- used to generate each cohort.
# This function will be run to generate a cohort 
# -- for each simulation.
each_sim <- function(n_sim, n, gw_index=10){
  
  initial_seed <- list(.Random.seed)
  
  # Generate 0 through 40 gestational weeks - 1 vector
  gw = list(seq(0,40, by = 1))
  
  # Create data.table
  data <- data.table(
    sim_id = n_sim, #Simulation ID
    id = 1:n, #Each individual's ID
    first_index=gw_index #Index date
  )
  
  # Generate phase 1 outcomes
  data[, phase1_outcomes := lapply(1:.N, function(i) sample_outcomes_for_id(phase1))]
  
  # Compute last GA before 18 weeks in batch
  data[, last_GA_pre18 := sapply(phase1_outcomes, pre18)] 

  # Compute trial participation indicator
  data[, trial_participant := (first_index <= last_GA_pre18)]
  
  # Precompute phase 2 (i.e., treated) outcomes ONCE for all rows
  data[, phase2_outcomes_pre := lapply(1:.N, function(i) sample_outcomes_for_id(phase2))]
  
  # Replace the outcomes prior to the index dates with those from the phase 1 (i.e., untreated) outcomes
  data[, phase2_outcomes := lapply(1:.N, function(i) {
    resample_outcomes(first_index[[i]], phase1_outcomes[[i]], phase2_outcomes_pre[[i]])
  })]
  
  #### PHASE 3 -- determine whether a person developed preeclampsia & revised outcomes
  
  ## Untreated
  data[, odds_preeclampsia_untrt := list(exp(phase3$ln_odds_preeclampsia))]
  data[, prob_preeclampsia_untrt := lapply(odds_preeclampsia_untrt, function(odds) {
    # Return the transformed probabilities, appending 0 for the first 25 elements
    c(rep(0, 25), odds / (1 + odds))
  })]
  data[, preeclampsia_list_untrt := lapply(prob_preeclampsia_untrt, 
                                           function(p) rbinom(length(p), size = 1, prob = p))]
  
  ## Treated
  data[, odds_preeclampsia_trt := list(exp(phase3$ln_odds_preeclampsia +
                                             phase3$treat_effect_ln_OR))]
  data[, prob_preeclampsia_trt := lapply(odds_preeclampsia_trt, function(odds) {
    # Return the transformed probabilities, appending 0 for the first 25 elements
    c(rep(0, 25), odds / (1 + odds))
  })]
  data[, preeclampsia_list_trt := lapply(prob_preeclampsia_trt, 
                                           function(p) rbinom(length(p), size = 1, prob = p))]
  
  # Determine the final pregnancy outcomes with preeclampsia timing
  
  ## Untreated
  data[, final_pregnancy_outcomes_untrt := lapply(1:.N, function(i) 
    preg_outcome(phase1_outcomes[[i]], preeclampsia_list_untrt[[i]], phase3))
  ]
  
  ## Treated
  data[, final_pregnancy_outcomes_trt := lapply(1:.N, function(i) 
    preg_outcome(phase2_outcomes[[i]], preeclampsia_list_trt[[i]], phase3))
    ]
  
  ##### PHASE 4 -- Determine if people have SGA
  
  ## Untreated
  data[, sga_untrt := lapply(1:.N, function(i)
    sga_func(final_pregnancy_outcomes_untrt[[i]], 1, phase4))
  ]
  
  ## Treated
  data[, sga_trt := lapply(1:.N, function(i)
    sga_func(final_pregnancy_outcomes_trt[[i]], 1, phase4))
    ]
  
  ##### PHASE 5 -- Introduce censoring
  data[, first_censoring_gw := lapply(1:.N, function(i)
    censoring(phase5, first_index[[i]]))
    ]
  
  # Make data into a tibble and then do the rest of the calculations
  data2 <- data %>% 
    as_tibble() %>% 
    mutate(final_pregnancy_outcomes_trt = map(final_pregnancy_outcomes_trt, 
                                              ~set_names(.x, str_c(names(.x), "_trt"))),
           final_pregnancy_outcomes_untrt = map(final_pregnancy_outcomes_untrt, 
                                                ~set_names(.x, str_c(names(.x), "_untrt")))) %>% 
    unnest_wider(c(final_pregnancy_outcomes_trt, final_pregnancy_outcomes_untrt)) 
  
  finish_seed <- list(.Random.seed)
  
  ## Store the random states
  data3 <- data2 %>% 
    mutate(
      start_seed = initial_seed,
      end_seed = finish_seed
    )
  
  return(data3)
  
}







### Below, are all of the functions that will be used within the `each_sim()` function.



# This function will create the pregnancy outcomes based upon the probabilities
# -- recorded in phase1, phase2, phase3, and phase4.
sample_outcomes_for_id <- function(data) {
  #id_data <- subset(your_data, id == id)  # Filter data for the specific id
  
  # Apply this function to each row of phase1-phase4 data where needed.
  # -- Create vector of 1:nrow(data) and then apply the function below
  outcomes <- sapply(1:nrow(data), function(i) { 
    
    # Indicate the potential options to select from
    options <- c("fetaldeath_next", "livebirth_next", "contpreg_next")
    
    # Assign the probabilities to each of the potential pregnancy outcome options based upon 
    # -- the corresponding rows in the phase1-4 files.
    probabilities <- data[i, c("p_fetaldeath_next", "p_livebirth_next", "p_contpreg_next")]
    
    # Sample an option based on probabilities
    sampled_option <- sample(options, size = 1, prob = probabilities)
    
    # Return that sampled option. This will be stored in the vector outcomes
    return(sampled_option)
  })
  
  #list(outcomes = outcomes)
  # return(list(outcomes))
  return(outcomes)
}

#Testing: outcomes <- test$phase1_outcomes[[1]]




# This function outputs the week of the first fetal death event that occurs after the index date
# but prior to 18 weeks of gestation (i.e., 20 weeks post-LMP)
pre18 <- function(outcomes) {
  # Extract outcomes for gestational weeks 0 through 18 (indices 1:19)
  outcomes_sub <- outcomes[1:19]  
  
  # Find the first occurrence of 'fetaldeath_next'
  first_ga <- which(outcomes_sub == 'fetaldeath_next')
  
  # If no fetal death is found, return 18; otherwise, return first occurrence - 1
  last_ga <- if (length(first_ga) == 0) 18 else first_ga[1]
  
  return(last_ga)
}




# Re-sample pregnancy outcomes for those pregnancies that were treated.
# -- Want to retain their phase1 outcomes until their index event, and then 
# -- resample all pregnancy outcomes starting at their index.
# -- This assumes an immediate treatment effect.
resample_outcomes <- function(first_index, phase1_outcomes, precomputed_phase2) {
  index <- first_index + 1
  phase1 <- phase1_outcomes[1:(index - 1)]
  outcomes <- c(phase1, precomputed_phase2[index:length(precomputed_phase2)])
  return(outcomes)
}


## This function generates the final pregnancy outcome for each of the observed pregnancies

preg_outcome <- function(phase2_outcomes, preeclampsia_list, phase3){
  #browser()
  
  # Determine which occurred first: pregnancy outcome or preeclampsia.
  # -- If they occurred within the same gestational week, we will assume that the preeclampsia occurred first.
  
  ## First outcome from phase2_outcomes
  first_outcome_index <- which(phase2_outcomes %in% c('fetaldeath_next','livebirth_next'))[1]
  
  ## First preeclampsia from preeclampsia_list
  first_preeclampsia <- which(preeclampsia_list == 1)[1]
  
  # If no preeclampsia evidence or first outcome from phase2_outcomes, 
  # -- then return a list of pregnancy outcomes from phase2_outcomes
  # -- Do not worry about +/- 1 here because they're both indexed in a vector from 1.
  if(first_outcome_index < first_preeclampsia | is.na(first_preeclampsia)){
    return(list(
      
      # Determine what the final outcome was from phase2_outcomes
      preg_outcome_final = sub("_next$", "", phase2_outcomes[first_outcome_index]),
      
      # Determine the gestational timing of the first outcome
      # -- Originally, we added 1 here because we are assuming that the outcome is observed at the 
      # -- week after they're determined.
      # -- However, R indexes a vector from 1, not 0, and we are indexing from 0, so we would
      # -- subtract 1 to account for that. -- They equal out.
      preg_outcome_final_gw = first_outcome_index,
      
      # Make preeclampsia indicator - Preeclampsia didn't occur before the outcome and so not observed
      preeclampsia = 0
    ))
  }
  
  
  ## If first or equally first outcome is preeclampsia, then regenerate the pregnancy outcome and timing
  if(first_outcome_index >= first_preeclampsia){
    
    # Regenerate the pregnancy outcome
    p_fetaldeath <- subset(phase3, gestweek_conception == (first_preeclampsia - 1))$p_fetaldeath
    p_livebirth <- subset(phase3, gestweek_conception == (first_preeclampsia - 1))$p_livebirth
    options <- c("fetaldeath", "livebirth")
    # Assign the corresponding probabilities to each of the potential pregnancy outcomes
    probabilities <- c(p_fetaldeath, p_livebirth)
    outcome <- sample(options, size=1, prob=probabilities)
    
    return(list(
      
      # Determine the final pregnancy outcome from the probabilities in phase 3
      preg_outcome_final = outcome,
      
      # Determine the gestational timing of the outcome - same logic on not adding/subtracting 1
      preg_outcome_final_gw = first_preeclampsia,
      
      # Make preeclampsia indicator - Preeclampsia observed before the outcome so observed
      preeclampsia = 1
      
    ))
  }
  
}

sga_func <- function(final_pregnancy_outcomes, trt, phase4){
  
  # Determine probability of SGA based upon trt and preeclampsia
  prob_sga <- subset(phase4, trt_value == trt & preeclampsia_flag ==
                       final_pregnancy_outcomes$preeclampsia)$p_sga
  
  # Determine if they actually had SGA based upon this probability
  sga <- ifelse(final_pregnancy_outcomes$preg_outcome_final == "fetaldeath",
                0,
                rbinom(1,1, prob = prob_sga))

  return(sga)
}


# This function will determine censoring events based upon the probabilities 
# -- recorded in phase5.
censoring <- function(data, first_index){
  
  # Create a vector of the censoring probabilities
  # -- Each element is a gestational week
  prob_censoring <- data$p_censoring
  
  # Determine the length of prob_censoring for easy reference later
  n <- length(prob_censoring)
  
  # Create a vector with the realized censoring events based
  # -- upon the prob_censoring vecotr
  censor <- rbinom(n, size = 1, prob=prob_censoring)
  
  # Determine the first_index - we are interested in the first censoring
  # -- event after this index.
  first_index2 <- first_index + 1
  # We add one because first_index gw starts at 0, but R vectors start at 1.
  
  # Determine the gw of the first censoring event
  censoring <- which(censor == 1)
  #first_censor <- which(censor[first_index2:n]==1)[1] - 1
  # Subtract one to get back to gw indexed at 0 or conception
  
  first_censor_index <- which(censoring > first_index2)
  
  first_censor <- ifelse(all(censor == 0),
                         NA,
                         censoring[first_censor_index] - 1)
  
  # Return the gestational week of the first censoring event
  # -- Note that this is independent of outcome generation and exposure
  # -- Thus, uninformative.
  return(first_censor)
  
}



