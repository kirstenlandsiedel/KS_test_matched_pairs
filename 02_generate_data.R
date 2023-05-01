# Header -----------------------------------------------------------------------
# Proj: Experimental Design Final Project
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Generates simulated data sets to test the efficiency of various test
#       statistics for matched pairs experiments

library(tidyverse)

# Function to generate potential outcomes --------------------------------------

generate_data <- function(n_pairs, treatment_outcome_fxn)
{
  pair_covs <- rnorm(n_pairs)
  data_unitlevel <- data.frame(pair_id = rep(1:n_pairs, each = 2),
                               unit_id = rep(1:2, times = n_pairs),
                               covariate = rep(pair_covs, each = 2)) %>%
    mutate(control_outcome = rnorm(n_pairs*2, mean = covariate)) %>%
    mutate(treat_outcome = treatment_outcome_fxn(control_outcome, covariate)) %>%
    mutate(treated_unit_in_pair = rep(rbinom(n_pairs, 1,  prob = 0.5) + 1, each = 2)) %>%
    mutate(treatment = as.numeric(treated_unit_in_pair == unit_id)) %>%
    mutate(outcome = treatment * treat_outcome + (1 - treatment) * control_outcome) %>%
    mutate(treatment_name = ifelse(treatment == 1, "treated", "control"))
 
  data_pairlevel <- pivot_wider(data_unitlevel,
                                names_from = treatment_name,
                                values_from = outcome,
                                id_cols = pair_id) %>%
    mutate(difference = treated - control)
  
  data_pairlevel
}

# Outcome functions -----------------------------------------------------------

# Various outcome functions, which determines how the treated potential outcome
# relates to the control potential outcome

outcome_1 <- function(control_outcome, covariate)
{
  control_outcome + 1
}

outcome_2 <- function(control_outcome, covariate)
{
  n <- length(control_outcome)
  control_outcome + rexp(n)
}

outcome_3 <- function(control_outcome, covariate)
{
  control_outcome + abs(covariate)
}

outcome_4 <- function(control_outcome, covariate)
{
  control_outcome + rnorm(length(control_outcome))
}

outcome_5 <- function(control_outcome, covariate)
{
  control_outcome + rexp(length(control_outcome)) - 1
}

generate_data(10000, outcome_5)
