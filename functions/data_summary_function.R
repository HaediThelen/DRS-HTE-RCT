#Function to create a data summary
data_summary <- function(data, scenario_name) {
  summary_table <- data %>%
    group_by(objective_strata, A) %>%
    summarise(
      n = n(),
      n_outcome = sum(Y == 1, na.rm = TRUE),
      prop_outcome = mean(Y == 1, na.rm = TRUE)
    ) %>%
    pivot_wider(
      names_from = A,
      values_from = c(n, n_outcome, prop_outcome),
      names_glue = "{.value}_{A}"
    ) %>%
    mutate(
      diff_prop = round(prop_outcome_0 - prop_outcome_1, 3),
      scenario = scenario_name
    ) %>%
    dplyr::select( scenario,
                   objective_strata,
                   n_outcome_0,  n_0, prop_outcome_0,
                   n_outcome_1,  n_1, prop_outcome_1,
                   diff_prop
    )
  return(summary_table)
}

#Function to create a data summary including ORs
data_summary_or <- function(data, scenario_name) {
  summary_table <- data %>%
    group_by(objective_strata, A) %>%
    summarise(
      n = n(),
      n_outcome = sum(Y == 1, na.rm = TRUE),
      prop_outcome = mean(Y == 1, na.rm = TRUE)
    ) %>%
    pivot_wider(
      names_from = A,
      values_from = c(n, n_outcome, prop_outcome),
      names_glue = "{.value}_{A}"
    ) %>%
    mutate(
      diff_prop = round(prop_outcome_0 - prop_outcome_1, 3),
      odds_ratio = round((prop_outcome_1 / (1 - prop_outcome_1)) / 
                           (prop_outcome_0 / (1 - prop_outcome_0)), 3),
      scenario = scenario_name
    ) %>%
    dplyr::select( scenario,
                   objective_strata,
                   n_outcome_0,  n_0, prop_outcome_0,
                   n_outcome_1,  n_1, prop_outcome_1,
                   diff_prop, odds_ratio
    )
  return(summary_table)
}
