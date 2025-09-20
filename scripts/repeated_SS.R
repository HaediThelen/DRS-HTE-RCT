library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(pROC)
library(patchwork)
library(scales)


#Set WD
#setwd()

source("./functions/gen_pop_cf.R")
source("./functions/rd_function.R")
source("./functions/stratify_function.R") 
source("./functions/simulation_functions.R")
source("./functions/data_summary_function.R") 
source("./scripts/trial_settings.R")
source("./functions/results_plots.R")

## Treatment Scenarios explored in all trial settings
# 1. Generate data for treatment scenarios
# a.	Treatment Scenario 1: no treatment effect
# b.	Treatment Scenario 2: small treatment effect (OR = 0.8)
# c.	Treatment Scenario 3: large treatment effect (OR = 0.5)
# d. Treatment Scenario 5: small treatment effect (OR = 0.8) + HTE on OR scale 
# e.	Treatment Scenario 4: large treatment effect (OR = 0.5) + HTE on OR scale 

# Choose Trial Setting from data frame all_trial_settings
# "base_case"  # Here we focus on the base case

current_trial <- all_trial_settings[all_trial_settings$trial_setting == "base_case",]

trial_setting <- current_trial$trial_setting
file_path <- current_trial$file_path
file_path <- sub("main_analysis", "RSS", file_path) # update file path
if (!dir.exists(file_path)) { #ensure results folder exists locally
  print("The directory will be created.")
  dir.create(file_path, recursive = TRUE)
}
cov_ORs_cx <- unlist(current_trial$cov_ORs_cx)
cov_ORs_tx <- unlist(current_trial$cov_ORs_tx)
n <- current_trial$n
covs_n <- current_trial$covs_n
cov_prev <- current_trial$cov_prev
cx_intercept <- current_trial$cx_intercept
tx_intercept <- current_trial$tx_intercept
covs <- unlist(current_trial$covs)
obj_strata_cuts <-  unlist(current_trial$obj_strata_cuts)

# 1a. Generate base population for 1a. Treatment Scenario 1: no treatment effect
# Generate Controls
set.seed(1234)
data_TxSc1 <- gen_pop_cf(
  n = n, 
  X = covs_n, 
  cov_prev = cov_prev, 
  cx_cov_ORs = cov_ORs_cx,
  cx_intercept = cx_intercept, 
  tx_cov_ORs = cov_ORs_tx, 
  tx_intercept = tx_intercept, 
)

# identify external risk strata
data_TxSc1$objective_strata <- stratify_function(data_TxSc1, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc1_sum <- data_summary(data_TxSc1, "OR=1")
data_TxSc1_sum 

# 1b. Generate base population for 1b. Treatment Scenario 2: small treatment effect (OR = 0.8)
tx_intercept <- cx_intercept + log(0.8)

# Generate treated, combine data, and stratify
set.seed(3456)
data_TxSc2<- gen_pop_cf(
  n = n, 
  X = covs_n, 
  cov_prev = cov_prev, 
  cx_cov_ORs = cov_ORs_cx,
  cx_intercept = cx_intercept, 
  tx_cov_ORs = cov_ORs_tx, 
  tx_intercept = tx_intercept
)

data_TxSc2$objective_strata <- stratify_function(data_TxSc2, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc2_sum <- data_summary(data_TxSc2, "OR =0.8")
data_TxSc2_sum 

# 1c. Treatment Scenario 3: large treatment effect (OR = 0.5)
tx_intercept <- cx_intercept + log(0.5)

# Generate treated, combine data, and stratify
set.seed(3456)
data_TxSc3 <- gen_pop_cf(
  n = n, 
  X = covs_n, 
  cov_prev = cov_prev, 
  cx_cov_ORs = cov_ORs_cx,
  cx_intercept = cx_intercept, 
  tx_cov_ORs = cov_ORs_tx, 
  tx_intercept = tx_intercept
)
data_TxSc3$objective_strata <- stratify_function(data_TxSc3, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc3_sum <- data_summary(data_TxSc3, "OR = 0.5")
data_TxSc3_sum 

# 1d. Treatment Scenario 3: large treatment effect (OR = 0.8 + interactions)
# Generate population and stratify
tx_intercept <- cx_intercept + log(0.8)
cov_ORs_tx <- c(1, 1, 1, 1.4, 1.2, 1, 2, 1.5, 1, 2.5, 2, 1.5) # Include interactions
set.seed(3456)
data_TxSc4 <- gen_pop_cf(
  n = n, 
  X = covs_n, 
  cov_prev = cov_prev, 
  cx_cov_ORs = cov_ORs_cx,
  cx_intercept = cx_intercept, 
  tx_cov_ORs = cov_ORs_tx, 
  tx_intercept = tx_intercept
)
data_TxSc4$objective_strata <- stratify_function(data_TxSc4, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc4_sum <- data_summary(data_TxSc4, "OR = 0.8 + HTE")
data_TxSc4_sum

# 1e. Generate base population for Treatment Scenario 4: large treatment effect (OR = 0.5) + HTE on OR scale
tx_intercept <- cx_intercept + log(0.5)
cov_ORs_tx <- c(1, 1, 1, 1.4, 1.2, 1, 2, 1.5, 1, 2.5, 2, 1.5) # Include interactions

# Generate treated, combine data, and stratify
set.seed(3456)
data_TxSc5 <- gen_pop_cf(
  n = n, 
  X = covs_n, 
  cov_prev = cov_prev, 
  cx_cov_ORs = cov_ORs_cx,
  cx_intercept = cx_intercept, 
  tx_cov_ORs = cov_ORs_tx, 
  tx_intercept = tx_intercept
)

data_TxSc5$objective_strata <- stratify_function(data_TxSc5, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc5_sum <- data_summary(data_TxSc5, "OR = 0.5 + Interactions")
data_TxSc5_sum

# 2. Run simulation
n_sim <- current_trial$n_sim
n_sample_cx <- current_trial$n_sample_cx
n_sample_tx <- current_trial$n_sample_tx

m_splits <- 50

system.time(TxSc1 <- simulation_function_RSS_parallel(data_TxSc1, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, m_splits = 50))
TxSc2 <- simulation_function_RSS_parallel(data_TxSc2, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts, m_splits)
TxSc3 <- simulation_function_RSS_parallel(data_TxSc3, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts, m_splits)
TxSc4 <- simulation_function_RSS_parallel(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts, m_splits)
TxSc5 <- simulation_function_RSS_parallel(data_TxSc5, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts, m_splits)

# #save results as csv
write.csv(TxSc1, "./results/RSS/base_case/TxSc1.csv", row.names = FALSE)
write.csv(TxSc2, "./results/RSS/base_case/TxSc2.csv", row.names = FALSE)
write.csv(TxSc3, "./results/RSS/base_case/TxSc3.csv", row.names = FALSE)
write.csv(TxSc4, "./results/RSS/base_case/TxSc4.csv", row.names = FALSE)
write.csv(TxSc5, "./results/RSS/base_case/TxSc5.csv", row.names = FALSE)


