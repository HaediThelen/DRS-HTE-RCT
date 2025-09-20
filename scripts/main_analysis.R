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

source("./scripts/trial_settings.R")
source("./functions/gen_pop_cf.R")
source("./functions/stratify_function.R") 
source("./functions/data_summary_function.R") 
source("./functions/rd_function.R")
source("./functions/simulation_functions.R")
source("./functions/results_plots.R")

## Treatment Scenarios explored in all trial settings
# 1. Generate data for treatment scenarios
# a.	Treatment Scenario 1: no treatment effect
# b.	Treatment Scenario 2: small treatment effect (OR = 0.8)
# c.	Treatment Scenario 3: large treatment effect (OR = 0.5)
# d.  Treatment Scenario 5: small treatment effect (OR = 0.8) + treatment-covariate interactions
# e.	Treatment Scenario 4: large treatment effect (OR = 0.5) + treatment-covariate interactions


# Choose Trial Setting from data frame all_trial_settings
# "base_case"      "a_smplsz500"    "b_smplsz1000"   "c_smplsz5000"   "d_smplsz10000" 
# "e_outcome_0.15" "f_outcome_0.05" "g_outcome_0.5"  "h_ratio_2to1"   "i_ratio_3to1"  
# "j_ratio_4to1"   "k_auc_0.70"    "l_auc_0.75"     "m_auc_0.80"     "n_auc_0.85" 


current_trial <- all_trial_settings[all_trial_settings$trial_setting == "n_auc_0.85",]

trial_setting <- current_trial$trial_setting
file_path <- current_trial$file_path
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

# 1a. Generate base population for Treatment Scenario 1: no treatment effect
# Generate population and stratify
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
data_TxSc1$objective_strata <- stratify_function(data_TxSc1, "pY0", 4, cuts = obj_strata_cuts)
data_TxSc1_sum <- data_summary(data_TxSc1, "OR=1")
data_TxSc1_sum 

# 1b. Generate base population for 1b. Treatment Scenario 2: small treatment effect (OR = 0.8)
tx_intercept <- cx_intercept + log(0.8)
# Generate population and stratify
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

# Generate population and stratify
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

# 1d. Treatment Scenario 4: large treatment effect (OR = 0.8) + treatment-covariate interactions
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
data_TxSc4_sum <- data_summary(data_TxSc4, "OR = 0.8 + Interactions")
data_TxSc4_sum


# 1e. Generate base population for Treatment Scenario 5: large treatment effect (OR = 0.5) + treatment-covariate interactions
tx_intercept <- cx_intercept + log(0.5)
cov_ORs_tx <- c(1, 1, 1, 1.4, 1.2, 1, 2, 1.5, 1, 2.5, 2, 1.5) # Include interactions

# Generate population and stratify
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

TxSc1 <- simulation_function_parallel(data_TxSc1, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts)
TxSc2 <- simulation_function_parallel(data_TxSc2, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts)
TxSc3 <- simulation_function_parallel(data_TxSc3, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts)
TxSc4 <- simulation_function_parallel(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts)
TxSc5 <- simulation_function_parallel(data_TxSc5, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts)


# # Save simulation results as csv
# write.csv(TxSc1, paste0(file_path, "/", trial_setting, "_TxSc1.csv"), row.names = FALSE)
# write.csv(TxSc2, paste0(file_path, "/", trial_setting, "_TxSc2.csv"), row.names = FALSE)
# write.csv(TxSc3, paste0(file_path, "/", trial_setting, "_TxSc3.csv"), row.names = FALSE)
# write.csv(TxSc4, paste0(file_path, "/", trial_setting, "_TxSc4.csv"), row.names = FALSE)
# write.csv(TxSc5, paste0(file_path, "/", trial_setting, "_TxSc5.csv"), row.names = FALSE)


# # Load results
# TxSc1 <- read.csv(paste0(file_path, "/", trial_setting, "_TxSc1.csv"))
# TxSc2 <- read.csv(paste0(file_path, "/", trial_setting, "_TxSc2.csv"))
# TxSc3 <- read.csv(paste0(file_path, "/", trial_setting, "_TxSc3.csv"))
# TxSc4 <- read.csv(paste0(file_path, "/", trial_setting, "_TxSc4.csv"))
# TxSc5 <- read.csv(paste0(file_path, "/", trial_setting, "_TxSc5.csv"))


# 3. Results
# Risk distribution
risk_dist <- function(data, risk_col, obj_strata_cuts, method) {
  risk_values <- data[[risk_col]]
  
  finite_cuts <- obj_strata_cuts
  finite_cuts[finite_cuts == -Inf] <- min(risk_values, na.rm = TRUE)
  finite_cuts[finite_cuts == Inf]  <- max(risk_values, na.rm = TRUE)
  
  midpoints <- (head(finite_cuts, -1) + tail(finite_cuts, -1)) / 2
  strata_labels <- paste0("S", seq_along(midpoints))
  
  y_max <- 65000
  y_label <- y_max *1.05
  
  # Create a data frame for strata labels
  strata_df <- data.frame(
    x = midpoints,
    y = y_label,
    label = strata_labels
  )
  
  p <- ggplot(data, aes(x = .data[[risk_col]])) +
    geom_histogram(
      bins = 32,
      fill = "#2366cc",
      color = "#2366cc",
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = obj_strata_cuts[is.finite(obj_strata_cuts)],
      linewidth = 0.5,
      linetype = "dashed",
      color = "black"
    ) +
    geom_text(
      data = strata_df, aes(x = x, y = y, label = label), vjust = 0, size = 3,
      color = "black", inherit.aes = FALSE ) +
    facet_wrap(~ A, labeller = as_labeller(c("0" = "Control Group", "1" = "Treatment Group"))) +
    labs( title = "", x = "Disease Risk Score", y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5), axis.text = element_text(color = "black"),
          text = element_text(size = 10),
          legend.text = element_text(size = 10)) 
  
  return(p)
}

risk_dist_TxSc1 <- risk_dist(data_TxSc1, "pY0", obj_strata_cuts, "TxSc1")
risk_dist_TxSc1 # check other data sets as needed
# Save as PDF 
ggsave(paste0(file_path, "/", trial_setting, "_risk_dist_TxSc1.pdf"), plot = risk_dist_TxSc1, device = "pdf", width = 7, height = 5, units = "in")

# Average outcomes in sample, EPV, and AUC
auc<-suppressMessages(auc(data_TxSc1$Y, data_TxSc1$pY0)[1])

count_outcomes <- function(data) {
  sample <- data[sample(1:nrow(data), n_sample_cx+n_sample_tx),]
  
  # Calculate metrics
  outcomes <- sum(sample$Y)
  epv <- outcomes/12
  
  # Subset for treatment A=0 
  sample_cx <- sample[sample$A==0,]
  outcomes_cx <- sum(sample_cx$Y)
  epv_ss <- outcomes_cx/12/2
  
  return(c(outcomes=outcomes, epv=epv, 
           outcomes_cx=outcomes_cx, epv_ss=epv_ss))
}

# Run 100 iterations
results <- replicate(1000, count_outcomes(data_TxSc1)) 

# Calculate averages
averages <- as.data.frame(t(rowMeans(results)))
averages$AUC <- auc
averages
write.csv(averages, paste0(file_path, "/", trial_setting, "_sample_outcomes.csv"))

# Bar chart of patients in each strata, color coded by treated and control
dist_GT_strata <- data_TxSc1 %>%
  ggplot(aes(x = objective_strata, fill = factor(A))) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(title = "Distribution of Ground Truth DRS Strata",
       x = "Ground Truth DRS Strata",
       y = "Number of Subjects") +
  scale_fill_manual(values = c("orange", "#1f64b4"), 
                    name = NULL, 
                    labels = c("Control","Treated")) +
  scale_y_continuous(labels = comma)+
  theme_minimal()
dist_GT_strata

# Save as png
ggsave(paste0(file_path, "/", trial_setting, "_dist_GT_strata.png"), plot = dist_GT_strata, device = "png", width = 6, height = 4, units = "in", dpi = 600)


# Summary Table
c_sum_table_short <- bind_rows(data_TxSc1_sum, data_TxSc2_sum, data_TxSc3_sum, data_TxSc4_sum, data_TxSc5_sum)
c_sum_table_short <- c_sum_table_short %>%
  select(-scenario)

formatted_table_short <- c_sum_table_short %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  kable(
    col.names = c("Strata", "Event", "N", "Risk", "Event", "N", "Risk", "RD"),
    align = c('l', rep('c', 7)),
    caption = "Summary of Outcomes by Scenario, Objective Strata, and Treatment"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Control" = 3, "Treatment" = 3, " " = 1)) %>%
  pack_rows("No Tx Effect", 1, 4) %>%
  pack_rows("Tx Effect: 0.8", 5, 8) %>%
  pack_rows("Tx Effect: 0.5", 9, 12)%>%
  pack_rows("Tx Effect: 0.8 + Interactions", 13, 16) %>%
  pack_rows("Tx Effect: 0.5 + Interactions", 17, 20)

formatted_table_short
# Save summary table
write.csv(c_sum_table_short, paste0(file_path, "/", trial_setting, "_summary_table.csv"))

# Plot Bias
# Data Prep: calculate bias
TxSc1_bias <- rd_bias_function(TxSc1)
TxSc2_bias <- rd_bias_function(TxSc2)
TxSc3_bias <- rd_bias_function(TxSc3)
TxSc4_bias <- rd_bias_function(TxSc4)
TxSc5_bias <- rd_bias_function(TxSc5)
c_rd_data_short <- list(TxSc1_bias,TxSc2_bias, TxSc3_bias, TxSc4_bias, TxSc5_bias)

# Point Plot of bias
methods <- c("CO", "FS", "SS")
sim_results <-list(TxSc1, TxSc2, TxSc3, TxSc4, TxSc5)
OPB_results <- apply_percent_bias_function(sim_results, methods, "True_risk_strata")
OAB_results <- apply_OAB_function(sim_results, methods, "True_risk_strata")

# Save summary results
write.csv(OAB_results, paste0(file_path, "/", trial_setting, "_OAB_results.csv"))
write.csv(OPB_results, paste0(file_path, "/", trial_setting, "_OPB_results.csv"))


small_titles = c("No Treatment Effect", "Treatment Effect: OR = 0.8", "Treatment Effect: OR = 0.5", "Treatment Effect:\nOR = 0.8 + Covariate Interactions", "Treatment Effect:\nOR = 0.5 + Covariate Interactions")
point_bias_plot <- composite_pointplot_bias(c_rd_data_short, small_titles,
                                            overall_title = trial_setting,
                                            OPB_results, OAB_results)
point_bias_plot
# Save point plot of bias
ggsave(paste0(file_path, "/", trial_setting, "_Point_Bias_Results.pdf"), plot = point_bias_plot, width = 6.5, height = 9.5, units = "in")


# Point Plot of risk difference
methods <- c("CO", "FS", "SS")
point_rd_plot <- composite_pointplot_rd(sim_results, small_titles, overall_title = trial_setting, OPB_results, OAB_results)
point_rd_plot
# Save point Plot of risk difference
ggsave(paste0(file_path, "/", trial_setting, "_point_rd_plot.pdf"), plot = point_rd_plot, width = 6.5, height = 9.5, units = "in")


