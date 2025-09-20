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
source("./functions/alt_ss_drs_train_pct_simulaiton_function.R") 
source("./functions/data_summary_function.R") 
source("./scripts/trial_settings.R")
source("./functions/results_plots.R")

## Treatment Scenarios explored in all trial settings
# 1. Generate data for treatment scenarios
# a.	Treatment Scenario 1: no treatment effect
# b.	Treatment Scenario 2: small treatment effect (OR = 0.8)
# c.	Treatment Scenario 3: large treatment effect (OR = 0.5)
# d.  Treatment Scenario 5: small treatment effect (OR = 0.8) + treatment-covariate interactions
# e.	Treatment Scenario 4: large treatment effect (OR = 0.5)+ treatment-covariate interactions


# Choose Trial Setting from data frame all_trial_settings
# Here we focus on settings with samples sizes 500, 2600 (base case) and 1000
# "base_case" "a_smplsz500"    "b_smplsz1000"  

current_trial <- all_trial_settings[all_trial_settings$trial_setting == "b_smplsz1000",]

trial_setting <- current_trial$trial_setting
file_path <- current_trial$file_path
file_path <- sub("main_analysis", "alt_ss_proportion", file_path) # update file path
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


# 2. Run simulation
n_sim <- current_trial$n_sim
#n_sim <- 5000
n_sample_cx <- current_trial$n_sample_cx
n_sample_tx <- current_trial$n_sample_tx


drs_train_10 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.1)
drs_train_20 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.2)
drs_train_30 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.3)
drs_train_40 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.4)
drs_train_50 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.5)
drs_train_60 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.6)
drs_train_70 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.7)
drs_train_80 <- simulation_function_parallel_alt_drs_train_pct(data_TxSc4, "Y", covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata = "objective_strata", obj_strata_cuts = obj_strata_cuts, split_pct = 0.8)

# OPB 
sim_results <-list(drs_train_10, drs_train_20, drs_train_30, drs_train_40, drs_train_50, drs_train_60, drs_train_70, drs_train_80)

small_titles <-  c(
                   "DRS Train Size: 10%", 
                   "DRS Train Size: 20%", 
                   "DRS Train Size: 30%", 
                   "DRS Train Size: 40%", 
                   "DRS Train Size: 50%",
                   "DRS Train Size: 60%",
                   "DRS Train Size: 70%",
                   "DRS Train Size: 80%")
number_titles <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

# RD plots
composite_pointplot_bias <- function(composite_rd_data, small_titles, overall_title = NULL) {
  
  y_limits <- get_y_limits_rd(composite_rd_data)
  y_min <- floor(y_limits[1] * 10) / 10 -0.03
  y_max <- ceiling(y_limits[2] * 10) / 10 
  print(y_limits)
  print(y_min)
  print(y_max)
  
  # Initialize an empty list to store the plots
  plot_list <- list()
  
  # Loop over the data for the four scenarios
  for (i in 1:length(composite_rd_data)) {
    print(i)
    rd_data <- composite_rd_data[[i]]
    
    # identify and flag NAs
    na_rows <- rd_data %>%
     # select(starts_with("DRS_SS_")) %>%
      select(1:12) %>% #just the RD rows
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "DRS_(.*)_strata(\\d+)_sampled",
        values_to = "diff"
      ) %>%
      filter(if_any(everything(), is.na)) # Filter rows with NA
    
    # Print the number of rows dropped and the rows themselves
    if (nrow(na_rows) > 0) {
      cat("Dropped", nrow(na_rows), "rows with NA:\n")
      print(na_rows)
    }
    
    # Reshape the data into long format
    rd_data_long <- rd_data  %>% # Drop columns that are for CO and FS methods
      select(starts_with("DRS_SS_"))%>%
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "DRS_(.*)_strata(\\d+)_sampled",
        values_to = "diff"
      ) %>%
      drop_na() %>%  # Drop rows with NA
      mutate(Strata = as.factor(strata))  
    
    # Calculate mean and standard deviation of bias for error bars
    summary_data <- rd_data_long %>%
      group_by(strata, method) %>%
      summarise(mean = mean(diff), SD_diff = sd(diff), .groups = "drop")
    print(summary_data)
    
    # Extract Ground Truth values for each strata
    ground_truth<- data.frame()
    ground_truth <- data.frame(
      strata = c(1, 2, 3, 4),
      mean = c(
        rd_data$True_risk_strata1[1], 
        rd_data$True_risk_strata2[1], 
        rd_data$True_risk_strata3[1], 
        rd_data$True_risk_strata4[1]))
    
    ground_truth <- ground_truth %>%
      mutate(method = "SS") %>%
      select(strata, method, mean)
    
    print(ground_truth)
    p <-ggplot(summary_data, aes(x = as.numeric(as.factor(method)), y = mean, color = as.factor(strata))) + 
      geom_point(data = ground_truth, aes(x = as.numeric(as.factor(method)), y = mean, 
                                          group = as.factor(strata), color = "Ground Truth"), 
                 position = position_dodge(width = 0.8), size = 9, shape = 95) + #ground truth
      geom_point(position = position_dodge(width = 0.8), size = 3) + # point estimates
      geom_errorbar(aes(ymin = mean - SD_diff, ymax = mean + SD_diff), 
                    position = position_dodge(width = 0.8), width = 0.2) +
      labs(x = " ", y = "Risk Difference", color = "Legend", title = small_titles[i]) + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 10, hjust = 0.5), axis.text = element_text(color = "black"),
            text = element_text(size = 10),
            legend.text = element_text(size = 10)) + 
      scale_color_manual(values = c("#6BAED6", "#3182BD", "#08519C", "navy", "red"), 
                         labels = c("Strata 1", "Strata 2", "Strata 3", "Strata 4", "Ground Truth \nRisk Difference")) + 
      ylim(y_min, y_max) +
      guides(color = guide_legend(
        override.aes = list(
          color = c("#6BAED6", "#3182BD", "#08519C", "navy", "red"),  
          shape = c(16, 16, 16, 16, 95),  
          size = 5)))
    
    #Remove legend      
    p <- p + theme(legend.position = "none")
    
    # Add the plot to the list
    plot_list[[i]] <- p
  }
  
  # Combine the plots into a grid using ggarrange
  composite_plot <- ggarrange(plot_list[[1]],
                              plot_list[[2]],
                              plot_list[[3]],
                              plot_list[[4]],
                              plot_list[[5]],
                              plot_list[[6]],
                              plot_list[[7]],
                              plot_list[[8]],
                              ncol = 4, nrow = 2,
                              font.label = list(size = 9)
  )
  # Add the overall title if specified
  if (!is.null(overall_title)) {
    composite_plot <- composite_plot + plot_annotation(title = overall_title)
  }
  
  # Return the composite plot
  return(composite_plot)
}
composite_pointplot_bias(sim_results, small_titles, overall_title = "RD in Each Strata in Split-Sample Method by Split Percent in DRS vs Outcome Model")

# Calculate Bias
drs_train_10_bias <- rd_bias_function(drs_train_10)
drs_train_20_bias <- rd_bias_function(drs_train_20)
drs_train_30_bias <- rd_bias_function(drs_train_30)
drs_train_40_bias <- rd_bias_function(drs_train_40)
drs_train_50_bias <- rd_bias_function(drs_train_50)
drs_train_60_bias <- rd_bias_function(drs_train_60)
drs_train_70_bias <- rd_bias_function(drs_train_70)
drs_train_80_bias <- rd_bias_function(drs_train_80)

c_rd_data_short <- list(drs_train_10_bias, drs_train_20_bias, drs_train_30_bias, drs_train_40_bias, drs_train_50_bias, drs_train_60_bias, drs_train_70_bias, drs_train_80_bias)


# Bias plots
composite_pointplot_bias <- function(composite_rd_data, small_titles, overall_title = NULL) {
  
  y_limits <- get_y_limits(composite_rd_data)
  # Round up to nearest 0.05
  y_lim <- ceiling(max(abs(y_limits)) * 20) / 20  +0.03 
  
  # Initialize an empty list to store the plots
  plot_list <- list()
  
  # Loop over the data for the four scenarios
  for (i in 1:length(composite_rd_data)) {
    rd_data <- composite_rd_data[[i]] %>% # Drop columns that are for CO and FS methods
      select(starts_with("DRS_SS_"))
    
    # identify and flag NAs
    na_rows <- rd_data %>%
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "DRS_(.*)_strata(\\d+)_bias",
        values_to = "diff"
      ) %>%
      filter(if_any(everything(), is.na)) # Filter rows with NA
    
    # Print the number of rows dropped and the rows themselves
    if (nrow(na_rows) > 0) {
      cat("Dropped", nrow(na_rows), "rows with NA:\n")
      print(na_rows)
    }
    
    # Reshape the data into long format
    rd_data_long <- rd_data %>%
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "DRS_(.*)_strata(\\d+)_bias",
        values_to = "diff"
      ) %>%
      drop_na() %>%  # Drop rows with NA
      mutate(Strata = as.factor(strata))  
    
    # Calculate mean and standard deviation of bias for error bars
    summary_data <- rd_data_long %>%
      group_by(strata, method) %>%
      summarise(
        bias = mean(diff),
        SD_diff = sd(diff),
        .groups = "drop"
      )
    
    
    # Create the plot
    p <- ggplot(summary_data, 
                aes(x = as.numeric(as.factor(method)), 
                    y = bias, 
                    color = as.factor(strata), 
                    shape = as.factor(strata))) + 
      #annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_point(position = position_dodge(width = 0.8), size = 3) + 
      geom_errorbar(
        aes(ymin = bias - SD_diff, ymax = bias + SD_diff), 
        position = position_dodge(width = 0.8), 
        width = 0.2) + 
      labs( x = "", y = "Bias", color = " ", shape = " ", title = small_titles[i]) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10, hjust = 0.5), axis.text = element_text(color = "black"),
            text = element_text(size = 10),
            legend.text = element_text(size = 10)) + 
      scale_color_manual(values = c( "#6BAED6", "#3182BD","#08519C","navy"),
                         labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4")) + 
      scale_shape_manual(values = c(16, 17, 15, 18),
                         labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4")) +
      ylim(-y_lim, y_lim) 
    
    
    #Remove legend      
    p <- p + theme(legend.position = "none")
    
    # Add the plot to the list
    plot_list[[i]] <- p
  }
  
  # Combine the plots into a grid using ggarrange
  composite_plot <- ggarrange(plot_list[[1]],
                              plot_list[[2]],
                              plot_list[[3]],
                              plot_list[[4]],
                              plot_list[[5]],
                              plot_list[[6]],
                              plot_list[[7]],
                              plot_list[[8]],
                              ncol = 4, nrow = 2,
                              font.label = list(size = 9)
  )
  # Add the overall title if specified
  if (!is.null(overall_title)) {
    composite_plot <- composite_plot + plot_annotation(title = overall_title)
  }
  
  # Return the composite plot
  return(composite_plot)
}
composite_pointplot_bias(c_rd_data_short, small_titles, overall_title = "Overall Bias in Split-Sample Method by Split Percent in DRS vs Outcome Model")

# Percent Bias Function
percent_bias_function  <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
  percent_bias_list <- list()
  
  for (method in methods) {
    percent_bias <- numeric()
    
    for (i in 1:4) { #strata 1 to 4
      sampled_col <- paste0("DRS_", method, "_strata", i, "_sampled")
      ground_truth_col <- paste0(ground_truth_prefix, i)
      
      mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
      true_value <- data[[ground_truth_col]][1]  #ground truth is the same across rows
      percent_bias[i] <- ((mean_sampled - true_value) / true_value) * 100
    }
    percent_bias_list[[method]] <- percent_bias
  }
  
  percent_bias_df <- data.frame(
    Strata = paste0("strata", 1:4),
    FS_percent_bias = percent_bias_list[["FS"]],
    CO_percent_bias = percent_bias_list[["CO"]],
    SS_percent_bias = percent_bias_list[["SS"]]
  )
  
  return(percent_bias_df)
}
overall_percent_bias_function <- function (data){
  OPB_FS <- mean(abs(data$FS_percent_bias))
  OPB_CO <- mean(abs(data$CO_percent_bias))
  OPB_SS <- mean(abs(data$SS_percent_bias))
  OPB <- data.frame(OPB_CO = OPB_CO, OPB_FS = OPB_FS, OPB_SS = OPB_SS)
  return(OPB)
}
apply_percent_bias_function <- function(data_list, methods, ground_truth_prefix) {
  results_list <- list()
  
  for (data in data_list) {
    percent_bias_results <- percent_bias_function(data, methods, ground_truth_prefix)
    overall_percent_bias <- overall_percent_bias_function(percent_bias_results)
    results_list[[length(results_list) + 1]] <- overall_percent_bias
  }
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- number_titles
  return(results_df)
}
methods <- c("CO", "FS", "SS")
OPB_results <- apply_percent_bias_function(sim_results, methods, "True_risk_strata")

# Save OPB_results as csv
write.csv(OPB_results, file = paste0(file_path, "/",trial_setting, "_OPB_results.csv"), row.names = T)

# MSE and Overall MSE
# Calculate MSE for each method
MSE_function <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
  MSE_list <- list()
  
  for (method in methods) {
    MSE <- numeric()
    
    for (i in 1:4) { #strata 1 to 4
      sampled_col <- paste0("DRS_", method, "_strata", i, "_sampled")
      ground_truth_col <- paste0(ground_truth_prefix, i)
      
      mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
      true_value <- data[[ground_truth_col]][1]  #ground truth is the same across rows
      MSE[i] <- mean((data[[sampled_col]]-true_value)^2, na.rm = TRUE)
    }
    MSE_list[[method]] <- MSE
  }
  
  MSE_df <- data.frame(
    Strata = paste0("strata", 1:4),
    FS_MSE = MSE_list[["FS"]],
    CO_MSE = MSE_list[["CO"]],
    SS_MSE = MSE_list[["SS"]]
  )
  return(MSE_df)
}

overall_MSE_function <- function(data){
  O_MSE_FS <- mean(data$FS_MSE)
  O_MSE_CO <- mean(data$CO_MSE)
  O_MSE_SS <- mean(data$SS_MSE)
  MSE <- data.frame(O_MSE_FS = O_MSE_FS, O_MSE_CO = O_MSE_CO, O_MSE_SS = O_MSE_SS)
  return(MSE)
}

apply_O_MSE_funciton <- function(datalist, methods, ground_truth_prefix){
  results_list <- list()
  
  for (data in datalist){
    MSE_results <- MSE_function(data, methods, ground_truth_prefix)
    O_MSE <- overall_MSE_function(MSE_results)
    results_list[[length(results_list) + 1]] <- O_MSE
  }
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- number_titles
  return(results_df)
}

OMSE_results <- apply_O_MSE_funciton(sim_results, methods, "True_risk_strata")

write.csv(OMSE_results, file = paste0(file_path, "/", trial_setting, "_OMSE_results.csv"), row.names = T)





