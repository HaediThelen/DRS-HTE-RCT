# Simulation Functions
library(doRNG)
library(doParallel)
library(foreach)
library(pROC)

treatment <- "A"

# Main Stratify by quartile, not objective strata
simulate_once_rank <- function(data, outcome, covs, n_sample_cx, n_sample_tx) {
  # Create ID
  data$ID <- 1:nrow(data)
  
  # Randomly draw n_sample_cx controls (treatment A = 0)
  sampled_data_cx <- data %>% 
    filter(A == 0) %>% 
    sample_n(n_sample_cx, replace = FALSE)
  
  # Randomly draw n_sample_ctx controls (treatment A = 1)
  sampled_data_tx <- data %>% 
    filter(A == 1) %>% 
    sample_n(n_sample_tx, replace = FALSE)
  
  # Combine sampled treated and controls
  sampled_data <- rbind(sampled_data_cx, sampled_data_tx)
  
  formula <- reformulate(covs, response = outcome)
  
  # Controls Only Method
  DRM_CO <- glm(formula, data = filter(sampled_data, A == 0), family = "binomial")
  
  # Full Sample Method
  DRM_FS <- glm(formula, data = sampled_data, family = "binomial")
  
  # Split Sample Method
  sample_data_cx <- filter(sampled_data, A == 0)
  n_sample_cx <- nrow(sample_data_cx)
  sampled_data_cx_train <- sample_data_cx %>% sample_n(n_sample_cx / 2, replace = FALSE)
  sampled_data_predict <- sampled_data %>% anti_join(sampled_data_cx_train, by = "ID")
  
  DRM_SS <- glm(formula, data = sampled_data_cx_train, family = "binomial")
  
  # Predicted risks
  sampled_data$DRS_FS <- predict(DRM_FS, newdata = sampled_data, type = "response")
  sampled_data$DRS_CO <- predict(DRM_CO, newdata = sampled_data, type = "response")
  sampled_data_predict$DRS_SS <- predict(DRM_SS, newdata = sampled_data_predict, type = "response")
  
  # Use the full sampled_data for all stratification and risk difference calculations,
  ####### change to quartiles based on the sample, not the objective strata   ####### 
  sampled_data$DRS_FS_strata <- stratify_function(sampled_data, "DRS_FS", 4)
  sampled_data$DRS_CO_strata <- stratify_function(sampled_data, "DRS_CO", 4)
  sampled_data_predict$DRS_SS_strata <- stratify_function(sampled_data_predict, "DRS_SS", 4)
  
  # Risk differences using full sampled_data
  rd_DRS_FS_sampled_data <- rd_function(sampled_data, "DRS_FS_strata", treatment = "A", outcome = "Y")
  rd_DRS_CO_sampled_data <- rd_function(sampled_data, "DRS_CO_strata", treatment = "A", outcome = "Y")
  rd_DRS_SS_sampled_data_predict <- rd_function(sampled_data_predict, "DRS_SS_strata", treatment = "A", outcome = "Y")
  
  # Get AUCs
  auc_DRS_FS <- auc(sampled_data$Y, sampled_data$DRS_FS)[1]
  auc_DRS_CO <- auc(sampled_data$Y, sampled_data$DRS_CO)[1]
  auc_DRS_SS <- auc(sampled_data_predict$Y, sampled_data_predict$DRS_SS)[1]
  
  ###
  # if any value in rd_DRS_CO_sampled_data = NaN or na then print "NaN or NA"
  print(any(is.na(rd_DRS_CO_sampled_data)))
  
  ###
  
  # Combine results into a row
  data.frame(
    DRS_FS_strata1_sampled = rd_DRS_FS_sampled_data[1], 
    DRS_FS_strata2_sampled = rd_DRS_FS_sampled_data[2],
    DRS_FS_strata3_sampled = rd_DRS_FS_sampled_data[3], 
    DRS_FS_strata4_sampled = rd_DRS_FS_sampled_data[4],
    
    DRS_CO_strata1_sampled = rd_DRS_CO_sampled_data[1], 
    DRS_CO_strata2_sampled = rd_DRS_CO_sampled_data[2],
    DRS_CO_strata3_sampled = rd_DRS_CO_sampled_data[3], 
    DRS_CO_strata4_sampled = rd_DRS_CO_sampled_data[4],
    
    DRS_SS_strata1_sampled = rd_DRS_SS_sampled_data_predict[1], 
    DRS_SS_strata2_sampled = rd_DRS_SS_sampled_data_predict[2],
    DRS_SS_strata3_sampled = rd_DRS_SS_sampled_data_predict[3], 
    DRS_SS_strata4_sampled = rd_DRS_SS_sampled_data_predict[4],
    
    AUC_DRS_FS = auc_DRS_FS,
    AUC_DRS_CO = auc_DRS_CO,
    AUC_DRS_SS = auc_DRS_SS
  )
}

simulation_function_parallel_rank <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata) {
  
  # Set up parallel backend
  set.seed(12345)
  registerDoParallel(cores = detectCores() - 1)
  
  # Use foreach with reproducible random numbers
  rd_list <- foreach(i = 1:n_sim, .combine = rbind, .packages = c("dplyr")) %dorng% {
    simulate_once_rank(data, outcome, covs, n_sample_cx, n_sample_tx)
  }
  
  # Add true risk differences
  #rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y")#conditional RD
  # use counterfactual outcomes to calculate true risk
  rd_true_data <- c(
    rd_strata1 = mean(data[data$objective_strata == 1,]$Y0) - mean(data[data$objective_strata == 1,]$Y1),
    rd_strata2 = mean(data[data$objective_strata == 2,]$Y0) - mean(data[data$objective_strata == 2,]$Y1),
    rd_strata3 = mean(data[data$objective_strata == 3,]$Y0) - mean(data[data$objective_strata == 3,]$Y1),
    rd_strata4 = mean(data[data$objective_strata == 4,]$Y0) - mean(data[data$objective_strata == 4,]$Y1))
  rd_list$True_risk_strata1 <- rd_true_data[1]
  rd_list$True_risk_strata2 <- rd_true_data[2]
  rd_list$True_risk_strata3 <- rd_true_data[3]
  rd_list$True_risk_strata4 <- rd_true_data[4]
  
  # Add ground truth AUC
  auc_true_data <- auc(data$Y, data$pY0)[1]
  rd_list$AUC_true <- auc_true_data
  rd_list <- rd_list %>% select(-starts_with("AUC"), starts_with("AUC"))
  return(rd_list)
}
