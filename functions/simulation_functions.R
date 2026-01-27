# Simulation Functions
library(doRNG)
library(doParallel)
library(foreach)
library(pROC)

# Both parallel and serial versions provided for compatibility across different systems

# Main simulation function, includes CO, FS, and SS methods
simulation_function <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts){
  rd <- data.frame()
  
  # create ID for each patient
  data$ID <- 1:nrow(data)
  
  for(k in 1:n_sim) { 
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
    DRM_FS <- glm(formula = formula, data = sampled_data, family = "binomial")
    
    # Split Sample Method
    # Split controls into two groups
    sample_data_cx <- filter(sampled_data, A == 0)
    n_sample_cx <- nrow(sample_data_cx)
    sampled_data_cx_train <- sample_data_cx %>%
      sample_n(n_sample_cx/2, replace = FALSE)    
    sampled_data_predict <- sampled_data %>%
      anti_join(sampled_data_cx_train, 
                by = "ID") # remove controls that were trained on from prediction sample, keep unused controls and treated
    
    # Fit Split Sample Method on training data
    DRM_SS <- glm(formula = formula, data = sampled_data_cx_train, family = "binomial")
    
    # Calculate predicted risk for each patient in the sample 
    # Predicted risks in the sample 
    sampled_data$DRS_FS <- predict(DRM_FS, newdata = sampled_data, type = "response")
    sampled_data$DRS_CO <- predict(DRM_CO, newdata = sampled_data, type = "response")
    sampled_data_predict$DRS_SS <- predict(DRM_SS, newdata = sampled_data_predict, type = "response")
    
    #stratify each DRS risk score by objective strata, add column to sampled_data
    sampled_data$DRS_FS_strata <- stratify_function(sampled_data, "DRS_FS", 4, cuts = obj_strata_cuts)
    sampled_data$DRS_CO_strata <- stratify_function(sampled_data, "DRS_CO", 4, cuts = obj_strata_cuts)
    sampled_data_predict$DRS_SS_strata <- stratify_function(sampled_data_predict, "DRS_SS", 4, cuts = obj_strata_cuts)
    
    # use rd_function to calculate risk differences for each strata for each strata type
    rd_DRS_FS_sampled_data <- rd_function(sampled_data, "DRS_FS_strata", treatment = "A", outcome = "Y")
    rd_DRS_CO_sampled_data <- rd_function(sampled_data, "DRS_CO_strata", treatment = "A", outcome = "Y")
    rd_DRS_SS_sample_data_predict <- rd_function(sampled_data_predict, "DRS_SS_strata", treatment = "A", outcome = "Y")
    
    # Get AUCs
    auc_DRS_FS <-  suppressMessages(auc(sampled_data$Y, sampled_data$DRS_FS)[1])
    auc_DRS_CO <-  suppressMessages(auc(sampled_data$Y, sampled_data$DRS_CO)[1])
    auc_DRS_SS <-  suppressMessages(auc(sampled_data_predict$Y, sampled_data_predict$DRS_SS)[1])
    
    new_row <- data.frame(
      DRS_FS_strata1_sampled = rd_DRS_FS_sampled_data[1],
      DRS_FS_strata2_sampled = rd_DRS_FS_sampled_data[2],
      DRS_FS_strata3_sampled = rd_DRS_FS_sampled_data[3],
      DRS_FS_strata4_sampled = rd_DRS_FS_sampled_data[4],
      
      DRS_CO_strata1_sampled = rd_DRS_CO_sampled_data[1],
      DRS_CO_strata2_sampled = rd_DRS_CO_sampled_data[2],
      DRS_CO_strata3_sampled = rd_DRS_CO_sampled_data[3],
      DRS_CO_strata4_sampled = rd_DRS_CO_sampled_data[4],
      
      DRS_SS_strata1_sampled = rd_DRS_SS_sample_data_predict[1],
      DRS_SS_strata2_sampled = rd_DRS_SS_sample_data_predict[2],
      DRS_SS_strata3_sampled = rd_DRS_SS_sample_data_predict[3],
      DRS_SS_strata4_sampled = rd_DRS_SS_sample_data_predict[4],
      
      AUC_DRS_FS = auc_DRS_FS,
      AUC_DRS_CO = auc_DRS_CO,
      AUC_DRS_SS = auc_DRS_SS
      
    )
    
    rd <- rbind(rd, new_row)
    
    #print every 100th iteration
    if(k %% 10 == 0) {
      print(k)
    }
  }
  
  #calculate true risk difference for objective strata
  #rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y") #conditional RD
  # use counterfactual outcomes to calculate true risk
  rd_true_data <- c(
    rd_strata1 = mean(data[data$objective_strata == 1,]$Y0) - mean(data[data$objective_strata == 1,]$Y1),
    rd_strata2 = mean(data[data$objective_strata == 2,]$Y0) - mean(data[data$objective_strata == 2,]$Y1),
    rd_strata3 = mean(data[data$objective_strata == 3,]$Y0) - mean(data[data$objective_strata == 3,]$Y1),
    rd_strata4 = mean(data[data$objective_strata == 4,]$Y0) - mean(data[data$objective_strata == 4,]$Y1))
  #Create a new column of rd named True_risk_strata_x which equals rd_true_data[x]
  
  rd$True_risk_strata1 <- rd_true_data[1]
  rd$True_risk_strata2 <- rd_true_data[2]
  rd$True_risk_strata3 <- rd_true_data[3]
  rd$True_risk_strata4 <- rd_true_data[4]
  
  # Add ground truth AUC
  auc_true_data <-  suppressMessages(auc(data$Y, data$pY0)[1])
  rd$AUC_true <- auc_true_data
  rd <- rd %>% select(-starts_with("AUC"), starts_with("AUC"))

  return(rd)
}

# Main simulation function, parallel (simulate once and parallel function)
simulate_once <- function(data, outcome, covs, n_sample_cx, n_sample_tx, obj_strata_cuts, incl_se = FALSE) {
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
  
  # Use the full sampled_data for all stratification and risk difference calculations
  sampled_data$DRS_FS_strata <- stratify_function(sampled_data, "DRS_FS", 4, cuts = obj_strata_cuts)
  sampled_data$DRS_CO_strata <- stratify_function(sampled_data, "DRS_CO", 4, cuts = obj_strata_cuts)
  sampled_data_predict$DRS_SS_strata <- stratify_function(sampled_data_predict, "DRS_SS", 4, cuts = obj_strata_cuts)
  
  # Risk differences using full sampled_data
  rd_DRS_FS_sampled_data <- rd_function(sampled_data, "DRS_FS_strata", treatment = "A", outcome = "Y")
  rd_DRS_CO_sampled_data <- rd_function(sampled_data, "DRS_CO_strata", treatment = "A", outcome = "Y")
  rd_DRS_SS_sampled_data_predict <- rd_function(sampled_data_predict, "DRS_SS_strata", treatment = "A", outcome = "Y")
  
  # If incl_se = TRUE, get Standard Errors for RD estimates
  if (incl_se == TRUE) {
    se_rd_DRS_FS_sampled_data <- se_rd_function(sampled_data, "DRS_FS_strata", treatment = "A", outcome = "Y")
    se_rd_DRS_CO_sampled_data <- se_rd_function(sampled_data, "DRS_CO_strata", treatment = "A", outcome = "Y")
    se_rd_DRS_SS_sampled_data_predict <- se_rd_function(sampled_data_predict, "DRS_SS_strata", treatment = "A", outcome = "Y")
  }
  
  
  # Get AUCs
  auc_DRS_FS <- auc(sampled_data$Y, sampled_data$DRS_FS)[1]
  auc_DRS_CO <- auc(sampled_data$Y, sampled_data$DRS_CO)[1]
  auc_DRS_SS <- auc(sampled_data_predict$Y, sampled_data_predict$DRS_SS)[1]
  
  ###
 # if any value in rd_DRS_CO_sampled_data = NaN or na then print "NaN or NA"
 print(any(is.na(rd_DRS_CO_sampled_data)))

  ###
  
  # Combine results into a row
 results <- data.frame(
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
  
  # Add SE columns if requested
  if (incl_se) {
    results$SE_DRS_FS_strata1_sampled <- se_rd_DRS_FS_sampled_data[1]
    results$SE_DRS_FS_strata2_sampled <- se_rd_DRS_FS_sampled_data[2]
    results$SE_DRS_FS_strata3_sampled <- se_rd_DRS_FS_sampled_data[3]
    results$SE_DRS_FS_strata4_sampled <- se_rd_DRS_FS_sampled_data[4]
    
    results$SE_DRS_CO_strata1_sampled <- se_rd_DRS_CO_sampled_data[1]
    results$SE_DRS_CO_strata2_sampled <- se_rd_DRS_CO_sampled_data[2]
    results$SE_DRS_CO_strata3_sampled <- se_rd_DRS_CO_sampled_data[3]
    results$SE_DRS_CO_strata4_sampled <- se_rd_DRS_CO_sampled_data[4]
    
    results$SE_DRS_SS_strata1_sampled <- se_rd_DRS_SS_sampled_data_predict[1]
    results$SE_DRS_SS_strata2_sampled <- se_rd_DRS_SS_sampled_data_predict[2]
    results$SE_DRS_SS_strata3_sampled <- se_rd_DRS_SS_sampled_data_predict[3]
    results$SE_DRS_SS_strata4_sampled <- se_rd_DRS_SS_sampled_data_predict[4]
  }
    return(results)
}

simulation_function_parallel <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts, incl_se = FALSE) {
  
  # Set up parallel backend
  set.seed(12345)
  registerDoParallel(cores = detectCores() - 1)
  
  # Use foreach with reproducible random numbers
  rd_list <- foreach(i = 1:n_sim, .combine = rbind, .packages = c("dplyr")) %dorng% {
    simulate_once(data, outcome, covs, n_sample_cx, n_sample_tx, obj_strata_cuts, incl_se)
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


# RSS function
simulation_function_RSS <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata, m_splits){
  rd <- data.frame()
  
  # create ID for each patient
  data$ID <- 1:nrow(data)
  
  for(k in 1:n_sim) { 
    # Simulation number
    simulation <- k
    
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
    
    # Define regression formula 
    formula <- reformulate(covs, response = outcome)
    
    # Split Sample Method
    # for each M = 50 splits
    # 1) subset data to controls and treated
    # 2) split controls into two groups
    # 3) fit model on one group of controls
    # 4) predict DRS for the other group of controls and all treated
    # 5) stratify by DRS risk score by objective strata
    # 6) calculate risk difference for each strata for each strata type
    # 7) add strata specific risk difference to data frame (M rows added)
    
    for(i in 1:m_splits){
      # Split number 
      split <- i
      # Split controls into two groups
      sample_data_cx <- filter(sampled_data, A == 0)
      n_sample_cx <- nrow(sample_data_cx)
      sampled_data_cx_train <- sample_data_cx %>%
        sample_n(n_sample_cx/2, replace = FALSE)    
      sampled_data_predict <- sampled_data %>%
        anti_join(sampled_data_cx_train, 
                  by = "ID") # remove controls that were trained on from prediction sample, keep unused controls and treated
      
      # Fit Split Sample Method on training data
      DRM_RSS <- glm(formula = formula, data = sampled_data_cx_train, family = "binomial")
      
      # Calculate predicted risk for each patient in the sample and in the population
      # Predicted risks in the sample 
      sampled_data_predict$DRS_RSS <- predict(DRM_RSS, newdata = sampled_data_predict, type = "response")
      
      #stratify each DRS risk score by objective strata, add column to sampled_data
      sampled_data_predict$DRS_RSS_strata <- stratify_function(sampled_data_predict, "DRS_RSS", 4, cuts = obj_strata_cuts)
      
      # use rd_function to calculate risk differences for each strata for each strata type
      rd_DRS_RSS_sample_data_predict <- rd_function(sampled_data_predict, "DRS_RSS_strata", treatment = "A", outcome = "Y")
      
      # Get AUCs
      auc_DRS_RSS <- suppressMessages(auc(sampled_data_predict$Y, sampled_data_predict$DRS_RSS)[1])
      
      new_row <- data.frame(
        simulation = k,
        split = i,
        
        DRS_RSS_strata1_sampled = rd_DRS_RSS_sample_data_predict[1],
        DRS_RSS_strata2_sampled = rd_DRS_RSS_sample_data_predict[2],
        DRS_RSS_strata3_sampled = rd_DRS_RSS_sample_data_predict[3],
        DRS_RSS_strata4_sampled = rd_DRS_RSS_sample_data_predict[4],
        
        AUC_DRS_RSS = auc_DRS_RSS
      )
      
      rd <- rbind(rd, new_row)
      #print(paste0("split sample: ", i)) 
    }
    print(paste0("simulation: ", k)) 
  }
  
  #calculate true risk difference for objective strata
  rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y")
  
  #Create a new column of rd named True_risk_strata_x which equals rd_true_data[x]
  rd$True_risk_strata1 <- rd_true_data[1]
  rd$True_risk_strata2 <- rd_true_data[2]
  rd$True_risk_strata3 <- rd_true_data[3]
  rd$True_risk_strata4 <- rd_true_data[4]
  
  # Add ground truth AUC
  auc_true_data <- suppressMessages(auc(data$Y, data$pY0)[1])
  rd$AUC_true <- auc_true_data
  rd <- rd %>% dplyr::select(-starts_with("AUC"), starts_with("AUC"))
  
  return(rd)
}

# RSS Function Parallel (simulate once and parallel function)
simulate_once_RSS <- function(k, data, outcome, covs, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts, m_splits) {
  # Create a copy of the data and add ID
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
  
  # Define regression formula 
  formula <- reformulate(covs, response = outcome)
  
  # Initialize results for this simulation
  rd_sim <- data.frame()
  
  # Split Sample Method
  for(i in 1:m_splits){
    # Split controls into two groups
    sample_data_cx <- filter(sampled_data, A == 0)
    n_sample_cx <- nrow(sample_data_cx)
    sampled_data_cx_train <- sample_data_cx %>%
      sample_n(n_sample_cx/2, replace = FALSE)    
    sampled_data_predict <- sampled_data %>%
      anti_join(sampled_data_cx_train, by = "ID")
    
    # Fit Split Sample Method on training data
    DRM_RSS <- glm(formula = formula, data = sampled_data_cx_train, family = "binomial")
    
    # Calculate predicted risk for each patient in the sample
    sampled_data_predict$DRS_RSS <- predict(DRM_RSS, newdata = sampled_data_predict, type = "response")
    
    # Stratify DRS risk score by objective strata
    sampled_data_predict$DRS_RSS_strata <- stratify_function(sampled_data_predict, "DRS_RSS", 4, cuts = obj_strata_cuts)
    
    # Calculate risk differences for each strata
    rd_DRS_RSS_sample_data_predict <- rd_function(sampled_data_predict, "DRS_RSS_strata", treatment = "A", outcome = "Y")
    
    # Get AUC
    auc_DRS_RSS <- suppressMessages(auc(sampled_data_predict$Y, sampled_data_predict$DRS_RSS)[1])
    
    # Create row for this split
    new_row <- data.frame(
      simulation = k,
      split = i,
      
      DRS_RSS_strata1_sampled = rd_DRS_RSS_sample_data_predict[1],
      DRS_RSS_strata2_sampled = rd_DRS_RSS_sample_data_predict[2],
      DRS_RSS_strata3_sampled = rd_DRS_RSS_sample_data_predict[3],
      DRS_RSS_strata4_sampled = rd_DRS_RSS_sample_data_predict[4],
      
      AUC_DRS_RSS = auc_DRS_RSS
    )
    
    rd_sim <- rbind(rd_sim, new_row)
  }
  
  return(rd_sim)
}

simulation_function_RSS_parallel <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts, m_splits) {
  # Set up parallel backend
  registerDoParallel(cores = detectCores() - 1)
  
  # Calculate true risk differences once
  #rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y") #conditional RD
  
  set.seed(1234)  # Use foreach with reproducible random numbers
  
  # Parallel simulation
  rd_list <- foreach(i = 1:n_sim, .combine = rbind, .packages = c("dplyr")) %dorng% {
    simulate_once_RSS(i, data, outcome, covs, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts, m_splits)
  }
 
  # use counterfactual outcomes to calculate true risk
  rd_true_data <- c(
    rd_strata1 = mean(data[data$objective_strata == 1,]$Y0) - mean(data[data$objective_strata == 1,]$Y1),
    rd_strata2 = mean(data[data$objective_strata == 2,]$Y0) - mean(data[data$objective_strata == 2,]$Y1),
    rd_strata3 = mean(data[data$objective_strata == 3,]$Y0) - mean(data[data$objective_strata == 3,]$Y1),
    rd_strata4 = mean(data[data$objective_strata == 4,]$Y0) - mean(data[data$objective_strata == 4,]$Y1))
  # Add true risk differences to each row
  rd_list$True_risk_strata1 <- rd_true_data[1]
  rd_list$True_risk_strata2 <- rd_true_data[2]
  rd_list$True_risk_strata3 <- rd_true_data[3]
  rd_list$True_risk_strata4 <- rd_true_data[4]
  
  
  # Add ground truth AUC
  auc_true_data <- suppressMessages(auc(data$Y, data$pY0)[1])
  rd_list$AUC_true <- auc_true_data
  rd_list <- rd_list %>% dplyr::select(-starts_with("AUC"), starts_with("AUC")) 
  
  return(rd_list)
}


# LOO
simulation_function_LOO <- function(data, outcome, covs, n_sim, n_sample, true_risk_strata, obj_strata_cuts){
  rd <- data.frame()
  
  for(k in 1:n_sim) { 
    # Simulation number
    simulation <- k
    
    # Randomly draw a sample of n_sample patients from data
    sampled_data <- data %>% sample_n(n_sample, replace = FALSE)
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
    
    # Define regression formula 
    formula <- reformulate(covs, response = outcome)
    
    # 1. Fit DRM_tx on controls
    DRS_LOO <- speedglm(formula = formula, data = sampled_data_cx , family = binomial())
    
    # 2. Predict DRS_LOO on Treated using DRM_tx
    sampled_data_tx$DRS_LOO <- predict(DRS_LOO, newdata = sampled_data_tx, type = "response")
    
    # 3. For loop for LOO training on controls
    sampled_data_cx$ID <- 1:nrow(sampled_data_cx)
    sampled_data_cx$DRS_LOO <- NA
    
    for(i in 1:nrow(sampled_data_cx)){
      sampled_data_cx_train <- sampled_data_cx %>% filter(ID != i)
      sampled_data_cx_predict <- sampled_data_cx %>% filter(ID == i)
      
      # Fit DRM_cx_i on all controls except i
      DRM_cx_i <- speedglm(formula = formula, data = sampled_data_cx_train, family = binomial())
      
      # Predict DRS on control i using DRM_cx_i
      sampled_data_cx$DRS_LOO[i] <- predict(DRM_cx_i, newdata = sampled_data_cx_predict, type = "response")
    }
    # add sampled_data_cx to sampled_data_tx
    sampled_data_cx$ID <- NULL
    sampled_data_tx <- rbind(sampled_data_tx, sampled_data_cx)
    
    #stratify each DRS risk score by objective strata, add column to sampled_data
    sampled_data_tx$DRS_LOO_strata <- stratify_function(sampled_data_tx, "DRS_LOO", 4, cuts = obj_strata_cuts)
    
    # use rd_function to calculate risk differences for each strata for each strata type
    rd_DRS_LOO_sample_data_predict <- rd_function(sampled_data_tx, "DRS_LOO_strata", treatment = "A", outcome = "Y")
    
    # Get AUCs
    auc_DRS_LOO <- suppressMessages(auc(sampled_data_tx$Y, sampled_data_tx$DRS_LOO)[1])
    
    new_row <- data.frame(
      DRS_LOO_strata1_sampled = rd_DRS_LOO_sample_data_predict[1],
      DRS_LOO_strata2_sampled = rd_DRS_LOO_sample_data_predict[2],
      DRS_LOO_strata3_sampled = rd_DRS_LOO_sample_data_predict[3],
      DRS_LOO_strata4_sampled = rd_DRS_LOO_sample_data_predict[4],
      
      AUC_DRS_LOO = auc_DRS_LOO
    )
    
    rd <- rbind(rd, new_row)
    
    print(paste0("simulation: ", k)) 
  }
  
  #calculate true risk difference for objective strata
  rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y")
  #Create a new column of rd named True_risk_strata_x which equals rd_true_data[x]
  rd$True_risk_strata1 <- rd_true_data[1]
  rd$True_risk_strata2 <- rd_true_data[2]
  rd$True_risk_strata3 <- rd_true_data[3]
  rd$True_risk_strata4 <- rd_true_data[4]
  
  # Add ground truth AUC
  auc_true_data <- suppressMessages(auc(data$Y, data$pY0)[1])
  rd$AUC_true <- auc_true_data
  rd <- rd[, c(setdiff(names(rd), "AUC_DRS_LOO"), "AUC_DRS_LOO")]
  
  return(rd)
}

# LOO Function Parallel (Simulate once and parallel function)
simulate_LOO_once <- function(data, outcome, covs, n_sample_cx, n_sample_tx, obj_strata_cuts) {
  
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
  
  # Define regression formula 
  formula <- reformulate(covs, response = outcome)
  
  # 1. Fit DRM on controls
  DRS_LOO <- speedglm(formula = formula, data = sampled_data_cx, family = binomial())
  
  # 2. Predict DRS_LOO on Treated using DRM
  sampled_data_tx$DRS_LOO <- predict(DRS_LOO, newdata = sampled_data_tx, type = "response")
  
  # 3. Leave-One-Out training on controls
  sampled_data_cx$ID <- 1:nrow(sampled_data_cx)
  sampled_data_cx$DRS_LOO <- NA
  
  # Perform Leave-One-Out prediction for controls
  for(i in 1:nrow(sampled_data_cx)){
    sampled_data_cx_train <- sampled_data_cx %>% filter(ID != i)
    sampled_data_cx_predict <- sampled_data_cx %>% filter(ID == i)
    
    # Fit DRM_cx_i on all controls except i
    DRM_cx_i <- speedglm(formula = formula, data = sampled_data_cx_train, family = binomial())
    
    # Predict DRS on control i using DRM_cx_i
    sampled_data_cx$DRS_LOO[i] <- predict(DRM_cx_i, newdata = sampled_data_cx_predict, type = "response")
  }
  
  # Combine treated and control data
  sampled_data_cx$ID <- NULL
  sampled_data_tx <- rbind(sampled_data_tx, sampled_data_cx)
  
  # Stratify DRS risk scores
  sampled_data_tx$DRS_LOO_strata <- stratify_function(sampled_data_tx, "DRS_LOO", 4, cuts = obj_strata_cuts)
  
  # Calculate risk differences for each strata
  rd_DRS_LOO_sample_data_predict <- rd_function(sampled_data_tx, "DRS_LOO_strata", treatment = "A", outcome = "Y")
  
  # Get AUCs
  auc_DRS_LOO <- suppressMessages(auc(sampled_data_tx$Y, sampled_data_tx$DRS_LOO)[1])
  
  # Return results as a data frame
  data.frame(
    DRS_LOO_strata1_sampled = rd_DRS_LOO_sample_data_predict[1],
    DRS_LOO_strata2_sampled = rd_DRS_LOO_sample_data_predict[2],
    DRS_LOO_strata3_sampled = rd_DRS_LOO_sample_data_predict[3],
    DRS_LOO_strata4_sampled = rd_DRS_LOO_sample_data_predict[4],
    
    AUC_DRS_LOO = auc_DRS_LOO
  )
}

simulation_function_LOO_parallel <- function(data, outcome, covs, n_sim, n_sample_cx, n_sample_tx, true_risk_strata, obj_strata_cuts) {
  # Set up parallel backend
  set.seed(1234)
  registerDoParallel(cores = detectCores() - 1)
  
  # Use foreach with reproducible random numbers
  rd_list <- foreach(i = 1:n_sim, .combine = rbind, .packages = c("dplyr", "speedglm")) %dorng% {
    simulate_LOO_once(data, outcome, covs, n_sample_cx, n_sample_tx, obj_strata_cuts)
  }
  
  # Calculate true risk differences
  #rd_true_data <- rd_function(data, true_risk_strata, treatment = "A", outcome = "Y") #conditional RD
  # use counterfactual outcomes to calculate true risk
  rd_true_data <- c(
    rd_strata1 = mean(data[data$objective_strata == 1,]$Y0) - mean(data[data$objective_strata == 1,]$Y1),
    rd_strata2 = mean(data[data$objective_strata == 2,]$Y0) - mean(data[data$objective_strata == 2,]$Y1),
    rd_strata3 = mean(data[data$objective_strata == 3,]$Y0) - mean(data[data$objective_strata == 3,]$Y1),
    rd_strata4 = mean(data[data$objective_strata == 4,]$Y0) - mean(data[data$objective_strata == 4,]$Y1))
  
  # Add true risk differences to the results
  rd_list$True_risk_strata1 <- rd_true_data[1]
  rd_list$True_risk_strata2 <- rd_true_data[2]
  rd_list$True_risk_strata3 <- rd_true_data[3]
  rd_list$True_risk_strata4 <- rd_true_data[4]
  
  # Add ground truth AUC
  auc_true_data <- suppressMessages(auc(data$Y, data$pY0)[1])
  rd_list$AUC_true <- auc_true_data
  rd_list <- rd_list[, c(setdiff(names(rd_list), "AUC_DRS_LOO"), "AUC_DRS_LOO")]
  
  return(rd_list)
}
