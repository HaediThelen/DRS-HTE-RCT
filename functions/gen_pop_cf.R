# Function to generate the population with counterfactual outcomes
gen_pop_cf <- function(n, X, cov_prev, cx_cov_ORs, cx_intercept, tx_cov_ORs, 
                       tx_intercept, 
                       tx_assign_ORs = c(1,1,1,1,1,1,1,1,1,1,1,1) # no confounding
                       ){
  # Function to generate population
  # n = number of individuals
  # X = number of covariates
  # cov_prev = prevalence of the covariates
  # cx_cov_ORs = odds ratios for the covariates under control
  # cx_intercept = intercept under control
  # tx_cov_OR = odds ratios for the covariates under treatment
  # tx_intercept = intercept under treatment
  # tx_assign_ORs = odds ratios defining association between covs and tx assignment
  
  covariates <- matrix(rbinom(n*X, 1, cov_prev), nrow = n, ncol = X)
  
  cx_coefficients <- log(cx_cov_ORs)
  tx_coefficients <- log(tx_cov_ORs)
  tx_assign_coefficients <- log(tx_assign_ORs)
  
  # Define treatment status given covariates
  ps_linear_predictor <- covariates %*% tx_assign_coefficients
  ps_true <- exp(ps_linear_predictor)/(1 + exp(ps_linear_predictor)) # PS, propensity score, treatment assignment probability
  A <- rbinom(n, 1, ps_true) # A = treatment 
  
  # make linear predictor
  pY0_linear_predictor <- covariates %*% cx_coefficients + cx_intercept
  pY1_linear_predictor <- covariates %*% tx_coefficients + tx_intercept
  
  # transform linear_predictor to probabilities 
  pY0  <- exp(pY0_linear_predictor)/(1 + exp(pY0_linear_predictor))
  pY1 <- exp(pY1_linear_predictor)/(1 + exp(pY1_linear_predictor))
  
  # generate a binary outcome variable based on the probabilities of the linear predictor
  Y0 <- rbinom(n, 1, pY0) 
  Y1 <- rbinom(n, 1, pY1)
  
  # Outcome observed 
  Y <- ifelse(A == 0, Y0, Y1)
  
  data <- data.frame(covariates, pY0, pY1, Y0, Y1, A, Y) # don't need to rturn Ps_true, all = 0.5
  
  return(data)
}


