# Data frame that contains parameters for each trial setting
# column names
cols<- c("trial_setting",
         "file_path",
         "cov_ORs_cx",
         "cov_ORs_tx",
         "n",
         "covs_n",
         "covs_prev",
         "cx_intercept",
         "tx_intercept",
          "covs", 
         "obj_strata_cuts",
         "n_sim",
         "n_sample_cx",
         "n_sample_tx")

trial_settings<- data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(trial_settings)<- cols

#Base Case
base_case <- data.frame(
  trial_setting = "base_case",
  file_path = "./results/main_analysis/base_case",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

#Trial Setting A: Sample Size 500 
a_smplsz500 <- data.frame(
  trial_setting = "a_smplsz500",
  file_path = "./results/main_analysis/a_smplsz500",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 250,
  n_sample_tx = 250
)
#Trial Setting B: Sample Size 1000 
b_smplsz1000 <- data.frame(
  trial_setting = "b_smplsz1000",
  file_path = "./results/main_analysis/b_smplsz1000",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 500,
  n_sample_tx = 500
)

#Trial Setting C: Sample Size 5000
c_smplsz5000 <- data.frame(
  trial_setting = "c_smplsz5000",
  file_path = "./results/main_analysis/c_smplsz5000",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 2500,
  n_sample_tx = 2500
)

# Trial Setting D: Sample Size 10000
D_smplsz10000 <- data.frame(
  trial_setting = "d_smplsz10000",
  file_path = "./results/main_analysis/d_smplsz10000",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 5000,
  n_sample_tx = 5000
)

#Trial Setting E: Outcome 15%
e_outcome_0.15 <- data.frame(
  trial_setting = "e_outcome_0.15",
  file_path = "./results/main_analysis/e_outcome_0.15",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -2.6,
  tx_intercept = -2.6,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.0967, 0.130, 0.183, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

#Trial Setting F: Outcome 5%
f_outcome_0.05 <- data.frame(
  trial_setting = "f_outcome_0.05",
  file_path = "./results/main_analysis/f_outcome_0.05",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -3.96,
  tx_intercept = -3.96,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.0268, 0.0369, 0.0542, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

#Trial Setting G: Outcome 50%
g_outcome_0.5 <- data.frame(
  trial_setting = "g_outcome_0.5",
  file_path = "./results/main_analysis/g_outcome_0.5",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -0.76,
  tx_intercept = -0.76,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.403, 0.484, 0.585, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

#Trial Setting H: Randomization Ratio 2:1
h_ratio_2to1 <- data.frame(
  trial_setting = "h_ratio_2to1",
  file_path = "./results/main_analysis/h_ratio_2to1",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 3600
)

#Trial Setting i: Randomization Ratio 3:1
i_ratio_3to1 <- data.frame(
  trial_setting = "i_ratio_3to1",
  file_path = "./results/main_analysis/i_ratio_3to1",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 5400
)

#Trial Setting j: Randomization Ratio 4:1
j_ratio_4to1 <- data.frame(
  trial_setting = "j_ratio_4to1",
  file_path = "./results/main_analysis/j_ratio_4to1",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 7200
)

#Trial Setting k:AUC 70%
k_auc_0.70 <- data.frame(
  trial_setting = "k_auc_0.70",
  file_path = "./results/main_analysis/k_auc_0.70",
  cov_ORs_cx = I(list(1.17*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  cov_ORs_tx = I(list(1.17*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -2.4,
  tx_intercept = -2.4,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.138, 0.208, 0.316, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

l_auc_0.75 <- data.frame(
  trial_setting = "l_auc_0.75",
  file_path = "./results/main_analysis/l_auc_0.75",
  cov_ORs_cx = I(list(1.4*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  cov_ORs_tx = I(list(1.4*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -2.9,
  tx_intercept = -2.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.116, 0.2061, 0.3371, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

m_auc_0.80 <- data.frame(
  trial_setting = "m_auc_0.80",
  file_path = "./results/main_analysis/m_auc_0.80",
  cov_ORs_cx = I(list(1.8*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  cov_ORs_tx = I(list(1.8*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -3.6,
  tx_intercept = -3.6,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.0897, 0.176, 0.366, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

n_auc_0.85 <- data.frame(
  trial_setting = "n_auc_0.85",
  file_path = "./results/main_analysis/n_auc_0.85",
  cov_ORs_cx = I(list(2.4*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  cov_ORs_tx = I(list(2.4*(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2)))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -4.5,
  tx_intercept = -4.5,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list( c(-Inf, 0.0507, 0.134, 0.357, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)

#Base Case OR
base_case_or <- data.frame(
  trial_setting = "base_case_or",
  file_path = "./results/main_analysis/base_case_or",
  cov_ORs_cx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  cov_ORs_tx = I(list(c(1, 1, 1, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 2, 2, 2))),
  n = 1000000,
  covs_n = 12,
  cov_prev = 0.2,
  cx_intercept = -1.9,
  tx_intercept = -1.9,
  covs = I(list(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"))),
  obj_strata_cuts =  I(list(c(-Inf, 0.178, 0.231, 0.311, Inf))),
  n_sim = 500,
  n_sample_cx = 1800,
  n_sample_tx = 1800
)


all_trial_settings <- rbind(trial_settings, base_case, a_smplsz500, b_smplsz1000, 
                            c_smplsz5000, D_smplsz10000, e_outcome_0.15,
                            f_outcome_0.05, g_outcome_0.5, h_ratio_2to1, 
                            i_ratio_3to1, j_ratio_4to1, k_auc_0.70, 
                            l_auc_0.75, m_auc_0.80, n_auc_0.85,
                            base_case_or)



