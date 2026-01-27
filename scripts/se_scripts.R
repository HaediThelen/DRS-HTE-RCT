# Calculate 95% Confidence Interval Coverage Rates

# Function 1: Calculate coverage indicators
calculate_se_coverage <- function(data) {
  data %>%
    select(-c(AUC_DRS_CO, AUC_DRS_FS, AUC_DRS_SS, AUC_true)) %>%
    mutate(
      # DRS_FS - all 4 strata
      ts_DRS_FS_strata1_sampled = 1.96*SE_DRS_FS_strata1_sampled,
      lb_DRS_FS_strata1_sampled = DRS_FS_strata1_sampled - ts_DRS_FS_strata1_sampled,
      ub_DRS_FS_strata1_sampled = DRS_FS_strata1_sampled + ts_DRS_FS_strata1_sampled,
      cover_DRS_FS_strata1_sampled = ifelse(True_risk_strata1 >= lb_DRS_FS_strata1_sampled & True_risk_strata1 <= ub_DRS_FS_strata1_sampled, 1, 0),
      
      ts_DRS_FS_strata2_sampled = 1.96*SE_DRS_FS_strata2_sampled,
      lb_DRS_FS_strata2_sampled = DRS_FS_strata2_sampled - ts_DRS_FS_strata2_sampled,
      ub_DRS_FS_strata2_sampled = DRS_FS_strata2_sampled + ts_DRS_FS_strata2_sampled,
      cover_DRS_FS_strata2_sampled = ifelse(True_risk_strata2 >= lb_DRS_FS_strata2_sampled & True_risk_strata2 <= ub_DRS_FS_strata2_sampled, 1, 0),
      
      ts_DRS_FS_strata3_sampled = 1.96*SE_DRS_FS_strata3_sampled,
      lb_DRS_FS_strata3_sampled = DRS_FS_strata3_sampled - ts_DRS_FS_strata3_sampled,
      ub_DRS_FS_strata3_sampled = DRS_FS_strata3_sampled + ts_DRS_FS_strata3_sampled,
      cover_DRS_FS_strata3_sampled = ifelse(True_risk_strata3 >= lb_DRS_FS_strata3_sampled & True_risk_strata3 <= ub_DRS_FS_strata3_sampled, 1, 0),
      
      ts_DRS_FS_strata4_sampled = 1.96*SE_DRS_FS_strata4_sampled,
      lb_DRS_FS_strata4_sampled = DRS_FS_strata4_sampled - ts_DRS_FS_strata4_sampled,
      ub_DRS_FS_strata4_sampled = DRS_FS_strata4_sampled + ts_DRS_FS_strata4_sampled,
      cover_DRS_FS_strata4_sampled = ifelse(True_risk_strata4 >= lb_DRS_FS_strata4_sampled & True_risk_strata4 <= ub_DRS_FS_strata4_sampled, 1, 0),
      
      # DRS_CO - all 4 strata
      ts_DRS_CO_strata1_sampled = 1.96*SE_DRS_CO_strata1_sampled,
      lb_DRS_CO_strata1_sampled = DRS_CO_strata1_sampled - ts_DRS_CO_strata1_sampled,
      ub_DRS_CO_strata1_sampled = DRS_CO_strata1_sampled + ts_DRS_CO_strata1_sampled,
      cover_DRS_CO_strata1_sampled = ifelse(True_risk_strata1 >= lb_DRS_CO_strata1_sampled & True_risk_strata1 <= ub_DRS_CO_strata1_sampled, 1, 0),
      
      ts_DRS_CO_strata2_sampled = 1.96*SE_DRS_CO_strata2_sampled,
      lb_DRS_CO_strata2_sampled = DRS_CO_strata2_sampled - ts_DRS_CO_strata2_sampled,
      ub_DRS_CO_strata2_sampled = DRS_CO_strata2_sampled + ts_DRS_CO_strata2_sampled,
      cover_DRS_CO_strata2_sampled = ifelse(True_risk_strata2 >= lb_DRS_CO_strata2_sampled & True_risk_strata2 <= ub_DRS_CO_strata2_sampled, 1, 0),
      
      ts_DRS_CO_strata3_sampled = 1.96*SE_DRS_CO_strata3_sampled,
      lb_DRS_CO_strata3_sampled = DRS_CO_strata3_sampled - ts_DRS_CO_strata3_sampled,
      ub_DRS_CO_strata3_sampled = DRS_CO_strata3_sampled + ts_DRS_CO_strata3_sampled,
      cover_DRS_CO_strata3_sampled = ifelse(True_risk_strata3 >= lb_DRS_CO_strata3_sampled & True_risk_strata3 <= ub_DRS_CO_strata3_sampled, 1, 0),
      
      ts_DRS_CO_strata4_sampled = 1.96*SE_DRS_CO_strata4_sampled,
      lb_DRS_CO_strata4_sampled = DRS_CO_strata4_sampled - ts_DRS_CO_strata4_sampled,
      ub_DRS_CO_strata4_sampled = DRS_CO_strata4_sampled + ts_DRS_CO_strata4_sampled,
      cover_DRS_CO_strata4_sampled = ifelse(True_risk_strata4 >= lb_DRS_CO_strata4_sampled & True_risk_strata4 <= ub_DRS_CO_strata4_sampled, 1, 0),
      
      # DRS_SS - all 4 strata
      ts_DRS_SS_strata1_sampled = 1.96*SE_DRS_SS_strata1_sampled,
      lb_DRS_SS_strata1_sampled = DRS_SS_strata1_sampled - ts_DRS_SS_strata1_sampled,
      ub_DRS_SS_strata1_sampled = DRS_SS_strata1_sampled + ts_DRS_SS_strata1_sampled,
      cover_DRS_SS_strata1_sampled = ifelse(True_risk_strata1 >= lb_DRS_SS_strata1_sampled & True_risk_strata1 <= ub_DRS_SS_strata1_sampled, 1, 0),
      
      ts_DRS_SS_strata2_sampled = 1.96*SE_DRS_SS_strata2_sampled,
      lb_DRS_SS_strata2_sampled = DRS_SS_strata2_sampled - ts_DRS_SS_strata2_sampled,
      ub_DRS_SS_strata2_sampled = DRS_SS_strata2_sampled + ts_DRS_SS_strata2_sampled,
      cover_DRS_SS_strata2_sampled = ifelse(True_risk_strata2 >= lb_DRS_SS_strata2_sampled & True_risk_strata2 <= ub_DRS_SS_strata2_sampled, 1, 0),
      
      ts_DRS_SS_strata3_sampled = 1.96*SE_DRS_SS_strata3_sampled,
      lb_DRS_SS_strata3_sampled = DRS_SS_strata3_sampled - ts_DRS_SS_strata3_sampled,
      ub_DRS_SS_strata3_sampled = DRS_SS_strata3_sampled + ts_DRS_SS_strata3_sampled,
      cover_DRS_SS_strata3_sampled = ifelse(True_risk_strata3 >= lb_DRS_SS_strata3_sampled & True_risk_strata3 <= ub_DRS_SS_strata3_sampled, 1, 0),
      
      ts_DRS_SS_strata4_sampled = 1.96*SE_DRS_SS_strata4_sampled,
      lb_DRS_SS_strata4_sampled = DRS_SS_strata4_sampled - ts_DRS_SS_strata4_sampled,
      ub_DRS_SS_strata4_sampled = DRS_SS_strata4_sampled + ts_DRS_SS_strata4_sampled,
      cover_DRS_SS_strata4_sampled = ifelse(True_risk_strata4 >= lb_DRS_SS_strata4_sampled & True_risk_strata4 <= ub_DRS_SS_strata4_sampled, 1, 0)
    )
}

# Function 2: Summarize coverage
summarize_coverage <- function(data) {
  data %>%
    summarise(
      cover_DRS_CO_strata1 = mean(cover_DRS_CO_strata1_sampled),
      cover_DRS_CO_strata2 = mean(cover_DRS_CO_strata2_sampled),
      cover_DRS_CO_strata3 = mean(cover_DRS_CO_strata3_sampled),
      cover_DRS_CO_strata4 = mean(cover_DRS_CO_strata4_sampled),
      
      cover_DRS_FS_strata1 = mean(cover_DRS_FS_strata1_sampled),
      cover_DRS_FS_strata2 = mean(cover_DRS_FS_strata2_sampled),
      cover_DRS_FS_strata3 = mean(cover_DRS_FS_strata3_sampled),
      cover_DRS_FS_strata4 = mean(cover_DRS_FS_strata4_sampled),
    
      
      cover_DRS_SS_strata1 = mean(cover_DRS_SS_strata1_sampled),
      cover_DRS_SS_strata2 = mean(cover_DRS_SS_strata2_sampled),
      cover_DRS_SS_strata3 = mean(cover_DRS_SS_strata3_sampled),
      cover_DRS_SS_strata4 = mean(cover_DRS_SS_strata4_sampled)
    )
}

# Usage for all 5 datasets:
results <- bind_rows(
  TxSc1 %>% calculate_se_coverage() %>% summarize_coverage() %>% mutate(dataset = "TxSc1"),
  TxSc2 %>% calculate_se_coverage() %>% summarize_coverage() %>% mutate(dataset = "TxSc2"),
  TxSc3 %>% calculate_se_coverage() %>% summarize_coverage() %>% mutate(dataset = "TxSc3"),
  TxSc4 %>% calculate_se_coverage() %>% summarize_coverage() %>% mutate(dataset = "TxSc4"),
  TxSc5 %>% calculate_se_coverage() %>% summarize_coverage() %>% mutate(dataset = "TxSc5")
) %>%
  select(dataset, everything())  

results

# Results CO
results %>% select(dataset, starts_with("cover_DRS_CO"))
results %>% select(dataset, starts_with("cover_DRS_FS"))
results %>% select(dataset, starts_with("cover_DRS_SS"))

# average across the methods
results_avg <- results %>%
  rowwise() %>%
  mutate(
    cover_DRS_FS_avg = mean(c(cover_DRS_FS_strata1, cover_DRS_FS_strata2, 
                              cover_DRS_FS_strata3, cover_DRS_FS_strata4)),
    cover_DRS_CO_avg = mean(c(cover_DRS_CO_strata1, cover_DRS_CO_strata2, 
                              cover_DRS_CO_strata3, cover_DRS_CO_strata4)),
    cover_DRS_SS_avg = mean(c(cover_DRS_SS_strata1, cover_DRS_SS_strata2, 
                              cover_DRS_SS_strata3, cover_DRS_SS_strata4))
  ) %>%
  ungroup() 
results_avg

# reaname datasets to be more descriptive 
results_clean <- results_avg %>%
  mutate(dataset = case_when(
    dataset == "TxSc1" ~ "No Treatment Effect",
    dataset == "TxSc2" ~ "OR = 0.8",
    dataset == "TxSc3" ~ "OR = 0.5",
    dataset == "TxSc4" ~ "OR = 0.8 + Interactions",
    dataset == "TxSc5" ~ "OR = 0.5 + Interactiont"
  )) %>%
  relocate(cover_DRS_FS_avg, .after = cover_DRS_FS_strata4) %>%
  relocate(cover_DRS_CO_avg, .after = cover_DRS_CO_strata4) %>%
  relocate(cover_DRS_SS_avg, .after = cover_DRS_SS_strata4) %>%
  pivot_longer(cols = -dataset, names_to = "measure", values_to = "coverage") %>%
  pivot_wider(names_from = dataset, values_from = coverage) 
results_clean

