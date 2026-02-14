# Load RD bias data
rd_file_path <- "./results/main_analysis/base_case/"
or_file_path <- "./results/main_analysis/base_case_or/"

# load OPB results 
rd_OPB <- read.csv(paste0(rd_file_path, "base_case_OPB_results.csv"))
or_OPB<- read.csv(paste0(or_file_path, "base_case_or_OPB_results.csv"))

# make summary table comparing OPB across methods and scales. 
combined_OPB <- data.frame(
  Scenario = rd_OPB$X,
  CO_RD = rd_OPB$OPB_CO,
  CO_logOR = or_OPB$OPB_CO,
  FS_RD = rd_OPB$OPB_FS,
  FS_logOR = or_OPB$OPB_FS,
  SS_RD = rd_OPB$OPB_SS,
  SS_logOR = or_OPB$OPB_SS
) %>% slice(-1) # remove null treatment scenario for OPB
combined_OPB

