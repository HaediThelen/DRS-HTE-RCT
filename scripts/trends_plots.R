library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(patchwork)
library(ggpubr)

# This script generates figure 3; Trends in bias as parameters vary in the small
  # treatment effect with treatment-covariate interactions scenario. 

# Results for each trial settings (base_case and a-n) must already be generated
  # using main_analysis.R

#Set WD
#setwd()

# Check for trends plots results directory
if (!dir.exists("./results/summary/")) { #ensure results folder exists locally
  print("The directory will be created.")
  dir.create("./results/summary/", recursive = TRUE)
}

# Summary Plots
plot_OPB <- function(data_list, x_variable, plot_titles = NULL, x_lab) {
  # Add x_variable column to each data frame
  for (i in seq_along(data_list)) {
    data_list[[i]]$x_variable <- x_variable[i]
  }
  
  # Remove 'Null' treatment scenario
  data_list <- lapply(data_list, function(df) df %>% filter(X != "Null"))
  
  # Combine all data frames
  data_combined <- bind_rows(data_list)
  
  # Reshape data
  data_long <- data_combined %>%
    pivot_longer(cols = c(OPB_CO, OPB_FS, OPB_SS), 
                 names_to = "Model", 
                 values_to = "OPB") %>%
    mutate(TxEffect = factor(X, levels = unique(X))) %>%
    mutate(Model = case_when(
      Model == "OPB_CO" ~ "Controls-Only",
      Model == "OPB_FS" ~ "Full-Sample",
      Model == "OPB_SS" ~ "Split-Sample"
    )) %>%
    select(-X)
  
  # Set y-max
  y_max <- 100
  # Use custom plot titles if provided, otherwise use unique TxEffect
  if (is.null(plot_titles)) {
    plot_titles <- unique(data_long$TxEffect)
  }
  
  # Generate plots
  OPB_plots <- data_long %>%
    group_by(TxEffect) %>%
    group_split() %>%
    lapply(function(df, titles) {
      idx <- match(unique(df$TxEffect), unique(data_long$TxEffect))
      ggplot(df, aes(x = x_variable, y = OPB, color = Model, shape = Model, group = Model)) +
        geom_line() +
        geom_point() +
        labs(
         # title = titles[idx], #turn titles off
          x = x_lab,
          y = "Overall Percent Bias",
          color = "DRS Method",
          shape = "DRS Method"
        ) +
        ylim(0, y_max) +
        scale_color_manual(values = c("#c82a0d","#ce8e00", "#269fe2")) + 
        scale_shape_manual(values = c(16, 17, 15)) + 
        theme_minimal() +
        theme(
          #legend.position = "bottom",
          axis.text.x = element_text(angle = 0, color = "black"),
          axis.text.y = element_text(color = "black"),
          text = element_text(size = 10),
          legend.text = element_text(size = 9),
          axis.title.x = element_text(margin = margin(t = 10)),
        )
    }, #titles = plot_titles # turn titles off
    )
  
  return(OPB_plots)
} 

# OPB vs Sample Size Plot
base_case_OPB <- read.csv("./results/main_analysis/base_case/base_case_OPB_results.csv")
a_samplesz500_OPB <- read.csv("./results/main_analysis/a_smplsz500/a_smplsz500_OPB_results.csv")
b_samplesz1000_OPB <- read.csv("./results/main_analysis/b_smplsz1000/b_smplsz1000_OPB_results.csv")
c_samplesz5000_OPB <- read.csv("./results/main_analysis/c_smplsz5000/c_smplsz5000_OPB_results.csv")
d_samplesz10000_OPB <- read.csv("./results/main_analysis/d_smplsz10000/d_smplsz10000_OPB_results.csv")

# make list of dataframes to include
sample_size_OBP_list <- list(base_case_OPB, a_samplesz500_OPB, b_samplesz1000_OPB, c_samplesz5000_OPB, d_samplesz10000_OPB)

#make vector of x-axis values, # Order must mach data_list
sample_size <- c(3600, 500, 1000, 5000, 10000)

# make vector of plot titles
OPB_plot_titles <- c("Small Treatment Effect", "Large Treatment Effect", "Large Treatment Effect with Interactions") #these are likely wrong for the 5 scenarios we now hove
OPB_plots_sample_size <- plot_OPB(sample_size_OBP_list, sample_size, OPB_plot_titles, x_lab = "Sample Size")

# OPB vs Outcome Proportion
# Load data
base_case_OPB <- read.csv("./results/main_analysis/base_case/base_case_OPB_results.csv")
e_outcome_0.15_OPB <- read.csv("./results/main_analysis/e_outcome_0.15/e_outcome_0.15_OPB_results.csv")
f_outcome_0.05_OPB <- read.csv("./results/main_analysis/f_outcome_0.05/f_outcome_0.05_OPB_results.csv")
g_outcome_0.5_OPB <- read.csv("./results/main_analysis/g_outcome_0.5/g_outcome_0.5_OPB_results.csv")
OPB_outcome_prop_list <- list(base_case_OPB, e_outcome_0.15_OPB, f_outcome_0.05_OPB, g_outcome_0.5_OPB)

# Order must mach data_list
outcome_prop <- c(0.25, 0.15, 0.05, 0.5)
OPB_plots_outcome_prop <- plot_OPB(OPB_outcome_prop_list, outcome_prop, OPB_plot_titles, x_lab = "Outcome Incidence")
OPB_plots_outcome_prop[[3]]
# OPB vs Randomization Ratio
base_case_OPB <- read.csv("./results/main_analysis/base_case/base_case_OPB_results.csv")
h_ratio_2to1_OPB <- read.csv("./results/main_analysis/h_ratio_2to1/h_ratio_2to1_OPB_results.csv")
i_ratio_3to1_OPB <- read.csv("./results/main_analysis/i_ratio_3to1/i_ratio_3to1_OPB_results.csv")
j_ratio_4to1_OPB <- read.csv("./results/main_analysis/j_ratio_4to1/j_ratio_4to1_OPB_results.csv")
OPB_rand_ratio_list <- list(base_case_OPB, h_ratio_2to1_OPB, i_ratio_3to1_OPB, j_ratio_4to1_OPB)
rand_ratios <- c(1, 2, 3, 4)

OPB_plots_rand_ratio <- plot_OPB(OPB_rand_ratio_list, rand_ratios, OPB_plot_titles, x_lab = "Randomization Ratio")

# OPB vs AUC
base_case_OPB <- read.csv("./results/main_analysis/base_case/base_case_OPB_results.csv")
k_auc_0.70_OPB <- read.csv("./results/main_analysis/k_auc_0.70/k_auc_0.70_OPB_results.csv")
l_auc_0.75_OPB <- read.csv("./results/main_analysis/l_auc_0.75/l_auc_0.75_OPB_results.csv")
m_auc_0.80_OPB <- read.csv("./results/main_analysis/m_auc_0.80/m_auc_0.80_OPB_results.csv")
n_auc_0.85_OPB <- read.csv("./results/main_analysis/n_auc_0.85/n_auc_0.85_OPB_results.csv")
OPB_auc_list <- list(base_case_OPB, k_auc_0.70_OPB, l_auc_0.75_OPB, m_auc_0.80_OPB, n_auc_0.85_OPB)
auc_vals <- c(0.65, 0.70, 0.75, 0.80, 0.85)

OPB_plots_auc <- plot_OPB(OPB_auc_list, auc_vals, OPB_plot_titles, x_lab = "C-statistic for the DRS model ")

# Figure 3: extract plots corresponding to small treatment effect with interactions [[3]]
OPB_plots_Fig3_O8ixn <-ggarrange(OPB_plots_sample_size[[3]],
                                 OPB_plots_outcome_prop[[3]] + theme(axis.title.y = element_text(color = "transparent")),
                                 OPB_plots_rand_ratio[[3]],
                                 OPB_plots_auc[[3]]+ theme(axis.title.y = element_text(color = "transparent")),
                                 ncol = 2, nrow = 2, common.legend = TRUE, legend = "right",
                                 labels = c("A)", "B)", "C)", "D)"),
                                 font.label = list(size = 9)
                                 )


OPB_plots_Fig3_O8ixn <- annotate_figure(OPB_plots_Fig3_O8ixn, 
                                        top = text_grob("Trends in Bias with Treatment Effect: OR = 0.8 + Covariate Interactions", 
                                        color = "Black", size = 14))
OPB_plots_Fig3_O8ixn 

# Save as PDF
ggsave("./results/summary/OPBOR08ixn.pdf", plot = OPB_plots_Fig3_O8ixn, 
       width = 6.5, height = 6.5, units = "in")


