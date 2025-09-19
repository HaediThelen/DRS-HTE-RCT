library(ggpubr)
library(patchwork)

# This script generates figure 4: Trends in bias and precision as the percentage
  # of controls used to fit the DRS model varies. 
# Simulation results for each sample size (500, 1000, 3600)
  # must already be generated using main_alt_ss_drs_train_pct.R

# Set working directory
setwd("/Users/haedi/Library/CloudStorage/Box-Box/Repos/DRS-HTE-RCT")


# Read in OPB REsults
a_smplsz500 <- read.csv("./results/alt_ss_proportion/a_smplsz500_OPB_results.csv", row.names = 1)
b_smplsz1000 <- read.csv("./results/alt_ss_proportion/b_smplsz1000_OPB_results.csv", row.names = 1)
base_case <- read.csv("./results/alt_ss_proportion/base_case_OPB_results.csv", row.names = 1)

# Add a column to each identifying the source
a_smplsz500 <- a_smplsz500 %>%
  mutate(source = "Sample Size 500") %>%
  tibble::rownames_to_column("Percent")
b_smplsz1000 <- b_smplsz1000 %>% 
  mutate(source = "Sample Size 1000") %>%
  tibble::rownames_to_column("Percent")
base_case <- base_case %>%
  mutate(source = "Sample Size 3600") %>%
  tibble::rownames_to_column("Percent")

# Combine into one long data frame
combined_df <- bind_rows(a_smplsz500, b_smplsz1000, base_case)

# Add split denominator as a column (from rownames)
combined_df <- combined_df %>%
  mutate(Percent = factor(Percent, levels = a_smplsz500$Percent))%>%
  mutate(source = factor(
    source,
    levels = c("Sample Size 500", "Sample Size 1000", "Sample Size 3600")
  ))%>%
  mutate(Percent = gsub("%", "", Percent))


# Plot
OPB <- ggplot(combined_df, aes(x = Percent, y = OPB_SS, group = source, color = source, shape = source)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    #title = "OPB in Split-Sample Method by Proportion in DRS vs Outcome Model",
    x = "Split Denominator",
    y = "Overall Percent Bias",
    color = "Sample Size",
    shape = "Sample Size"
  ) +
  ylim(0, 50) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, color = "black"),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 10)
  ) +
  scale_color_manual(values = c(
    "Sample Size 500"  = "#E64B35",  
    "Sample Size 1000" = "#4DBBD5",  
    "Sample Size 3600" = "#00A087"   
  ),
  labels = c(
    "Sample Size 500"  = "500",
    "Sample Size 1000" = "1000",
    "Sample Size 3600" = "3600"
  )) +
  scale_shape_manual(values = c(
    "Sample Size 500"  = 16, 
    "Sample Size 1000" = 15, 
    "Sample Size 3600" = 17 
  ),
  labels = c(
    "Sample Size 500"  = "500",
    "Sample Size 1000" = "1000",
    "Sample Size 3600" = "3600"
  ))
OPB


# Read in OPB REsults
a_smplsz500_OMSE <- read.csv("./results/alt_ss_proportion/a_smplsz500_OMSE_results.csv", row.names = 1)
b_smplsz1000_OMSE <- read.csv("./results/alt_ss_proportion/b_smplsz1000_OMSE_results.csv", row.names = 1)
base_case_OMSE <- read.csv("./results/alt_ss_proportion/base_case_OMSE_results.csv", row.names = 1)

# Add a column to each identifying the source
a_smplsz500_OMSE <- a_smplsz500_OMSE %>%
  mutate(source = "Sample Size 500") %>%
  tibble::rownames_to_column("Percent")
b_smplsz1000_OMSE <- b_smplsz1000_OMSE %>% 
  mutate(source = "Sample Size 1000") %>%
  tibble::rownames_to_column("Percent")
base_case_OMSE <- base_case_OMSE %>%
  mutate(source = "Sample Size 3600") %>%
  tibble::rownames_to_column("Percent")

# Combine into one long data frame
combined_df_OMSE <- bind_rows(a_smplsz500_OMSE, b_smplsz1000_OMSE, base_case_OMSE)

# Add Percent of split as a column (from rownames)
combined_df_OMSE <- combined_df_OMSE %>%
  mutate(Percent = factor(Percent, levels = a_smplsz500_OMSE$Percent))%>%
  mutate(source = factor(
    source,
    levels = c("Sample Size 500", "Sample Size 1000", "Sample Size 3600")
  )) %>%
  mutate(Percent = gsub("%", "", Percent))

# Plot
OMSE <- ggplot(combined_df_OMSE, aes(x = Percent, y = O_MSE_SS, 
                                     group = source, color = source, shape = source)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Percent of Controls Used to Fit DRS Model",
    y = "Overall MSE",
    color = "Sample Size",
    shape = "Sample Size"
  ) +
  ylim(0, 0.075) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 10)),
    text = element_text(size = 10)
  ) +
  scale_color_manual(values = c(
    "Sample Size 500"  = "#E64B35",  
    "Sample Size 1000" = "#4DBBD5",  
    "Sample Size 3600" = "#00A087"   
  ),
  labels = c(
    "Sample Size 500"  = "500",
    "Sample Size 1000" = "1000",
    "Sample Size 3600" = "3600"
  )) +
  scale_shape_manual(values = c(
    "Sample Size 500"  = 16, 
    "Sample Size 1000" = 15, 
    "Sample Size 3600" = 17  
  ),
  labels = c(
    "Sample Size 500"  = "500",
    "Sample Size 1000" = "1000",
    "Sample Size 3600" = "3600"
  ))

OMSE


OPB_plot_no_x <- OPB + theme(
  axis.title.x = element_blank() # remove axis label from OPB plot
)
composite_OPB_MSE_plot <-
  ggarrange(OPB_plot_no_x, OMSE,
            ncol = 1, nrow = 2,
            labels = c("A", "B"),
            font.label = list(size = 9),
            common.legend = TRUE, legend = "bottom")

composite_OPB_MSE_plot
ggsave("./results/alt_ss_proportion/OPB_OMSE_plot.pdf", plot = composite_OPB_MSE_plot,width = 4.5, height = 6.5, units = "in")

