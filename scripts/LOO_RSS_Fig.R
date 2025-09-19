library(ggpubr)
library(tidyr)
library(dplyr)
library(patchwork)


# This script generates figure 5: Bias in Alternate Split-sample Methods

# Results for base case trial setting must already be generated for simple split-sample, LOO and RSS methods
# using main_analysis.R, LOO.R and repeated_SS.R scripts, respectively.


# Set working directory
setwd("/Users/haedi/Library/CloudStorage/Box-Box/Repos/DRS-HTE-RCT")

# Load the SS results
SS_TxSc1 <- read.csv("./results/main_analysis/base_case/base_case_TxSc1.csv")
SS_TxSc2 <- read.csv("./results/main_analysis/base_case/base_case_TxSc2.csv")
SS_TxSc3 <- read.csv("./results/main_analysis/base_case/base_case_TxSc3.csv")
SS_TxSc4 <- read.csv("./results/main_analysis/base_case/base_case_TxSc4.csv")
SS_TxSc5 <- read.csv("./results/main_analysis/base_case/base_case_TxSc5.csv")

# Load the RSS results
RSS_TxSc1 <- read.csv("./results/RSS/base_case/TxSc1.csv")
RSS_TxSc2 <- read.csv("./results/RSS/base_case/TxSc2.csv")
RSS_TxSc3 <- read.csv("./results/RSS/base_case/TxSc3.csv")
RSS_TxSc4 <- read.csv("./results/RSS/base_case/TxSc4.csv")
RSS_TxSc5 <- read.csv("./results/RSS/base_case/TxSc5.csv")

# Load the LOO results
LOO_TxSc1 <- read.csv("./results/LOO/base_case/TxSc1.csv")
LOO_TxSc2 <- read.csv("./results/LOO/base_case/TxSc2.csv")
LOO_TxSc3 <- read.csv("./results/LOO/base_case/TxSc3.csv")
LOO_TxSc4 <- read.csv("./results/LOO/base_case/TxSc4.csv")
LOO_TxSc5 <- read.csv("./results/LOO/base_case/TxSc5.csv")

# Remove uneeded columns from SS tables (keep only cols that start with with DRS_SS or True_risk)
process_SS_data <- function(df) {
  df %>%
    select(matches("^DRS_SS|^True_risk")) %>%
    rename(
      SS_True_risk_strata1 = True_risk_strata1,
      SS_True_risk_strata2 = True_risk_strata2,
      SS_True_risk_strata3 = True_risk_strata3,
      SS_True_risk_strata4 = True_risk_strata4
    ) %>%
    mutate(SS_RD_strata1 = DRS_SS_strata1_sampled - SS_True_risk_strata1,
           SS_RD_strata2 = DRS_SS_strata2_sampled - SS_True_risk_strata2,
           SS_RD_strata3 = DRS_SS_strata3_sampled - SS_True_risk_strata3,
           SS_RD_strata4 = DRS_SS_strata4_sampled - SS_True_risk_strata4)
    
}
SS_TxSc1 <- process_SS_data(SS_TxSc1) 
SS_TxSc2 <- process_SS_data(SS_TxSc2)
SS_TxSc3 <- process_SS_data(SS_TxSc3)
SS_TxSc4 <- process_SS_data(SS_TxSc4)
SS_TxSc5 <- process_SS_data(SS_TxSc5)

# Remove columns that start with AUC from LOO
process_LOO_data <- function(df) {
  df %>%
    select(matches("^DRS_LOO|^True_risk")) %>%
    rename(
      LOO_True_risk_strata1 = True_risk_strata1,
      LOO_True_risk_strata2 = True_risk_strata2,
      LOO_True_risk_strata3 = True_risk_strata3,
      LOO_True_risk_strata4 = True_risk_strata4
    ) %>%
    mutate(LOO_RD_strata1 = DRS_LOO_strata1_sampled - LOO_True_risk_strata1,
           LOO_RD_strata2 = DRS_LOO_strata2_sampled - LOO_True_risk_strata2,
           LOO_RD_strata3 = DRS_LOO_strata3_sampled - LOO_True_risk_strata3,
           LOO_RD_strata4 = DRS_LOO_strata4_sampled - LOO_True_risk_strata4)
}
LOO_TxSc1 <-process_LOO_data(LOO_TxSc1)
LOO_TxSc2 <-process_LOO_data(LOO_TxSc2)
LOO_TxSc3 <-process_LOO_data(LOO_TxSc3)
LOO_TxSc4 <-process_LOO_data(LOO_TxSc4)
LOO_TxSc5 <-process_LOO_data(LOO_TxSc5)

# clean up the RSS results
# Make a function to summarise results (TrSt1_TxSc1) so that we group by simulation number, average the RD across all Ms
RSS_summary_function <- function(data){
  data_summary <- data %>%
    group_by(simulation) %>% 
    summarize(
      DRS_RSS_strata1_mean = mean(DRS_RSS_strata1_sampled),
      DRS_RSS_strata2_mean = mean(DRS_RSS_strata2_sampled),
      DRS_RSS_strata3_mean = mean(DRS_RSS_strata3_sampled),
      DRS_RSS_strata4_mean = mean(DRS_RSS_strata4_sampled), 
      True_risk_strata1 = first(True_risk_strata1),
      True_risk_strata2 = first(True_risk_strata2),
      True_risk_strata3 = first(True_risk_strata3),
      True_risk_strata4 = first(True_risk_strata4)
    ) %>%
  select(-c(simulation)) %>%
    rename(
      RSS_True_risk_strata1 = True_risk_strata1,
      RSS_True_risk_strata2 = True_risk_strata2,
      RSS_True_risk_strata3 = True_risk_strata3,
      RSS_True_risk_strata4 = True_risk_strata4
    ) %>%
    mutate(RSS_RD_strata1 = DRS_RSS_strata1_mean - RSS_True_risk_strata1,
           RSS_RD_strata2 = DRS_RSS_strata2_mean - RSS_True_risk_strata2,
           RSS_RD_strata3 = DRS_RSS_strata3_mean - RSS_True_risk_strata3,
           RSS_RD_strata4 = DRS_RSS_strata4_mean - RSS_True_risk_strata4)
  return(data_summary)
}

RSS_TxSc1_means <- RSS_summary_function(RSS_TxSc1)
RSS_TxSc2_means <- RSS_summary_function(RSS_TxSc2)
RSS_TxSc3_means <- RSS_summary_function(RSS_TxSc3)
RSS_TxSc4_means <- RSS_summary_function(RSS_TxSc4)
RSS_TxSc5_means <- RSS_summary_function(RSS_TxSc5)


# Combine methods' results and format to calcuate OPB
TxSc1 <- cbind(SS_TxSc1, RSS_TxSc1_means, LOO_TxSc1)
TxSc2 <- cbind(SS_TxSc2, RSS_TxSc2_means, LOO_TxSc2)
TxSc3 <- cbind(SS_TxSc3, RSS_TxSc3_means, LOO_TxSc3)
TxSc4 <- cbind(SS_TxSc4, RSS_TxSc4_means, LOO_TxSc4)
TxSc5 <- cbind(SS_TxSc5, RSS_TxSc5_means, LOO_TxSc5)

format_data_for_OPB<- function(df) {
  formatted_df <- df %>% 
    select(c("DRS_SS_strata1_sampled", "DRS_SS_strata2_sampled", "DRS_SS_strata3_sampled", "DRS_SS_strata4_sampled",
             "DRS_RSS_strata1_mean", "DRS_RSS_strata2_mean", "DRS_RSS_strata3_mean", "DRS_RSS_strata4_mean",
             "DRS_LOO_strata1_sampled", "DRS_LOO_strata2_sampled", "DRS_LOO_strata3_sampled", "DRS_LOO_strata4_sampled",
             "SS_True_risk_strata1", "SS_True_risk_strata2", "SS_True_risk_strata3", "SS_True_risk_strata4")) %>%
    rename(True_risk_strata1 = SS_True_risk_strata1,
           True_risk_strata2 = SS_True_risk_strata2,
           True_risk_strata3 = SS_True_risk_strata3,
           True_risk_strata4 = SS_True_risk_strata4,
           # rename DRS_SS_strata1_sampled to DRS_SS_strata1
           DRS_SS_strata1 = DRS_SS_strata1_sampled,
           DRS_SS_strata2 = DRS_SS_strata2_sampled,
           DRS_SS_strata3 = DRS_SS_strata3_sampled,
           DRS_SS_strata4 = DRS_SS_strata4_sampled,
           # rename DRS_LOO_strata1_sampled to DRS_LOO_strata1
           DRS_LOO_strata1 = DRS_LOO_strata1_sampled,
           DRS_LOO_strata2 = DRS_LOO_strata2_sampled,
           DRS_LOO_strata3 = DRS_LOO_strata3_sampled,
           DRS_LOO_strata4 = DRS_LOO_strata4_sampled,
           # rename DRS_RSS_strata1_mean to DRS_RSS_strata1
           DRS_RSS_strata1 = DRS_RSS_strata1_mean,
           DRS_RSS_strata2 = DRS_RSS_strata2_mean,
           DRS_RSS_strata3 = DRS_RSS_strata3_mean,
           DRS_RSS_strata4 = DRS_RSS_strata4_mean)
  
  return(formatted_df)
}
TxSc1 <- format_data_for_OPB(TxSc1)
TxSc2 <- format_data_for_OPB(TxSc2)
TxSc3 <- format_data_for_OPB(TxSc3)
TxSc4 <- format_data_for_OPB(TxSc4)
TxSc5 <- format_data_for_OPB(TxSc5)

sim_results <- list(TxSc1, TxSc2, TxSc3, TxSc4, TxSc5)

# combine the 4 RD columns from each TxSc1 to form 1 table, then for TxSc2, etc.
TxSc1_RD <- cbind(SS_TxSc1, RSS_TxSc1_means, LOO_TxSc1)
TxSc1_RD <- TxSc1_RD %>% select(matches("RD"))
TxSc2_RD <- cbind(SS_TxSc2, RSS_TxSc2_means, LOO_TxSc2)
TxSc2_RD <- TxSc2_RD %>% select(matches("RD"))
TxSc3_RD <- cbind(SS_TxSc3, RSS_TxSc3_means, LOO_TxSc3)
TxSc3_RD <- TxSc3_RD %>% select(matches("RD"))
TxSc4_RD <- cbind(SS_TxSc4, RSS_TxSc4_means, LOO_TxSc4)
TxSc4_RD <- TxSc4_RD %>% select(matches("RD"))
TxSc5_RD <- cbind(SS_TxSc5, RSS_TxSc5_means, LOO_TxSc5)
TxSc5_RD <- TxSc5_RD %>% select(matches("RD"))

c_rd_data_short <- list(TxSc1_RD, TxSc2_RD, TxSc3_RD, TxSc4_RD, TxSc5_RD)

# Calculate Overall Percent Bias OPB
  percent_bias_function  <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
    percent_bias_list <- list()
    
    for (method in methods) {
      percent_bias <- numeric()
      
      for (i in 1:4) { #strata 1 to 4
        sampled_col <- paste0("DRS_", method, "_strata", i)
        ground_truth_col <- paste0(ground_truth_prefix, i)
        
        mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
        true_value <- data[[ground_truth_col]][1]  #ground truth is the same across rows
        percent_bias[i] <- ((mean_sampled - true_value) / true_value) * 100
      }
      percent_bias_list[[method]] <- percent_bias
    }
    
    percent_bias_df <- data.frame(
      Strata = paste0("strata", 1:4),
      SS_percent_bias = percent_bias_list[["SS"]],
      RSS_percent_bias = percent_bias_list[["RSS"]],
      LOO_percent_bias = percent_bias_list[["LOO"]]
    )
    
    return(percent_bias_df)
  }
  overall_percent_bias_function <- function (data){
    OPB_SS <- mean(abs(data$SS_percent_bias))
    OPB_RSS <- mean(abs(data$RSS_percent_bias))
    OPB_LOO <- mean(abs(data$LOO_percent_bias))
    OPB <- data.frame(OPB_SS = OPB_SS, OPB_RSS = OPB_RSS, OPB_LOO = OPB_LOO)
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
    rownames(results_df) <- c("Null", "Tx Effect: OR 0.8", "Tx Effect 0.5", "Tx Effect 0.8 + Ixn", "Tx Effect 0.5 + Ixn")
    return(results_df)
  }
  
# Calculate Mean Absolute Mean Bias MAMD
  mean_difference_function  <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
    difference_list <- list()
    
    for (method in methods) {
      difference <- numeric()
      
      for (i in 1:4) { #strata 1 to 4
        sampled_col <- paste0("DRS_", method, "_strata", i)
        ground_truth_col <- paste0(ground_truth_prefix, i)
        
        mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
        true_value <- data[[ground_truth_col]][1]  #ground truth is the same across rows
        difference[i] <- (mean_sampled - true_value)
      }
      difference_list[[method]] <- difference
    } 
    
    difference_df <- data.frame(
      Strata = paste0("strata", 1:4),
      SS_difference = difference_list[["SS"]],
      RSS_difference = difference_list[["RSS"]],
      LOO_difference = difference_list[["LOO"]]
    )
    
    return(difference_df)
  }
  
  mean_absolute_mean_difference_function <- function (data){
    MAMD_SS <- mean(abs(data$SS_difference))
    MAMD_RSS <- mean(abs(data$RSS_difference))
    MAMD_LOO <- mean(abs(data$LOO_difference))
    MAMD <- data.frame(MAMD_SS = MAMD_SS,MAMD_RSS = MAMD_RSS, MAMD_LOO = MAMD_LOO)
    return(MAMD)
  }
  
  apply_MAMD_function <- function(data_list, methods, ground_truth_prefix) {
    results_list <- list()
    
    for (data in data_list) {
      mean_difference_results <- mean_difference_function(data, methods, ground_truth_prefix)
      MAMD <- mean_absolute_mean_difference_function(mean_difference_results)
      results_list[[length(results_list) + 1]] <- MAMD
    }
    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- c("Null", "Tx Effect: OR 0.8", "Tx Effect 0.5", "Tx Effect 0.8 + Ixn", "Tx Effect 0.8 + HTE")
    return(results_df)
  }

get_y_limits <- function(composite_rd_data) {
  all_values <- c()  # Vector to store all bias ± SD values
  
  for (i in 1:4) {
    rd_data <- composite_rd_data[[i]]
    
    # Reshape the data into long format
    rd_data_long <- rd_data %>%
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "DRS_(.*)_strata(\\d+)_bias",
        values_to = "diff"
      ) %>%
      drop_na()   # Drop rows with NA
    
    # Calculate summary statistics
    summary_data <- rd_data_long %>%
      group_by(strata, method) %>%
      summarise(
        bias = mean(diff),
        SD_diff = sd(diff),
        .groups = "drop"
      )
    
    # Add bias ± SD_diff to the vector
    all_values <- c(all_values, summary_data$bias + summary_data$SD_diff, summary_data$bias - summary_data$SD_diff)
  }
  
  # Return the overall min and max
  return(c(min(all_values), max(all_values)))
}

composite_pointplot_bias <- function(composite_rd_data, small_titles, overall_title = NULL, OPB_summary = NULL, MAMD_summary = NULL, O_MSE_summary = NULL) {
  # Ensure `titles` is a character vector of length(composite_rd_data)
  if (length(small_titles) != length(composite_rd_data)) {
    stop("`small_titles` must be a character vector of length(composite_rd_data)")
  }
  #y_limits <- get_y_limits(composite_rd_data)
  # y_lim <- ceiling(max(abs(y_limits)) * 10) / 10 +0.03 # Round up to nearest 0.1
  y_lim <- 0.1
  # Initialize an empty list to store the plots
  plot_list <- list()
  
  # Loop over the data for the four scenarios
  for (i in 1:length(composite_rd_data)) {
    rd_data <- composite_rd_data[[i]]
    
    # Reshape the data into long format
    rd_data_long <- rd_data %>%
      pivot_longer(
        cols = everything(), 
        names_to = c("method", "strata"),
        names_pattern = "(.*)_RD_strata(\\d+)",
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
    
    # Add OPB metrics if provided
    if (!is.null(OPB_summary)) {
      # Extract the corresponding row of summary metrics for this plot
      OPB_row <- OPB_summary[i, ]
      OPB_text <- tibble(
        method = c("SS", "RSS", "LOO"),
        OPB_value = as.numeric(OPB_row)
      )
    }
    
    # Add MAMD metrics if provided
    if (!is.null(MAMD_summary)) {
      # Extract the corresponding row of summary metrics for this plot
      MAMD_row <- MAMD_summary[i, ]
      MAMD_text <- tibble(
        method = c("SS", "RSS", "LOO"),
        MAMD_value = as.numeric(MAMD_row)
      )
    }
    
    # Add O_MSE metrics if provided
    if (!is.null(O_MSE_summary)) {
      # Extract the corresponding row of summary metrics for this plot
      O_MSE_row <- O_MSE_summary[i, ]
      O_MSE_text <- tibble(
        method = c("SS", "RSS", "LOO"),
        O_MSE_value = as.numeric(O_MSE_row)
      )
    }
    
    summary_data$method <- factor(summary_data$method, levels = c("SS", "RSS", "LOO"))
    
    # Create the plot
    p <- ggplot(summary_data, aes(x = as.numeric(method), 
                                  y = bias, 
                                  color = as.factor(strata),
                                  shape = as.factor(strata))) + 
      annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_point(position = position_dodge(width = 0.8), size = 3) + 
      geom_errorbar(
        aes(ymin = bias - SD_diff, ymax = bias + SD_diff), 
        position = position_dodge(width = 0.8), 
        width = 0.2) + 
      labs( x = "", y = "Bias", color = "Strata", shape = "Strata", title = small_titles[i]) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10,hjust = 0.5),axis.text = element_text(color = "black"),
            text = element_text(size = 10),
            legend.text = element_text(size = 9)) + 
      scale_color_manual(values = c( "#6BAED6", "#3182BD","#08519C","navy"),
                         labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4"))+
      scale_shape_manual(values = c(16, 17, 15, 18),
                         labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4")) +
      ylim(-y_lim, y_lim) +
     scale_x_continuous(breaks = 1:3, labels = c("Split-Sample", "Repeated-\nSplit-Sample", "Leave-One-Out"))
    
    if (i == 2 | i == 4){ 
      p <- p + labs( x = "", y = "", color = "Strata", title = small_titles[i]) 
    }
    
    # Add MAMD to plots if provided
    if (!is.null(MAMD_summary)) {
      MAMD_text$method <- factor(MAMD_text$method, levels = c("SS", "RSS", "LOO"))
      p <- p + geom_text(
        data = MAMD_text,
        aes(
          x = as.numeric(as.factor(method)),
          y = -y_lim+0.02,  # Place below the plot area
          label = sprintf("MAMD: %.3f", MAMD_value)
        ),
        inherit.aes = FALSE,
        color = "black",
        size = 3
      )
    }   
    
    # Add OPB to second 3 plots Remove the legend from all but the first plot
    if (i > 1) {
      if (!is.null(OPB_summary)) {
        OPB_text$method <- factor(OPB_text$method, levels = c("SS", "RSS", "LOO"))
        p <- p + geom_text(
          data = OPB_text,
          aes(
            x = as.numeric(as.factor(method)),
            y = -y_lim,  # Place below the plot area
            label = sprintf("OPB: %.1f", OPB_value)
          ),
          inherit.aes = FALSE,
          color = "black",
          size = 3
        )
      } 
      p <- p + theme(legend.position = "none")
    }
    
    if (i == 1){
      if (!is.null(OPB_summary)) {
        OPB_text$method <- factor(OPB_text$method, levels = c("SS", "RSS", "LOO"))
        p <- p + geom_text(
          data = OPB_text,
          aes(
            x = as.numeric(as.factor(method)),
            y = -y_lim,  # Place below the plot area
            label = sprintf("—")
          ),
          inherit.aes = FALSE,
          color = "black",
          size = 3
        )
      } 
    }
    
    # Add O_MSE to plots if provided
    if (!is.null(O_MSE_summary)) {
      O_MSE_text$method <- factor(O_MSE_text$method, levels = c("SS", "RSS", "LOO"))
      p <- p + geom_text(
        data = O_MSE_text,
        aes(
          x = as.numeric(as.factor(method)),
          y = -y_lim+0.02,  # Place below the plot area
          label = sprintf("OMSE: %.4f", O_MSE_value)
        ),
        inherit.aes = FALSE,
        color = "black",
        size = 3
      )
    } 
    
    # Add the plot to the list
    plot_list[[i]] <- p
  }
  
  # Legend 
  legend_only <- as_ggplot(get_legend(plot_list[[1]]))
  
  # Remove legend from plot_list[[1]]
  plot_list[[1]] <- plot_list[[1]] + theme(legend.position = "none")
  
  # Combine the plots into a 2x2 grid using ggarrange
  composite_plot <- ggarrange(plot_list[[1]],
                              legend_only,
                              plot_list[[2]],
                              plot_list[[3]],
                              plot_list[[4]],
                              plot_list[[5]],
                              ncol = 2, nrow = 3,
                              labels = c("A", "", "B", "C", "D", "E"),
                              font.label = list(size = 9)
                              )
  # Add the overall title if specified
  if (!is.null(overall_title)) {
    composite_plot <- composite_plot + plot_annotation(title = overall_title)
  }
  
  # Return the composite plot
  return(composite_plot)
}

# Calculate MSE for each method
MSE_function <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
  MSE_list <- list()
  
  for (method in methods) {
    MSE <- numeric()
    
    for (i in 1:4) { #strata 1 to 4
      sampled_col <- paste0("DRS_", method, "_strata", i)
      ground_truth_col <- paste0(ground_truth_prefix, i)
      
      mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
      true_value <- data[[ground_truth_col]][1]  #ground truth is the same across rows
      MSE[i] <- mean((data[[sampled_col]]-true_value)^2)
    }
    MSE_list[[method]] <- MSE
  }
  
  MSE_df <- data.frame(
    Strata = paste0("strata", 1:4),
    SS_MSE = MSE_list[["SS"]],
    RSS_MSE = MSE_list[["RSS"]],
    LOO_MSE = MSE_list[["LOO"]]
  )
  return(MSE_df)
}

overall_MSE_function <- function(data){
  O_MSE_SS <- mean(data$SS_MSE)
  O_MSE_RSS <- mean(data$RSS_MSE)
  O_MSE_LOO <- mean(data$LOO_MSE)
  MSE <- data.frame(O_MSE_SS = O_MSE_SS, O_MSE_RSS = O_MSE_RSS, O_MSE_LOO = O_MSE_LOO)
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
  rownames(results_df) <- c("Null", "Tx Effect: OR 0.8", "Tx Effect 0.5", "Tx Effect 0. + Ixn", "Tx Effect 0.5 + Ixn")
  return(results_df)
}


methods <- c("SS", "RSS", "LOO")
sim_results <-list(TxSc1, TxSc2, TxSc3, TxSc4, TxSc5)
OPB_results <- apply_percent_bias_function(sim_results, methods, "True_risk_strata")
MAMD_results <- apply_MAMD_function(sim_results, methods, "True_risk_strata")
O_MSE_results <- apply_O_MSE_funciton(sim_results, methods, "True_risk_strata")

small_titles = c("No Treatment Effect", "Treatment Effect: OR = 0.8", "Treatment Effect: OR = 0.5", 
                 "Treatment Effect: \nOR = 0.8 + Covariate Interactions", "Treatment Effect: \nOR = 0.5 + Covariate Interactions")
bias_alt_methods_plot <- composite_pointplot_bias(c_rd_data_short, small_titles, 
                                                  overall_title = "Bias in Alternate Split Sample Methods", 
                                                  OPB_summary = OPB_results,
                                                  MAMD_summary = MAMD_results)
bias_alt_methods_plot

ggsave("./results/summary/bias_alt_methods_plot.pdf", plot = bias_alt_methods_plot, 
       width = 7, height = 7, units = "in")

bias_alt_methods_plot_MSE <- composite_pointplot_bias(c_rd_data_short, small_titles, overall_title = NULL,
                                                      O_MSE_summary = O_MSE_results)
bias_alt_methods_plot_MSE

ggsave("./results/summary/bias_alt_methods_plot.pdf", plot = bias_alt_methods_plot_MSE, 
       width = 7, height = 10, units = "in")
