library(ggpubr)
library(patchwork)
# Functions for main results Plots

# Plotting Bias
# Function to calculate bias
rd_bias_function <- function(data){
  rd_bias <- data %>%
    mutate(DRS_FS_strata1_bias = DRS_FS_strata1_sampled - True_risk_strata1,
           DRS_FS_strata2_bias = DRS_FS_strata2_sampled - True_risk_strata2,
           DRS_FS_strata3_bias = DRS_FS_strata3_sampled - True_risk_strata3,
           DRS_FS_strata4_bias = DRS_FS_strata4_sampled - True_risk_strata4,

           DRS_CO_strata1_bias = DRS_CO_strata1_sampled - True_risk_strata1,
           DRS_CO_strata2_bias = DRS_CO_strata2_sampled - True_risk_strata2,
           DRS_CO_strata3_bias = DRS_CO_strata3_sampled - True_risk_strata3,
           DRS_CO_strata4_bias = DRS_CO_strata4_sampled - True_risk_strata4,

           DRS_SS_strata1_bias = DRS_SS_strata1_sampled - True_risk_strata1,
           DRS_SS_strata2_bias = DRS_SS_strata2_sampled - True_risk_strata2,
           DRS_SS_strata3_bias = DRS_SS_strata3_sampled - True_risk_strata3,
           DRS_SS_strata4_bias = DRS_SS_strata4_sampled - True_risk_strata4) %>%
    # keep only rows that end in _bias
    dplyr::select(ends_with("_bias"))
    #select(-c(1:20))

  return(rd_bias)
}

# Functions to calculate overall percent bias 
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
   rownames(results_df) <- c("Null", "Tx Effect: OR 0.8", "Tx Effect 0.5", "Tx Effect 0.8 + Interactions", "Tx Effect 0.5 + Interactions")
   return(results_df)
 }

 # Functions to calculate overall absolute bias 
 mean_difference_function  <- function(data, methods, ground_truth_prefix = "True_risk_strata") {
   difference_list <- list()
   
   for (method in methods) {
     difference <- numeric()
     
     for (i in 1:4) { #strata 1 to 4
       sampled_col <- paste0("DRS_", method, "_strata", i, "_sampled")
       ground_truth_col <- paste0(ground_truth_prefix, i)
       
       mean_sampled <- mean(data[[sampled_col]], na.rm = TRUE)
       true_value <- data[[ground_truth_col]][1]  # ground truth is the same across rows
       difference[i] <- (mean_sampled - true_value)
     }
     difference_list[[method]] <- difference
   } 
   
   difference_df <- data.frame(
     Strata = paste0("strata", 1:4),
     FS_difference = difference_list[["FS"]],
     CO_difference = difference_list[["CO"]],
     SS_difference = difference_list[["SS"]]
   )
   
   return(difference_df)
 }
 Overall_absolute_mean_difference_function <- function (data){
   OAB_FS <- mean(abs(data$FS_difference))
   OAB_CO <- mean(abs(data$CO_difference))
   OAB_SS <- mean(abs(data$SS_difference))
   OAB <- data.frame(OAB_CO = OAB_CO,OAB_FS = OAB_FS, OAB_SS = OAB_SS)
   return(OAB)
 }
 apply_OAB_function <- function(data_list, methods, ground_truth_prefix) {
   results_list <- list()
   
   for (data in data_list) {
     mean_difference_results <- mean_difference_function(data, methods, ground_truth_prefix)
     OAB <- Overall_absolute_mean_difference_function(mean_difference_results)
     results_list[[length(results_list) + 1]] <- OAB
   }
   results_df <- do.call(rbind, results_list)
   rownames(results_df) <- c("Null", "Tx Effect: OR 0.8", "Tx Effect 0.5","Tx Effect 0.8 + Interactions", "Tx Effect 0.5 + Interactions")
   return(results_df)
 }

 # Point Plot of bias
 get_y_limits <- function(composite_rd_data) {
   all_values <- c()  # Vector to store all bias ± SD values
   
   for (i in 1:length(composite_rd_data)) {
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
 
 composite_pointplot_bias <- function(composite_rd_data, small_titles, overall_title = NULL, OPB_summary = NULL, OAB_summary = NULL) {
   # Ensure `titles` is a character vector of length matching the data lists
   if (length(small_titles) != length(composite_rd_data)) {
     stop("`small_titles` must be a character vector of length matching data lists")
   }
   y_limits <- get_y_limits(composite_rd_data)
   # Round up to nearest 0.05
   y_lim <- ceiling(max(abs(y_limits)) * 20) / 20  +0.03 

   # Initialize an empty list to store the plots
   plot_list <- list()
   
   # Loop over the data for the four scenarios
   for (i in 1:length(composite_rd_data)) {
     rd_data <- composite_rd_data[[i]]
     
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
     
     # Add OPB metrics if provided
     if (!is.null(OPB_summary)) {
       # Extract the corresponding row of summary metrics for this plot
       OPB_row <- OPB_summary[i, ]
       OPB_text <- tibble(
         method = c("CO", "FS", "SS"),
         OPB_value = as.numeric(OPB_row)
       )
     }
     
     # Add OAB metrics if provided
     if (!is.null(OAB_summary)) {
       # Extract the corresponding row of summary metrics for this plot
       OAB_row <- OAB_summary[i, ]
       OAB_text <- tibble(
         method = c("CO", "FS", "SS"),
         OAB_value = as.numeric(OAB_row)
       )
     }
     
     # Create the plot
     p <- ggplot(summary_data, 
                 aes(x = as.numeric(as.factor(method)), 
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
       labs( x = "", y = "Bias", color = " ", shape = " ", title = small_titles[i]) +
       theme_minimal() +
       theme(plot.title = element_text(size = 10, hjust = 0.5), axis.text = element_text(color = "black"),
             text = element_text(size = 10),
             legend.text = element_text(size = 10)) + 
       scale_color_manual(values = c( "#6BAED6", "#3182BD","#08519C","navy"),
                          labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4")) + 
       scale_shape_manual(values = c(16, 17, 15, 18),
                          labels = c("DRS Strata 1", "DRS Strata 2", "DRS Strata 3", "DRS Strata 4")) +
       ylim(-y_lim, y_lim) +
       scale_x_continuous(breaks = 1:3, labels = c("Controls-Only", "Full-Sample", "Split-Sample"))
     
     # Remove y axis for plots on the right
     # if (i == 3 | i == 5){ 
     #   p <- p + labs( x = "", y = "", color = "Strata", title = small_titles[i]) 
     # }
     
     # Add OAB to plots if provided
     if (!is.null(OAB_summary)) {
       p <- p + geom_text(
         data = OAB_text,
         aes(
           x = as.numeric(as.factor(method)),
           y = -y_lim+0.02,  # Place below the plot area
           label = sprintf("OAB: %.4f", OAB_value)
         ),
         inherit.aes = FALSE,
         color = "black",
         size = 2.75
       )
     } 
     
     # Add OPB to second 3 plots Remove the legend from all but the first plot
     if (i > 1) {
       if (!is.null(OPB_summary)) {
         p <- p + geom_text(
           data = OPB_text,
           aes(
             x = as.numeric(as.factor(method)),
             y = -y_lim,  # Place below the plot area
             label = sprintf("OPB: %.1f", OPB_value)
           ),
           inherit.aes = FALSE,
           color = "black",
           size = 2.75
         )
       } 
       p <- p + theme(legend.position = "none")
     }
     
     if (i == 1){
       if (!is.null(OPB_summary)) {
         p <- p + geom_text(
           data = OPB_text,
           aes(
             x = as.numeric(as.factor(method)),
             y = -y_lim,  # Place below the plot area
             label = sprintf("—")
           ),
           inherit.aes = FALSE,
           color = "black",
           size = 2.75
         )
       } 
     }
     
     # Add the plot to the list
     plot_list[[i]] <- p
   }
   
   # Legend 
   legend_only <- as_ggplot(get_legend(plot_list[[1]]))
   
   # Remove legend from plot_list[[1]]
   plot_list[[1]] <- plot_list[[1]] + theme(legend.position = "none")
   
   # Combine the plots into a grid using ggarrange
   composite_plot <- ggarrange(plot_list[[1]],
                               legend_only,
                               plot_list[[2]],
                               plot_list[[3]],
                               plot_list[[4]],
                               plot_list[[5]],
                               ncol = 2, nrow = 3,
                                labels = c("A)", "", "B)", "C)", "D)", "E)"),
                              font.label = list(size = 9)
                              )
   # Add the overall title if specified
   if (!is.null(overall_title)) {
     composite_plot <- composite_plot + plot_annotation(title = overall_title)
   }
   
   # Return the composite plot
   return(composite_plot)
 }

 # Point plot risk difference
 get_y_limits_rd <- function(composite_rd_data) {
   all_values <- c()  # Vector to store all bias ± SD values
   
   for (i in 1:4) {
     rd_data <- composite_rd_data[[i]]
     
     # Reshape the data into long format
     rd_data_long <- rd_data %>%
       select(1:12) %>%
       pivot_longer(
         cols = everything(), 
         names_to = c("method", "strata"),
         names_pattern = "DRS_(.*)_strata(\\d+)_sampled",
         values_to = "diff"
       )%>%
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

 composite_pointplot_rd <- function(composite_rd_data, small_titles, overall_title = NULL, OPB_summary = NULL, OAB_summary = NULL) {
   # Ensure `titles` is a character vector of length 4
   if (length(small_titles) != length(composite_rd_data)) {
     stop("`small_titles` must be a character vector of length 4")
   }
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
     rd_data <- composite_rd_data[[i]]
     
     # identify and flag NAs
     na_rows <- rd_data %>%
       select(1:12) %>%
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
     
     # Prepare data for plot
     rd_data_long <- rd_data %>%
       select(1:12) %>%
       pivot_longer(
         cols = everything(), 
         names_to = c("method", "strata"),
         names_pattern = "DRS_(.*)_strata(\\d+)_sampled",
         values_to = "diff"
       ) %>%
       drop_na() %>%  # Drop rows with NA
       mutate(Strata = as.factor(strata))  
     
     # Calculate mean and standard deviation of mean for error bars
     summary_data <- rd_data_long %>%
       group_by(strata, method) %>%
       summarise(mean = mean(diff), SD_diff = sd(diff), .groups = "drop")
     
     # Extract Ground Truth values for each strata
     ground_truth<- data.frame()
     ground_truth <- data.frame(
       strata = c(1, 2, 3, 4),
       mean = c(
         rd_data$True_risk_strata1[1], 
         rd_data$True_risk_strata2[1], 
         rd_data$True_risk_strata3[1], 
         rd_data$True_risk_strata4[1]))
     
     ground_truth <- expand.grid(strata = unique(ground_truth$strata), 
                                 method = c("CO", "FS", "SS")) %>%
       left_join(ground_truth, by = "strata")
     
     # Add OPB metrics if provided
     if (!is.null(OPB_summary)) {
       OPB_row <- OPB_summary[i, ]
       OPB_text <- tibble(
         method = c("CO", "FS", "SS"),
         OPB_value = as.numeric(OPB_row))}
     
     # Add OAB metrics if provided
     if (!is.null(OAB_summary)) {
       OAB_row <- OAB_summary[i, ]
       OAB_text <- tibble(
         method = c("CO", "FS", "SS"),
         OAB_value = as.numeric(OAB_row))}
     
     # Create the plot
     p <- ggplot(summary_data, aes(x = as.numeric(as.factor(method)), y = mean, color = as.factor(strata))) + 
       annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) + # gray box
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
       scale_x_continuous(breaks = 1:3, labels = c("Controls-Only", "Full-Sample", "Split-Sample")) +
       guides(color = guide_legend(
         override.aes = list(
           color = c("#6BAED6", "#3182BD", "#08519C", "navy", "red"),  
           shape = c(16, 16, 16, 16, 95),  
           size = 5)))
     
     if (i == 3 | i == 5){ 
       p <- p + labs( x = "", y = "", color = "Legend", title = small_titles[i]) 
     }
     
     # Add OAB to plots if provided
     if (!is.null(OAB_summary)) {
       p <- p + geom_text(
         data = OAB_text,
         aes(
           x = as.numeric(as.factor(method)),
           y = y_min+0.03,  # Place below the plot area
           label = sprintf("OAB: %.4f", OAB_value)
         ),
         inherit.aes = FALSE,
         color = "black",
         size = 3
       )
     } 
     
     # Add OPB to second 3 plots Remove the legend from all but the first plot
     if (i > 1) {
       if (!is.null(OPB_summary)) {
         p <- p + geom_text(
           data = OPB_text,
           aes(x = as.numeric(as.factor(method)),
               y = y_min,  # Place below the plot area
               label = sprintf("OPB: %.1f", OPB_value)),
           inherit.aes = FALSE,
           color = "black",
           size = 3)} 
       p <- p + theme(legend.position = "none")
     }
     
     if (i == 1){
       if (!is.null(OPB_summary)) {
         p <- p + geom_text(
           data = OPB_text,
           aes(
             x = as.numeric(as.factor(method)),
             y = y_min,  # Place below the plot area
             label = sprintf("—")
           ),
           inherit.aes = FALSE,
           color = "black",
           size = 3
         )
       } 
     }
     
     # Add the plot to the list
     plot_list[[i]] <- p
   }
   
   # Legend 
   legend_only <- as_ggplot(get_legend(plot_list[[1]]))
   
   # Remove legend from plot_list[[1]]
   plot_list[[1]] <- plot_list[[1]] + theme(legend.position = "none")
   
   # Combine the plots into a grid using ggarrange
   composite_plot <- ggarrange(plot_list[[1]],
                               legend_only,
                               plot_list[[2]],
                               plot_list[[3]],
                               plot_list[[4]],
                               plot_list[[5]],
                               ncol = 2, nrow = 3,
                               labels = c("A)", "", "B)", "C)", "D)", "E)"),
                               font.label = list(size = 9)
   )
   
   # Add the overall title if specified
   if (!is.null(overall_title)) {
     composite_plot <- composite_plot + plot_annotation(title = overall_title)
   }
   
   # Return the composite plot
   return(composite_plot)
 }
