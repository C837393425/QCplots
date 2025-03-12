library(ggplot2)
library(gridExtra)
library(dplyr)


#time_series_plot_function: 
  #Function to plot the average annual predictions of a given variable by year and state

#Objective: 
  # This function computes the weighted average of a specified variable across two datasets (two daycent outputs),
  # and creates a time series plot for each state showing the weighted averages over the years.

# Input: 
  #df_final: A data frame containing the data with columns including year, state_name, variable_name.x (from first input data (i.e. 491)), variable_name.y (from first input data (i.e. 513)),, and area_ha.
  #variable_name: A string representing the name of the variable 
  #unit: A string representing the unit of the variable (e.g. ton/ha, cm).

# Output: 
# A list of ggplot objects, each representing a plot for a state showing the time series of weighted averages for the two datasets.
#'@noRd

time_series_plot_function <- function(df_final, variable_name, unit) {
  
  # Compute the weighted average by year and state
  df_combined <- df_final %>%
    group_by(state_name, year) %>%
    summarise(
      !!paste0("avg_", variable_name, ".x") := sum(.data[[paste0(variable_name, ".x")]] * area_ha) / sum(area_ha),
      !!paste0("avg_", variable_name, ".y") := sum(.data[[paste0(variable_name, ".y")]] * area_ha) / sum(area_ha),
      .groups = "drop"
    )
  
  # Prepare the combined data for plotting
  df_combined1 <- df_combined %>%
    select(year, state_name,
           avg_variable_df1 = !!paste0("avg_", variable_name, ".x"),
           avg_variable_df2 = !!paste0("avg_", variable_name, ".y"))
  
  states <- unique(df_combined1$state_name)
  plot_list <- list()
  
  for (i in 1:length(states)) {
    
    state = states[i]
    state_data <- df_combined1 %>% filter(state_name == state)
    
    p <- ggplot(state_data, aes(x = year)) +
      geom_line(aes(y = avg_variable_df1, color = "DayCent_491")) +
      geom_line(aes(y = avg_variable_df2, color = "DayCent_513")) +
      geom_point(aes(y = avg_variable_df1, color = "DayCent_491")) +
      geom_point(aes(y = avg_variable_df2, color = "DayCent_513")) +
      labs(
        title = paste("Year-wise Average", variable_name,unit, "for", state),
        x = "Year", 
        y = paste("Average Annual \n", variable_name, unit, "Predictions"),
        color = "Prediction Source"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    plot_list[[i]] <- p
  }
  
  return(plot_list)
}




# scatter_plot_function: Function to create a scatter plot comparing two variables from two datasets
#
# Objective: 
# This function creates a scatter plot to compare the predicted values of a given variable between two datasets,
# and calculates the Pearson correlation coefficient between the two variables.
#
# Input: 
# - df_scatter_plot: A data frame containing the two variables to be compared.
#   - variable_df1: Predicted values of the variable from the first dataset (e.g., DayCent_491).
#   - variable_df2: Predicted values of the variable from the second dataset (e.g., DayCent_513).
# - variable_name: A string representing the name of the variable.
# - unit: A string representing the unit of the variable.
#
# Output: 
# - A ggplot object representing the scatter plot comparing the two datasets' predicted values.
#'@noRd




scatter_plot_function <- function(df_scatter_plot, variable_name, unit) {
  
  cor_value <- cor(df_scatter_plot$variable_df1, df_scatter_plot$variable_df2, 
                   use = "complete.obs", method = "pearson")
  
  ggplot(df_scatter_plot, aes(x = variable_df1, y = variable_df2)) +
    geom_point(size = 1) +  # Scatter plot points
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +  # y = x line
    labs(x = paste("Predicted Annual \n", variable_name, unit, "_DayCent_491"), 
         y = paste("Predicted Annual \n", variable_name, unit, "_DayCent_513") 
    ) +
    annotate("text", 
             x = min(df_scatter_plot$variable_df1, na.rm = TRUE), 
             y = max(df_scatter_plot$variable_df2, na.rm = TRUE), 
             label = paste("Correlation:", round(cor_value, 3)), 
             hjust = 0, vjust = 1, size = 5, color = "red") +
    theme_bw(base_size = 10)  # Apply a clean theme
}




# print_line_plots_per_page: Function to print multiple line plots on each page
#
# Objective: 
# This function arranges and displays a list of line plots across multiple pages, showing a specified number of plots per page.
#
# Input: 
# - plot_list: A list containing ggplot objects (line plots) that you want to display.
# - plots_per_page: An integer specifying how many plots should be shown on each page.
#
# Output: 
# - The function will print the plots in pages with the specified number of plots per page.
#'@noRd


print_line_plots_per_page <- function(plot_list,  plots_per_page) {
  
  plot_list <- unlist(plot_list, recursive = FALSE)
  num_pages <- ceiling(length(plot_list) / plots_per_page)
  
  for (i in seq_len(num_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(plot_list))
    
    grid.arrange(grobs = plot_list[start_idx:end_idx])
  }
}






# print_save_scatter_plots_per_page: Function to print and save multiple scatter plots across pages
#
# Objective:
# This function arranges and saves a list of scatter plots across multiple pages, showing a specified number of plots per page.
# Each page is saved as a PNG file in the specified output directory.
#
# Input:
# - plot_list: A list containing ggplot scatter plot objects to be saved.
# - plots_per_page: An integer specifying how many plots should be shown on each page.
# - output_dir: The directory where the plots will be saved as PNG files.
#
# Output:
# - The function saves PNG files of scatter plot pages in the specified directory.
#'@noRd


print_save_scatter_plots_per_page <- function(plot_list, plots_per_page, output_dir) {
  
  num_pages <- ceiling(length(plot_list) / plots_per_page)
  
  for (i in seq_len(num_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, length(plot_list))
    
    # Arrange multiple plots
    combined_plot <- grid.arrange(grobs = plot_list[start_idx:end_idx], ncol = 2)
    
    # Define full file path
    file_name <- file.path(output_dir, paste0("scatter_plots_page_", i, ".png"))
    
    # Save each page as a PNG with lower quality
    ggsave(file_name, combined_plot, width = 8.27, height = 9.69, dpi = 100) # Adjust width, height, and dpi for lower quality
  }
}







# create_line_plots_pdf: Function to create and save line plots of variables in a PDF file
#
# Objective: 
# This function generates line plots for each variable and saves them in a PDF file, with the option to display multiple plots per page.
#
# Input:
# - df1: A data frame containing the first set of data (e.g., model 1 predictions).
# - df2: A data frame containing the second set of data (e.g., model 2 predictions).
# - nri_all_info: A data frame containing additional information (e.g., state, area) used for merging and plotting.
# - variables: A vector of variable names to be plotted.
# - variable_units: A vector of corresponding units for each variable.
# - no_of_plots_per_page: The number of plots to display on each PDF page.
# - output_dir: The directory where the PDF file will be saved.
#
# Output:
# - A PDF file saved in the specified directory, containing the line plots for each variable.
#'@noRd


create_line_plots_pdf <- function(df1, df2, nri_all_info, variables, variable_units, no_of_plots_per_page, output_dir) {
  
  # Create an empty list for line plots
  line_plot_list <- list()
  
  # Loop through the variables
  for (i in 1:length(variables)) {
    
    # Check if the input data frames have the necessary columns
    if (!all(c("recordid2017", "year") %in% colnames(df1))) {
      stop("Error: 'df1' must contain 'recordid2017' and 'year' columns.")
    }
    if (!all(c("recordid2017", "year") %in% colnames(df2))) {
      stop("Error: 'df2' must contain 'recordid2017' and 'year' columns.")
    }
    
    variable_name <- variables[i]
    unit <- variable_units[i]
    
    # Ensure filtered_df1 and filtered_df2 are initialized before any condition
    filtered_df1 <- df1
    filtered_df2 <- df2
    
    # Handle cases where the variable name ends with "_diff" (e.g., somsc_diff)
    if (grepl("_diff$", variable_name)) {
      base_variable <- str_remove(variable_name, "_diff$")
      
      # Check if base_variable exists in both dataframes
      if (!base_variable %in% colnames(df1)) {
        stop(paste("Error: base variable", base_variable, "is not found in df1."))
      }
      if (!base_variable %in% colnames(df2)) {
        stop(paste("Error: base variable", base_variable, "is not found in df2."))
      }
      
      # Process df1
      filtered_df1 <- df1 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
      # Process df2 similarly
      filtered_df2 <- df2 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
    } else {
      # If the variable does not have "_diff", select it directly from both dataframes
      if (!variable_name %in% colnames(filtered_df1)) {
        stop(paste("Error: Variable", variable_name, "is not found in df1."))
      }
      if (!variable_name %in% colnames(filtered_df2)) {
        stop(paste("Error: Variable", variable_name, "is not found in df2."))
      }
      
      filtered_df1 <- filtered_df1 %>% select(recordid2017, year, all_of(variable_name))
      filtered_df2 <- filtered_df2 %>% select(recordid2017, year, all_of(variable_name))
    }
    
    
    # Check if nri_all_info has the necessary columns for merging
    if (!all(c("recordid2017", "year", "state_name") %in% colnames(nri_all_info))) {
      stop("Error: 'nri_all_info' must contain 'recordid2017' and 'year' columns for merging.")
    }
    
    # Merge datasets by 'recordid2017' and 'year'
    df_merged <- merge(filtered_df1, filtered_df2, by = c("recordid2017", "year"), all = TRUE)
    df_final <- left_join(df_merged, nri_all_info, by = c("recordid2017", "year"))
    
    # Check if area_ha exists in the dataframe
    if (!"area_ha" %in% colnames(df_final)) {
      stop("Error: 'area_ha' column is missing in the final dataframe.")
    }
    
    # Convert units from g/m² to ton/ha (1 g/m² = 10 kg/ha = 0.01 t/ha)
    df_final[[paste0(variable_name, ".x")]] <- df_final[[paste0(variable_name, ".x")]] * 0.01
    df_final[[paste0(variable_name, ".y")]] <- df_final[[paste0(variable_name, ".y")]] * 0.01
    
    
    plot_line <- time_series_plot_function(df_final, variable_name, unit)
    line_plot_list[[i]] <- plot_line
    
  }
  
  # Save line plots to a PDF file
  pdf_path <- file.path(output_dir, "state_wise_line_plots.pdf")
  pdf(pdf_path, width = 14, height = 14)
  print_line_plots_per_page(line_plot_list,  no_of_plots_per_page) 
  dev.off()
}









# Function to create and save scatter plots

# Input:
# - df1: A data frame containing the first set of data (e.g., model 1 predictions).
# - df2: A data frame containing the second set of data (e.g., model 2 predictions).
# variables: A vector of variable names to create scatter plots for.
# variable_units: A vector of corresponding units for each variable
# no_of_plots_per_page: The number of plots to display per page in the output image
# output_dir: The directory where the resulting scatter plots should be saved

# Output:
# Saves scatter plots as images in the specified output directory
#'@noRd


create_scatter_plots_image <- function(df1, df2, variables, variable_units, no_of_plots_per_page, output_dir) {
  
  # Create an empty list for line plots
  scatter_plot_list <- list() 
  
  # Loop through the variables
  for (i in 1:length(variables)) {
    
    # Check if the input data frames have the necessary columns
    if (!all(c("recordid2017", "year") %in% colnames(df1))) {
      stop("Error: 'df1' must contain 'recordid2017' and 'year' columns.")
    }
    if (!all(c("recordid2017", "year") %in% colnames(df2))) {
      stop("Error: 'df2' must contain 'recordid2017' and 'year' columns.")
    }
    
    variable_name <- variables[i]
    unit <- variable_units[i]
    
    # Ensure filtered_df1 and filtered_df2 are initialized before any condition
    filtered_df1 <- df1
    filtered_df2 <- df2
    
    # Handle cases where the variable name ends with "_diff" (e.g., somsc_diff)
    if (grepl("_diff$", variable_name)) {
      base_variable <- str_remove(variable_name, "_diff$")
      
      # Check if base_variable exists in both dataframes
      if (!base_variable %in% colnames(df1)) {
        stop(paste("Error: base variable", base_variable, "is not found in df1."))
      }
      if (!base_variable %in% colnames(df2)) {
        stop(paste("Error: base variable", base_variable, "is not found in df2."))
      }
      
      # Process df1
      filtered_df1 <- df1 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
      # Process df2 similarly
      filtered_df2 <- df2 %>%
        select(recordid2017, year, all_of(base_variable)) %>% # Select relevant columns
        mutate(across(all_of(base_variable), as.numeric)) %>% # Ensure base_variable is numeric
        arrange(recordid2017, year) %>% # Ensure sorting
        group_by(recordid2017) %>% # Group by recordid2017
        mutate(!!variable_name := !!sym(base_variable) - lag(!!sym(base_variable))) %>% # Compute lag difference
        ungroup() %>%
        filter(!is.na(!!sym(variable_name))) %>% # Remove rows where somsc_diff is NA (year = 1979)
        select(-all_of(base_variable)) # Drop the original base_variable column
      
    } else {
      # If the variable does not have "_diff", select it directly from both dataframes
      if (!variable_name %in% colnames(filtered_df1)) {
        stop(paste("Error: Variable", variable_name, "is not found in df1."))
      }
      if (!variable_name %in% colnames(filtered_df2)) {
        stop(paste("Error: Variable", variable_name, "is not found in df2."))
      }
      
      filtered_df1 <- filtered_df1 %>% select(recordid2017, year, all_of(variable_name))
      filtered_df2 <- filtered_df2 %>% select(recordid2017, year, all_of(variable_name))
    }
    
    
    # Check if nri_all_info has the necessary columns for merging
    if (!all(c("recordid2017", "year", "state_name") %in% colnames(nri_all_info))) {
      stop("Error: 'nri_all_info' must contain 'recordid2017' and 'year' columns for merging.")
    }
    
    # Merge datasets by 'recordid2017' and 'year'
    df_final <- merge(filtered_df1, filtered_df2, by = c("recordid2017", "year"), all = TRUE)
    
    # Convert units from g/m² to ton/ha (1 g/m² = 10 kg/ha = 0.01 t/ha)
    df_final[[paste0(variable_name, ".x")]] <- df_final[[paste0(variable_name, ".x")]] * 0.01
    df_final[[paste0(variable_name, ".y")]] <- df_final[[paste0(variable_name, ".y")]] * 0.01
    
    # Create a combined dataset for scatter plot
    df_scatter_plot <- data.frame(
      variable_df1 = df_final[[paste0(variable_name, ".x")]],
      variable_df2 = df_final[[paste0(variable_name, ".y")]]
    )
    
    plot_scatter <- scatter_plot_function(df_scatter_plot, variable_name, unit)
    scatter_plot_list[[i]] = plot_scatter
    
  }
  
  #saving scatter plots
  print_save_scatter_plots_per_page(scatter_plot_list, no_of_plots_per_page, output_dir)
}









#' Comparison Plots for Quality Control (QC)
#' 
#' Generates comparison plots based on specified variables and plot types.
#' (please make sure to install ggplot2, dplyr, gridExtra install in your computer)
#' 
#' @param df1 First data frame (e.g., model 1 predictions).
#' @param df2 Second data frame (e.g., model 2 predictions).
#' @param nri_all_info Data frame containing additional information such as state_name, recordid2017, and year. Ensure these columns are present in the dataset.
#' @param variables A vector of variable names for which plots should be created. To generate lag difference plots, append "_diff" to the variable name (e.g., "somc_diff").
#' @param variable_units A vector of corresponding units (e.g., "(ton/ha)", "(cm)") for each variable. Note: g/m² has already been converted to ton/ha. Include brackets in units.
#' @param plot_types A vector specifying the plot type for each variable ("line", "scatter", or "both").
#' @param no_of_plots_per_page Maximum number of plots to display per page.
#' @param output_dir Directory where output plots will be saved.
#' 
#' @return The function generates and saves line plots as a PDF and scatter plots as image files.
#' 
#' @examples
#' variables <- c("somsc", "n2oflux", "strmac1", "sdrema", "egrain1", "fertot21", "annppt", "accrst")
#' variable_units <- c("(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(ton/ha)", "(cm)", "(ton/ha)")
#' plot_types <- c("line", "scatter", "both", "scatter", "scatter", "scatter", "scatter", "scatter")
#' output_dir <- "C:/temp/DayCent_output_comparison"
#' highest_no_of_plots_per_page <- 6
#' df_491 <- daycent_output_from_491_version_model
#' df_513 <- daycent_output_from_513_version_model
#' create_plots(df1 = df_491, df2 = df_513, nri_all_info = nri_all_info, 
#'              variables = variables, variable_units = variable_units, 
#'              plot_types = plot_types, no_of_plots_per_page = highest_no_of_plots_per_page, 
#'              output_dir = output_dir)
#' 
#' @export

create_QC_comparison_plots <- function(df1, df2, nri_all_info, variables, variable_units, plot_types, 
                         no_of_plots_per_page, output_dir) {
  
  # Initialize lists to hold the variables and units grouped by plot type
  line_vars <- list()
  scatter_vars <- list()
  both_vars <- list()
  
  for (i in 1:length(variables)) {
    variable_name <- variables[i]
    unit <- variable_units[i]
    plot_type <- plot_types[i]
    
    # Check if plot_type is valid (not NA or missing)
    if (is.na(plot_type) || !(plot_type %in% c("line", "scatter", "both"))) {
      warning(paste("Invalid or missing plot type for variable:", variable_name))
      next  # Skip the current iteration if plot_type is invalid
    }
    
    # Group by plot type
    if (plot_type == "line") {
      line_vars <- append(line_vars, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "scatter") {
      scatter_vars <- append(scatter_vars, list(list(variable_name = variable_name, unit = unit)))
    } else if (plot_type == "both") {
      both_vars <- append(both_vars, list(list(variable_name = variable_name, unit = unit)))
    }
  }
  
  
  #combining both variables list with scatter and line plot list
  line_vars_merged <- c(line_vars, both_vars)
  scatter_vars_merged <- c(scatter_vars, both_vars)
  
  # Generate line plots for grouped variables
  if (length(line_vars_merged) > 0) {
    line_variables <- sapply(line_vars_merged, function(x) x$variable_name)
    line_units <- sapply(line_vars_merged, function(x) x$unit)
    create_line_plots_pdf(df1, df2, nri_all_info, line_variables, line_units, 
                          no_of_plots_per_page, output_dir)
  }
  
  # Generate scatter plots for grouped variables
  if (length(scatter_vars_merged) > 0) {
    scatter_variables <- sapply( scatter_vars_merged, function(x) x$variable_name)
    scatter_units <- sapply( scatter_vars_merged, function(x) x$unit)
    create_scatter_plots_image(df1, df2, scatter_variables, scatter_units, 
                               no_of_plots_per_page, output_dir)
  }
  
  
}
