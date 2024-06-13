library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

# Create a temporary dataset and remove NA values
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, A, D, E, AC, EPS, DD, NegE, NegEPS, NegEPS_EPS) %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(A) & !is.na(D) & !is.na(E) & !is.na(AC) & !is.na(EPS) & !is.na(DD) & !is.na(NegE) & !is.na(NegEPS) & !is.na(NegEPS_EPS))

# Function to categorize firms by Market Cap and BM sizes
categorize_firms <- function(data) {
  data %>%
    mutate(
      BM_category = case_when(
        BM <= quantile(BM, 0.30, na.rm = TRUE) ~ "Low BM",
        BM <= quantile(BM, 0.70, na.rm = TRUE) ~ "Med BM",
        TRUE ~ "High BM"
      ),
      Size_category = case_when(
        MthCap <= quantile(MthCap, 0.30, na.rm = TRUE) ~ "Small cap",
        MthCap <= quantile(MthCap, 0.70, na.rm = TRUE) ~ "Medium cap",
        TRUE ~ "Large cap"
      )
    )
}

# Function to identify overall outliers
identify_overall_outliers <- function(data, variable, lower_limit = 0.005, upper_limit = 0.995) {
  data %>%
    mutate(
      lower_bound = quantile(.data[[variable]], probs = lower_limit, na.rm = TRUE),
      upper_bound = quantile(.data[[variable]], probs = upper_limit, na.rm = TRUE),
      outlier = ifelse(.data[[variable]] < lower_bound | .data[[variable]] > upper_bound, TRUE, FALSE)
    )
}

# Function to plot overall outliers and create a summary table
plot_overall_outliers <- function(data, variable, window = 10, save_path = "results/") {
  data <- data %>%
    arrange(mapped_fyear) %>%
    mutate(group = floor((mapped_fyear - 1963) / window)) 
  
  data <- categorize_firms(data)
  data <- identify_overall_outliers(data, variable)
  
  # Create a summary table for outliers
  outlier_summary <- data %>%
    filter(outlier == TRUE) %>%
    group_by(mapped_fyear, Size_category, BM_category) %>%
    summarise(outlier_count = n(), .groups = 'drop')
  
  # Save the summary table as a CSV file
  write_csv(outlier_summary, file = paste0(save_path, "outlier_summary_", variable, ".csv"))
  
  # Plot with adjusted scale to better visualize small cap outliers
  plot <- ggplot(data, aes(x = mapped_fyear, y = .data[[variable]], color = outlier)) +
    geom_point(size = 1) +
    facet_wrap(~Size_category + BM_category, scales = "free_y") +
    labs(title = paste("Outliers for", variable, "in 10-Year Rolling Windows by Size and BM Categories"),
         x = "Year",
         y = variable) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"), labels = c("Normal", "Outlier")) 
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(save_path, "overall_outliers_", variable, ".jpeg"), plot = plot, width = 10, height = 6)
}

# Variables to plot
variables_to_plot <- c("A", "E", "D", "AC", "EPS")

# Loop through each variable and plot
for (variable in variables_to_plot) {
  plot_overall_outliers(data_temp, variable, window = 10, save_path = "results/")
}
