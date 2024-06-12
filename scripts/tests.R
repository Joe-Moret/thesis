# Outliers: analyzing input Variables of HVZ and LM models -----------------------------------
# *** Code identifies and plots outliers for each size and BM category seperately based on a 10-year rolling window ***

# Create a temporary dataset and remove NA values
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, A, D, E, AC, EPS) %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(A) & !is.na(D) & !is.na(E) & !is.na(AC) & !is.na(EPS))

# Function to identify outliers (at the 0.5% level) within Market Cap and BM categories
identify_outliers <- function(data, variable, lower_limit = 0.005, upper_limit = 0.995) {
  data %>%
    group_by(group, Size_category, BM_category) %>%
    mutate(
      lower_bound = quantile(.data[[variable]], probs = lower_limit, na.rm = TRUE),
      upper_bound = quantile(.data[[variable]], probs = upper_limit, na.rm = TRUE),
      outlier = ifelse(.data[[variable]] < lower_bound | .data[[variable]] > upper_bound, TRUE, FALSE)
    ) %>%
    ungroup()
}

# Function to categorize firms into BM and Size categories
categorize_firms <- function(data) {
  data %>%
    mutate(
      BM_category = case_when(
        BM <= quantile(BM, 0.30, na.rm = TRUE) ~ "Low BM",
        BM <= quantile(BM, 0.70, na.rm  = TRUE) ~ "Med BM",
        TRUE ~ "High BM"
      ),
      Size_category = case_when(
        MthCap <= quantile(MthCap, 0.30, na.rm = TRUE) ~ "Small cap",
        MthCap <= quantile(MthCap, 0.70, na.rm = TRUE) ~ "Medium cap",
        TRUE ~ "Large cap"
      )
    )
}

# Function to plot outliers within 10-year rolling windows
plot_outliers <- function(data, variable, window = 10, save_path = "plots/") {
  data <- data %>%
    arrange(mapped_fyear) %>%
    mutate(group = floor((mapped_fyear - 1963) / window)) # Adjusting the start year to 1963
  
  data <- categorize_firms(data)
  data <- identify_outliers(data, variable)
  
  plot <- ggplot(data, aes(x = mapped_fyear, y = .data[[variable]], color = outlier)) +
    geom_point() +
    facet_wrap(~Size_category + BM_category) +
    labs(title = paste("Outliers for", variable, "in 10-Year Rolling Windows by Size and BM Categories"),
         x = "Year",
         y = variable) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"), labels = c("Normal", "Outlier"))
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(save_path, "outliers_", variable, ".jpeg"), plot = plot, width = 10, height = 6)
}

# Variables to plot
variables_to_plot <- c("A", "E", "D", "AC", "EPS")

# Loop through each variable and plot
for (variable in variables_to_plot) {
  plot_outliers(data_temp, variable, window = 10)
}
