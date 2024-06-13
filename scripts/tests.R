# Counting Dummy Variables: DD and NegE -----------------------------------
# *** surprisingly many firms have negative earnings (NegE), which messes with the coefficients (t-stats) of the regression models *** 
# -> running the regressions without the dummies (also exclude DD)
# 1. Total Count -----------------------------------
data_temp <- data %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(dependent_E) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC)) %>% 
  select(UGVKEY, mapped_fyear, MthCap, BM, dependent_E, A, D, E, AC, DD, NegE, NegEPS, NegEPS_EPS)
  
# Summarize DD, NegE, and total firms by mapped_fyear
summary_dummies_count_total <- data_temp %>%
  group_by(mapped_fyear) %>%
  summarise(
    total_DD = sum(DD, na.rm = TRUE),
    total_NegE = sum(NegE, na.rm = TRUE),
    total_firms = n_distinct(UGVKEY),
    .groups = 'drop'
  )

plot_dummies_count_total <- function(data_temp) {
  ggplot(data_temp, aes(x = mapped_fyear)) +
    geom_bar(aes(y = total_DD, fill = "total_DD"), stat = "identity", position = "dodge", alpha = 0.5) +
    geom_bar(aes(y = total_NegE, fill = "total_NegE"), stat = "identity", position = "dodge", alpha = 0.5) +
    geom_line(aes(y = total_firms, color = "firms_per_category"), alpha = 0.7, linewidth = 0.5) +
    labs(title = "DD and NegE Over Time",
         x = "Year",
         y = "Count",
         fill = "Variable",
         color = "Legend") +  # Update legend title
    scale_color_manual(values = c("firms_per_category" = "black")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Plot the data
plot_dummies_count_total(summary_dummies_count_total)

# saving output
write.csv(summary_dummies_count_total, "results/outliers/summary_dummies_count_total.csv")
ggsave("plots/outliers/plot_dummies_count_total.jpeg", plot = plot_dummies_count_total(summary_dummies_count_total), width = 10, height = 6)

# 2. Grouped  Count -----------------------------------
# Apply the categorization function to the data
data_categorized <- categorize_firms(data) %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(dependent_E) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC)) %>% 
  select(UGVKEY, mapped_fyear, MthCap, BM, dependent_E, A, D, E, AC, DD, NegE, NegEPS, NegEPS_EPS, Size_category, BM_category)

# Summarize DD, NegE, and total firms by mapped_fyear, Size_category, and BM_category
summary_dummies_count_grouped <- data_categorized %>%
  group_by(mapped_fyear, Size_category, BM_category) %>%
  summarise(
    total_DD = sum(DD, na.rm = TRUE),
    total_NegE = sum(NegE, na.rm = TRUE),
    total_firms = n_distinct(UGVKEY),
    .groups = 'drop'
  )

# plotting dummmies by categories
plot_dummies_count_grouped <- function(data) {
  ggplot(data, aes(x = mapped_fyear)) +
    geom_bar(aes(y = total_DD, fill = "total_DD"), stat = "identity", position = "dodge", alpha = 0.5) +
    geom_bar(aes(y = total_NegE, fill = "total_NegE"), stat = "identity", position = "dodge", alpha = 0.5) +
    geom_line(aes(y = total_firms, color = "firms_per_category"), alpha = 0.7, linewidth = 0.5) +  
    facet_grid(Size_category ~ BM_category) +  
    labs(title = "DD and NegE Over Time by Size and BM Category",
         x = "Year",
         y = "Count",
         fill = "Variable",
         color = "Legend") +  # Update legend title
    scale_color_manual(values = c("firms_per_category" = "black")) +  
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Plot the data
plot_dummies_count_grouped(summary_dummies_count_grouped)

# saving output
write.csv(summary_dummies_count_grouped, "results/outliers/summary_dummies_count_grouped.csv")
ggsave("plots/outliers/plot_dummies_count_grouped.jpeg", plot = plot_dummies_count_grouped(summary_dummies_count_grouped), width = 10, height = 6)

# remove redundant objects
rm(data_categorized, data_temp, summary_dummies_count_total, summary_dummies_count_grouped, plot_dummies_count_total, plot_dummies_count_grouped)


# Outliers: non-dummy Variables of HVZ and LM models -----------------------------------
# 3. Outliers (at the 0.5% level) for all companies based on a 10-year rolling window, but plots outliers for each size and BM category seperately *** -----------------------------------
# create a temporary dataset and remove NA values
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, A, D, E, AC, EPS) %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(A) & !is.na(D) & !is.na(E) & !is.na(AC) & !is.na(EPS))

# function: identifies overall outliers within each 10 year window for each year, but plots outliers for each size and BM category seperately
plot_overall_outliers <- function(data, variable, window = 10, save_path = "plots/outliers/") {
  data <- data %>%
    arrange(mapped_fyear) %>%
    mutate(group = floor((mapped_fyear - 1963) / window)) 
  
  data <- categorize_firms(data)
  data <- identify_overall_outliers(data, variable)
  
  # Plot
  plot <- ggplot(data, aes(x = mapped_fyear, y = .data[[variable]], color = outlier)) +
    geom_point() +
    facet_wrap(~Size_category + BM_category, scales = "free") +
    labs(title = paste("Outliers for", variable, "in 10-Year Rolling Windows by Size and BM Categories"),
         x = "Year",
         y = variable) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"), labels = c("Normal", "Outlier"))
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(save_path, "outliers_overall_", variable, ".jpeg"), plot = plot, width = 10, height = 6)
}

# Variables to plot
variables_to_plot <- c("A", "E", "D", "AC", "EPS")

# Loop through each variable and plot
for (variable in variables_to_plot) {
  plot_overall_outliers(data_temp, variable, window = 10)
}



# 4. Outliers within each Market Cap (size) and BM category based on a 10-year rolling window and plots these outliers for each category  *** -----------------------------------
# Function to plot outliers within 10-year rolling windows
plot_grouped_outliers <- function(data, variable, window = 10, save_path = "plots/outliers/") {
  data <- data %>%
    arrange(mapped_fyear) %>%
    mutate(group = floor((mapped_fyear - 1963) / window)) 
  
  data <- categorize_firms(data)
  data <- identify_grouped_outliers(data, variable)
  
  plot <- ggplot(data, aes(x = mapped_fyear, y = .data[[variable]], color = outlier)) +
    geom_point() +
    facet_wrap(~Size_category + BM_category, scales = "free") +
    labs(title = paste("Outliers for", variable, "in 10-Year Rolling Windows by Size and BM Categories"),
         x = "Year",
         y = variable) +
    theme_minimal() +
    scale_color_manual(values = c("black", "red"), labels = c("Normal", "Outlier"))
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(save_path, "grouped_outliers_", variable, ".jpeg"), plot = plot, width = 10, height = 6)
}

# Loop through each variable and plot
for (variable in variables_to_plot) {
  plot_grouped_outliers(data_temp, variable, window = 10)
}

# remove redundant objects
rm(data_temp, variable, variables_to_plot, identify_overall_outliers, identify_grouped_outliers, plot_overall_outliers, plot_grouped_outliers)
  














