# Counting Dummy Variables: DD and NegE  -----------------------------------
# 1. Total Count -----------------------------------
# create temporary dataset
data_temp <- data %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(AC) & !is.na(NegE) & !is.na(NegEPS) & !is.na(NegEPS_EPS)) %>% 
  select(UGVKEY, mapped_fyear, MthCap, BM, A, D, E, AC, DD, NegE, NegEPS, NegEPS_EPS)
  
# summarize DD, NegE, and total firms by mapped_fyear
count_dummies_total <- data_temp %>%
  group_by(mapped_fyear) %>%
  summarise(
    total_firms = n_distinct(UGVKEY),
    total_DD = sum(DD),
    total_NegE = sum(NegE),
    percent_DD = (total_DD / total_firms) * 100,
    percent_NegE = (total_NegE / total_firms) * 100,
    .groups = 'drop'
  )

# plotting the total count of DD and NegE
plot_count_dummies_total <- function(data_temp) {
  ggplot(data_temp, aes(x = mapped_fyear)) +
    geom_bar(aes(y = total_DD, fill = "Dividends paid"), stat = "identity", position = "dodge", alpha = 0.6) +
    geom_bar(aes(y = total_NegE, fill = "Negative Earnings"), stat = "identity", position = "dodge", alpha = 0.6) +
    geom_line(aes(y = total_firms, color = "Number of Companies"), alpha = 0.7, linewidth = 0.5) +
    labs(title = "Dummy Variables throughout the Sample Period",
         x = "Year",
         y = "Count",
         fill = "Variable",
         color = "Legend") +  # Update legend title
    scale_fill_manual(name = "", values = c("Dividends paid" = "blue", "Negative Earnings" = "red")) +
    scale_color_manual(name = "", values = c("Number of Companies" = "black")) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
}

# plot the data
plot_count_dummies_total(count_dummies_total)

# saving output in the directory
write.csv(count_dummies_total, "results/count_dummies_total.csv")
ggsave("plots/plot_count_dummies_total.jpeg", plot = plot_count_dummies_total(count_dummies_total), width = 10, height = 6)

# 2. Grouped Count -----------------------------------
# grouped by Market Cap and BM categories (30th and 70th percentiles)
# create dataset
data_categorized <- categorize_firms(data) %>% 
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(AC) & !is.na(NegE) & !is.na(NegEPS) & !is.na(NegEPS_EPS)) %>% 
  select(UGVKEY, mapped_fyear, MthCap, BM, A, D, E, AC, DD, NegE, NegEPS, NegEPS_EPS, Size_category, BM_category)

# creating table to count dummy variables
count_dummies_grouped <- data_categorized %>%
  group_by(mapped_fyear, Size_category, BM_category) %>%
  summarise(
    total_firms = n_distinct(UGVKEY),
    total_DD = round(sum(DD), 0),
    total_NegE = round(sum(NegE), 0),
    percent_DD = round((total_DD / total_firms) * 100, 0),
    percent_NegE = round((total_NegE / total_firms) * 100, 0),
    .groups = 'drop'
  )

# plotting dummy variables grouped by Market Cap and BM categories
plot_count_dummies_grouped <- function(data) {
  ggplot(data, aes(x = mapped_fyear)) +
    geom_bar(aes(y = total_DD, fill = "Dividends paid"), stat = "identity", position = "dodge", alpha = 0.6) +
    geom_bar(aes(y = total_NegE, fill = "Negative Earnings"), stat = "identity", position = "dodge", alpha = 0.6) +
    geom_line(aes(y = total_firms, color = "Number of Companies"), alpha = 0.7, linewidth = 0.5) +  
    facet_grid(Size_category ~ BM_category) +  
    labs(title = "Dummy Variables categorized by Market Cap and BM",
         x = "Year",
         y = "Count",
         fill = "Variable",
         color = "Legend") +  
    scale_fill_manual(name = "", values = c("Dividends paid" = "blue", "Negative Earnings" = "red")) +
    scale_color_manual(name = "", values = c("Number of Companies" = "black")) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
}

# Plot the data
plot_count_dummies_grouped(count_dummies_grouped)

# saving output
write.csv(count_dummies_grouped, "results/count_dummies_grouped.csv")
ggsave("plots/plot_count_dummies_grouped.jpeg", plot = plot_count_dummies_grouped(count_dummies_grouped), width = 10, height = 6)

# remove redundant objects
rm(data_categorized, data_temp, count_dummies_total, count_dummies_grouped, plot_count_dummies_total, plot_count_dummies_grouped)


# Outliers (at the 0.5% winsorization level): non-dummy Variables of HVZ and LM models -----------------------------------
# 1. Outliers for all companies based on a 10-year rolling window, but plots outliers for each size and BM category seperately *** -----------------------------------
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



# 2. Grouped Outliers within each Market Cap (size) and BM category based on a 10-year rolling window and plots these outliers for each category  *** -----------------------------------
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
  


# Counting NA (missing) values for variables (A, D, E, AC) in the HVZ regression -----------------------------------
# Create a function to count NAs by year for a given column and count distinct UGVKEY
count_na_and_firms_by_year <- function(data, column) {
  data %>%
    group_by(mapped_fyear) %>%
    summarize(
      NA_count = sum(is.na(.data[[column]])),
      total_firms = n_distinct(UGVKEY)
    )
}

# Apply the function to each column of interest
na_counts <- list(
  A = count_na_and_firms_by_year(data, "A"),
  D = count_na_and_firms_by_year(data, "D"),
  E = count_na_and_firms_by_year(data, "E"),
  AC = count_na_and_firms_by_year(data, "AC")
)

# Combine all counts into one data frame for easier viewing
na_counts_combined <- Reduce(function(x, y) {
  merge(x, y, by = c("mapped_fyear", "total_firms"), all = TRUE)
}, na_counts)

# Rename columns for clarity
colnames(na_counts_combined) <- c("mapped_fyear", "total_firms", "A_NA", "D_NA", "E_NA", "AC_NA")

# View the combined results
print(na_counts_combined)

# save output
write.csv(na_counts_combined, "results/counts_NA_variables.csv")

# remove redundant objects
rm(count_na_and_firms_by_year, na_counts, na_counts_combined)



# Interest Rates -----------------------------------
interest_rate_plot <- data %>%
  ggplot(aes(x = mapped_fyear)) +
  geom_line(aes(y = `rate_1y`, color = "1y T-Bill")) +
  geom_line(aes(y = `rate_10y`, color = "10y T-Bill")) +
  labs(title = "Interest Rates throughout Sample Period",
       x = "Year",
       y = "Interest Rate (%)") +
  scale_color_manual(name = "", values = c("1y T-Bill" = "blue", "10y T-Bill" = "red")) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

print(interest_rate_plot)
# Save and remove the plot
ggsave("plots/interest_rates.jpeg", plot = interest_rate_plot, width = 10, height = 6)
rm(interest_rate_plot)






