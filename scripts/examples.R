# Subset the data
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, EPS, dependent_EPS, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(EPS) & !is.na(dependent_EPS))

# Create lagged dependent variables for forecast horizons 1 to 5 years
for (k in 1:5) {
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>%
    arrange(mapped_fyear) %>%
    mutate(!!paste0("dependent_EPS_t", k) := lead(dependent_EPS, k)) %>%
    ungroup()
}

# Function to run rolling regression with Newey-West standard errors for each forecast horizon
run_rolling_regression <- function(data_temp, forecast_horizon) {
  results <- list()
  
  for (year in 1968:2023) {
    # Subset the data for the rolling window regression
    data_subset <- data_temp %>%
      filter(mapped_fyear >= (year - 10) & mapped_fyear < year)
    
    # Winsorize the relevant variables
    data_subset <- winsorize_regression(data_subset, c("EPS"))
    
    if (nrow(data_subset) > 0) {
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_EPS_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ EPS")), data = data_subset)
      nw_se <- sqrt(diag(NeweyWest(model, lag = 1)))  
      coefficients <- coef(model)
      t_stats <- coefficients / nw_se
      adj_r_squared <- summary(model)$adj.r.squared
      
      # Store the results
      results[[paste0(year, "_t", forecast_horizon)]] <- data.frame(
        year = year,
        forecast_horizon = forecast_horizon,
        intercept = coefficients[1],
        EPS = coefficients[2],
        t_intercept = t_stats[1],
        t_EPS = t_stats[2],
        adj_r_squared = adj_r_squared
      )
    }
  }
  do.call(rbind, results)
}

# List to store results for all forecast horizons
all_results <- list()

# Run the rolling regression for forecast horizons 1 to 5
for (k in 1:5) {
  all_results[[paste0("t", k)]] <- run_rolling_regression(data_temp, k)
}

# Combine results into a single dataframe
final_results <- bind_rows(all_results)

# Calculate average R-squared, intercept, coefficients, and t-statistics for each forecast horizon
summary_results <- final_results %>%
  group_by(forecast_horizon) %>%
  summarise(
    intercept = mean(intercept, na.rm = TRUE),
    EPS = mean(EPS, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_EPS = mean(t_EPS, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output without italics for t-statistics
summary_table <- summary_results %>%
  mutate(LHS = paste0("E_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, EPS, adj_r_squared, t_intercept, t_EPS) %>%
  arrange(forecast_horizon)

# Display the table
print(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, file = "results/01_table/earnings_forecast_LM_summary_EPS_only_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_LM_all_years_EPS_only_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)
