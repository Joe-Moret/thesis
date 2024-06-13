# Load necessary libraries
library(dplyr)
library(broom)
library(purrr)

# 4. All Variables & winsorized: grouped by Market Cap and Book-to-Market -----------------------------------
# Create a temporary dataset
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, dependent_E, A, D, DD, E, NegE, AC, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(dependent_E) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC))

# Create a 1-year ahead lagged dependent variable
data_temp <- data_temp %>%
  group_by(UGVKEY) %>%
  arrange(mapped_fyear) %>%
  mutate(dependent_E_t1 = lead(dependent_E, 1)) %>%
  ungroup()

# Categorize firms into Book-to-Market and size groups
data_temp <- data_temp %>%
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

run_rolling_regression <- function(data_temp) {
  results <- list()
  
  for (year in 1968:2023) {
    for (size_group in unique(data_temp$Size_category)) {
      for (bm_group in unique(data_temp$BM_category)) {
        # Subset the data for the specific group and rolling window
        data_subset <- data_temp %>%
          filter(mapped_fyear >= (year - 10) & mapped_fyear < year) %>%
          filter(Size_category == size_group & BM_category == bm_group)
        
        # Winsorize
        data_subset <- winsorize_regression(data_subset, c("A", "D", "E", "AC"))
        
        if (nrow(data_subset) > 0) {
          
          # Define the dependent variable based on the forecast horizon
          dependent_var <- "dependent_E_t1"
          
          # Estimate the regression coefficients using data from the past 10 years
          model <- try(lm(as.formula(paste(dependent_var, "~ A + D + DD + E + NegE + AC")), data = data_subset), silent = TRUE)
          
          if (!inherits(model, "try-error")) {
            coefficients <- tidy(model)
            adj_r_squared <- glance(model)$adj.r.squared
            coefficients$year <- year
            coefficients$Size_category <- size_group
            coefficients$BM_category <- bm_group
            coefficients$adj_r_squared <- adj_r_squared
            
            results[[paste0(year, "_", size_group, "_", bm_group)]] <- coefficients
          }
        }
      }
    }
  }
  bind_rows(results)
}

# Run the rolling regression for the 1-year ahead forecast
results <- run_rolling_regression(data_temp)

# Calculate average R-squared, intercept, coefficients and t-statistics for each group
summary_results <- results %>%
  group_by(Size_category, BM_category, term) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    std.error = mean(std.error, na.rm = TRUE),
    statistic = mean(statistic, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic))

# Format the summary results for table output
summary_table <- summary_results %>%
  mutate(LHS = "E_{t+1}") %>%
  arrange(factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")), 
          factor(BM_category, levels = c("Low BM", "Med BM", "High BM")))

# Display the table
print(summary_table)

# Save the summary table to a CSV file 
write.csv(summary_table, file = "results/earnings_forecast_HVZ_summary_all_Variables_winsorized_grouped.csv", row.names = FALSE)
write.csv(results, file = "results/earnings_forecast_HVZ_all_years_all_Variables_winsorized_grouped.csv", row.names = FALSE)

# Remove redundant objects
rm(data_temp, results, summary_results, run_rolling_regression, summary_table)
