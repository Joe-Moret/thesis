# Average cross-sectional earnings forecast regression summary statistics (Table 1)
# Winsorizing: 0.5% level for each Size and BM category for each 10 year window
# Panel A: HVZ: summary statistics -----------------------------------
# winsorize (at the 1% level) 
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, A, E, dependent_E, dependent_EPS, NegE, D, DD, AC, EPS, NegEPS, NegEPS_EPS) %>% 
  winsorize(c("A", "E", "D", "AC", "EPS"))

# returns summary statistics for the HVZ model (A, E, NegE, D, DD, AC)
summary_panel_HVZ <- bind_rows(
  tibble(Period = "1963-2009") %>% bind_cols(
    data_temp %>%
      filter(mapped_fyear >= 1963 & mapped_fyear <= 2009) %>%
      summarise_stats(c("A", "E", "NegE", "D", "DD", "AC"))
  ),
  tibble(Period = "1963-2023") %>% bind_cols(
    data_temp %>%
      filter(mapped_fyear >= 1963 & mapped_fyear <= 2023) %>%
      summarise_stats(c("A", "E", "NegE", "D", "DD", "AC"))
  )
)

# returns summary statistics for the LM model (EPS, NegEPS, NegEPS_EPS)
summary_panel_LM <- bind_rows(
  tibble(Period = "1963-2009") %>% bind_cols(
    data_temp %>%
      filter(mapped_fyear >= 1963 & mapped_fyear <= 2009) %>%
      summarise_stats(c("EPS", "NegEPS", "NegEPS_EPS"))
  ),
  tibble(Period = "1963-2023") %>% bind_cols(
    data_temp %>%
      filter(mapped_fyear >= 1963 & mapped_fyear <= 2023) %>%
      summarise_stats(c("EPS", "NegEPS", "NegEPS_EPS"))
  )
)

# Display the tables
print(summary_panel_HVZ)
print(summary_panel_LM)
 
# Save the summary table to a CSV file
write.csv(summary_panel_HVZ, file = "results/01_table/stats_summary_HVZ.csv", row.names = FALSE)
write.csv(summary_panel_LM, file = "results/01_table/stats_summary_LM.csv", row.names = FALSE)

# remove redundant objects
rm(winsorize, data_temp, summarise_stats, summary_panel_HVZ, summary_panel_LM)


# Panel B: HVZ: Earnings Forecast -----------------------------------
# 1. All Variables & not winsorized -----------------------------------
# Create a temporary dataset
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, A, D, DD, E, NegE, AC, dependent_E)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC) & !is.na(dependent_E))

# Create lagged dependent variables for forecast horizons 1 to 5 years
for (k in 1:5) {
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>% 
    arrange(mapped_fyear) %>%
    mutate(!!paste0("dependent_E_t", k) := lead(dependent_E, k)) %>%
    ungroup()
}

# Function to run rolling regression with Newey-West standard errors for each forecast horizon
run_rolling_regression <- function(data_temp, forecast_horizon) {
  results <- list()
  
  for (year in 1968:2023) {
    # Subset the data for the rolling window regression
    data_subset <- data_temp %>%
      filter(mapped_fyear >= (year - 10) & mapped_fyear < year)
    
    if (nrow(data_subset) > 0) {
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_E_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ A + D + DD + E + NegE + AC")), data = data_subset)
      nw_se <- sqrt(diag(NeweyWest(model, lag = 1)))  
      coefficients <- coef(model)
      t_stats <- coefficients / nw_se
      adj_r_squared <- summary(model)$adj.r.squared
      
      # Store the results
      results[[paste0(year, "_t", forecast_horizon)]] <- data.frame(
        year = year,
        forecast_horizon = forecast_horizon,
        intercept = coefficients[1],
        A = coefficients[2],
        D = coefficients[3],
        DD = coefficients[4],
        E = coefficients[5],
        NegE = coefficients[6],
        AC = coefficients[7],
        t_intercept = t_stats[1],
        t_A = t_stats[2],
        t_D = t_stats[3],
        t_DD = t_stats[4],
        t_E = t_stats[5],
        t_NegE = t_stats[6],
        t_AC = t_stats[7],
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
    A = mean(A, na.rm = TRUE),
    D = mean(D, na.rm = TRUE),
    DD = mean(DD, na.rm = TRUE),
    E = mean(E, na.rm = TRUE),
    NegE = mean(NegE, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_A = mean(t_A, na.rm = TRUE),
    t_D = mean(t_D, na.rm = TRUE),
    t_DD = mean(t_DD, na.rm = TRUE),
    t_E = mean(t_E, na.rm = TRUE),
    t_NegE = mean(t_NegE, na.rm = TRUE),
    t_AC = mean(t_AC, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output without italics for t-statistics
summary_table <- summary_results %>%
  mutate(LHS = paste0("E_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, A, D, DD, E, NegE, AC, adj_r_squared, t_intercept, t_A, t_D, t_DD, t_E, t_NegE, t_AC) %>%
  arrange(forecast_horizon)

# Step 11: Display the table
print(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, file = "results/01_table/earnings_forecast_HVZ_summary_all_Variables_not_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_HVZ_all_years_all_Variables_not_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)






# 2. All Variables & winsorized -----------------------------------
# Subset the data
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, A, D, DD, E, NegE, AC, dependent_E, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC) & !is.na(dependent_E))

# Create lagged dependent variables for forecast horizons 1 to 5 years
for (k in 1:5) {
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>%
    arrange(mapped_fyear) %>%
    mutate(!!paste0("dependent_E_t", k) := lead(dependent_E, k)) %>%
    ungroup()
}

# Function to run rolling regression with Newey-West standard errors for each forecast horizon
run_rolling_regression <- function(data_temp, forecast_horizon) {
  results <- list()
  
  for (year in 1968:2023) {
    # Subset the data for the rolling window regression
    data_subset <- data_temp %>%
      filter(mapped_fyear >= (year - 10) & mapped_fyear < year)
    
    # Winsorize the relevant variables, including dummy and interaction terms
    data_subset <- winsorize_regression(data_subset, c("A", "D", "E", "AC"))
    
    if (nrow(data_subset) > 0) {
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_E_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ A + D + DD + E + NegE + AC")), data = data_subset)
      nw_se <- sqrt(diag(NeweyWest(model, lag = 1)))  
      coefficients <- coef(model)
      t_stats <- coefficients / nw_se
      adj_r_squared <- summary(model)$adj.r.squared
      
      # Store the results
      results[[paste0(year, "_t", forecast_horizon)]] <- data.frame(
        year = year,
        forecast_horizon = forecast_horizon,
        intercept = coefficients[1],
        A = coefficients[2],
        D = coefficients[3],
        DD = coefficients[4],
        E = coefficients[5],
        NegE = coefficients[6],
        AC = coefficients[7],
        t_intercept = t_stats[1],
        t_A = t_stats[2],
        t_D = t_stats[3],
        t_DD = t_stats[4],
        t_E = t_stats[5],
        t_NegE = t_stats[6],
        t_AC = t_stats[7],
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
    A = mean(A, na.rm = TRUE),
    D = mean(D, na.rm = TRUE),
    DD = mean(DD, na.rm = TRUE),
    E = mean(E, na.rm = TRUE),
    NegE = mean(NegE, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_A = mean(t_A, na.rm = TRUE),
    t_D = mean(t_D, na.rm = TRUE),
    t_DD = mean(t_DD, na.rm = TRUE),
    t_E = mean(t_E, na.rm = TRUE),
    t_NegE = mean(t_NegE, na.rm = TRUE),
    t_AC = mean(t_AC, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output without italics for t-statistics
summary_table <- summary_results %>%
  mutate(LHS = paste0("E_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, A, D, DD, E, NegE, AC, adj_r_squared, t_intercept, t_A, t_D, t_DD, t_E, t_NegE, t_AC) %>%
  arrange(forecast_horizon)

# Display the table
print(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, file = "results/01_table/earnings_forecast_HVZ_summary_all_Variables_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_HVZ_all_years_all_Variables_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)


# 3. Without Dummy Variables & winsorized -----------------------------------
# Subset the data
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, A, D, E, AC, dependent_E, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(A) & !is.na(D) & !is.na(E) & !is.na(AC) & !is.na(dependent_E))

# Create lagged dependent variables for forecast horizons 1 to 5 years
for (k in 1:5) {
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>%
    arrange(mapped_fyear) %>%
    mutate(!!paste0("dependent_E_t", k) := lead(dependent_E, k)) %>%
    ungroup()
}

# Function to run rolling regression with Newey-West standard errors for each forecast horizon
run_rolling_regression <- function(data_temp, forecast_horizon) {
  results <- list()
  
  for (year in 1968:2023) {
    # Subset the data for the rolling window regression
    data_subset <- data_temp %>%
      filter(mapped_fyear >= (year - 10) & mapped_fyear < year)
    
    # Winsorize the relevant variables, including dummy and interaction terms
    data_subset <- winsorize_regression(data_subset, c("A", "D", "E", "AC"))
    
    if (nrow(data_subset) > 0) {
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_E_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ A + D + E + AC")), data = data_subset)
      nw_se <- sqrt(diag(NeweyWest(model, lag = 1)))  
      coefficients <- coef(model)
      t_stats <- coefficients / nw_se
      adj_r_squared <- summary(model)$adj.r.squared
      
      # Store the results
      results[[paste0(year, "_t", forecast_horizon)]] <- data.frame(
        year = year,
        forecast_horizon = forecast_horizon,
        intercept = coefficients[1],
        A = coefficients[2],
        D = coefficients[3],
        E = coefficients[4],
        AC = coefficients[5],
        t_intercept = t_stats[1],
        t_A = t_stats[2],
        t_D = t_stats[3],
        t_E = t_stats[4],
        t_AC = t_stats[5],
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
    A = mean(A, na.rm = TRUE),
    D = mean(D, na.rm = TRUE),
    E = mean(E, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_A = mean(t_A, na.rm = TRUE),
    t_D = mean(t_D, na.rm = TRUE),
    t_E = mean(t_E, na.rm = TRUE),
    t_AC = mean(t_AC, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output without italics for t-statistics
summary_table <- summary_results %>%
  mutate(LHS = paste0("E_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, A, D, E, AC, adj_r_squared, t_intercept, t_A, t_D, t_E, t_AC) %>%
  arrange(forecast_horizon)

# Display the table
print(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, file = "results/01_table/earnings_forecast_HVZ_summary_without_Dummies_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_HVZ_all_years_without_Dummies_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)






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
          model <- lm(as.formula(paste(dependent_var, "~ A + D + DD + E + NegE + AC")), data = data_subset)
          coefficients <- coef(model)
          t_stats <- summary(model)$coefficients[, "t value"]
          adj_r_squared <- summary(model)$adj.r.squared
          
          # Store the results
          results[[paste0(year, "_", size_group, "_", bm_group)]] <- data.frame(
            year = year,
            Size_category = size_group,
            BM_category = bm_group,
            intercept = coefficients[1],
            A = coefficients[2],
            D = coefficients[3],
            DD = coefficients[4],
            E = coefficients[5],
            NegE = coefficients[6],
            AC = coefficients[7],
            t_intercept = t_stats[1],
            t_A = t_stats[2],
            t_D = t_stats[3],
            t_DD = t_stats[4],
            t_E = t_stats[5],
            t_NegE = t_stats[6],
            t_AC = t_stats[7],
            adj_r_squared = adj_r_squared
          )
        }
      }
    }
  }
  do.call(rbind, results)
}

# Run the rolling regression for the 1-year ahead forecast
results <- run_rolling_regression(data_temp)

# Calculate average R-squared, intercept, coefficients and t-statistics for each group
summary_results <- results %>%
  group_by(Size_category, BM_category) %>%
  summarise(
    intercept = mean(intercept, na.rm = TRUE),
    A = mean(A, na.rm = TRUE),
    D = mean(D, na.rm = TRUE),
    DD = mean(DD, na.rm = TRUE),
    E = mean(E, na.rm = TRUE),
    NegE = mean(NegE, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_A = mean(t_A, na.rm = TRUE),
    t_D = mean(t_D, na.rm = TRUE),
    t_DD = mean(t_DD, na.rm = TRUE),
    t_E = mean(t_E, na.rm = TRUE),
    t_NegE = mean(t_NegE, na.rm = TRUE),
    t_AC = mean(t_AC, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output
summary_table <- summary_results %>%
  mutate(LHS = "E_{t+1}") %>%
  select(Size_category, BM_category, LHS, intercept, A, D, DD, E, NegE, AC, adj_r_squared, t_intercept, t_A, t_D, t_DD, t_E, t_NegE, t_AC) %>%
  arrange(factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")), 
          factor(BM_category, levels = c("Low BM", "Med BM", "High BM"))) 

# Display the table
print(summary_table)

# Save the summary table to a CSV file 
write.csv(summary_table, file = "results/earnings_forecast_HVZ_summary_all_Variables_winsorized_grouped.csv", row.names = FALSE)
write.csv(results, file = "results/earnings_forecast_HVZ_all_years_all_Variables_winsorized_grouped.csv", row.names = FALSE)

# Remove redundant objects
rm(data_temp, results, summary_results, run_rolling_regression, summary_table)








# 5. All Variables & winsorized: grouped by Market Cap and Book-to-Market (Robustness Test: market scaling) -----------------------------------
# Create a temporary dataset
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, dependent_E, A, D, DD, E, NegE, AC, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(dependent_E) & !is.na(A) & !is.na(D) & !is.na(DD) & !is.na(E) & !is.na(NegE) & !is.na(AC))

# Scale the specified variables by MthCap
data_temp <- data_temp %>%
  mutate(
    dependent_E = dependent_E / MthCap,
    E = E / MthCap,
    A = A / MthCap,
    D = D / MthCap,
    AC = AC / MthCap
  )

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
          model <- lm(as.formula(paste(dependent_var, "~ A + D + DD + E + NegE + AC")), data = data_subset)
          coefficients <- coef(model)
          t_stats <- summary(model)$coefficients[, "t value"]
          adj_r_squared <- summary(model)$adj.r.squared
          
          # Store the results
          results[[paste0(year, "_", size_group, "_", bm_group)]] <- data.frame(
            year = year,
            Size_category = size_group,
            BM_category = bm_group,
            intercept = coefficients[1],
            A = coefficients[2],
            D = coefficients[3],
            DD = coefficients[4],
            E = coefficients[5],
            NegE = coefficients[6],
            AC = coefficients[7],
            t_intercept = t_stats[1],
            t_A = t_stats[2],
            t_D = t_stats[3],
            t_DD = t_stats[4],
            t_E = t_stats[5],
            t_NegE = t_stats[6],
            t_AC = t_stats[7],
            adj_r_squared = adj_r_squared
          )
        }
      }
    }
  }
  do.call(rbind, results)
}

# Run the rolling regression for the 1-year ahead forecast
results <- run_rolling_regression(data_temp)

# Calculate average R-squared, intercept, coefficients and t-statistics for each group
summary_results <- results %>%
  group_by(Size_category, BM_category) %>%
  summarise(
    intercept = mean(intercept, na.rm = TRUE),
    A = mean(A, na.rm = TRUE),
    D = mean(D, na.rm = TRUE),
    DD = mean(DD, na.rm = TRUE),
    E = mean(E, na.rm = TRUE),
    NegE = mean(NegE, na.rm = TRUE),
    AC = mean(AC, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_A = mean(t_A, na.rm = TRUE),
    t_D = mean(t_D, na.rm = TRUE),
    t_DD = mean(t_DD, na.rm = TRUE),
    t_E = mean(t_E, na.rm = TRUE),
    t_NegE = mean(t_NegE, na.rm = TRUE),
    t_AC = mean(t_AC, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output
summary_table <- summary_results %>%
  mutate(LHS = "E_{t+1}") %>%
  select(Size_category, BM_category, LHS, intercept, A, D, DD, E, NegE, AC, adj_r_squared, t_intercept, t_A, t_D, t_DD, t_E, t_NegE, t_AC) %>%
  arrange(factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")), 
          factor(BM_category, levels = c("Low BM", "Med BM", "High BM"))) 

# Display the table
print(summary_table)

# Save the summary table to a CSV file 
write.csv(summary_table, file = "results/earnings_forecast_HVZ_summary_grouped_E.csv", row.names = FALSE)
write.csv(results, file = "results/earnings_forecast_HVZ_all_years_grouped_E.csv", row.names = FALSE)

# Remove redundant objects
rm(data_temp, results, summary_results, run_rolling_regression, summary_table)




# Panel C: LM: regression results -----------------------------------
# 1. All Variables & winsorized -----------------------------------
# Subset the data
data_temp <- categorize_firms(data) %>%
  select(UGVKEY, mapped_fyear, EPS, NegEPS, NegEPS_EPS, dependent_EPS, Size_category, BM_category)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(EPS) & !is.na(NegEPS) & !is.na(NegEPS_EPS) & !is.na(dependent_EPS))

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
    
    # Winsorize the relevant variables, including dummy and interaction terms
    data_subset <- winsorize_regression(data_subset, c("EPS"))
    
    if (nrow(data_subset) > 0) {
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_EPS_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ EPS + NegEPS + NegEPS_EPS")), data = data_subset)
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
        NegEPS = coefficients[3],
        NegEPS_EPS = coefficients[4],
        t_intercept = t_stats[1],
        t_EPS = t_stats[2],
        t_NegEPS = t_stats[3],
        t_NegEPS_EPS = t_stats[4],
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
    NegEPS = mean(NegEPS, na.rm = TRUE),
    NegEPS_EPS = mean(NegEPS_EPS, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_EPS = mean(t_EPS, na.rm = TRUE),
    t_NegEPS = mean(t_NegEPS, na.rm = TRUE),
    t_NegEPS_EPS = mean(t_NegEPS_EPS, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output without italics for t-statistics
summary_table <- summary_results %>%
  mutate(LHS = paste0("E_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, EPS, NegEPS, NegEPS_EPS, adj_r_squared, t_intercept, t_EPS, t_NegEPS, t_NegEPS_EPS) %>%
  arrange(forecast_horizon)

# Display the table
print(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, file = "results/01_table/earnings_forecast_LM_summary_all_Variables_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_LM_all_years_all_Variables_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)










# 2. Without Dummy Variables & winsorized -----------------------------------
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
write.csv(summary_table, file = "results/01_table/earnings_forecast_LM_summary_without_Dummies_winsorized.csv", row.names = FALSE)
write.csv(final_results, file = "results/01_table/earnings_forecast_LM_all_years_without_Dummies_winsorized.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)




.


# 1. change: Without Division among Market Cap and Book-to-Market: WINSORIZED -----------------------------------
# Define the winsorize function
winsorize <- function(df, cols, lower_limit = 0.005, upper_limit = 0.995) {
  df %>%
    mutate(across(all_of(cols), ~ {
      quantiles <- quantile(., probs = c(lower_limit, upper_limit), na.rm = TRUE)
      pmax(pmin(., quantiles[2]), quantiles[1])
    }))
}
# Create a temporary dataset 
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, dependent_EPS, EPS, NegEPS, NegEPS_EPS)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(dependent_EPS) & !is.na(EPS) & !is.na(NegEPS) & !is.na(NegEPS_EPS))

# Create lagged dependent variables for forecast horizons 1 to 5 years
for (k in 1:5) {
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>%
    arrange(mapped_fyear) %>%
    mutate(!!paste0("dependent_EPS_t", k) := lead(dependent_EPS, k)) %>%
    ungroup()
}

# Function to run rolling regression for each forecast horizon
run_rolling_regression <- function(data_temp, forecast_horizon) {
  results <- list()
  
  for (year in 1968:2023) {
    # Subset the data for the rolling window regression
    data_subset <- data_temp %>%
      filter(mapped_fyear >= (year - 10) & mapped_fyear < year)
    
    if (nrow(data_subset) > 0) {
      # Winsorize the data within the rolling window
      data_subset <- winsorize(data_subset, cols = c("EPS", "NegEPS", "NegEPS_EPS"))
      
      # Define the dependent variable based on the forecast horizon
      dependent_var <- paste0("dependent_EPS_t", forecast_horizon)
      
      # Estimate the regression coefficients using data from the past 10 years
      model <- lm(as.formula(paste(dependent_var, "~ EPS + NegEPS + NegEPS_EPS")), data = data_subset)
      coefficients <- coef(model)
      t_stats <- summary(model)$coefficients[, "t value"]
      adj_r_squared <- summary(model)$adj.r.squared
      
      # Store the results
      results[[paste0(year, "_t", forecast_horizon)]] <- data.frame(
        year = year,
        forecast_horizon = forecast_horizon,
        intercept = coefficients[1],
        EPS = coefficients[2],
        NegEPS = coefficients[3],
        NegEPS_EPS = coefficients[4],
        t_intercept = t_stats[1],
        t_EPS = t_stats[2],
        t_NegEPS = t_stats[3],
        t_NegEPS_EPS = t_stats[4],
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

# Calculate average R-squared, intercept, coefficients and t-statistics for each forecast horizon
summary_results <- final_results %>%
  group_by(forecast_horizon) %>%
  summarise(
    intercept = mean(intercept, na.rm = TRUE),
    EPS = mean(EPS, na.rm = TRUE),
    NegEPS = mean(NegEPS, na.rm = TRUE),
    NegEPS_EPS = mean(NegEPS_EPS, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_EPS = mean(t_EPS, na.rm = TRUE),
    t_NegEPS = mean(t_NegEPS, na.rm = TRUE),
    t_NegEPS_EPS = mean(t_NegEPS_EPS, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output
summary_table <- summary_results %>%
  mutate(LHS = paste0("EPS_{t+", forecast_horizon, "}")) %>%
  select(forecast_horizon, LHS, intercept, EPS, NegEPS, NegEPS_EPS, adj_r_squared, t_intercept, t_EPS, t_NegEPS, t_NegEPS_EPS) %>%
  arrange(forecast_horizon)

# Display the table
print(summary_table)

# Save the summary table to a CSV file 
write.csv(summary_table, file = "results/earnings_forecast_LM_summary.csv", row.names = FALSE)
write.csv(final_results, file = "results/earnings_forecast_LM_all_years.csv", row.names = FALSE)

# Remove redundant objects
rm(k, data_temp, all_results, final_results, summary_results, run_rolling_regression, summary_table)


# 2. change: With Division among Market Cap and Book-to-Market (Robustness Test: per share scaling) -----------------------------------
# Inputs: data, dependent_var, independent_vars
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, MthCap, BM, dependent_EPS, EPS, NegEPS, NegEPS_EPS)

# Remove NA values for the relevant variables
data_temp <- data_temp %>%
  filter(!is.na(MthCap) & !is.na(BM) & !is.na(dependent_EPS) & !is.na(EPS) & !is.na(NegEPS) & !is.na(NegEPS_EPS))

# Create a 1-year ahead lagged dependent variable
data_temp <- data_temp %>%
  group_by(UGVKEY) %>%
  arrange(mapped_fyear) %>%
  mutate(dependent_EPS_t1 = lead(dependent_EPS, 1)) %>%
  ungroup()

# Categorize firms into Book-to-Market and size groups
data_temp <- data_temp %>%
  mutate(
    BM_category = case_when(
      BM <= quantile(BM, 0.33, na.rm = TRUE) ~ "Low BM",
      BM <= quantile(BM, 0.67, na.rm = TRUE) ~ "Med BM",
      TRUE ~ "High BM"
    ),
    Size_category = case_when(
      MthCap <= quantile(MthCap, 0.33, na.rm = TRUE) ~ "Small cap",
      MthCap <= quantile(MthCap, 0.67, na.rm = TRUE) ~ "Medium cap",
      TRUE ~ "Large cap"
    )
  )

# Function to run rolling regression for 1-year ahead forecast
run_rolling_regression <- function(data_temp) {
  results <- list()
  
  for (year in 1968:2023) {
    for (size_group in unique(data_temp$Size_category)) {
      for (bm_group in unique(data_temp$BM_category)) {
        # Subset the data for the specific group and rolling window
        data_subset <- data_temp %>%
          filter(mapped_fyear >= (year - 10) & mapped_fyear < year) %>%
          filter(Size_category == size_group & BM_category == bm_group)
        
        if (nrow(data_subset) > 0) {
          # Define the dependent variable based on the forecast horizon
          dependent_var <- "dependent_EPS_t1"
          
          # Estimate the regression coefficients using data from the past 10 years
          model <- lm(as.formula(paste(dependent_var, "~ EPS + NegEPS + NegEPS_EPS")), data = data_subset)
          coefficients <- coef(model)
          t_stats <- summary(model)$coefficients[, "t value"]
          adj_r_squared <- summary(model)$adj.r.squared
          
          # Store the results
          results[[paste0(year, "_", size_group, "_", bm_group)]] <- data.frame(
            year = year,
            Size_category = size_group,
            BM_category = bm_group,
            intercept = coefficients[1],
            EPS = coefficients[2],
            NegEPS = coefficients[3],
            NegEPS_EPS = coefficients[4],
            t_intercept = t_stats[1],
            t_EPS = t_stats[2],
            t_NegEPS = t_stats[3],
            t_NegEPS_EPS = t_stats[4],
            adj_r_squared = adj_r_squared
          )
        }
      }
    }
  }
  do.call(rbind, results)
}

# Run the rolling regression for the 1-year ahead forecast
results <- run_rolling_regression(data_temp)

# Calculate average R-squared, intercept, coefficients and t-statistics for each group
summary_results <- results %>%
  group_by(Size_category, BM_category) %>%
  summarise(
    intercept = mean(intercept, na.rm = TRUE),
    EPS = mean(EPS, na.rm = TRUE),
    NegEPS = mean(NegEPS, na.rm = TRUE),
    NegEPS_EPS = mean(NegEPS_EPS, na.rm = TRUE),
    t_intercept = mean(t_intercept, na.rm = TRUE),
    t_EPS = mean(t_EPS, na.rm = TRUE),
    t_NegEPS = mean(t_NegEPS, na.rm = TRUE),
    t_NegEPS_EPS = mean(t_NegEPS_EPS, na.rm = TRUE),
    adj_r_squared = mean(adj_r_squared, na.rm = TRUE)
  )

# Format the summary results for table output
summary_table <- summary_results %>%
  mutate(LHS = "EPS_{t+1}") %>%
  select(Size_category, BM_category, LHS, intercept, EPS, NegEPS, NegEPS_EPS, adj_r_squared, t_intercept, t_EPS, t_NegEPS, t_NegEPS_EPS) %>%
  arrange(factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")), 
          factor(BM_category, levels = c("Low BM", "Med BM", "High BM")))

# Display the table
print(summary_table)

# Save the summary table to a CSV file 
write.csv(summary_table, file = "results/earnings_forecast_LM_summary_per_share.csv", row.names = FALSE)
write.csv(results, file = "results/earnings_forecast_LM_all_years_per_share.csv", row.names = FALSE)

# Remove redundant objects
rm(data_temp, results, summary_results, run_rolling_regression, summary_table)





# 3. chnage: With Division among Market Cap and Book-to-Market (Robustness: market scaling) -----------------------------------





