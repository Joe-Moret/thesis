# Table 1: Panel A - Summary Statistics -----------------------------------
# winsorize independent variables at 1% level
winsorize <- function(df, cols, lower_limit = 0.005, upper_limit = 0.995) {
  df %>%
    mutate(across(all_of(cols), ~ {
      quantiles <- quantile(., probs = c(lower_limit, upper_limit), na.rm = TRUE)
      pmax(pmin(., quantiles[2]), quantiles[1])
    }))
}

# computing Summary Statistics
summarise_stats <- function(df, variables) {
  df %>%
    summarise(across(all_of(variables), 
                     list(mean = ~mean(.x, na.rm = TRUE), 
                          sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{col}_{fn}")) %>%
    mutate(across(where(is.numeric), ~na_if(.x, Inf))) %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
}





# Table 1: Panel B - HVZ Forecast Regression  -----------------------------------
# Winsorization rolling_regression (at 0.5% level)
winsorize_regression <- function(df, cols, lower_limit = 0.005, upper_limit = 0.995) {
  df <- df %>%
    group_by(Size_category) %>%
    mutate(across(all_of(cols), ~ {
      quantiles <- quantile(., probs = c(lower_limit, upper_limit), na.rm = TRUE)
      ifelse(. < quantiles[1], quantiles[1], 
             ifelse(. > quantiles[2], quantiles[2], .))
    })) %>%
    ungroup()
  return(df)
}



# Function to run rolling grouped (market cap and BM) regression for 1-year ahead forecast
run_grouped_regression <- function(data, dependent_var, independent_vars) {
  
  # Create a temporary dataset
  data_temp <- data %>%
    select(UGVKEY, mapped_fyear, MthCap, BM, all_of(dependent_var), all_of(independent_vars))
  
  # Subset the data to only one row per mapped_fyear per UGVKEY
  data_temp <- data_temp %>%
    group_by(UGVKEY, mapped_fyear) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  # Remove NA values for the relevant variables
  data_temp <- data_temp %>%
    filter(!is.na(MthCap) & !is.na(BM) & !is.na(.data[[dependent_var]]) & 
             !if_any(all_of(independent_vars), is.na))
  
  # Create a 1-year ahead lagged dependent variable
  data_temp <- data_temp %>%
    group_by(UGVKEY) %>%
    arrange(mapped_fyear) %>%
    mutate(!!paste0(dependent_var, "_t1") := lead(.data[[dependent_var]], 1)) %>%
    ungroup()
  
  # Categorize firms into Book-to-Market and size groups using 30th and 70th percentiles
  data_temp <- data_temp %>%
    mutate(
      BM_category = case_when(
        BM <= quantile(BM, 0.3, na.rm = TRUE) ~ "Low BM",
        BM <= quantile(BM, 0.7, na.rm = TRUE) ~ "Med BM",
        TRUE ~ "High BM"
      ),
      Size_category = case_when(
        MthCap <= quantile(MthCap, 0.3, na.rm = TRUE) ~ "Small cap",
        MthCap <= quantile(MthCap, 0.7, na.rm = TRUE) ~ "Medium cap",
        TRUE ~ "Large cap"
      )
    )
  
  # Function to run rolling regression for 1-year ahead forecast
  run_rolling_regression <- function(data_temp) {
    results <- list()
    
    for (year in 1968:2009) {
      for (size_group in unique(data_temp$Size_category)) {
        for (bm_group in unique(data_temp$BM_category)) {
          # Subset the data for the specific group and rolling window
          data_subset <- data_temp %>%
            filter(mapped_fyear >= (year - 10) & mapped_fyear < year) %>%
            filter(Size_category == size_group & BM_category == bm_group)
          
          # Print the data subset for debugging
          print(paste0("Year: ", year, ", Size group: ", size_group, ", BM group: ", bm_group))
          print(summary(data_subset))
          
          if (nrow(data_subset) > 0) {
            # winsorize_regression the independent variables in the subset
            data_subset <- winsorize(data_subset, cols = independent_vars)
            
            # Define the dependent variable based on the forecast horizon
            dependent_var_t1 <- paste0(dependent_var, "_t1")
            
            # Ensure the data_subset has variation
            if (all(sapply(data_subset[independent_vars], function(x) length(unique(x)) > 1))) {
              
              # Estimate the regression coefficients using data from the past 10 years
              model <- lm(as.formula(paste(dependent_var_t1, "~", paste(independent_vars, collapse = " + "))), data = data_subset)
              coefficients <- coef(model)
              t_stats <- summary(model)$coefficients[, "t value"]
              adj_r_squared <- summary(model)$adj.r.squared
              
              # Store the results
              results[[paste0(year, "_", size_group, "_", bm_group)]] <- data.frame(
                year = year,
                Size_category = size_group,
                BM_category = bm_group,
                intercept = coefficients[1],
                t_intercept = t_stats[1],
                adj_r_squared = adj_r_squared
              )
              
              for (i in seq_along(independent_vars)) {
                results[[paste0(year, "_", size_group, "_", bm_group)]][[independent_vars[i]]] <- coefficients[i + 1]
                results[[paste0(year, "_", size_group, "_", bm_group)]][[paste0("t_", independent_vars[i])]] <- t_stats[i + 1]
              }
            } else {
              message("Skipping regression for year ", year, ", size group ", size_group, ", BM group ", bm_group, " due to lack of variation.")
            }
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
    summarise(across(starts_with("t_"), \(x) mean(x, na.rm = TRUE)),
              across(setdiff(names(.), c("year", "Size_category", "BM_category", starts_with("t_"))), \(x) mean(x, na.rm = TRUE)))
  
  # Format the summary results for table output
  summary_table <- summary_results %>%
    mutate(LHS = paste0(dependent_var, "_{t+1}")) %>%
    select(Size_category, BM_category, LHS, intercept, all_of(independent_vars), adj_r_squared, starts_with("t_")) %>%
    arrange(factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")), 
            factor(BM_category, levels = c("Low BM", "Med BM", "High BM"))) 
  
  # Display the table
  print(summary_table)
  
  # Return the summary table
  return(summary_table)
  
  # load summary_table and results into environment
  assign("summary_table", summary_table, envir = .GlobalEnv)
  assign("results", results, envir = .GlobalEnv)
}





