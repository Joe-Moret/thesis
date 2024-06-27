# tests.R: Outliers ------------------------
# categorize by Market Cap and BM sizes 
categorize_firms <- function(data) {
  data %>%
    group_by(mapped_fyear) %>%  # grouping done for each year seperately
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
    ) %>%
    ungroup() %>%  
    mutate(BM_category = factor(BM_category, levels = c("Low BM", "Med BM", "High BM"))) %>% 
    mutate(Size_category = factor(Size_category, levels = c("Small cap", "Medium cap", "Large cap")))
}

# identify overall outliers (i.e. across all firms) within each 10 year window for each year
identify_overall_outliers <- function(data, variable, lower_limit = 0.005, upper_limit = 0.995) {
  data %>%
    mutate(
      lower_bound = quantile(.data[[variable]], probs = lower_limit, na.rm = TRUE),
      upper_bound = quantile(.data[[variable]], probs = upper_limit, na.rm = TRUE),
      outlier = ifelse(.data[[variable]] < lower_bound | .data[[variable]] > upper_bound, TRUE, FALSE)
    )
}

# identify grouped outliers (i.e. within each Market Cap and BM category) within each 10 year window for each year
identify_grouped_outliers <- function(data, variable, lower_limit = 0.005, upper_limit = 0.995) {
  data %>%
    group_by(group, Size_category, BM_category) %>%
    mutate(
      lower_bound = quantile(.data[[variable]], probs = lower_limit, na.rm = TRUE),
      upper_bound = quantile(.data[[variable]], probs = upper_limit, na.rm = TRUE),
      outlier = ifelse(.data[[variable]] < lower_bound | .data[[variable]] > upper_bound, TRUE, FALSE)
    ) %>%
    ungroup()
}

# Table 1 -----------------------------------
# winsorize independent variables at 1% level
winsorize <- function(df, cols, lower_limit = 0.01, upper_limit = 0.99) {
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

# Winsorization rolling_regression (at 0.5% level)
winsorize_regression <- function(df, cols, lower_limit = 0.0025, upper_limit = 0.9975) {
  df <- df %>%
    group_by(Size_category, BM_category) %>%
    mutate(across(all_of(cols), ~ {
      quantiles <- quantile(., probs = c(lower_limit, upper_limit), na.rm = TRUE)
      ifelse(. < quantiles[1], quantiles[1], 
             ifelse(. > quantiles[2], quantiles[2], .))
    })) %>%
    ungroup()
  return(df)
}



# Table 2: Forecast Bias (FB) -----------------------------------





