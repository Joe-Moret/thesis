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

# Convert mapped_fyear to numeric year if it is a Date object
data_temp <- data %>%
  select(UGVKEY, mapped_fyear, A, E, NegE, D, DD, AC, EPS, NegEPS, NegEPS_EPS) %>%
  mutate(mapped_fyear = as.numeric(mapped_fyear))

# Create a function to apply a moving 10-year window winsorization
apply_moving_winsorize <- function(df, cols) {
  df %>%
    mutate(group = floor((mapped_fyear - 1963) / 10)) %>%
    group_by(group) %>%
    group_modify(~ winsorize(.x, cols)) %>%
    ungroup() %>%
    select(-group)
}

# Apply the moving window winsorization
data_temp <- apply_moving_winsorize(data_temp, c("A", "E", "D", "AC", "EPS"))

# Debugging: Check unique years in data_temp
unique_years <- unique(data_temp$mapped_fyear)
print(unique_years)

# Compute summary statistics for the HVZ model (A, E, NegE, D, DD, AC)
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

# Debugging: Check if filtered data is different
data_1963_2009 <- data_temp %>% filter(mapped_fyear >= 1963 & mapped_fyear <= 2009)
data_1963_2023 <- data_temp %>% filter(mapped_fyear >= 1963 & mapped_fyear <= 2023)
print(nrow(data_1963_2009))
print(nrow(data_1963_2023))

# Compute summary statistics for the LM model (EPS, NegEPS, NegEPS_EPS)
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

# Print summary statistics
summary_panel_HVZ
summary_panel_LM