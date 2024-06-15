

# Join the variable E from the data dataset to the HVZ dataset
earnings_forecasts_HVZ_each_company <- earnings_forecasts_HVZ_each_company %>%
  left_join(data %>% select(UGVKEY, mapped_fyear, E), by = c("UGVKEY", "mapped_fyear"))

earnings_forecasts_LM_each_company <- earnings_forecasts_LM_each_company %>%
  left_join(data %>% select(UGVKEY, mapped_fyear, E), by = c("UGVKEY", "mapped_fyear"))

