
# Interest Rates -----------------------------------
data %>%
  ggplot(aes(x = mapped_fyear)) +
  geom_line(aes(y = `1y_T-Bill`, color = "1y T-Bill")) +
  geom_line(aes(y = `10y_T-Bill`, color = "10y T-Bill")) +
  labs(title = "1-year and 10-year Treasury Bill Rates",
       x = "Year",
       y = "Interest Rate (%)") +
  scale_color_manual(values = c("1y T-Bill" = "blue", "10y T-Bill" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")

