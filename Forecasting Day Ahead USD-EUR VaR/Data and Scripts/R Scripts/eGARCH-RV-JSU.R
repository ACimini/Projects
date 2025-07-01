library(tidyverse)
library(rugarch)

# Read and preprocess the data
data <- read.csv("Combined_Data.csv")  # Use your data file path
data <- data %>%
  select(date, Close, RR, atm1W, atm1M, RealizedVariance) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

data$RealizedVariance[1:4] = data$RealizedVariance[5]

window_size <- 1500
total_days <- nrow(data)
start_day <- window_size

eGARCH_RV_JSU <- data[, 1:3]
eGARCH_RV_JSU$ES0.01 <- NA
eGARCH_RV_JSU$ES0.025 <- NA
eGARCH_RV_JSU$ES0.05 <- NA
eGARCH_RV_JSU$ES0.95 <- NA
eGARCH_RV_JSU$ES0.975 <- NA
eGARCH_RV_JSU$ES0.99 <- NA

for (i in start_day:(total_days - 1)) {
  window_data <- data[(i - window_size):(i - 1), ]
  
  # Specify the EGARCH model with the external regressors
  egarch_spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, 
                      external.regressors = as.matrix(window_data$RealizedVariance)),
    distribution.model = "jsu"  # Using Johnson's SU distribution
  )
  
  fit <- ugarchfit(spec = egarch_spec, data = window_data$RR)
  
  forecast <- ugarchforecast(fit, n.ahead = 1, 
                             external.forecast = list(mregfor = as.matrix(data$RealizedVariance[i])))
  
  # Calculate VaR for the lower tail (0.01, 0.025, 0.05)
  VaR01 <- qdist("jsu", p = 0.01, mu = fitted(forecast), sigma = sigma(forecast), 
                 skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_01 <- mean(window_data$RR[window_data$RR < VaR01], na.rm = TRUE)
  
  VaR025 <- qdist("jsu", p = 0.025, mu = fitted(forecast), sigma = sigma(forecast), 
                  skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_025 <- mean(window_data$RR[window_data$RR < VaR025], na.rm = TRUE)
  
  VaR05 <- qdist("jsu", p = 0.05, mu = fitted(forecast), sigma = sigma(forecast), 
                 skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_05 <- mean(window_data$RR[window_data$RR < VaR05], na.rm = TRUE)
  
  # Calculate VaR for the upper tail (0.95, 0.975, 0.99)
  VaR95 <- qdist("jsu", p = 0.95, mu = fitted(forecast), sigma = sigma(forecast), 
                 skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_95 <- mean(window_data$RR[window_data$RR > VaR95], na.rm = TRUE)  # Adjusted condition
  
  VaR975 <- qdist("jsu", p = 0.975, mu = fitted(forecast), sigma = sigma(forecast), 
                  skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_975 <- mean(window_data$RR[window_data$RR > VaR975], na.rm = TRUE)  # Adjusted condition
  
  VaR99 <- qdist("jsu", p = 0.99, mu = fitted(forecast), sigma = sigma(forecast), 
                 skew = coef(fit)["skew"], shape = coef(fit)["shape"])
  ES_99 <- mean(window_data$RR[window_data$RR > VaR99], na.rm = TRUE)  # Adjusted condition
  
  # Store the ES predictions
  eGARCH_RV_JSU$ES0.01[i + 1] <- ES_01
  eGARCH_RV_JSU$ES0.025[i + 1] <- ES_025
  eGARCH_RV_JSU$ES0.05[i + 1] <- ES_05
  
  eGARCH_RV_JSU$ES0.95[i + 1] <- ES_95
  eGARCH_RV_JSU$ES0.975[i + 1] <- ES_975
  eGARCH_RV_JSU$ES0.99[i + 1] <- ES_99
  
  if (i %% 10 == 0) {
    print(paste("Iteration", i))
  }
}

library(lmtest)
library(sandwich)

# Example for backtesting ES0.01
losses <- data$RR[start_day:total_days]  # Actual losses
predicted_ES <- eGARCH_RV_JSU$ES0.01[start_day:total_days]  # Predicted ES values

# Run the regression: losses ~ predicted_ES
regression_model <- lm(losses ~ predicted_ES)

# Perform hypothesis testing
summary(regression_model)  # Check regression coefficients
coeftest(regression_model, vcov = NeweyWest(regression_model))  # Hypothesis test with Newey-West standard errors


library(ggplot2)

# Create a long format data frame that includes RR and ES values
# Filter out rows with NA ES values for plotting
valid_data <- eGARCH_RV_JSU %>% filter(!is.na(ES0.01))

plot_data <- valid_data %>%
  select(date, RR, ES0.01, ES0.025, ES0.05, ES0.95, ES0.975, ES0.99) %>%
  pivot_longer(cols = -c(date, RR), names_to = "Quantile", values_to = "ES")

# Plot the daily returns and ES values
ggplot() +
  geom_line(data = plot_data, aes(x = date, y = RR), color = "black") +
  geom_line(data = plot_data, aes(x = date, y = ES, color = Quantile), size = 0.8) +
  labs(
    title = "Daily Returns (RR) with Expected Shortfall (ES) Over Time",
    x = "Date",
    y = "Value",
    color = "Quantile"
  ) +
  theme_minimal()

# Save the updated data frame to a CSV file
write.csv(eGARCH_RV_JSU, file = "eGARCH_RV_JSU.csv", row.names = FALSE)
