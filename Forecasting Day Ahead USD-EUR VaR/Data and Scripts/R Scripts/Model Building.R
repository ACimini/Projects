# Load necessary libraries
library(tidyverse)
library(PerformanceAnalytics) # For VaR and ES calculation
library(rugarch)

# Read and preprocess the data
data <- read.csv("Combined_Data.csv")  # Use your data file path
data <- data %>%
  select(date, Close, RR, atm1W, atm1M) %>%  # Select relevant columns
  mutate(date = as.Date(date, format = "%Y-%m-%d"))  # Convert date to Date type

# Initialize vectors to store VaR and ES results
returns <- data$RR / 100 

# Initialize vectors to store VaR and ES results
n <- nrow(data)
window_size <- 1500
VaR_results <- numeric(n - window_size)
ES_results <- numeric(n - window_size)

# Rolling window loop
for (i in 1:(n - window_size)) {
  # Subset the data for the current rolling window
  window_data <- returns[i:(i + window_size - 1)]
  
  # Specify and fit the EGARCH(1,1) model
  egarch_spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "std" # Skewed Student's t-distribution if needed
  )
  egarch_model <- ugarchfit(spec = egarch_spec, data = window_data)
  
  # Forecast volatility for the next day
  forecast <- ugarchforecast(egarch_model, n.ahead = 1)
  predicted_volatility <- sigma(forecast)
  
  # Calculate VaR99 (1% level, left tail)
  VaR_results[i] <- -qnorm(0.01) * predicted_volatility
  
  # Calculate ES as the mean of returns that are less than or equal to the VaR threshold
  ES_results[i] <- mean(window_data[window_data <= -VaR_results[i]], na.rm = TRUE)
  
  # Print progress every 100 iterations
  if (i %% 100 == 0) {
    print(paste("Iteration:", i, "VaR99:", VaR_results[i], "ES:", ES_results[i]))
  }
}

# Combine results into a data frame
results <- data.frame(Date = data$date[(window_size + 1):n], VaR99 = VaR_results, ES = ES_results)
print(head(results))


library(ggplot2)

# Combine your original data with the results
plot_data <- data.frame(
  Date = data$date[(window_size + 1):n],
  RR = data$RR[(window_size + 1):n] / 100, # Ensure RR is in decimal form
  VaR99 = results$VaR99, # Keep VaR99 as a positive value
  ES = abs(results$ES)   # Take the absolute value of ES to make it positive
)

# Create the plot
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = RR, color = "RR (Returns)")) +
  geom_line(aes(y = -VaR99, color = "VaR99 (1% Level)"), linetype = "dashed") + # Make VaR99 negative
  geom_line(aes(y = -ES, color = "ES (Expected Shortfall)"), linetype = "dotted") + # Make ES negative
  labs(title = "VaR99 and ES Around Daily Returns",
       y = "Value",
       x = "Date") +
  scale_color_manual(values = c("RR (Returns)" = "black", 
                                "VaR99 (1% Level)" = "red", 
                                "ES (Expected Shortfall)" = "blue")) + theme_minimal()
