library(tidyverse)
library(PerformanceAnalytics) 
library(rugarch)
library(quantreg)

# Read and preprocess the data
data <- read.csv("Combined_Data.csv")  # Use your data file path
data <- data %>%
  select(date, Close, RR, atm1M, atm1W, RealizedVariance) %>%  # Select relevant columns
  mutate(date = as.Date(date, format = "%Y-%m-%d"))  # Convert date to Date type

# Initialize vectors to store VaR and ES results
data$returns <- data$RR / 100 

data1 = data[5:nrow(data),]

# Initialize vectors to store VaR and ES results
n <- nrow(data)
window_size <- 1500
compute_rolling_var_es_multiple_tau <- function(data, window_size = 1500, taus = c(0.05, 0.025, 0.01)) {
  
  # Initialize lists to store VaR and ES values for each quantile
  var_values_list <- list()
  es_values_list <- list()
  
  # Loop over each quantile in taus
  for (tau in taus) {
    
    # Initialize vectors to store VaR and ES values for this quantile
    var_values <- numeric(nrow(data))  
    es_values <- numeric(nrow(data))  
    
    # Loop over each day starting from the window_size + 1 day
    for (i in (window_size + 1):nrow(data)) {
      
      # Subset the data for the current rolling window
      window_data <- data[(i - window_size):(i - 1), ]
      
      # Extract values for the CC_t formula
      pi_val <- pi  # Pi constant
      N <- nrow(window_data)  # Length of the returns vector for the current window
      
      term1 <- pi_val / (6 - 4 * sqrt(3) + pi_val)
      term2 <- (N / (N - 2))
      
      # Initialize CC_t and JC_t vectors
      CC_t <- numeric(N)
      JC_t <- numeric(N)
      
      # Calculate the sum part: sum of squared medians
      sum_medians <- 0
      for (j in 2:(N - 1)) {
        # Extract the absolute values of the returns at j-1, j, j+1
        med_vals <- abs(c(window_data$returns[j-1], window_data$returns[j], window_data$returns[j+1]))
        # Compute the median of absolute returns and square it
        med_squared <- median(med_vals)^2
        sum_medians <- sum_medians + med_squared
        
        CC_t[j] <- pi_val * term1 * term2 * sum_medians
        JC_t[j] <- max(window_data$RealizedVariance[i-1] - CC_t[i-1], 0)  # Ensure element-wise operation
      }
      
      # Add CC_t and JC_t to the window data
      window_data$CC_t <- CC_t
      window_data$JC_t <- JC_t
      
      # Fit the quantile regression model on the rolling window data
      qr_model <- rq(returns ~ RealizedVariance + atm1W + 
                       atm1M + CC_t , data = window_data, tau = tau)
      
      # Predict the return for the next day (one-period ahead forecast)
      next_day_data <- data.frame(RealizedVariance = data$RealizedVariance[i], 
                                  atm1W = data$atm1W[i], 
                                  atm1M = data$atm1M[i], CC_t = CC_t[N])# Use the last value from the window
      
      # Predict VaR as the quantile regression forecast
      predicted_return <- predict(qr_model, newdata = next_day_data)
      var_values[i] <- predicted_return  # Store the predicted return as VaR
      
      # For ES, compute the expected shortfall as the mean of returns below VaR
      es_values[i] <- mean(window_data$returns[window_data$returns <= var_values[i-1]], na.rm = TRUE)
    }
    
    # Add the VaR and ES values for this quantile to the lists
    var_values_list[[paste0("var_", tau * 100)]] <- var_values
    es_values_list[[paste0("es_", tau * 100)]] <- es_values
  }
  
  # Combine all VaR and ES values into separate data frames
  var_values_df <- data.frame(var_values_list)
  es_values_df <- data.frame(es_values_list)
  
  # Return the data frames with VaR and ES values for all quantiles
  return(list(var_values = var_values_df, es_values = es_values_df))
}

# Define the quantiles
taus <- c(0.05, 0.025, 0.01, 0.95, 0.975, 0.99)

# Compute one-day-ahead VaR and ES using a rolling window of 1500 days for multiple quantiles
results <- compute_rolling_var_es_multiple_tau(data, window_size = 1500, taus = taus)

data <- cbind(data, results$var_values, results$es_values)


data1 <- data[, !(names(data) %in% c("var_5", "var_2.5", "var_1", "var_95", 
                                     "var_97.5", "var_99"))]

write.csv(data1, file = "QR-IV-CJ.csv", row.names = FALSE)

# ---
plot_data <- data.frame(
  Date = data$date,
  RR = data$RR / 100, # Ensure RR is in decimal form
  VaR99 = data$var_5, # Keep VaR99 as a positive value
  ES = data$es_5  # Take the absolute value of ES to make it positive
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
