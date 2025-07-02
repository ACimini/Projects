
library(tidyverse)
library(PerformanceAnalytics) 
library(rugarch)
library(quantreg)

# Read and preprocess the data
data <- read.csv("Combined_Data.csv")  # Use your data file path
data <- data %>%
  select(date, Close, RR, RealizedVariance, atm1M, atm1W) %>%  # Select relevant columns
  mutate(date = as.Date(date, format = "%Y-%m-%d"))  # Convert date to Date type

# Initialize vectors to store VaR and ES results
data$returns <- data$RR / 100 

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
    
    # Loop over each day starting from the 1501st day (because you need a window_size of 1500)
    for (i in (window_size + 1):nrow(data)) {
      
      # Subset the data for the current rolling window (previous 1500 days)
      window_data <- data[(i - window_size):(i - 1), ]
      
      NV_t <- numeric(N)
      PV_t <- numeric(N)
      
      for (j in 5:N) {
        if (window_data$RealizedVariance[j]-mean(window_data$RealizedVariance[5:N]) < 0) {
          NV_t[j] <- (window_data$RealizedVariance[j]-mean(window_data$RealizedVariance[5:N]))^2
        } else {
          NV_t[j] <- 0
        }
        
        if (window_data$RealizedVariance[j]-mean(window_data$RealizedVariance[5:N]) >= 0) {
          PV_t[j] <- (window_data$RealizedVariance[j]-mean(window_data$RealizedVariance[5:N]))^2
        } else {
          PV_t[j] <- 0
        }
      }
      
      window_data$PV_t = PV_t
      window_data$NV_t = NV_t
      # Fit the quantile regression model on the rolling window data
      qr_model <- rq(returns ~ RealizedVariance + NV_t + PV_t + atm1W + atm1M,
                     data = window_data, 
                     tau = tau)
      
      # Prepare the predictor values for the next day (i.e., day i) based on the previous day's values (i-1)
      next_day_data <- data.frame(RealizedVariance = data$RealizedVariance[i], 
                                  atm1W = data$atm1W[i],atm1M = data$atm1M[i] , NV_t = 
                                    window_data$NV_t[N], 
                                  PV_t = window_data$PV_t[N])  # Ensure it's a data frame with column name
      
      # Predict the return for the next day (one-period ahead forecast)
      predicted_return <- predict(qr_model, newdata = next_day_data)
      
      # VaR is the predicted return at the given quantile (i.e., the quantile prediction of returns)
      var_values[i] <- predicted_return  # Directly store the predicted return as VaR
      
      # For ES, compute the expected shortfall as the mean of returns below VaR
      # ES is the mean of the returns below the VaR for the distribution
      es_values[i] <- mean(window_data$returns[window_data$returns <= var_values[i]], na.rm = TRUE)
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

# Set quantiles for which you want to calculate VaR and ES
taus <- c(0.05, 0.025, 0.01, 0.95, 0.975, 0.99)

# Compute one-day-ahead VaR and ES using a rolling window of 1500 days for multiple quantiles
results <- compute_rolling_var_es_multiple_tau(data = data, window_size = 1500, taus = taus)

# Add the VaR and ES results to the data frame
data <- cbind(data, results$var_values, results$es_values)

data1 <- data[, !(names(data) %in% c("var_5", "var_2.5", "var_1", "var_95", 
                                     "var_97.5", "var_99"))]

write.csv(data1, file = "QR-IV-SV.csv", row.names = FALSE)
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

