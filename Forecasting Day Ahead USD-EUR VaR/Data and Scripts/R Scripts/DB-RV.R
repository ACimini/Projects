library(tidyverse)
library(esreg)
library(ggplot2)

data <- read.csv("Combined_Data.csv")  # Use your data file path
data <- data %>%
  select( - X) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

data$RealizedVariance[1:4] = data$RealizedVariance[5]

DB_RV = data[,1:3]
DB_RV$ES0.01 = NA
DB_RV$ES0.025 = NA
DB_RV$ES0.05 = NA
DB_RV$ES0.95 = NA
DB_RV$ES0.975 = NA
DB_RV$ES0.99 = NA

window_size = 1500
total_days <- nrow(data)
start_day <- window_size

for (i in start_day:(total_days - 1)) {
  window_data <- data[(i - window_size + 1):i, ]
  
  # Fit models for different alpha levels
  fit0.01 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.01)
  fit0.025 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.025)
  fit0.05 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.05)
  fit0.95 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.95)
  fit0.975 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.975)
  fit0.99 <- esreg(window_data$RR ~ window_data$RealizedVariance, alpha = 0.99)
  
  # Extract coefficients for ES calculations
  coefficients0.01 <- fit0.01$coefficients
  coefficients0.025 <- fit0.025$coefficients
  coefficients0.05 <- fit0.05$coefficients
  coefficients0.95 <- fit0.95$coefficients
  coefficients0.975 <- fit0.975$coefficients
  coefficients0.99 <- fit0.99$coefficients
  
  # Calculate ES for each alpha level
  DB_RV$ES0.01[i + 1] <- coefficients0.01[1] + (coefficients0.01[2] * window_data$RealizedVariance[1500])
  DB_RV$ES0.025[i + 1] <- coefficients0.025[1] + (coefficients0.025[2] * window_data$RealizedVariance[1500])
  DB_RV$ES0.05[i + 1] <- coefficients0.05[1] + (coefficients0.05[2] * window_data$RealizedVariance[1500])
  DB_RV$ES0.95[i + 1] <- coefficients0.95[1] + (coefficients0.95[2] * window_data$RealizedVariance[1500])
  DB_RV$ES0.975[i + 1] <- coefficients0.975[1] + (coefficients0.975[2] * window_data$RealizedVariance[1500])
  DB_RV$ES0.99[i + 1] <- coefficients0.99[1] + (coefficients0.99[2] * window_data$RealizedVariance[1500])
  
  if(i %% 10 == 0){
    print(paste("iteration", i))
  }
  
}

DB_RV[1509,]


ggplot(DB_RV, aes(x = date)) + geom_line(aes(y = RR), color = "black") + geom_line(aes(y = ES0.99), color = "red") + geom_line(aes(y = ES0.05), color = "blue")

write.csv(DB_RV, file = "DB_RV.csv", row.names = FALSE)




window = data[1:1500,]
fit = esreg(data$RR ~ data$atm1M, alpha = 0.99)
coef = fit$coefficients
coef[4]
summary(fit)
test = estfun.esreg(fit)

plot(fitted(fit), residuals(fit), main = "Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
