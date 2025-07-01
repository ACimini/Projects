library(tidyverse)
library(doBy)
library(dplyr)
library(zoo) # For rollapply function

# Cleaning and Formatting the FX Data #########################################
data <- read.csv("EURUSD_Candlestick_1_D_ASK_31.12.2002-01.11.2024.csv")
data <- data |> select(Gmt.time, Close)

# Convert 'Gmt.time' to a date-time format
data$Gmt.time <- as.Date(data$Gmt.time, format = "%d.%m.%Y %H:%M:%S")

# Rename columns
names(data)[1] <- "date"

# Calculate the Return Ratio (RR)
data$RR <- 100 * ((data$Close - lag(data$Close, 1)) / lag(data$Close))

# Loading and Prepping the IV Data ############################################
load("Euro_FX.Rdata")
df1$date <- as.Date(df1$date)

# Subset and select necessary columns for 1M and 1W IV
df2 <- subset(df1, expiration == "1M") |> select(date, atm)
df3 <- subset(df1, expiration == "1W") |> select(date, atm)

# Rename columns
names(df2)[2] <- "atm1M"
names(df3)[2] <- "atm1W"

# Merge the datasets
combined_data <- left_join(data, df2, by = "date")
combined_data <- left_join(combined_data, df3, by = "date")
combined_data <- na.omit(combined_data)

# Calculate Realized Variance (RV) using a 5-day rolling window
window_size <- 5
combined_data$RealizedVariance <- rollapply(combined_data$RR, 
                                            width = window_size, 
                                            FUN = function(x) mean(x^2, na.rm = TRUE), 
                                            fill = NA, 
                                            align = "right")

# Annualize the Realized Variance
combined_data$RealizedVariance_Annualized <- combined_data$RealizedVariance * 252

# Winsorize the Annualized Realized Variance at the 0.995 quantile
upper_limit <- quantile(combined_data$RealizedVariance_Annualized, 0.995, na.rm = TRUE)
combined_data$RealizedVariance_Annualized <- pmin(combined_data$RealizedVariance_Annualized, upper_limit)

# Plot the Annualized Realized Variance
plot.ts(combined_data$RealizedVariance_Annualized, 
        main = "Annualized Realized Variance", 
        ylab = "Realized Variance (Annualized)", 
        xlab = "Date")

# Annualize the IV data if necessary
combined_data$atm1W_annualized <- combined_data$atm1W * sqrt(52)  # Annualize 1-week IV
combined_data$atm1M_annualized <- combined_data$atm1M * sqrt(12)  # Annualize 1-month IV

# Plot the Annualized IV Data
plot.ts(combined_data$atm1W_annualized, main = "Annualized 1-Week ATM Implied Volatility", ylab = "IV", xlab = "Date")
plot.ts(combined_data$atm1M_annualized, main = "Annualized 1-Month ATM Implied Volatility", ylab = "IV", xlab = "Date")

# View the adjusted dataset
head(combined_data)

# Save the final dataset to a CSV file
write.csv(combined_data, file = "Combined_Data.csv")
