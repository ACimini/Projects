library(esback)
library(esreg)
library(tidyverse)
library(ggplot2)

data <- read.csv("eGARCH_IV_SSK.csv")  # Use your data file path
data <- data %>%
  select( - X) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
data$date = as.Date(data$date, format = "%Y-%m-%d")

total_days <- nrow(data)
start_day <- 1500

returns = data$RR[(start_day + 1):total_days] / 100

ES_forecasts = data$es_1[(start_day + 1):total_days]

valid_indices <- complete.cases(returns, ES_forecasts)
returns <- returns[valid_indices]
ES_forecasts <- ES_forecasts[valid_indices]

result <- esr_backtest(
  r = returns,  # Vector of returns
  e = ES_forecasts,  # Vector of ES forecasts
  alpha = 0.01,  # Probability level
  version = 1,  # "Strict ESR" backtest
  B = 1000  # Number of bootstrap samples for robust p-values
)


print(result)


ES_IV = ggplot(data, aes(x = date))+ geom_line(aes(y = RR), col = 'black') + 
  geom_line(aes(y = ES0.01), col = 'red') + 
  geom_line(aes(y = ES0.99), col = 'blue') + 
  xlab("Returns") + 
  ylab("Date") + 
  ggtitle("eGARCH IV SSK Expected Shortfall")

ggsave("eGARCH_IV_SSK.png", plot = ES_IV, width = 15, height = 5, dpi = 300)


