library(tidyverse)
library(ggplot2)


data = read.csv("Combined_Data.csv")

data <- data %>%
  select( - X) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

subset_data <- data[(nrow(data) - 3100):nrow(data), ]

Returns = ggplot(subset_data, aes(x=date,y=RR)) + geom_line() + theme_bw() + xlab("Date") + ylab("Returns")

IV = ggplot(subset_data, aes(x=date,y=atm1W)) + geom_line() + theme_bw() + xlab("Date") + ylab("Implied Vol")

RV = ggplot(subset_data, aes(x=date,y=RealizedVariance)) + geom_line() + theme_bw() + xlab("Date") + ylab("Realized Variance")

ggsave("Returns.png", plot = Returns, width = 15, height = 5, dpi = 300)
ggsave("IV.png", plot = IV, width = 15, height = 5, dpi = 300)
ggsave("RV.png", plot = RV, width = 15, height = 5, dpi = 300)

cor(subset_data$atm1W,subset_data$RealizedVariance)

