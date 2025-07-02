library(rvest)
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(dplyr)

#This file is for turning the separate data files into one dataframe

#Loads in all the CSV Files
Stock = read.csv("msft_daily_historical-data-10-28-2024.csv")
VIX = read.csv("vix_daily_historical-data-10-28-2024.csv")
CA = read.csv("msft_corporate_actions-10-28-2024.csv")
DollerIndex = read.csv("dxy_daily_historical-data-10-28-2024.csv")
ExRate = read.csv("usdeur_daily_historical-data-10-28-2024.csv")
spy = read.csv("spy_daily_historical-data-10-28-2024.csv")

#Individually clean each dataset before we combine

#Stock Dataset
Stock = Stock |> select(Time,Last,Change,X.Chg,Volume)
names = c("Date","StockClose","StockChange","StockPrctChange","StockVolume")
names(Stock) = names
Stock$StockPrctChange = as.numeric(gsub("[+%]", "", Stock$StockPrctChange))
Stock$StockPrctChange = Stock$StockPrctChange / 100
Stock$Date = as.Date(Stock$Date, format = "%m/%d/%Y")
Stock = na.omit(Stock)

ggplot(Stock, aes(x=Date,y=StockClose)) + geom_line() + theme_bw()

#VIX Dataset
VIX = VIX |> select(Time,Last)
names = c("Date","VIX")
names(VIX) = names
VIX$Date = as.Date(VIX$Date, format = "%m/%d/%Y")
VIX = na.omit(VIX)

#Corporate Actions Dataset
CA = CA |> select(Date,Event.Type)
names = c("Date","Event")
names(CA) = names
CA$Date = as.Date(CA$Date, format = "%m/%d/%y")
CA = na.omit(CA)

#Index Dataset
spy = spy |> select(Time,Last,Volume)
names = c("Date","SPY_Close","SPY_Volume")
names(spy) = names
spy$Date = as.Date(spy$Date, format = "%m/%d/%Y")
spy = na.omit(spy)

#Merging it all into one data frame
main = left_join(Stock,VIX, by = "Date")
main = left_join(main,CA,by = "Date")
main = left_join(main,spy,by = "Date")

write.csv(main,"Complete_Dataset.csv")
