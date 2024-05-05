#load libraries
install.packages("PerformanceAnalytics")
install.packages("xts")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("data.table")
install.packages("forecast")
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(tidyverse)
library(data.table)
library(quantmod)
library(forecast)
library(MLmetrics)

#download data
getSymbols("AAPL",
           from = "2017/11/30",
           to = "2022/11/30",
           periodicity = "daily")
plot(AAPL$AAPL.Close)

#split into training data
train_AAPL =AAPL[1:1007]
plot(decompose(ts((train_AAPL$AAPL.Close),frequency=5)))

#split into testing data
test_AAPL=AAPL[1008:1258]

#apply holt-winters multiplicative
ets_AAPL=ets(ts(train_AAPL$AAPL.Close,frequency=4),allow.multiplicative.trend = FALSE)
summary(ets_AAPL)

#forecasting
ets_forecast_AAPL=forecast(ets_AAPL,h=length(test_AAPL$AAPL.Close))
plot(ets_forecast_AAPL)
MAPE(ets_forecast_AAPL$mean, test_AAPL$AAPL.Close)* 100
summary(ets_forecast_AAPL)
