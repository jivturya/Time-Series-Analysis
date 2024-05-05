#load libraries
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(tidyverse)
library(data.table)
library(quantmod)
library(forecast)
library(MLmetrics)

#download data
getSymbols("GOOGL",
           from = "2017/11/30",
           to = "2022/11/30",
           periodicity = "daily")
plot(GOOGL$GOOGL.Close)

#split into training data
train_GOOGL =GOOGL[1:1007]
plot(decompose(ts((train_GOOGL$GOOGL.Close),frequency=5)))
#split into testing data
test_GOOGL=GOOGL[1008:1258]

#apply holt-winters multiplicative
ets_GOOGL=ets(ts(train_GOOGL$GOOGL.Close,frequency=4),allow.multiplicative.trend = FALSE)
summary(ets_GOOGL)

#forecasting
ets_forecast_GOOGL=forecast(ets_GOOGL,h=length(test_GOOGL$GOOGL.Close))
plot(ets_forecast_GOOGL)
MAPE(ets_forecast_GOOGL$mean, test_GOOGL$GOOGL.Close)* 100
summary(ets_forecast_GOOGL)
