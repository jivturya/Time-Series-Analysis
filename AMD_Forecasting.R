#load libraries
library(PerformanceAnalytics)
library(xts)
library(tidyverse)
library(forecast)
library(MLmetrics)

#download data
getSymbols("AMD",
           from = "2017/11/30",
           to = "2022/11/30",
           periodicity = "daily")
plot(AMD$AMD.Close)

#split into training data
train_AMD =AMD[1:1007]
plot(decompose(ts((train_AMD$AMD.Close),frequency=5)))
#split into testing data
test_AMD=AMD[1008:1258]

#apply holt-winters multiplicative
ets_AMD=ets(ts(train_AMD$AMD.Close,frequency=4),allow.multiplicative.trend = FALSE)
summary(ets_AMD)

#forecasting
ets_forecast_AMD=forecast(ets_AMD,h=length(test_AMD$AMD.Close))
plot(ets_forecast_AMD)
MAPE(ets_forecast_AMD$mean, test_AMD$AMD.Close)* 100
summary(ets_forecast_AMD)
