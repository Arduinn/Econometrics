rm(list=ls())

# Load necessary libraries
library(tidyverse)
library(tseries)
library(quantmod)
library(forecast)

# Importing and Cleaning Data ---------------------------------------------

# Get data
getSymbols("USDBRL=X", src = "yahoo", from = "2014-01-01", to = "2024-07-05")

# Convert to data frame
dataBrl <- as.data.frame(`USDBRL=X`)

# Add the date column
dataBrl$date <- index(`USDBRL=X`)

# Reorder columns if necessary
dataBrl <- dataBrl[, c("date", names(dataBrl)[-ncol(dataBrl)])]

# Selecting Columns
dataBrl <- dataBrl[,c(1,4)]

# Renaming Columns
colnames(dataBrl) <- c('Date', 'Value')

# Cleaning Data
dataBrl$Date <- as.Date(dataBrl$Date, format="%Y-%m-%d")
dataBrl$Value <- as.numeric(dataBrl$Value)

# Remove NAs values
dataBrl <- na.omit(dataBrl)

# Convert to time series object
dataBrl_ts <- ts(dataBrl$Value)  # Assuming daily data with about 252 trading days a year

# Check for Stationarity --------------------------------------------------

# Perform Augmented Dickey-Fuller test
adf_test <- adf.test(dataBrl_ts)
adf_test # Null Hypothesis not rejected

diff1_dataBrl_ts <- diff(dataBrl_ts)
adf_test <- adf.test(diff1_dataBrl_ts)
adf_test # Null Hypothesis rejected

# Check ACF and PACF of the selected series
acf(diff1_dataBrl_ts, main="ACF of Selected Series")
pacf(diff1_dataBrl_ts, main="PACF of Selected Series") # There are some signifcant Lags

# Perform Ljung-Box test on selected series
Box.test(diff1_dataBrl_ts, lag=20, type="Ljung-Box")

# Model -------------------------------------------------------------------

# Fit ARIMA model
arima_model <- auto.arima(diff1_dataBrl_ts, seasonal=FALSE)
summary(arima_model)

# Check residuals
checkresiduals(arima_model)

# Perform Ljung-Box test on residuals
ljung_box_test <- Box.test(residuals(arima_model), lag=20, type="Ljung-Box")
print(ljung_box_test)

# Forecasting
h <- 30  # Number of periods to forecast
forecasts <- forecast(arima_model, h=h)
plot(forecasts)
