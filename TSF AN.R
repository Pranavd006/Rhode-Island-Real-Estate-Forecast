install.packages("tseries")
library(forecast)
library(tseries)
library(ggplot2)
library(readr)

data <- read_csv("MEDLISPRIRI.csv")

data$observation_date <- as.Date(data$observation_date)

housing_ts <- ts(
  data$MEDLISPRIRI,
  start = c(2016,7),
  frequency = 12
)

# ----------------------
# EDA
# ----------------------

autoplot(housing_ts)

summary(housing_ts)

Acf(housing_ts)

Pacf(housing_ts)

adf.test(housing_ts)

# ----------------------
# Train/Test Split
# ----------------------

n <- length(housing_ts)

test_size <- 12

train <- head(housing_ts, n - test_size)

test <- tail(housing_ts, test_size)

# ----------------------
# Benchmark Models
# ----------------------

fit_mean <- meanf(train, h=test_size)

fit_naive <- naive(train, h=test_size)

fit_drift <- rwf(train,
                 drift=TRUE,
                 h=test_size)

fit_snaive <- snaive(train,
                     h=test_size)

# ----------------------
# TSLM
# ----------------------

fit_tslm <- tslm(train ~ trend + season)

fc_tslm <- forecast(fit_tslm,
                    h=test_size)

# ----------------------
# ETS
# ----------------------

fit_ets <- ets(train)

fc_ets <- forecast(fit_ets,
                   h=test_size)

# ----------------------
# ARIMA
# ----------------------

ndiffs(train)

Acf(diff(train))

Pacf(diff(train))

fit_arima <- auto.arima(train)

fc_arima <- forecast(
  fit_arima,
  h=test_size
)

# ----------------------
# Accuracy Comparison
# ----------------------

acc_mean <- accuracy(fit_mean, test)

acc_naive <- accuracy(fit_naive, test)

acc_drift <- accuracy(fit_drift, test)

acc_snaive <- accuracy(fit_snaive, test)

acc_tslm <- accuracy(fc_tslm, test)

acc_ets <- accuracy(fc_ets, test)

acc_arima <- accuracy(fc_arima, test)

acc_mean
acc_naive
acc_drift
acc_snaive
acc_tslm
acc_ets
acc_arima

# ----------------------
# Diagnostics
# ----------------------

checkresiduals(fit_arima)

checkresiduals(fit_ets)

# ----------------------
# Final Forecast
# ----------------------

best_model <- fit_ets

final_fc <- forecast(
  best_model,
  h=12
)

autoplot(final_fc)

final_fc