library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(XML)
library(tidytext)
library(wordcloud)
library(dslabs)
library(caret)
library(pracma)
library(MASS)
library(tseries)
library(forecast)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# relative path to file
file <- ("aapl.us.txt")

# Read csv file into environment
aapl <- read_csv(file)

# Split the data set by date ranges 80% train 20% test
aapl_train <- aapl[1:round(nrow(aapl) * 0.95),]
aapl_test <- na.omit(aapl[round(nrow(aapl) * 0.95) + 1:nrow(aapl),])

# Test for the number of rows in each set 
nrow(aapl_train)
nrow(aapl_test)

# RMSE function defined
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Top 6 rows from the training data
head(aapl_train)

# plot of training data over time
aapl_train %>%
  ggplot() +
  geom_line(aes(Date, Close))

# plot of test data over time
aapl_test %>%
  ggplot() +
  geom_line(aes(Date, Close))
  
# Using the previous value as the prediction for the current value
aapl_test <- mutate(aapl_test, PrevClose = lag(Close))
aapl_test <- na.omit(aapl_test)

# plot of test data over time with the prev close prediction
aapl_test %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Close), colour = "black") +
  geom_line(aes(y = PrevClose), colour = "red")

# RMSE on test data set for prev close
naive_rmse <- RMSE(aapl_test$Close, aapl_test$PrevClose)

rmse_results <- data_frame(method = "Previous Value Prediction", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

# Range of moving averages to test
windows <- seq(2, 20, 1)

# function to test the moving averages and compare RMSE
RMSEs <- sapply(windows, function(w){
  
  aapl_train <- mutate(aapl_train, MovingAvg = movavg(aapl_train$Close, w, type=c("s")))
  
  return(RMSE(aapl_train$Close, aapl_train$MovingAvg))
})

# plot of moving average windows to RMSE
qplot(windows, RMSEs)

# Best moving average window value
best_window <- windows[which.min(RMSEs)]
best_window

# using the best moving average window to determine the close 
aapl_test <- mutate(aapl_test, MovingAvg = movavg(aapl_test$Close, best_window, type=c("s")))

# plot of test data over time with the prev close and moving average prediction
aapl_test %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Close), colour = "black") +
  geom_line(aes(y = PrevClose), colour = "red") +
  geom_line(aes(y = MovingAvg), colour = "green")

# RMSE for moving average
naive_rmse <- RMSE(aapl_test$Close, aapl_test$MovingAvg)

# Adding to the RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Moving Average Prediction",  
                                     RMSE = naive_rmse))






# prediction using ARIMA setting the close to log
LnClose = log(aapl_train$Close)
LnClose

# corilation between the current and previous closes
acf(LnClose, lag.max = 20)
 
# Indicates that the time series data is stationary meaning its good for this type of training
pacf(LnClose, lag.max = 20)

# build the time series and set the auto arima
CloseArima <- ts(LnClose, start = c(1984, 09), frequency = 365)
FitCloseLn <- auto.arima(CloseArima)
FitCloseLn
plot(CloseArima, type = 'l')
exp(LnClose)

# forecast out the close price for the duration of the test data
ForecastValueLn = forecast(FitCloseLn, h = 417)
ForecastValueLn
plot(ForecastValueLn)

# retrive the forecast data and convert from log
ForecastValuesExtracted = as.numeric(ForecastValueLn$mean)
FinalForecastValues = exp(ForecastValuesExtracted)
FinalForecastValues

# combined the test and forecast data together in a data frame
df <- data.frame(aapl_test$Close, FinalForecastValues)

# RMSE 
naive_rmse <- RMSE(df$aapl_test.Close, df$FinalForecastValues)

# Adding to the RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="ARIMA Prediction",  
                                     RMSE = naive_rmse))

# Final results
rmse_results








