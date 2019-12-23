library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(XML)
library(tidytext)
library(wordcloud)
library(dslabs)
library(caret)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# relative path to file
file <- ("aapl.us.txt")

# Read csv file into environment
aapl <- read_csv(file)

# Split the data set by date ranges 80% train 20% test
aapl_train <- aapl[1:round(nrow(aapl) * 0.8),]
aapl_test <- na.omit(aapl[round(nrow(aapl) * 0.8) + 1:nrow(aapl),])

# Test for the number of rows in each set 
nrow(aapl_train)
nrow(aapl_test)

# Top 6 rows from the training data
head(aapl_train)

# plot of training data over time
aapl_train %>%
  ggplot() +
  geom_line(aes(Date, Close))
  
# Using the previous value as the prediction for the current value
aapl_train <- mutate(aapl_train, PrevClose = lag(Close))

# plot of training data over time with the first prediction
aapl_train %>%
  ggplot() +
  geom_line(aes(Date, Close, colour = "red")) +
  geom_line(aes(Date, PrevClose, colour = "blue")) 



