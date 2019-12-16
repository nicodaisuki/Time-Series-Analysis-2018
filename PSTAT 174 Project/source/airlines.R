library(tseries)
library(forecast)
library(ggplot2)

# international-airline-passengers.csv contains airline passenger totals indexed by month 
data_dir <- '/Users/timmy/Documents/school/year-4/fall/pstat175_survival_analysis/final_project/data'
data_path <- file.path(data_dir, 'international-airline-passengers.csv')
airline <- read.csv(data_path)
colnames(airline) <- c('month', 'passengers')

# splitting into train (first 90%) and test (last (10%)) set
train_indices <- c(1:132) # leaving out last year
train_airline <- airline[train_indices,]
test_airline <- airline[-train_indices,]

# plotting time series
data <- airline$passengers
plot(ts(data))
# plot shows seasonality, an upward trend, and increasing variance over time


# apply difference
diff_data <- diff(log(data), lag = 12)
plot(ts(diff_data))
abline(lm(diff_data ~ as.numeric(1:length(diff_data))))
var(diff_data)

diff2_data <- diff(diff_data, lag = 1)
plot(ts(diff2_data))
abline(lm(diff2_data ~ as.numeric(1:length(diff2_data))))
var(diff2_data)

decomp_data <- decompose(ts(diff2_data, frequency = 12))
autoplot(decomp_data, xlab='Time (y)')