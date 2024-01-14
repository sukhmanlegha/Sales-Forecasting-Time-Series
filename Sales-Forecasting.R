## Install forecast library

install.packages("forecast")
library(forecast)
library(zoo)


# Set working directory for locating files.
setwd("C:/Users/STSC/Downloads/Time Series Case 1")

# Create data frame.
sales.data <- read.csv("673_case1.csv")

# See the first 6 records of the file.
head(sales.data)
tail(sales.data)




## Question 1(a)

# Create a time series data set using the ts() function.
sales.ts <- ts(sales.data$Sales, start = c(2015, 1), end = c(2022, 12), freq = 12)

# Print the data set
sales.ts



## Question 1(b)

# Plot the time series.
plot(sales.ts, main = "Monthly Sales from 2015 to 2022", 
     ylab = "Sales (in millions of dollars)")



## Question 1(c)

# Autocorrelation chart using the Acf() function
sales.autocor <- Acf(sales.ts, main = "Autocorrelation Chart for Sales Data")


# Display autocorrelation coefficients for various lags.
Lag <- round(sales.autocor$lag, 0)
ACF <- round(sales.autocor$acf, 3)
data.frame(Lag, ACF)





## Question 2(a)

# Define the numbers of months in the training and validation sets.
nTrain <- 72 
nValid <- 24

# Split the time series into training and validation partitions
train.ts <- window(sales.ts, start = c(2015, 1), end = c(2020, 12))
valid.ts <- window(sales.ts, start = c(2021, 1), 
                   end = c(2022, 12))



## Question 2(b)

# To create three trailing moving averages with window widths of 3, 8, and 12
ma3.trail <- rollmean(train.ts, k = 3, align = "right", fill = NA)
ma8.trail <- rollmean(train.ts, k = 8, align = "right", fill = NA)
ma12.trail <- rollmean(train.ts, k = 12, align = "right", fill = NA)

ma3.trail
ma8.trail
ma12.trail



## Question 2(c)

# Create a trailing MA forecast for validation period.
ma3.trail.pred <- forecast(ma3.trail, h = nValid, level = 0)
ma8.trail.pred <- forecast(ma8.trail, h = nValid, level = 0)
ma12.trail.pred <- forecast(ma12.trail, h = nValid, level = 0)

ma3.trail.pred
ma8.trail.pred
ma12.trail.pred



## Question 2(d)

# Calculate the accuracy measures for each trailing MA forecast
round(accuracy(ma3.trail.pred, valid.ts), 3)
round(accuracy(ma8.trail.pred, valid.ts), 3)
round(accuracy(ma12.trail.pred, valid.ts), 3)




## Question 3(a)

# To develop a regression model with linear trend and seasonality for 
# training partition.
trend.seas <- tslm(train.ts ~ trend + season)

# To print the model summary
summary(trend.seas)

# To create regression forecast with trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred



## Question 3(b)

# Identify and display regression residuals for training partition.
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Trailing MA for residuals with window width 3 for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res


# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

## Question 3(c)

# To develop a two-level forecast for validation period
first.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
first.2level

# To create a table with validation data, regression forecast, trailing MA for 
# residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             first.2level), 3)
names(valid.df) <- c("Ridership", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# To identify common accuracy measures.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(first.2level, valid.ts), 3)



## Question 3(d)


# To fit a regression model with linear trend and seasonality for entire data set.
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
summary(tot.trend.seas)

# To identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# To use trailing MA to with window width of 3 for regression residuals
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

# To create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# To develop 2-level forecast for future 12 periods
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level


# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df



## Question 3(e)


# Use snaive() to make seasonal naive forecast for validation data. 
sales.snaive.pred <- snaive(train.ts, h = nValid)
sales.snaive.pred

# Use accuracy() function to identify common accuracy measures.
round(accuracy(tot.trend.seas.pred$fitted, valid.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, valid.ts), 3)
round(accuracy((snaive(valid.ts))$fitted, valid.ts), 3)



## Question 4(a)

# To develop HW Model
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

# To make prediction using HW Model
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred




## Question 4(b)


# Create Holt-Winter's (HW) exponential smoothing for entire data set. 
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ 


# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred



## Question 4(c)

# Use accuracy() function to identify common accuracy measures.
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)