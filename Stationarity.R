
# Relevant packages
install.packages("tseries")
install.packages("lubridate")
install.packages("adf.test")
install.packages("zoo")

library(tseries)
library(zoo)
library(adf.test)
library(tidyverse)
library(lubridate)

# Loading Data
data <- read.csv("C:/Users/conta/OneDrive/Desktop/Time Series Project/Time Series/sales-of-shampoo-over-a-three-ye.csv") 

# Rename the variable to a shorter name
data <- data %>% rename(Sales = Sales.of.shampoo.over.a.three.year.period)

data

# Check class
class(data)

# Convert month column to Date format
data$Month <- as.Date(paste0(data$Month, "-01"), format = "%d-%b-%y")

# Create time series format
ts_data <- ts(data$Sales, start = c(2001, 1), frequency = 12)

ts_data

class(ts_data)
#This tells you that the data series is in a time series format

start(ts_data)
#This is the start of the time series

end(ts_data)
#This is the end of the time series

frequency(ts_data)
#The cycle of this time series is 12months in a year

summary(ts_data)
#Detailed Metrics

plot.ts(ts_data, xlab='Year', ylab='Sales', main='Sales by Year', col='blue', lwd=3)
#This will plot the time series

abline(reg=lm(ts_data~time(ts_data)))
# This will fit in a line

#Here are a few more operations to do:

cycle(ts_data)
#This will print the cycle across years.

plot(aggregate(ts_data,FUN=mean))
#This will aggregate the cycles and display a year on year trend

boxplot(ts_data~cycle(ts_data))
#Box plot across months will give us a sense on seasonal effect

#Augmented Dickey-Fuller Test
result_adf <- adf.test(ts_data)
print(result_adf)

#P value not less than 0.05, null hypothesis is not rejected


#Applying Differencing

diff_ts_data <- diff(ts_data)

result_adf2 <- adf.test(diff_ts_data)
print(result_adf2)

#P value is less than 0.05, null hyopthesis is rejected

# plotting the differenced data
plot.ts(diff_ts_data, xlab='Year', ylab='Sales', main='Sales by Year', col='blue', lwd=3)

abline(reg=lm(diff_ts_data~time(diff_ts_data)))
# fit in a line



