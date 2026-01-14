
# Lab 9: temporal data ----------------------------------------------------


library(TTR)      # for time series
library(forecast) # for forecasting
library(ggplot2)  # for data visualization
library(dplyr)    # for data management


# Google trends data
## I'll use https://trends.google.com/trends/explore?q=pratt%20institute&date=all&geo=US&legacy&hl=en
data <- read.csv("pratt_institute_trends.csv")



# Q1: format & decompose time series --------------------------------------

## format data as time-series
ts <- ts(data = data$pratt.institute...United.States., 
               start=c(2004, 1),
               frequency = 12)

## plot
autoplot(ts) + 
  ylim(0, 100) + 
  ggtitle('Google searches for "Pratt Institute" since 2004') 


## decompose the series
decomposed <- decompose(ts)
autoplot(decomposed) +
  ylim(0, 100) + 
  ggtitle('Google searches for "Pratt Institute" since 2004') 



# Q2: temporal autocorrelation --------------------------------------------

# View the autocorrelation
ggAcf(ts) +
  labs(title = "Temporal Autocorrelation",
       subtitle = "Google searches for 'Pratt Institute' trend series")

# test for autocorrelation
## H0: data are temporally random
## HA: data are not temporally random
Box.test(ts, type="Ljung-Box") 



# Q3: forecasting ---------------------------------------------------------

# single exponential smoothing
ses_forecast <- ses(y = ts, h = 24)
autoplot(ses_forecast)
summary(ses_forecast)
checkresiduals(ses_forecast)

# double exponential smoothing (Holt linear)
holt_forecast <- holt(y = ts, h = 24)
autoplot(holt_forecast)
summary(holt_forecast)
checkresiduals(holt_forecast)

# triple exponential smoothing (Holt-Winters method)
hw_forecast <- hw(y = ts, h = 24)
autoplot(hw_forecast)
summary(hw_forecast)
checkresiduals(hw_forecast)

