# Created 05/06/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# https://rpubs.com/JSHAH/481706
# https://medium.com/analytics-vidhya/interpreting-acf-or-auto-correlation-plot-d12e9051cd14
# https://towardsdatascience.com/identifying-ar-and-ma-terms-using-acf-and-pacf-plots-in-time-series-forecasting-ccb9fd073db8
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

# PLAN
# Time series in R
# Autocorrelation
# ACF/PACF
# ARIMA

# What is ARMA model?
# Yt follows an ARMA(p,q) if and only if it can be 
# represented as a combination of an Autoregressive 
# model AR(p) and a Moving Average model of order q, MA(q).

# What is ARIMA model?
# Variant of ARMA
# ARIMA(p,r,q), where p is the number of lags for the 
# autoregressive part, q the number of lags of the Moving 
# average part and r is the number of time we should 
# differentiate in order to obtain a stationary ARMA model.

library('ggplot2')
library('forecast')
library('tseries')

# load data
data("AirPassengers")
?AirPassengers

# check if it is time series?
is.ts(AirPassengers)

summary(AirPassengers)
start(AirPassengers)
end(AirPassengers)

time(AirPassengers)
# Frequency across the years
frequency(AirPassengers)


# Plot the time series
ts.plot(AirPassengers, xlab="Year", 
        ylab="Number of Passengers", 
        main="Monthly totals of international airline passengers, 1949-1960")

# Fit in a line
abline(reg=lm(AirPassengers~time(AirPassengers)))


# Autocorrelation
# A time series is a sequence of measurements of the same 
# variable(s) made over time. Usually, the measurements 
# are made at evenly spaced times — for example, 
# monthly or yearly. The coefficient of correlation between 
# two values in a time series is called the autocorrelation 
# function (ACF). In other words, Autocorrelation represents 
# the degree of similarity between a given time series and a 
# lagged version of itself over successive time intervals.

# Autocorrelation measures the relationship between a 
# variable’s current value and its past values.
# An autocorrelation of +1 represents a perfect positive 
# correlation, while an autocorrelation of negative 1 
# represents a perfect negative correlation.


# Why do we need it?
# 1. Help us uncover hidden patterns in our data and help us 
# select the correct forecasting methods.
# 2. Help identify seasonality in our time series data.
# 3. Analyzing the autocorrelation function (ACF) and partial 
# autocorrelation function (PACF) in conjunction is necessary 
# for selecting the appropriate ARIMA model for any time series 
# prediction.

?acf
acf(AirPassengers)

?arima
AR <- arima(AirPassengers, order = c(1,0,0))
# Here order is a specification of the non-seasonal part of 
# the ARIMA model: the three integer components (p, d, q) 
# are the AR order, the degree of differencing, and the MA order.
print(AR)

# Plot time series with fitted data
ts.plot(AirPassengers)
AR_fit <- AirPassengers - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)

# Forecasting
# 1-step forecast
predict_AR <- predict(AR)

# 1-step forecast using $pred[1]
predict_AR$pred[1]

# Alternatively Using predict to make 1-step 
# through 10-step forecasts
predict(AR, n.ahead = 10)

# time series plus the forecast and 95% prediction intervals
ts.plot(AirPassengers, xlim = c(1949, 1961))
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, 
       type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, 
       type = "l", col = 2, lty = 2)

# Moving average
# Fitting the MA model to AirPassengers
MA <- arima(AirPassengers, order = c(0,0,1))
print(MA)

# Time series with the MA fitted values
ts.plot(AirPassengers)
MA_fit <- AirPassengers - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)

# Forecasting using MA
# 1-step forecast based on MA
predict_MA <- predict(MA)
predict_MA$pred[1]

predict(MA,n.ahead=10)

ts.plot(AirPassengers, xlim = c(1949, 1961))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)

# AIC/BIC, lower - better
AIC(AR)
AIC(MA)
BIC(AR)
BIC(MA)

# Another example 1
# Age of Death of Successive Kings of England
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
kingstimeseries <- ts(kings)
kingstimeseries
# Number of births per month in New York city, 
# from January 1946 to December 1959 
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
# monthly sales for a souvenir shop at a beach resort 
# town in Queensland, Australia, for January 1987-December 1993
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries


plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

library("TTR")
# Try to get rid of random fluctuations using moving average
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
# Higher order smoothign:
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# Decomposition and substraction
# Lets take a look at decomposition:
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - 
    birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
# the seasonal variation has been removed from the seasonally 
# adjusted time series. The seasonally adjusted time 
# series now just contains the trend component and an 
# irregular component.

# Annual rainfall in inches for London, from 1813-1912
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

# To make forecasts using simple exponential smoothing in R, 
# we can fit a simple exponential smoothing predictive model 
# using the “HoltWinters()” function in R.
?HoltWinters
rainseriesforecasts <- HoltWinters(rainseries, 
                                   beta=FALSE, 
                                   gamma=FALSE)
rainseriesforecasts
# Alpha is very close to zero, telling us that the 
# forecasts are based on both recent and less recent 
# observations (although somewhat more weight is placed on 
# recent observations).

plot(rainseriesforecasts)

# You can specify the initial value for the level in the 
# HoltWinters() function by using the “l.start” parameter
# Ex. the first value is 23.56 (inches) for rainfall in 1813
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)

library("forecast")
rainseriesforecasts2 <- forecast:::forecast.HoltWinters(rainseriesforecasts, 
                                             h=8)

plot(rainseriesforecasts2)
# Here the forecasts for 1913-1920 are plotted as a blue line, 
# the 80% prediction interval as an orange shaded area, 
# and the 95% prediction interval as a yellow shaded area
acf(na.exclude(rainseriesforecasts2$residuals), lag.max=20)
# autocorrelation at lag 3 is just touching the significance 
# bounds


# To test whether there is significant evidence for non-zero 
# correlations
Box.test(rainseriesforecasts2$residuals, 
         lag=20, type="Ljung-Box")
# little evidence of non-zero autocorrelations in the in-sample 
# forecast errors at lags 1-20

plot.ts(rainseriesforecasts2$residuals)


# To check whether the forecast errors are normally distributed 
# with mean zero, we can plot a histogram of the forecast errors

plotForecastErrors <- function(forecasterrors) {
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd   <- sd(forecasterrors)
    mymin  <- min(forecasterrors) - mysd*5
    mymax  <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(na.exclude(rainseriesforecasts2$residuals))


# Holt’s Exponential Smoothing
# Annual diameter of women’s skirts at the hem
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, 
                                     gamma=FALSE)
plot(skirtsseriesforecasts)
skirtsseriesforecasts2 <- forecast:::forecast.HoltWinters(skirtsseriesforecasts, 
                                               h=19)
plot(skirtsseriesforecasts2)
acf(na.exclude(skirtsseriesforecasts2$residuals), lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, 
         lag=20, type="Ljung-Box")
# there is little evidence of non-zero autocorrelations 
# at lags 1-20


# ARIMA
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseries)
plot.ts(skirtsseriesdiff1)
# The resulting time series of first differences (above) 
# does not appear to be stationary in mean.

skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
# appear to be stationary in mean and variance
# Formal tests for stationarity called “unit root tests”

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)

# Selecting candidate ARIMA model
# If your time series is stationary, or if you have transformed 
# it to a stationary time series by differencing d times, 
# the next step is to select the appropriate ARIMA model, 
# which means finding the values of most appropriate values 
# of p and q for an ARIMA(p,d,q) model.

acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1, lag.max=20, plot = F)
# autocorrelation at lag 1 (-0.360) exceeds the significance 
# bounds, but all other autocorrelations between lags 1-20 do 
# not exceed the significance bound

pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20, plot = F)
# lags 1, 2 and 3 exceed the significance bounds, are negative, 
# and are slowly decreasing in magnitude with increasing 
# lag (lag 1: -0.360, lag 2: -0.335, lag 3:-0.321). 
# The partial autocorrelations tail off to zero after lag 3

# Since the correlogram is zero after lag 1, and the partial 
# correlogram tails off to zero after lag 3, this means that 
# the following ARMA (autoregressive moving average) models are 
# possible for the time series of first differences:
    
# ARMA(3,0) model, that is, an autoregressive 
#        model of order p=3, since the partial autocorrelogram 
#        is zero after lag 3, and the autocorrelogram tails off 
#        to zero (although perhaps too abruptly for this model 
#        to be appropriate)
# ARMA(0,1) model, that is, a moving average model of order q=1, 
#        since the autocorrelogram is zero after lag 1 and the 
#        partial autocorrelogram tails off to zero
# ARMA(p,q) model, that is, a mixed model with p and q greater 
#        than 0, since the autocorrelogram and partial 
#        correlogram tail off to zero (although the correlogram 
#        probably tails off to zero too abruptly for this model 
#        to be appropriate)

# The ARMA(3,0) model has 3 parameters, the ARMA(0,1) model 
# has 1 parameter, and the ARMA(p,q) model has at least 2 
# parameters. Therefore, the ARMA(0,1) model is taken as 
# the best model.

# Since an ARMA(0,1) model (with p=0, q=1) is taken to be the 
# best candidate model for the time series of first differences 
# of the ages at death of English kings, then the original time 
# series of the ages of death can be modelled using an 
# ARIMA(0,1,1) model (with p=0, d=1, q=1, where d is the 
# order of differencing required).
auto.arima(kings)

# Example

# data on the volcanic dust veil index in the northern 
# hemisphere, from 1500-1969 (original data from Hipel and Mcleod, 1994)
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)

acf(volcanodustseries, lag.max=20)
acf(volcanodustseries, lag.max=20, plot=FALSE)
# The autocorrelations for lags 1, 2, 3 are positive, and 
# decrease in magnitude with increasing lag

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)
# partial autocorrelation at lag 1 is positive and exceeds 
# the significance bounds (0.666), while the partial 
# autocorrelation at lag 2 is negative and also exceeds the 
# significance bounds (-0.126)

# Since the correlogram tails off to zero after lag 3, 
# and the partial correlogram is zero after lag 2, 
# the following ARMA models are possible for the time series:
    
# ARMA(2,0) pacf is zero after lag 2, and the correlogram tails off to zero after lag 3, and the partial correlogram is zero after lag 2
# ARMA(0,3) acf is zero after lag 3, and the partial correlogram tails off to zero (although perhaps too abruptly for this model to be appropriate)
# ARMA(p,q) mixed model, since the correlogram and partial correlogram tail off to zero (although the partial correlogram perhaps tails off too abruptly for this model to be appropriate)


# The ARMA(2,0) model has 2 parameters, the ARMA(0,3) model has 
# 3 parameters, and the ARMA(p,q) model has at least 2 parameters. 
# Therefore, using the principle of parsimony, the ARMA(2,0) 
# model and ARMA(p,q) model are equally good candidate models.
auto.arima(volcanodust, ic = "bic")

# Forecasting
# fit an ARIMA(0,1,1) model
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) 
kingstimeseriesarima

kingstimeseriesforecasts <- forecast:::forecast.Arima(kingstimeseriesarima, h=5)
plot(kingstimeseriesforecasts)

# Good idea to check:
# forecast errors of an ARIMA model are normally distributed 
#       with mean zero and constant variance
# and whether the are correlations between successive forecast 
#       errors

acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(kingstimeseriesforecasts$residuals)
plotForecastErrors(kingstimeseriesforecasts$residuals)


# Lets do the same for the volcanic data
volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima

volcanodustseriesforecasts <- forecast:::forecast.Arima(volcanodustseriesarima, 
                                             h=31)
volcanodustseriesforecasts
plot(volcanodustseriesforecasts)
# We predicted negative values. 
# ARIMA has no idea that values should be positive

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, 
         lag=20, 
         type="Ljung-Box")

plot.ts(volcanodustseriesforecasts$residuals) 
plotForecastErrors(volcanodustseriesforecasts$residuals)

# time series of forecast errors seems to have a negative mean, 
# rather than a zero mean

mean(volcanodustseriesforecasts$residuals)

# The histogram of forecast errors (above) shows that although 
# the mean value of the forecast errors is negative, the 
# distribution of forecast errors is skewed to the right 
# compared to a normal curve. Therefore, it seems that we 
# cannot comfortably conclude that the forecast errors are 
# normally distributed with mean zero and constant variance! 
# Thus, it is likely that our ARIMA(2,0,0) model for the time 
# series of volcanic dust veil index is not the best model 
# that we could make, and could almost definitely be improved 
# upon!






# Another example 2 (extra material)
library(astsa)
library(psych)
data(rec)
?rec
summary(rec)

data<-ts(rec)
plot(rec, col='red')

mean_ts<-mean(data)
mean_ts
variance_ts<-var(data)
variance_ts

auto_correlation<-acf(data,plot= TRUE,type = 'correlation',
                      main='ACF Plot',col='green')

auto_correlation

acvf<-acf(data,plot= TRUE,type = 'covariance',
          main='AVCF Plot',col='green')
acvf

pacf<-pacf(data,plot=TRUE,main="PACF Plot",col='green')
pacf
