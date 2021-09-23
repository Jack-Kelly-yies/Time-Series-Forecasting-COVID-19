require(graphics)
library(forecast)


############################################################### read in data

Covid_Data <- read.csv("COVID DATA.csv", header=TRUE) #both cases and deaths for march and april in italy 
Covid_Data

Covid_Data_Cases <- read.csv("COVID_CASES_ITALY_MARCH_APRIL.csv", header=TRUE) #confirmed cases in italy 
Covid_Data_Cases

Covid_Data_Cases_May <- read.csv("COVID_CASES_ITALY_MAY.csv", header=TRUE)
Covid_Data_Cases_May

Covid_Data_Deaths <- read.csv("COVID_DEATHS_ITALY_MARCH_APRIL.csv", header=TRUE)
Covid_Data_Deaths

Covid_Data_Deaths_May <- read.csv("COVID_DEATHS_ITALY_MAY.csv", header=TRUE)
Covid_Data_Deaths_May

Covid_Data.ts <- ts(Covid_Data)
Covid_Data.ts

Covid_Data_Cases.ts <- ts(Covid_Data_Cases)
Covid_Data_Cases.ts

Covid_Data_Deaths.ts <- ts(Covid_Data_Deaths)
Covid_Data_Deaths.ts

Covid_Data_Cases_May.ts <- ts(Covid_Data_Cases_May, start = 62)
Covid_Data_Cases_May.ts

Covid_Data_Deaths_May.ts <- ts(Covid_Data_Deaths_May, start = 62)
Covid_Data_Deaths_May.ts

CasesForecast120 <- read.csv("Cases120Forecast.csv", header=FALSE)
CasesForecast120

CasesForecast820 <- read.csv("Cases820Forecast.csv", header=FALSE)
CasesForecast820

DeathsForecast322 <- read.csv("Deaths322Forecast.csv", header=FALSE)
DeathsForecast322

DeathsForecast820 <- read.csv("Deaths820Forecast.csv", header=FALSE)
DeathsForecast820

############################################################### plotted time series

plot.ts(Covid_Data.ts, las=1, type="o", pch=20, main="Time Series of Daily COVID-19 Confirmed Cases & Deaths in Italy for March & April", xlab="Days")

############################################################### plots of the simple exponential smoothing 

fitCases <- ses(Covid_Data_Cases.ts, h=9) #let system determine optimal alpha value
fitCases$model
plot(fitCases, PI = TRUE,xlab="Index", ylab="Confirmed COVID-19 Cases", main="Time Series Vs. Exponential Smoothing Fitted model of COVID-19 Confirmed Cases (9 Days forecasting)", fcol="white",col="red", type="o")
lines(fitted(fitCases), col="blue", type="o")
lines(fitCases$mean, col="blue", type="o")
legend("topleft",lty=1, col=c("red","blue"),
       c("Covid_Data_Cases", "Simple Exponential Smoothing Fitted model"), pch=1)


fitDeaths <- ses(Covid_Data_Deaths.ts, h=9) #let system determine optimal alpha value
fitDeaths$model
plot(fitDeaths, PI = TRUE,xlab="Index", ylab="Confirmed COVID-19 Deaths", main="Time Series Vs. Exponential Smoothing Fitted model of COVID-19 Confirmed Deaths (9 Days forecasting)", fcol="white",col="red", type="o")
lines(fitted(fitDeaths), col="blue", type="o")
lines(fitDeaths$mean, col="blue", type="o")
legend("topleft",lty=1, col=c("red","blue"),
       c("Covid_Data_Deaths", "Simple Exponential Smoothing Fitted model"), pch=1)

############################################################### residuals for fitted models

Residuals_Cases <- Covid_Data_Cases.ts - fitCases$fitted

Residuals_Deaths <- Covid_Data_Deaths.ts - fitDeaths$fitted

############################################################### COVID CASEs MAE, RMSE, MAPE
#
MAE_Cases <- mean(abs(Residuals_Cases))
MAE_Cases
#
# Root Mean Squared Error
#
RMSE_Cases <- (mean(Residuals_Cases^2))^0.5
RMSE_Cases

#   Mean Absolute Percentage Error
#
MAPE_Cases <- mean(abs(100*Residuals_Cases/Covid_Data_Cases.ts))
MAPE_Cases

############################################################### COVID DEATHS MAE, RMSE, MAPE

MAE_Deaths <- mean(abs(Residuals_Deaths))
MAE_Deaths
#
# Root Mean Squared Error
#
RMSE_Deaths <- (mean(Residuals_Deaths^2))^0.5
RMSE_Deaths

#   Mean Absolute Percentage Error
#
MAPE_Deaths <- mean(abs(100*Residuals_Deaths/Covid_Data_Deaths.ts))
MAPE_Deaths

############################################################### Plot of the ACF to gauge stationarity 

Covid_Data_Cases.ts

Covid_Data_Deaths.ts

acf(Covid_Data_Cases.ts, lag.max=60, main ="ACF for Time Series of COVID-19 Confirmed Cases in Italy over March - April")

acf(Covid_Data_Deaths.ts, lag.max=60, main ="ACF for Time Series of COVID-19 Confirmed Deaths in Italy over March - April")

############################################################### Taking the difference of the series to attain stationarity

acf(diff(Covid_Data_Cases.ts, lag=1), lag.max = 60, main = "1st Difference ACF for Time Series of COVID-19 Confirmed Cases in Italy over March - April")

acf(diff(diff(Covid_Data_Cases.ts, lag=1)), lag.max = 20, main = "2nd Difference ACF for Time Series of COVID-19 Confirmed Cases in Italy over March - April")

ndiffs(Covid_Data_Cases.ts)

acf(diff(Covid_Data_Deaths.ts, lag=1), lag.max = 60, main = "1st Difference ACF for Time Series of COVID-19 Confirmed Deaths in Italy over March - April")

acf(diff(diff(Covid_Data_Deaths.ts, lag=1)), lag.max = 20, main = "2nd Difference ACF for Time Series of COVID-19 Confirmed Deaths in Italy over March - April")

ndiffs(Covid_Data_Deaths.ts)

############################################################### Plotting the PACF of the stationary series

Yt_Cases <- diff(diff(Covid_Data_Cases.ts, lag=1))
pacf(Yt_Cases, lag.max=10, main = "2nd Difference P.ACF for Time Series of COVID-19 Confirmed Cases in Italy over March - April")

Yt_Deaths <- diff(diff(Covid_Data_Deaths.ts, lag=1))
pacf(Yt_Deaths, lag.max=10, main = "2nd Difference P.ACF for Time Series of COVID-19 Confirmed Deaths in Italy over March - April")

############################################################### Fitting ARIMA Models

################################# CASES ARIMA(1,2,0)

ttt_Cases120 <- arima(Covid_Data_Cases.ts, order = c(1, 2, 0), method = c("ML"))

print(ttt_Cases120)
summary(ttt_Cases120)
autoplot(ttt_Cases120)
acf(ttt_Cases120$residuals)

################################# CASES ARIMA(8,2,0)

ttt_Cases820 <- arima(Covid_Data_Cases.ts, order = c(8, 2, 0), method = c("ML"))

print(ttt_Cases820)
summary(ttt_Cases820)
autoplot(ttt_Cases820)
acf(ttt_Cases820$residuals)

################################# DEATHS ARIMA(3,2,2)

ttt_Deaths322 <- arima(Covid_Data_Deaths.ts, order = c(3, 2, 2), method = c("ML"))

print(ttt_Deaths322)
summary(ttt_Deaths322)
autoplot(ttt_Deaths322)
acf(ttt_Deaths322$residuals)

################################# DEATHS ARIMA(8,2,0)

ttt_Deaths820 <- arima(Covid_Data_Deaths.ts, order = c(8, 2, 0), method = c("ML"))

print(ttt_Deaths820)
summary(ttt_Deaths820)
autoplot(ttt_Deaths820)
acf(ttt_Deaths820$residuals)


############################################################### AutoArima function to give insight on option for best fit

arimaCases <- auto.arima(Covid_Data_Cases.ts, trace = TRUE)
arimaCases

arimaDeaths <- auto.arima(Covid_Data_Deaths.ts, trace = TRUE)
arimaDeaths

############################################################### Ljung-Box tests for all fitted ARIMA models for comparison

numLag = 20 #set lags for all ljung box tests

################################# CASES ARIMA(1,2,0)

Box.test(x=ttt_Cases120$residuals, lag = c(numLag), type = c("Ljung-Box"))

################################# CASES ARIMA(8,2,0)

Box.test(x=ttt_Cases820$residuals, lag = c(numLag), type = c("Ljung-Box"))

################################# DEATHS ARIMA(3,2,2)

Box.test(x=ttt_Deaths322$residuals, lag = c(numLag), type = c("Ljung-Box"))

################################# DEATHS ARIMA(8,2,0)

Box.test(x=ttt_Deaths820$residuals, lag = c(numLag), type = c("Ljung-Box"))


############################################################### Forecasting with choosen ARIMA Models VS ACTUAL VALUES AS THEY HAVE APPEARED

daysAhead = 21

################################# CASES ARIMA(1,2,0) Forecasting #NOT USING

pred_ttt_Cases120 <- predict(ttt_Cases120, n.ahead = daysAhead)
pred_ttt_Cases120
tl <- pred_ttt_Cases120$pred - 1.96 * pred_ttt_Cases120$se
tu <- pred_ttt_Cases120$pred + 1.96 * pred_ttt_Cases120$se
ts.plot(Covid_Data_Cases.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Cases",main="Time Series of COVID-19 Confirmed Cases in Italy over March - May 
        21st & ARIMA(1,2,0) Forecasting for COVID-19 Cases over initial 3 weeks of May", col = c("black", "red","red"))
legend("topleft",lty=c(1,1,1,2), col=c("black","blue","red", "red"),
       c("Recorded March & April Confirmed Cases","Recorded May Confirmed Cases","ARIMA Forecasted May Values", "Forecasting 95% Confidence Interval"), cex = 0.75)
lines(pred_ttt_Cases120$pred, col = "red" , lty = 1)
lines(Covid_Data_Cases_May.ts, col = "blue" , lty = 1)

################################# CASES ARIMA(8,2,0) Forecasting

pred_ttt_Cases820 <- predict(ttt_Cases820, n.ahead = daysAhead)
pred_ttt_Cases820
tl <- pred_ttt_Cases820$pred - 1.96 * pred_ttt_Cases820$se
tu <- pred_ttt_Cases820$pred + 1.96 * pred_ttt_Cases820$se
ts.plot(Covid_Data_Cases.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Cases",main="Time Series of COVID-19 Confirmed Cases in Italy over March - May 
        21st & ARIMA(8,2,0) Forecasting for COVID-19 Cases over initial 3 weeks of May", col = c("black", "red","red"))
legend("topleft",lty=c(1,1,1,2), col=c("black","blue","red", "red"),
       c("Recorded March & April Confirmed Cases","Recorded May Confirmed Cases","ARIMA Forecasted May Values", "Forecasting 95% Confidence Interval"), cex = 0.75)
lines(pred_ttt_Cases820$pred, col = "red" , lty = 1)
lines(Covid_Data_Cases_May.ts, col = "blue" , lty = 1)

################################# DEATHS ARIMA(3,2,2) Forecasting

pred_ttt_Deaths322 <- predict(ttt_Deaths322, n.ahead = daysAhead)
pred_ttt_Deaths322
tl <- pred_ttt_Deaths322$pred - 1.96 * pred_ttt_Deaths322$se
tu <- pred_ttt_Deaths322$pred + 1.96 * pred_ttt_Deaths322$se
ts.plot(Covid_Data_Deaths.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Deaths",main="Time Series of COVID-19 Confirmed Deaths in Italy over March - May 
        21st & ARIMA(3,2,2) Forecasting for COVID-19 Deaths over initial 3 weeks of May", col = c("black", "red","red"))
legend("topleft",lty=c(1,1,1,2), col=c("black","blue","red", "red"),
       c("Recorded March & April Confirmed Deaths","Recorded May Confirmed Deaths","ARIMA Forecasted May Values", "Forecasting 95% Confidence Interval"), cex = 0.75)
lines(pred_ttt_Deaths322$pred, col = "red" , lty = 1)
lines(Covid_Data_Deaths_May.ts, col = "blue" , lty = 1)

################################# DEATHS ARIMA(8,2,0) Forecasting

pred_ttt_Deaths820 <- predict(ttt_Deaths820, n.ahead = daysAhead)
pred_ttt_Deaths820
tl <- pred_ttt_Deaths820$pred - 1.96 * pred_ttt_Deaths820$se
tu <- pred_ttt_Deaths820$pred + 1.96 * pred_ttt_Deaths820$se
ts.plot(Covid_Data_Deaths.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Deaths",main="Time Series of COVID-19 Confirmed Deaths in Italy over March - May 
        21st & ARIMA(8,2,0) Forecasting for COVID-19 Deaths over initial 3 weeks of May", col = c("black", "red","red"))
legend("topleft",lty=c(1,1,1,2), col=c("black","blue","red", "red"),
       c("Recorded March & April Confirmed Deaths","Recorded May Confirmed Deaths","ARIMA Forecasted May Values", "Forecasting 95% Confidence Interval"), cex = 0.75)
lines(pred_ttt_Deaths820$pred, col = "red" , lty = 1)
lines(Covid_Data_Deaths_May.ts, col = "blue" , lty = 1)

################################# Testing Forecasting accuracy

##### Cases ARIMA(8,2,0)

ForecaseErrorCases820error = Covid_Data_Cases_May[,1] - CasesForecast820[,1]
ForecaseErrorCases820error
RMSE_Cases820 = mean(abs(ForecaseErrorCases820error))
RMSE_Cases820

MAPE <- mean(abs(100*ForecaseErrorCases820error/Covid_Data_Cases_May[,1]))
MAPE

##### Deaths ARIMA(3,2,2)

ForecaseErrorDeaths322error = Covid_Data_Deaths_May[,1] - DeathsForecast322[,1]
ForecaseErrorDeaths322error
RMSE_Deaths322 = sum((ForecaseErrorDeaths322error^2)^0.5)/21
RMSE_Deaths322


MAPE <- mean(abs(100*ForecaseErrorDeaths322error/Covid_Data_Deaths_May[,1]))
MAPE

##### Deaths ARIMA(8,2,0)

ForecaseErrorDeaths820error = Covid_Data_Deaths_May[,1] - DeathsForecast820[,1]
ForecaseErrorDeaths820error
RMSE_Deaths820 = sum((ForecaseErrorDeaths820error^2)^0.5)/21
RMSE_Deaths820

MAPE <- mean(abs(100*ForecaseErrorDeaths820error/Covid_Data_Deaths_May[,1]))
MAPE

RMSE_Deaths820 = sum((ForecaseErrorDeaths322error^2)^0.5)/21
RMSE_Deaths820


############################################################### Bivariate correlation

####################################bind and plot

cbind(Covid_Data_Cases, Covid_Data_Deaths)

plot.ts(Covid_Data_Cases.ts,type= "l", col = "red")
lines(Covid_Data_Deaths.ts, type= "l", col = "green")

#################################### Gauge ACF PACF for second difference stationarity


secondDifference_Covid_Data_Cases <- diff(diff(Covid_Data_Cases.ts, lag=1), lag=1)[,1]
secondDifference_Covid_Data_Deaths <- diff(diff(Covid_Data_Deaths.ts, lag=1), lag=1)[,1]
secondDifference_Covid_Data_Deaths

plot.ts(secondDifference_Covid_Data_Cases,type= "l", col = "red", ylab="Series Values", main=paste("Plotted COVId-19 Confirmed Cases & Deaths Series after Differencing Twice"))
lines(secondDifference_Covid_Data_Deaths, type= "l", col = "blue")
legend("topleft",lty=c(1,1), col=c("red", "blue"),
       c("Confirmed Cases","Confirmed Deaths"), cex = 0.75)

ccf(Covid_Data_Cases,Covid_Data_Deaths)
ccf(secondDifference_Covid_Data_Cases,secondDifference_Covid_Data_Deaths, main=paste("CCF of the COVId-19 Confirmed Cases & Deaths Series")) #pretty sure we base observations from this, difference twice for stationarity, then we analyse the ccf

acf(secondDifference_Covid_Data_Cases) #MA = 1 or MA = 0

pacf(secondDifference_Covid_Data_Cases) #AR = 1 or AR = 8

####################################Build the model for ARIMA(8,2,0) on cases, we have a second difference time series so for continuity with the other series we fit with that

ttt_Cases820 <- arima(secondDifference_Covid_Data_Cases,order = c(8,0,0), include.mean = TRUE) #fit the previously optimal model to the new 2nd difference series
ttt_Cases820
#  The residuals reflect an autocorrelation function that reflects
#   a random process.
acf(ttt_Cases820$residuals)
ttt_Cases820$residuals

#################################### PREWHITTING DATA, 

ttt_Cases820$coef
alpha <- ttt_Cases820$coef
alpha                                              #AR(8) gives us 8 alpha terms
alpha[1]                                           #Ensuring elpha [1] is the first AR alpha term
k <- length(secondDifference_Covid_Data_Deaths)
length(ttt_Cases820$residuals)
k                                                  #pretty sure we use the length of the AR(8) fit after we difference seperately, see via this method first two terms from series are missing

mean_Covid_Data_Deaths <- mean(secondDifference_Covid_Data_Deaths)
mean_Covid_Data_Deaths
# create a vector of zeros the same length as the data.
FilteredCovid_Data_Deaths <- rep(0,k)
FilteredCovid_Data_Deaths

secondDifference_Covid_Data_Deaths[1] #1 is first recorded entry
secondDifference_Covid_Data_Deaths

####################################################### PREWHITTENING VALUES. Below is the algorithm to created the filtered series based on the AR(8) coefficients using the second difference of the deaths series

for (i in c(9:k)) FilteredCovid_Data_Deaths[i] <- (secondDifference_Covid_Data_Deaths[i]-mean_Covid_Data_Deaths) - alpha[1]*(secondDifference_Covid_Data_Deaths[i-8]-mean_Covid_Data_Deaths) - alpha[2]*(secondDifference_Covid_Data_Deaths[i-7]-mean_Covid_Data_Deaths) - alpha[3]*(secondDifference_Covid_Data_Deaths[i-6]-mean_Covid_Data_Deaths) - alpha[4]*(secondDifference_Covid_Data_Deaths[i-5]-mean_Covid_Data_Deaths) - alpha[5]*(secondDifference_Covid_Data_Deaths[i-4]-mean_Covid_Data_Deaths) - alpha[6]*(secondDifference_Covid_Data_Deaths[i-3]-mean_Covid_Data_Deaths) - alpha[7]*(secondDifference_Covid_Data_Deaths[i-2]-mean_Covid_Data_Deaths) - alpha[8]*(secondDifference_Covid_Data_Deaths[i-1]-mean_Covid_Data_Deaths)
FilteredCovid_Data_Deaths                       #see we are left with 8 zero values at the beginning of the vector, which should be fine.
ttt_Cases820$residuals


plot(ttt_Cases820$residuals,lty = 1, xlab="Index",ylab="ARIMA(8,2,0) Residuals",main=paste("Plot of Residuals for Cases ARIMA(8,2,0) and Filtered Death Data"))
lines(FilteredCovid_Data_Deaths, col = "red" , lty = 1)

ccf(ttt_Cases820$residuals, FilteredCovid_Data_Deaths)



################################################################################ testing arima for cases

par(mfrow=c(3,4)) #Output nxm graphs in a plot

for(i in 4:15) #where i variable = q in the ARIMA(p,d,q) model
{
  
  ttt_Cases <- arima(Covid_Data_Cases.ts, order = c(i, 2, 0), method = c("ML"))
  
  pred_ttt_Cases <- predict(ttt_Cases, n.ahead = 21)
  tl <- pred_ttt_Cases$pred - 1.96 * pred_ttt_Cases$se
  tu <- pred_ttt_Cases$pred + 1.96 * pred_ttt_Cases$se
  ts.plot(Covid_Data_Cases.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Deaths",main=paste("Time Series of COVID-19 Confirmed Cases in Italy over March - May 21st & ARIMA(",i,", 2 , 0 ) Forecasting for COVID-19 Cases over initial 3 weeks of May"), col = c("black", "red","red"))
  lines(pred_ttt_Cases$pred, col = "red" , lty = 1)
  lines(Covid_Data_Cases_May.ts, col = "blue" , lty = 1)
  
}

par(mfrow=c(1,1)) #just provides a bigger plot

ttt_Cases <- arima(Covid_Data_Cases.ts, order = c(8, 2, 0), method = c("ML"))

pred_ttt_Cases <- predict(ttt_Cases, n.ahead = 21)
tl <- pred_ttt_Cases$pred - 1.96 * pred_ttt_Cases$se
tu <- pred_ttt_Cases$pred + 1.96 * pred_ttt_Cases$se
ts.plot(Covid_Data_Cases.ts, tl, tu, lty = c(1, 3, 3), xlab="Index",ylab="Confirmed COVID-19 Deaths",main=paste("Time Series of COVID-19 Confirmed Cases in Italy over March - May 21st & ARIMA(",i,", 2 , 0 ) Forecasting for COVID-19 Cases over initial 3 weeks of May"), col = c("black", "red","red"))
lines(pred_ttt_Cases$pred, col = "red" , lty = 1)
lines(Covid_Data_Cases_May.ts, col = "blue" , lty = 1)


