#========================================Set Working Directory============================================
setwd("/Users/numerp/Documents/PGP-BABI/Module 7 Time Series Forecasting/Project 6")
getwd()
#========================================Install Required Packages========================================
library(forecast)
library(tseries)
library(fpp2)
library(quantmod)
#========================================Calling Dataset from forecast Package============================
mydata=forecast::gas
help(gas)
class(mydata)
start(mydata)
end(mydata)
plot(mydata)
monthplot(mydata)
forecast::seasonplot(mydata)
stl_for_mydata=stl(mydata,s.window = "periodic")#constant seasonality changes
plot(stl_for_mydata)
stl_for_mydata7=stl(mydata,s.window = 7)#seasonality changes
plot(stl_for_mydata7)
components_of_my_data=decompose(mydata,type = "multiplicative")#since seasonality is in multiplicative
plot(components_of_my_data)
periodicity(mydata)
#========================================Calling Dataset from 1970 for Analysis===========================
gas_1970=ts(mydata,start = c(1970,1),end = c(1995,8),frequency = 12)
ts.plot(gas_1970)
monthplot(gas_1970)
forecast::seasonplot(gas_1970)
stl_for_gas_1970=stl(gas_1970,s.window = "periodic")#constant seasonality changes
plot(stl_for_gas_1970)
stl_for_gas_1970_7=stl(gas_1970,s.window = 7)#seasonality changes
plot(stl_for_gas_1970_7)
gas_production=(stl_for_gas_1970_7$time.series[,2]+stl_for_gas_1970_7$time.series[,3])
ts.plot(gas_production,gas_1970,col=c("red","grey"),main="Comparison of Gas and Deseasonalized Gas")
log_gas_1970=log(gas_1970)
plot(log_gas_1970)
components_of_gas_1970=decompose(gas_1970,type = "multiplicative")#seasonalized gas - components
plot(components_of_gas_1970)
periodicity(gas_1970)
#========================================Partition of Dataset=============================================
train=window(gas_1970,start=c(1970,1),end=c(1993,12),frequency=12)
test=window(gas_1970,start=c(1994,1),frequency=12)
#========================================Stationary Check for train and test==============================
tseries::adf.test(train)
diff_train=diff(train)
tseries::adf.test(diff_train)
plot(diff_train)
plot(test)
stl_for_train=stl(diff_train,s.window = "periodic")
plot(stl_for_train)
#========================================Auto ARIMA Forecast for next 20 periods==========================
auto_arima_train=forecast::auto.arima(train,seasonal = TRUE,trace = TRUE)
hist(auto_arima_train$residuals,col = "aquamarine")
acf(auto_arima_train$residuals)
Box.test(auto_arima_train$residuals,lag = 30,type = "Ljung-Box")
ts.plot(train,fitted(auto_arima_train),col=c("blue","orange"))
forecast_auto_arima_train=plot(forecast::forecast(auto_arima_train,h=20))
forecast_auto_arima_train
#========================================ARIMA forecast for next 20 periods===============================
par(mfrow=c(1,2))
acf(diff_train,lag=50,main="ACF of Stationary Time Series")
pacf(diff_train,lag=50,main="PACF of Stationary Time Series")
arima_train=arima(train,order = c(1,1,2))#with Differentiation, with MA and with AR
arima_train
arima_train_seasonal=arima(train,order = c(1,1,2),seasonal = list(order=c(0,1,1),period=12))
arima_train_seasonal
par(mfrow=c(1,1))
hist(arima_train_seasonal$residuals,col = "cyan")
acf(arima_train_seasonal$residuals)
ts.plot(train,fitted(arima_train_seasonal),col=c("green","black"))
Box.test(arima_train_seasonal$residuals,lag=30,type = "Ljung-Box")
forecast_arima_train=plot(forecast::forecast(arima_train_seasonal,h=20))
forecast_arima_train
#========================================Auto ARIMA Final Forecast for 12 Periods=========================
auto_arima_final=forecast::auto.arima(gas_1970,seasonal = TRUE,trace = TRUE)
hist(auto_arima_final$residuals,col = "beige")
acf(auto_arima_final$residuals)
Box.test(auto_arima_final$residuals,lag=30,type = "Ljung-Box")
ts.plot(gas_1970,fitted(auto_arima_final),col=c("black","orange"))
auto_arima_final_forecast=plot(forecast::forecast(auto_arima_final,h=12))
auto_arima_final_forecast
#========================================ARIMA Forecast for 12 Periods====================================
tseries::adf.test(gas_1970)#stationary check
diff_gas=diff(gas_1970)#differentiation as the data is not stationary
tseries::adf.test(diff_gas)#checking the stationarity for the differentiation
plot(diff_gas)
par(mfrow=c(1,2))
acf(diff_gas,lag=50,main="ACF of Stationary Time Series")
pacf(diff_gas,lag=50,main="PACF of Statioanry Time Series")
arima_final=arima(gas_1970,order = c(5,1,2),seasonal = list(order=c(1,1,1),period=12))
arima_final
par(mfrow=c(1,1))
hist(arima_final$residuals,col = "blue")
acf(arima_final$residuals)
Box.test(arima_final$residuals,lag = 30,type = "Ljung-Box")
ts.plot(gas_1970,fitted(arima_final),col=c("grey","violet"))
arima_final_forecast=plot(forecast::forecast(arima_final,h=12))
arima_final_forecast
#========================================Accuracy of the Model============================================
forecast::accuracy(auto_arima_train)
forecast::accuracy(arima_train_seasonal)
forecast::accuracy(auto_arima_final)
forecast::accuracy(arima_final)
#*********************************************************************************************************