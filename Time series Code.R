install.packages('xts')
library(xts)
library(imputeTS)
library(tseries)
library(forecast)

#loading the data
fargo_data$Incoming.Examinations<-as.numeric(as.character(fargo_data$Incoming.Examinations))

fargo_data<-read.csv("E:/Healthcare project/Fargo Healthcare/farg.csv", header = TRUE,sep = ",")
View(fargo_data)
str(fargo_data)
str(fargo_data)

#Timeseries Exploration
fargo_ts<-ts(fargo_data$Incoming.Examinations,start = c(2006,1), end = c(2013,12),frequency = 12)
class(fargo_ts)
time(fargo_ts)
start(fargo_ts)
end(fargo_ts)
fargo_ts

frequency(fargo_ts)

summary(fargo_ts)


plot(fargo_ts)
plotNA.distribution(fargo_ts)

#Imputing Values
fargo_imp<-na.kalman(fargo_ts)
fargo_imp
plot(fargo_imp)
start(fargo_imp)
end(fargo_imp)
summary(fargo_imp)
plotNA.imputations(fargo_ts,fargo_imp)

abline(reg=lm(fargo_imp~time(fargo_imp)))

#trend
plot(aggregate(fargo_imp,FUN=mean))

#seasonality
boxplot(fargo_imp~cycle(fargo_imp))

#components
fargo_comp<-decompose(fargo_imp)
plot(fargo_comp)

#stationary series
plot(fargo_imp)

plot(log(fargo_imp))

plot(diff(log(fargo_imp)))

#Dickey-Fuller Test
adf.test((fargo_imp), alternative="stationary", k=0)

#AR I MA


#acf and pacf plots
acf(fargo_imp)

#value of q
acf(diff(log(fargo_imp)))
acf(fargo_imp)

#value of p
pacf(diff(log(fargo_imp)))
pacf(fargo_imp)

#Modelling
fit <- arima(log(fargo_imp), c(3, 1, 1),seasonal = list(order = c(3, 1, 1), period = 12))

fit<-auto.arima(fargo_imp)
summary(fit)

pred <- predict(fit, n.ahead = 2*12)

pred1<-2.718^pred$pred

pred1

ts.plot(fargo_imp,2.718^pred$pred, log = "y", lty = c(1,3))
