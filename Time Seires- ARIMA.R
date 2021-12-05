# Library
library(imputeTS) # library for imputing
library(forecast) # library for forcating

# Clear Global Environment variables
rm(list = ls())

# importing dataset to dataframe
dataset <- read.csv("C:/Bhavin Joy/meri gudiya/ARIMA/PCE.csv", stringsAsFactors = TRUE, na.strings = c("", NA))
str(dataset)
pce_TS_Miss <- ts(dataset$PCE, start = c(1959, 1), end = c(2021, 4), frequency = 12)
pce_TS <- ts(dataset$PCE, start = c(1959, 1), end = c(2021, 4), frequency = 12)

# imputing the missing values in the dataser
pce_d1 <- na_interpolation(pce_TS_Miss)
pce_d2 <- na_ma(pce_TS_Miss, k = 4, weighting = "exponential")
pce_d3 <- na_kalman(pce_TS_Miss)
pce_d4 <- na_kalman(pce_TS_Miss, model = "auto.arima")
# combining all imputed value columns
pce_view <- cbind(pce_TS, pce_TS_Miss, pce_d1, pce_d2, pce_d3, pce_d4)
# Viewing the combined dataframe
View(pce_view)

# Plotting moving average
par(mfrow=c(2,2))
plot(pce_d4)
plot(ma(pce_d4,3))
plot(ma(pce_d4,7))
plot(ma(pce_d4,13))

# step 10
par(mfrow=c(1,1))
logPass<- log(pce_d4)
plot(logPass)

# step 11
fit<- stl(logPass, s.window="periodic")
plot(fit)

# step 13
fc <- ses(pce_d4, h = 50)
autoplot(pce_d4) + autolayer(fc)

# step 14
pce_trainset <- window(pce_d4, start = c(1959, 1), end = c(2020, 4))
pce_testset <- window(pce_d4, start = c(2020, 5), end = c(2021, 4))

# step 15
ses_frcst <- ses(pce_trainset, h = 20)
naive_frcst <- naive(pce_trainset, h = 20)

# step 16
accuracy(ses_frcst, pce_d4)
accuracy(naive_frcst,pce_d4)

# step 17
fcholt <- holt(pce_d4, h=20)
summary(fcholt)
plot(fcholt)
fchw<- hw(pce_d4, h=20)
plot(fchw)

# step 18
fchw1<- hw(pce_d4, h=20, seasonal="multiplicative")
plot(fchw1)

# step 19
fcholtT <- holt(pce_trainset, h=20)
fchwT<- hw(pce_trainset, h=20)
fchw1T<- hw(pce_trainset, h=20, seasonal="multiplicative")
accuracy(fcholtT, pce_d4)
accuracy(fchwT, pce_d4)
accuracy(fchw1T, pce_d4)
autoplot(pce_d4) + autolayer(fcholtT$mean) + autolayer(fchwT$mean) + autolayer(fchw1T$mean)