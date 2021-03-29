w_c <- read.csv("E:/Assignments/sem 2/E2-Big data analytics/mini_project/w_c.txt", sep="")
library(forecast)
library(ggplot2)

#Fit simple exponential smoothing model to data and show summary
fit_ses <- ses(w_c$Modal)
#fit_ses <- ets(wheat_crop$Predicted, model = "ANN")
summary(fit_ses)
#Plot the forecasted values
plot(fit_ses,
     xlab="Days", 
     ylab="Predicted value ")

#Fit Holt exponential smoothing model to data and show summary
fit_holt <- holt(w_c$Modal)
summary(fit_holt)
#Plot the forecasted values
plot(fit_holt,
     xlab="Days", 
     ylab="Predicted value")


wheat_crop <- read.csv("E:/Assignments/sem 2/E2-Big data analytics/mini_project/wheat_crop.txt", sep="")


#Fit Holt-Winters exponential smoothing model to data and show summary
fit_hw <- hw(wheat_crop$Modal, f = 1, seasonal = "additive")
summary(fit_hw)
#Plot the forecasted values
plot(fit_hw)
#Plot the forecasted values
plot(fit_hw)

salesTS <- ts(wheat_crop$Modal, frequency = 1, start = c(1,1))
class(salesTS)
options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
plot(salesDecomp)
plot(salesTS)

salesLog <- log(salesTS)

salesLogHW <- HoltWinters(salesLog)

salesLogHW <- HoltWinters(salesLog,beta = FALSE, 
                          gamma = FALSE)

salesLogHW
options(repr.plot.width = 6, repr.plot.height = 4)
plot(salesLogHW)

str(wheat_crop)
head(wheat_crop, n = 12)
options(repr.plot.width = 6, repr.plot.height = 3)
salesTS <- ts(wheat_crop, frequency = 1, start = c(1,1))
class(salesTS)

options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
plot(salesDecomp)


##ARIMA Model
##Load the dataset
wheat_crop <- read.csv("E:/Assignments/sem 2/E2-Big data analytics/mini_project/wheat_crop.txt", sep="")
class(wheat_crop)

library(dplyr)
co2_ts <- ts(wheat_crop$Demand, start = c(1,1), end = c(333,333), frequency = 1)
##Plotting of Periodogram - to check for frequency
wheat_crop.ts <- ts(wheat_crop, frequency = 1, start = c(1,1))
class(co2_ts)

library(forecast)
library(ggplot2)

wheat_crop.df <- read.csv("E:/Assignments/sem 2/E2-Big data analytics/mini_project/wheat_crop.txt", sep="")
lapply(wheat_crop.df, class)
mod.1 <- arima(wheat_crop.df$Demand, order=c(1,0,0), method = "ML")
t<-mod.1
summary(mod.1)


h <- forecast(t, h = 100)
h
plot(h)
