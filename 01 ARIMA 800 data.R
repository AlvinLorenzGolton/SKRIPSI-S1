rm(list=ls())

# Load Packages -----------------------------------------------------------

library(forecast)
library(TSA)
library(lmtest)
library(tseries)

library(tidytext)
library(lubridate)
library(dplyr)

library(ggplot2)

# Load and Prepare Data ---------------------------------------------------

usdata.raw <- read.csv("01 HistoricalPrices 10-22.csv")

usdata <- usdata.raw[,c(1,5)]
usdata[,1] <- as.Date(usdata[,1], "%m/%d/%Y")

# Selecting data 1852:2651 (2017-02-03 hingga 2020-03-02) -----------------

usdex.dat <- usdata[1852:2651,]
usdex <- ts(usdata[1852:2651,2])

dim(usdex.dat)

# Data Visualization ------------------------------------------------------

ggplot(data = usdex.dat, aes(x=Date, y=Close)) + geom_line() + theme_light()

ggplot(data = usdex.dat, aes(x=Date, y=log(Close))) + geom_line() + theme_light()

usdex.dat1<- data.frame(Diff.Log.DXY = diff(log(usdex.dat[,2])),
                        Date = usdex.dat[-1,1])
ggplot(data = usdex.dat1, aes(x=Date, y=Diff.Log.DXY)) + geom_line() + theme_light()

# ACF, PACF, and EACF -----------------------------------------------------

adf.test(diff(log(usdex)))

ggtsdisplay(diff(log(usdex)))

eacf(diff(log(usdex)))

# Modelling: ARIMA --------------------------------------------------------

mod.arima <- Arima(y = log(usdex), order = c(0,1,1), include.mean = T)

autoplot(ts(usdex)) +
  autolayer(exp(mod.arima$fitted))

tail(exp(mod.arima$fitted),10)

summary(mod.arima)

# Err. Analysis -----------------------------------------------------------

shapiro.test(mod.arima$residuals)
plot(density(mod.arima$residuals))
ggtsdisplay(mod.arima$residuals)
Box.test(mod.arima$residuals)

# Forecasting -------------------------------------------------------------

fc.mod.arima <- forecast(mod.arima,h=5)

# MSE and RSQ -------------------------------------------------------------

mse.val <- function(pred,obs){
  n = length(obs)
  return(sum((pred - obs)^2)/n)
}

rsq.val <- function(pred,obs){
  n = length(obs)
  SSR <- sum((pred-mean(obs))^2)
  SSE <- sum((obs-pred)^2)
  SST <- sum((obs-mean(obs))^2)
  return(1-SSE/SST)
}

mse.fc.mod.arima <- mse.val(pred = exp(as.numeric(fc.mod.arima$mean)),
                            obs = usdex)
mse.fc.mod.arima

rsq.mod.arima <- rsq.val(pred = exp(fitted(mod.arima)),
                         obs = usdex)
rsq.mod.arima

# Saving Graphs -----------------------------------------------------------
# 
# png("12BAB4-01Vis_Index.png", width=400, height=300)
# ggplot(data = usdex.dat, aes(x=Date, y=Close)) + geom_line() + theme_light()
# dev.off()
# 
# png("12BAB4-02Vis_Log_Index.png", width=400, height=300)
# ggplot(data = usdex.dat, aes(x=Date, y=log(Close))) + geom_line() + theme_light()
# dev.off()
# 
# png("12BAB4-03Vis_Diff_Log_Index.png", width=400, height=300)
# ggplot(data = usdex.dat1, aes(x=Date, y=Diff.Log.DXY)) + geom_line() + theme_light()
# dev.off()
# 
# png("12BAB4-04ACFPACF_Index.png", width=400, height=300)
# ggtsdisplay(diff(log(usdex)))
# dev.off()