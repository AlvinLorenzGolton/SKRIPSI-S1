# Clean Environment -------------------------------------------------------

rm(list=ls())

# Load Packages -----------------------------------------------------------

library(forecast)
library(TSA)
library(lmtest)
library(tseries)

library(tidytext)
library(tidyverse)
library(lubridate)
library(dplyr)

library(ggplot2)
library(scales)
library(xtable)

# Load Analyzed Sentiments Data -------------------------------------------

source("02 Source Run.R")

# Sliding ARIMA & ARIMAX Functions ----------------------------------------

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

sw.arima <- function(yt,pdq,y=1.5){
  mse.cont <- NULL
  rsq.cont <- NULL
  aic.cont <- NULL
  for(i in seq(from=1, to=(length(yt)-(52*5*y)-5+1), by=5)){
    n = i+(52*5*y)-1
    traindata <- ts(log(yt[i:n])) # From data i to i+390-1 (length=390 data)
    model <- Arima(y = traindata, order=pdq)
    
    rsq.model <- rsq.val(pred = exp(fitted(model)),
                         obs = exp(traindata))
    rsq.cont <- c(rsq.cont, rsq.model)
    
    n1 <- n+1
    n5 <- n+5
    
    fc.model <- forecast(model, h=5)
    
    mse.fc.model <- mse.val(pred = exp(fc.model$mean),
                            obs = usdex.dat[n1:n5,2])
    
    mse.cont <- c(mse.cont,mse.fc.model)
    aic.cont <- c(aic.cont, AIC(model))
  }
  return(list(rsq.cont,mse.cont,aic.cont))
}

sw.arimax <- function(yt,xt,pdq,rsb=c(0,0,0),y=1.5){
  # This SlidWin only specified for (r,s,b) = (0,0,b), b can be any non-negative integer
  m = 52*5*y
  r = rsb[[1]]; s = rsb[[2]]; b = rsb[[3]]
  rsq.cont <- NULL
  mse.cont <- NULL
  aic.cont <- NULL
  tx.cont <- NULL
  for(i in seq(from=1, to=(length(yt)-m+1), by=5)){
    # i = 1; yt=mydata.raw$Close; xt=mydata.raw$SWN.SUM;
    # pdq=c(0,1,1); rsb=c(0,0,0); m=52*5*1.5; b=0
    n = i+m-1
    traindata <- ts(log(yt[i:n]))
    xreg.var <- ts(lag(xt[i:n],b))
    n1 <- n+1
    n5 <- n+5 ; if(n5 > length(xt)){next}
    
    model.fin <- Arima(y = traindata,
                       order = pdq,
                       xreg = xreg.var,
                       include.mean = T,
                       method = "ML")
    
    if(length(which(is.na(fitted(model.fin))==T))==0){
      fit.model <- fitted(model.fin)
      obs.model <- traindata
    } else {
      fit.model <- fitted(model.fin)[-which(is.na(fitted(model.fin))==T)]
      obs.model <- traindata[-which(is.na(fitted(model.fin))==T)]
    }
    
    rsq.model.fin <- rsq.val(pred = exp(fit.model),
                             obs = exp(obs.model))
    rsq.cont <- c(rsq.cont, rsq.model.fin)
    
    fc.model.fin <- forecast(model.fin, h=5, xreg=xt[n1:n5])
    
    mse.fc.model.fin <- mse.val(pred = exp(fc.model.fin$mean),
                                obs = yt[n1:n5])
    
    mse.cont <- c(mse.cont,mse.fc.model.fin)
    
    aic.cont <- c(aic.cont, AIC(model.fin))
  }
  return(list(rsq.cont,mse.cont,aic.cont))
}

outdat.df <- function(data=mydata.raw, pdq = c(0,1,1), y=1.5, out.rtn=1){
  # specialized only for mydata.raw df
  # where cols : Date, Close, SWN.SUM, HL.SUM, JKR.SUM, STN.SUM, MPQA.SUM, ... .
  # specialized only for (r,s,b,p,d,q) as follows
    rsb = data.frame(r = 0, s = 0, b = c(0,0,0,0,0,
                                       0,2,0,0,5,
                                       0,2,0,0,0),
                   row.names = c("SWN.SUM", "HL.SUM", "JKR.SUM",
                                 "STN.SUM", "MPQA.SUM",
                                 "SWN.AVG", "HL.AVG", "JKR.AVG",
                                 "STN.AVG", "MPQA.AVG",
                                 "SWN.PCTG", "HL.PCTG", "JKR.PCTG",
                                 "STN.PCTG", "MPQA.PCTG"))
  # data=mydata.raw; pdq=c(0,1,1); y=1.5
  
  outdat.arima <- sw.arima(yt=data[,2], pdq=pdq, y=y)[[out.rtn]]
  out.df <- outdat.arima
  
  n.xt <- dim(data)[2]
  for(i in 3:n.xt){
    j = i-2
    outdat.arimax <- sw.arimax(yt=data[,2],
                            xt=data[,i],
                            pdq=pdq,
                            rsb=rsb[j,],
                            y=y)[[out.rtn]]
    out.df <- cbind(out.df, outdat.arimax)
  }
  out.df <- as.data.frame(out.df)
  names(out.df)[1] <- "ARIMA"
  names(out.df)[2:length(names(out.df))] <- names(data[3:n.xt])
  return(out.df)
}

# Performing Sliding Windows ARIMA ----------------------------------------
# out.rtn = 1 --> returns RSQ
# out.rtn = 2 --> returns MSE
# out.rtn = 3 --> returns AIC
rsq.df.10 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1, out.rtn = 1)
rsq.df.15 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1.5, out.rtn = 1)
rsq.df.20 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=2, out.rtn = 1)

mse.df.10 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1, out.rtn=2)
mse.df.15 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1.5, out.rtn=2)
mse.df.20 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=2, out.rtn=2)

aic.df.10 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1, out.rtn=3)
aic.df.15 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=1.5, out.rtn=3)
aic.df.20 <- outdat.df(data = mydata.raw, pdq=c(0,1,1), y=2, out.rtn=3)

# Printing SUMMARY RSQ and MSE --------------------------------------------
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(rsq.df.10[,1])))),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.10[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.10[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.10[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(rsq.df.15[,1])))),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.15[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.15[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.15[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(rsq.df.20[,1])))),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.20[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.20[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(rsq.df.20[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(mse.df.10[,1])))),digits=5)
# xtable(do.call(cbind, lapply(mse.df.10[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.10[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.10[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(mse.df.15[,1])))),digits=5)
# xtable(do.call(cbind, lapply(mse.df.15[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.15[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.15[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(mse.df.20[,1])))),digits=5)
# xtable(do.call(cbind, lapply(mse.df.20[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.20[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(mse.df.20[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(aic.df.10[,1])))),digits=5)
# xtable(do.call(cbind, lapply(aic.df.10[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.10[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.10[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(aic.df.15[,1])))),digits=5)
# xtable(do.call(cbind, lapply(aic.df.15[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.15[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.15[,12:16], summary)),digits=5)
# 
# xtable(data.frame(ARIMA=do.call(cbind, list(summary(aic.df.20[,1])))),digits=5)
# xtable(do.call(cbind, lapply(aic.df.20[,2:6], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.20[,7:11], summary)),digits=5)
# xtable(do.call(cbind, lapply(aic.df.20[,12:16], summary)),digits=5)

# Scoring Performa ( Based on per Windows ) Model-Model ARIMAX vs. ARIMA ---------------------------
p.arimax.10 <- sapply(X = mse.df.10[,2:16],
                      FUN = function(x){(x - mse.df.10[,1])/mse.df.10[,1]})
p.arimax.15 <- sapply(X = mse.df.15[,2:16],
                      FUN = function(x){(x - mse.df.15[,1])/mse.df.15[,1]})
p.arimax.20 <- sapply(X = mse.df.20[,2:16],
                      FUN = function(x){(x - mse.df.20[,1])/mse.df.20[,1]})

p.arimax.10 <- as.data.frame(p.arimax.10)
p.arimax.15 <- as.data.frame(p.arimax.15)
p.arimax.20 <- as.data.frame(p.arimax.20)

p.arimax.10 <-
  p.arimax.10 %>%
  select(everything())%>%
  mutate("SUM" = rowSums(across()))
p.arimax.15 <-
  p.arimax.15 %>%
  select(everything())%>%
  mutate("SUM" = rowSums(across()))
p.arimax.20 <-
  p.arimax.20 %>%
  select(everything())%>%
  mutate("SUM" = rowSums(across()))

#View(p.arimax.10)
#View(p.arimax.15)
#View(p.arimax.20)

# Printing p.arimax Summary -----------------------------------------------

# xtable(do.call(cbind, lapply(p.arimax.10[,1:5], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.10[,6:10], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.10[,11:15], summary)),digits=5)
# 
# xtable(do.call(cbind, lapply(p.arimax.15[,1:5], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.15[,6:10], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.15[,11:15], summary)),digits=5)
# 
# xtable(do.call(cbind, lapply(p.arimax.20[,1:5], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.20[,6:10], summary)),digits=5)
# xtable(do.call(cbind, lapply(p.arimax.20[,11:15], summary)),digits=5)
