# Some Notes --------------------------------------------------------------
# uncomment the functions to print tables, write csv, and export images.

# Load Packages ----------------------------------------------------------

library(forecast)
library(TSA)
library(lmtest)
library(tseries)

library(tidytext)
library(lubridate)
library(dplyr)

library(ggplot2)
library(ggpubr)
library(xtable)

# Load Index Data ---------------------------------------------------------

usdata.raw <- read.csv("01 HistoricalPrices 10-22.csv")

usdata.raw <- usdata.raw[,c(1,5)]
usdata.raw[,1] <- as.Date(usdata.raw[,1], "%m/%d/%Y")

usdex.dat <- usdata.raw[1852:2651,]

# Load Sentiment Data -----------------------------------------------------
# SentiWordNet

sentiword.raw <- read.csv("00 Sentiword.csv")

sentiword <- sentiword.raw[,c(3,5)]

sentiword[,2] <- mdy(sentiword[,2])
class(sentiword[,2])

# Hu Liu
huliu.raw <- read.csv("00 HuLiu.csv")

huliu <- huliu.raw[,c(3,5)]

huliu[,2] <- mdy(huliu[,2])
class(huliu[,2])

# Jockers
jockers.raw <- read.csv("00 Jockers.csv")

jockers <- jockers.raw[,c(3,5)]

jockers[,2] <- mdy(jockers[,2])
class(jockers[,2])

# Sentic Net
senticnet.raw <- read.csv("00 SenticNet.csv")

senticnet <- senticnet.raw[,c(3,5)]

senticnet[,2] <- mdy(senticnet[,2])
class(senticnet[,2])

# MPQA Subjectivity
mpqa.raw <- read.csv("00 MPQA Subjectivity.csv")

mpqa <- mpqa.raw[,c(3,5)]

mpqa[,2] <- mdy(mpqa[,2])
class(mpqa[,2])

# Move Weekends Sentiments to Next Monday ---------------------------------
sentiword <-
  sentiword %>%
  mutate("wday" = wday(sentiword$date))
sentiword[which(sentiword$wday == 1),2] <- sentiword[which(sentiword$wday == 1),2]+1
sentiword[which(sentiword$wday == 7),2] <- sentiword[which(sentiword$wday == 7),2]+2
sentiword$wday = wday(sentiword$date)

huliu <-
  huliu %>%
  mutate("wday" = wday(huliu$date))
huliu[which(huliu$wday == 1),2] <- huliu[which(huliu$wday == 1),2]+1
huliu[which(huliu$wday == 7),2] <- huliu[which(huliu$wday == 7),2]+2
huliu$wday = wday(huliu$date)

jockers <-
  jockers %>%
  mutate("wday" = wday(jockers$date))
jockers[which(jockers$wday == 1),2] <- jockers[which(jockers$wday == 1),2]+1
jockers[which(jockers$wday == 7),2] <- jockers[which(jockers$wday == 7),2]+2
jockers$wday = wday(jockers$date)

senticnet <-
  senticnet %>%
  mutate("wday" = wday(senticnet$date))
senticnet[which(senticnet$wday == 1),2] <- senticnet[which(senticnet$wday == 1),2]+1
senticnet[which(senticnet$wday == 7),2] <- senticnet[which(senticnet$wday == 7),2]+2
senticnet$wday = wday(senticnet$date)

mpqa <-
  mpqa %>%
  mutate("wday" = wday(mpqa$date))
mpqa[which(mpqa$wday == 1),2] <- mpqa[which(mpqa$wday == 1),2]+1
mpqa[which(mpqa$wday == 7),2] <- mpqa[which(mpqa$wday == 7),2]+2
mpqa$wday = wday(mpqa$date)

# 3 Types of Grouping : SUM, AVG, and PCTG --------------------------------

# Lexicon : Sentiword
gr.sentiword.sum <- 
  sentiword %>%
  group_by(date) %>%
  summarise("sentiword.sum" = sum(sentiment))

gr.sentiword.avg <-
  sentiword %>%
  group_by(date) %>%
  summarise("sentiword.avg" = mean(sentiment))

sentiword <- 
  sentiword %>% 
  mutate(score=0)
sentiword[which(sentiword[,1]>0),]$score=1
sentiword[-which(sentiword[,1]>0),]$score=-1

gr.sentiword.pctg <-
  sentiword %>%
  group_by(date) %>%
  summarise("sentiword.pctg" = mean(score))

gr.sentiword <- cbind(gr.sentiword.sum,
                      gr.sentiword.avg[,2],
                      gr.sentiword.pctg[,2])

# Lexicon : Hu Liu
gr.huliu.sum <- 
  huliu %>%
  group_by(date) %>%
  summarise("huliu.sum" = sum(sentiment))

gr.huliu.avg <-
  huliu %>%
  group_by(date) %>%
  summarise("huliu.avg" = mean(sentiment))

huliu <- 
  huliu %>% 
  mutate(score=0)
huliu[which(huliu[,1]>0),]$score=1
huliu[-which(huliu[,1]>0),]$score=-1

gr.huliu.pctg <-
  huliu %>%
  group_by(date) %>%
  summarise("huliu.pctg" = mean(score))

gr.huliu <- cbind(gr.huliu.sum,
                      gr.huliu.avg[,2],
                      gr.huliu.pctg[,2])

# Lexicon : Jockers
gr.jockers.sum <- 
  jockers %>%
  group_by(date) %>%
  summarise("jockers.sum" = sum(sentiment))

gr.jockers.avg <-
  jockers %>%
  group_by(date) %>%
  summarise("jockers.avg" = mean(sentiment))

jockers <- 
  jockers %>% 
  mutate(score=0)
jockers[which(jockers[,1]>0),]$score=1
jockers[-which(jockers[,1]>0),]$score=-1

gr.jockers.pctg <-
  jockers %>%
  group_by(date) %>%
  summarise("jockers.pctg" = mean(score))

gr.jockers <- cbind(gr.jockers.sum,
                      gr.jockers.avg[,2],
                      gr.jockers.pctg[,2])

# Lexicon : Sentic Net
gr.senticnet.sum <- 
  senticnet %>%
  group_by(date) %>%
  summarise("senticnet.sum" = sum(sentiment))

gr.senticnet.avg <-
  senticnet %>%
  group_by(date) %>%
  summarise("senticnet.avg" = mean(sentiment))

senticnet <- 
  senticnet %>% 
  mutate(score=0)
senticnet[which(senticnet[,1]>0),]$score=1
senticnet[-which(senticnet[,1]>0),]$score=-1

gr.senticnet.pctg <-
  senticnet %>%
  group_by(date) %>%
  summarise("senticnet.pctg" = mean(score))

gr.senticnet <- cbind(gr.senticnet.sum,
                      gr.senticnet.avg[,2],
                      gr.senticnet.pctg[,2])

# Lexicon : MPQA Subjectivity
gr.mpqa.sum <- 
  mpqa %>%
  group_by(date) %>%
  summarise("mpqa.sum" = sum(sentiment))

gr.mpqa.avg <-
  mpqa %>%
  group_by(date) %>%
  summarise("mpqa.avg" = mean(sentiment))

mpqa <- 
  mpqa %>% 
  mutate(score=0)
mpqa[which(mpqa[,1]>0),]$score=1
mpqa[-which(mpqa[,1]>0),]$score=-1

gr.mpqa.pctg <-
  mpqa %>%
  group_by(date) %>%
  summarise("mpqa.pctg" = mean(score))

gr.mpqa <- cbind(gr.mpqa.sum,
                      gr.mpqa.avg[,2],
                      gr.mpqa.pctg[,2])


# Crosscheck between DXY Index and Sentiments -----------------------------

# Lexicon : Sentiword

mydata.raw <-
  left_join(x = usdex.dat, y = gr.sentiword, by = c("Date" = "date"))

# Lexicon : Hu Liu

mydata.raw <-
  left_join(x = mydata.raw, y = gr.huliu, by = c("Date" = "date"))

# Lexicon : Jockers

mydata.raw <-
  left_join(x = mydata.raw, y = gr.jockers, by = c("Date" = "date"))

# Lexicon : Sentic Net

mydata.raw <-
  left_join(x = mydata.raw, y = gr.senticnet, by = c("Date" = "date"))

# Lexicon : MPQA

mydata.raw <-
  left_join(x = mydata.raw, y = gr.mpqa, by = c("Date" = "date"))

# Removing NA Values with 0

mydata.raw[is.na(mydata.raw)]<-0

#View(mydata.raw)

# Rearranging mydata.raw

# View(mydata.raw)
mydata.raw <- 
  mydata.raw[,c(1,2,
                3,6,9,12,15,
                4,7,10,13,16,
                5,8,11,14,17)]

# Renaming ----------------------------------------------------------------
names(mydata.raw) <- c("Date","Close",
                       "SWN.SUM","HL.SUM","JKR.SUM","STN.SUM","MPQA.SUM",
                       "SWN.AVG","HL.AVG","JKR.AVG","STN.AVG","MPQA.AVG",
                       "SWN.PCTG","HL.PCTG","JKR.PCTG","STN.PCTG","MPQA.PCTG")

# Printing Sentiment Analysis ---------------------------------------------
#> Summary
# xtable(do.call(cbind, lapply(mydata.raw[,3:7], summary)),digits=5)
# xtable(do.call(cbind, lapply(mydata.raw[,8:12], summary)),digits=5)
# xtable(do.call(cbind, lapply(mydata.raw[,13:17], summary)),digits=5)

# #> All Data
# sent.ann <- mydata.raw[,-2]
# sent.ann$Date <- as.character(sent.ann$Date)
# 
# xtable(sent.ann[,c(1,2:6)], digits=5)
# xtable(sent.ann[,c(1,7:11)], digits=5)
# xtable(sent.ann[,c(1,12:16)], digits=5)

# Cross Correlation Function ----------------------------------------------
# NOTES: uncomment the png() and dev.off() functions to export the images

# png("12Bab4-10CCF-01SUM.png", width=600, height=300)
# ggarrange(
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$SWN.SUM) + ggtitle("SWN.SUM"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$HL.SUM) + ggtitle("HL.SUM"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$JKR.SUM) + ggtitle("JKR.SUM"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$STN.SUM) + ggtitle("STN.SUM"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$MPQA.SUM) + ggtitle("MPQA.SUM")
# )
# dev.off()
# png("12Bab4-10CCF-02AVG.png", width=600, height=300)
# ggarrange(
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$SWN.AVG) + ggtitle("SWN.AVG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$HL.AVG) + ggtitle("HL.AVG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$JKR.AVG) + ggtitle("JKR.AVG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$STN.AVG) + ggtitle("STN.AVG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$MPQA.AVG) + ggtitle("MPQA.AVG")
# )
# dev.off()
# png("12Bab4-10CCF-03PCTG.png", width=600, height=300)
# ggarrange(
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$SWN.PCTG) + ggtitle("SWN.PCTG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$HL.PCTG) + ggtitle("HL.PCTG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$JKR.PCTG) + ggtitle("JKR.PCTG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$STN.PCTG) + ggtitle("STN.PCTG"),
#   ggCcf(y=log(mydata.raw$Close), x=mydata.raw$MPQA.PCTG) + ggtitle("MPQA.PCTG")
# )
# dev.off()

# Model Spec (Complete) ---------------------------------------------------
# NOTES: uncomment the summary(), coeftest(), barplot() and other related functions to re-generate graphs and statistic tests

#> SWN.SUM ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.SWN.SUM.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$SWN.SUM),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.SWN.SUM.1)
# coeftest(M.SWN.SUM.1)
# barplot(abs(coef(M.SWN.SUM.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.SWN.SUM.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$SWN.SUM,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.SWN.SUM.2)
# coeftest(M.SWN.SUM.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.SWN.SUM.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$SWN.SUM,0)),
                     include.mean = T,
                     method = "ML"
                     )
# summary(M.SWN.SUM.3)
# coeftest(M.SWN.SUM.3)

#> SWN.AVG ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.SWN.AVG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$SWN.AVG,0)),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.SWN.AVG.1)
# coeftest(M.SWN.AVG.1)
# barplot(abs(coef(M.SWN.AVG.1)[-1]), las=2)
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.SWN.AVG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$SWN.AVG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.SWN.AVG.2)
# coeftest(M.SWN.AVG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.SWN.AVG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$SWN.AVG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.SWN.AVG.3)
# coeftest(M.SWN.AVG.3)

#> SWN.PCTG ---------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.SWN.PCTG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$SWN.PCTG),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.SWN.PCTG.1)
# coeftest(M.SWN.PCTG.1)
# barplot(abs(coef(M.SWN.PCTG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.SWN.PCTG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$SWN.PCTG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.SWN.PCTG.2)
# coeftest(M.SWN.PCTG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.SWN.PCTG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$SWN.PCTG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.SWN.PCTG.3)
# coeftest(M.SWN.PCTG.3)

#> HL.SUM -----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.HL.SUM.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$HL.SUM),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.HL.SUM.1)
# coeftest(M.HL.SUM.1)
# barplot(abs(coef(M.HL.SUM.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.HL.SUM.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$HL.SUM,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.HL.SUM.2)
# coeftest(M.HL.SUM.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.HL.SUM.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$HL.SUM,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.HL.SUM.3)
# coeftest(M.HL.SUM.3)
#> HL.AVG -----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.HL.AVG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$HL.AVG),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.HL.AVG.1)
# coeftest(M.HL.AVG.1)
# barplot(abs(coef(M.HL.AVG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,2)

# Final Model : Using the identified r,s,b
M.HL.AVG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$HL.AVG,2)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.HL.AVG.2)
# coeftest(M.HL.AVG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,2))
M.HL.AVG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$HL.AVG,2)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.HL.AVG.3)
# coeftest(M.HL.AVG.3)
#> HL.PCTG ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.HL.PCTG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$HL.PCTG),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.HL.PCTG.1)
# coeftest(M.HL.PCTG.1)
# barplot(abs(coef(M.HL.PCTG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,2)

# Final Model : Using the identified r,s,b
M.HL.PCTG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$HL.PCTG,2)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.HL.PCTG.2)
# coeftest(M.HL.PCTG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,2))
M.HL.PCTG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$HL.PCTG,2)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.HL.PCTG.3)
# coeftest(M.HL.PCTG.3)
#> JKR.SUM ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.JKR.SUM.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$JKR.SUM),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.JKR.SUM.1)
# coeftest(M.JKR.SUM.1)
# barplot(abs(coef(M.JKR.SUM.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.JKR.SUM.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$JKR.SUM,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.JKR.SUM.2)
# coeftest(M.JKR.SUM.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.JKR.SUM.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$JKR.SUM,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.JKR.SUM.3)
# coeftest(M.JKR.SUM.3)
#> JKR.AVG ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.JKR.AVG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$JKR.AVG,0)),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.JKR.AVG.1)
# coeftest(M.JKR.AVG.1)
# barplot(abs(coef(M.JKR.AVG.1)[-1]), las=2)
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.JKR.AVG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$JKR.AVG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.JKR.AVG.2)
# coeftest(M.JKR.AVG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.JKR.AVG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$JKR.AVG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.JKR.AVG.3)
# coeftest(M.JKR.AVG.3)
#> JKR.PCTG ---------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.JKR.PCTG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$JKR.PCTG),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.JKR.PCTG.1)
# coeftest(M.JKR.PCTG.1)
# barplot(abs(coef(M.JKR.PCTG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.JKR.PCTG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$JKR.PCTG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.JKR.PCTG.2)
# coeftest(M.JKR.PCTG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.JKR.PCTG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$JKR.PCTG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.JKR.PCTG.3)
# coeftest(M.JKR.PCTG.3)
#> STN.SUM ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.STN.SUM.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$STN.SUM),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.STN.SUM.1)
# coeftest(M.STN.SUM.1)
# barplot(abs(coef(M.STN.SUM.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.STN.SUM.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$STN.SUM,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.STN.SUM.2)
# coeftest(M.STN.SUM.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.STN.SUM.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$STN.SUM,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.STN.SUM.3)
# coeftest(M.STN.SUM.3)
#> STN.AVG ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.STN.AVG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$STN.AVG,0)),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.STN.AVG.1)
# coeftest(M.STN.AVG.1)
# barplot(abs(coef(M.STN.AVG.1)[-1]), las=2)
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.STN.AVG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$STN.AVG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.STN.AVG.2)
# coeftest(M.STN.AVG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.STN.AVG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$STN.AVG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.STN.AVG.3)
# coeftest(M.STN.AVG.3)
#> STN.PCTG ---------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.STN.PCTG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$STN.PCTG,0)),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.STN.PCTG.1)
# coeftest(M.STN.PCTG.1)
# barplot(abs(coef(M.STN.PCTG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.STN.PCTG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$STN.PCTG,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.STN.PCTG.2)
# coeftest(M.STN.PCTG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.STN.PCTG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$STN.PCTG,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.STN.PCTG.3)
# coeftest(M.STN.PCTG.3)
#> MPQA.SUM ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.MPQA.SUM.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$MPQA.SUM,0)),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.MPQA.SUM.1)
# coeftest(M.MPQA.SUM.1)
# barplot(abs(coef(M.MPQA.SUM.1)[-1]), las=2)
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.MPQA.SUM.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$MPQA.SUM,0)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.MPQA.SUM.2)
# coeftest(M.MPQA.SUM.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.MPQA.SUM.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$MPQA.SUM,0)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.MPQA.SUM.3)
# coeftest(M.MPQA.SUM.3)
#> MPQA.AVG ----------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.MPQA.AVG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(mydata.raw$MPQA.AVG),
                      transfer = list(c(0,10)),
                      include.mean = T,
                      method="ML")
# summary(M.MPQA.AVG.1)
# coeftest(M.MPQA.AVG.1)
# barplot(abs(coef(M.MPQA.AVG.1)[-1]), las=2) 
# Gunakan model dasar (r,s,b) = (0,0,5)

# Final Model : Using the identified r,s,b
M.MPQA.AVG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xtransf = ts(lag(mydata.raw$MPQA.AVG,5)),
                      transfer = list(c(0,0)),
                      include.mean = T,
                      method = "ML")
# summary(M.MPQA.AVG.2)
# coeftest(M.MPQA.AVG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,5))
M.MPQA.AVG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                     order = c(0,1,1),
                     xreg = ts(lag(mydata.raw$MPQA.AVG,5)),
                     include.mean = T,
                     method = "ML"
)
# summary(M.MPQA.AVG.3)
# coeftest(M.MPQA.AVG.3)
#> MPQA.PCTG ---------------------------------------------------------------
# Preliminary Model : For Identification of r,s,b
M.MPQA.PCTG.1 <- arimax(x = ts(log(mydata.raw$Close)),
                       order = c(0,1,1),
                       xtransf = ts(lag(mydata.raw$MPQA.PCTG,0)),
                       transfer = list(c(0,10)),
                       include.mean = T,
                       method="ML")
# summary(M.MPQA.PCTG.1)
# coeftest(M.MPQA.PCTG.1)
# barplot(abs(coef(M.MPQA.PCTG.1)[-1]), las=2)
# Gunakan model dasar (r,s,b) = (0,0,0)

# Final Model : Using the identified r,s,b
M.MPQA.PCTG.2 <- arimax(x = ts(log(mydata.raw$Close)),
                       order = c(0,1,1),
                       xtransf = ts(lag(mydata.raw$MPQA.PCTG,0)),
                       transfer = list(c(0,0)),
                       include.mean = T,
                       method = "ML")
# summary(M.MPQA.PCTG.2)
# coeftest(M.MPQA.PCTG.2)

# Using Arima and xreg (because we only use (r,s,b) = (0,0,0))
M.MPQA.PCTG.3 <- Arima(y = ts(log(mydata.raw$Close)),
                      order = c(0,1,1),
                      xreg = ts(lag(mydata.raw$MPQA.PCTG,0)),
                      include.mean = T,
                      method = "ML"
)
# summary(M.MPQA.PCTG.3)
# coeftest(M.MPQA.PCTG.3)


# Some Useful Function ----------------------------------------------------
# sigfun : function to determine the significance of a p-value
sigfun <- function(x){
  if(x<0.001){return("***")}
  if(x<0.01){return("**")}
  if(x<0.05){return("*")}
  if(x<0.1){return(".")}
  return("")
}

# Model Spec : Preliminary Models -----------------------------------------
M.SWN.SUM.1.DF <- as.data.frame(coeftest(M.SWN.SUM.1)[,])
M.SWN.AVG.1.DF <- as.data.frame(coeftest(M.SWN.AVG.1)[,])
M.SWN.PCTG.1.DF <- as.data.frame(coeftest(M.SWN.PCTG.1)[,])
M.HL.SUM.1.DF <- as.data.frame(coeftest(M.HL.SUM.1)[,])
M.HL.AVG.1.DF <- as.data.frame(coeftest(M.HL.AVG.1)[,])
M.HL.PCTG.1.DF <- as.data.frame(coeftest(M.HL.PCTG.1)[,])
M.JKR.SUM.1.DF <- as.data.frame(coeftest(M.JKR.SUM.1)[,])
M.JKR.AVG.1.DF <- as.data.frame(coeftest(M.JKR.AVG.1)[,])
M.JKR.PCTG.1.DF <- as.data.frame(coeftest(M.JKR.PCTG.1)[,])
M.STN.SUM.1.DF <- as.data.frame(coeftest(M.STN.SUM.1)[,])
M.STN.AVG.1.DF <- as.data.frame(coeftest(M.STN.AVG.1)[,])
M.STN.PCTG.1.DF <- as.data.frame(coeftest(M.STN.PCTG.1)[,])
M.MPQA.SUM.1.DF <- as.data.frame(coeftest(M.MPQA.SUM.1)[,])
M.MPQA.AVG.1.DF <- as.data.frame(coeftest(M.MPQA.AVG.1)[,])
M.MPQA.PCTG.1.DF <- as.data.frame(coeftest(M.MPQA.PCTG.1)[,])

AIC.M.1 <- c(AIC(M.SWN.SUM.1),
             AIC(M.HL.SUM.1),
             AIC(M.JKR.SUM.1),
             AIC(M.STN.SUM.1),
             AIC(M.MPQA.SUM.1),
             AIC(M.SWN.AVG.1),
             AIC(M.HL.AVG.1),
             AIC(M.JKR.AVG.1),
             AIC(M.STN.AVG.1),
             AIC(M.MPQA.AVG.1),
             AIC(M.SWN.PCTG.1),
             AIC(M.HL.PCTG.1),
             AIC(M.JKR.PCTG.1),
             AIC(M.STN.PCTG.1),
             AIC(M.MPQA.PCTG.1))
AIC.M.1 <- round(AIC.M.1, digits=5)

M.1.DF <- 
  data.frame("SWN.SUM" = M.SWN.SUM.1.DF[,1],
             "HL.SUM" = M.HL.SUM.1.DF[,1],
             "JKR.SUM" = M.JKR.SUM.1.DF[,1],
             "STN.SUM" = M.STN.SUM.1.DF[,1],
             "MPQA.SUM" = M.MPQA.SUM.1.DF[,1],
             "SWN.AVG" = M.SWN.AVG.1.DF[,1],
             "HL.AVG" = M.HL.AVG.1.DF[,1],
             "JKR.AVG" = M.JKR.AVG.1.DF[,1],
             "STN.AVG" = M.STN.AVG.1.DF[,1],
             "MPQA.AVG" = M.MPQA.AVG.1.DF[,1],
             "SWN.PCTG" = M.SWN.PCTG.1.DF[,1],
             "HL.PCTG" = M.HL.PCTG.1.DF[,1],
             "JKR.PCTG" = M.JKR.PCTG.1.DF[,1],
             "STN.PCTG" = M.STN.PCTG.1.DF[,1],
             "MPQA.PCTG" = M.MPQA.PCTG.1.DF[,1],
             row.names = row.names(M.SWN.SUM.1.DF))
is.num <- sapply(M.1.DF, is.numeric)
M.1.DF[is.num] <- sapply(M.1.DF[is.num], round,5)
M.1.DF[is.num] <- sapply(M.1.DF[is.num], format, scientific=F)

M.1.DF.S <-
  data.frame("SWN.SUM" = sapply(coeftest(M.SWN.SUM.1)[,4],sigfun),
             "HL.SUM" = sapply(coeftest(M.HL.SUM.1)[,4],sigfun),
             "JKR.SUM" = sapply(coeftest(M.JKR.SUM.1)[,4],sigfun),
             "STN.SUM" = sapply(coeftest(M.STN.SUM.1)[,4],sigfun),
             "MPQA.SUM" = sapply(coeftest(M.MPQA.SUM.1)[,4],sigfun),
             "SWN.AVG" = sapply(coeftest(M.SWN.AVG.1)[,4],sigfun),
             "HL.AVG" = sapply(coeftest(M.HL.AVG.1)[,4],sigfun),
             "JKR.AVG" = sapply(coeftest(M.JKR.AVG.1)[,4],sigfun),
             "STN.AVG" = sapply(coeftest(M.STN.AVG.1)[,4],sigfun),
             "MPQA.AVG" = sapply(coeftest(M.MPQA.AVG.1)[,4],sigfun),
             "SWN.PCTG" = sapply(coeftest(M.SWN.PCTG.1)[,4],sigfun),
             "HL.PCTG" = sapply(coeftest(M.HL.PCTG.1)[,4],sigfun),
             "JKR.PCTG" = sapply(coeftest(M.JKR.PCTG.1)[,4],sigfun),
             "STN.PCTG" = sapply(coeftest(M.STN.PCTG.1)[,4],sigfun),
             "MPQA.PCTG" = sapply(coeftest(M.MPQA.PCTG.1)[,4],sigfun))
M.1.DF <- sapply(1:15, function(x){paste0(M.1.DF[,x],M.1.DF.S[,x])})
M.1.DF <- as.data.frame(M.1.DF)
names(M.1.DF) <- names(M.1.DF.S)
row.names(M.1.DF) <- row.names(M.1.DF.S)
M.1.DF

M.1.DF <- rbind(M.1.DF,AIC.M.1)
row.names(M.1.DF)[13] <- "AIC"

# View(M.1.DF)
# View(M.1.DF.S)

# Print Model Spec : Preliminary Model
# xtable(t(M.1.DF), digits=5)

# Model Spec : Final Model (Arima) ----------------------------------------
M.SWN.SUM.3.DF <- as.data.frame(coeftest(M.SWN.SUM.3)[,])
M.SWN.AVG.3.DF <- as.data.frame(coeftest(M.SWN.AVG.3)[,])
M.SWN.PCTG.3.DF <- as.data.frame(coeftest(M.SWN.PCTG.3)[,])
M.HL.SUM.3.DF <- as.data.frame(coeftest(M.HL.SUM.3)[,])
M.HL.AVG.3.DF <- as.data.frame(coeftest(M.HL.AVG.3)[,])
M.HL.PCTG.3.DF <- as.data.frame(coeftest(M.HL.PCTG.3)[,])
M.JKR.SUM.3.DF <- as.data.frame(coeftest(M.JKR.SUM.3)[,])
M.JKR.AVG.3.DF <- as.data.frame(coeftest(M.JKR.AVG.3)[,])
M.JKR.PCTG.3.DF <- as.data.frame(coeftest(M.JKR.PCTG.3)[,])
M.STN.SUM.3.DF <- as.data.frame(coeftest(M.STN.SUM.3)[,])
M.STN.AVG.3.DF <- as.data.frame(coeftest(M.STN.AVG.3)[,])
M.STN.PCTG.3.DF <- as.data.frame(coeftest(M.STN.PCTG.3)[,])
M.MPQA.SUM.3.DF <- as.data.frame(coeftest(M.MPQA.SUM.3)[,])
M.MPQA.AVG.3.DF <- as.data.frame(coeftest(M.MPQA.AVG.3)[,])
M.MPQA.PCTG.3.DF <- as.data.frame(coeftest(M.MPQA.PCTG.3)[,])

AIC.M.3 <- c(AIC(M.SWN.SUM.3),
             AIC(M.HL.SUM.3),
             AIC(M.JKR.SUM.3),
             AIC(M.STN.SUM.3),
             AIC(M.MPQA.SUM.3),
             AIC(M.SWN.AVG.3),
             AIC(M.HL.AVG.3),
             AIC(M.JKR.AVG.3),
             AIC(M.STN.AVG.3),
             AIC(M.MPQA.AVG.3),
             AIC(M.SWN.PCTG.3),
             AIC(M.HL.PCTG.3),
             AIC(M.JKR.PCTG.3),
             AIC(M.STN.PCTG.3),
             AIC(M.MPQA.PCTG.3))
AIC.M.3 <- round(AIC.M.3, digits=5)

ORDER.M.3 <- c("(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,2,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,5,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,2,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)",
               "(0,0,0,0,1,1)")

M.3.DF <- 
  data.frame("SWN.SUM" = M.SWN.SUM.3.DF[,1],
             "HL.SUM" = M.HL.SUM.3.DF[,1],
             "JKR.SUM" = M.JKR.SUM.3.DF[,1],
             "STN.SUM" = M.STN.SUM.3.DF[,1],
             "MPQA.SUM" = M.MPQA.SUM.3.DF[,1],
             "SWN.AVG" = M.SWN.AVG.3.DF[,1],
             "HL.AVG" = M.HL.AVG.3.DF[,1],
             "JKR.AVG" = M.JKR.AVG.3.DF[,1],
             "STN.AVG" = M.STN.AVG.3.DF[,1],
             "MPQA.AVG" = M.MPQA.AVG.3.DF[,1],
             "SWN.PCTG" = M.SWN.PCTG.3.DF[,1],
             "HL.PCTG" = M.HL.PCTG.3.DF[,1],
             "JKR.PCTG" = M.JKR.PCTG.3.DF[,1],
             "STN.PCTG" = M.STN.PCTG.3.DF[,1],
             "MPQA.PCTG" = M.MPQA.PCTG.3.DF[,1],
             row.names = row.names(M.SWN.SUM.3.DF))
is.num <- sapply(M.3.DF, is.numeric)
M.3.DF[is.num] <- sapply(M.3.DF[is.num], round,5)
M.3.DF[is.num] <- sapply(M.3.DF[is.num], format, scientific=F)

M.3.DF.S <-
  data.frame("SWN.SUM" = sapply(coeftest(M.SWN.SUM.3)[,4],sigfun),
             "HL.SUM" = sapply(coeftest(M.HL.SUM.3)[,4],sigfun),
             "JKR.SUM" = sapply(coeftest(M.JKR.SUM.3)[,4],sigfun),
             "STN.SUM" = sapply(coeftest(M.STN.SUM.3)[,4],sigfun),
             "MPQA.SUM" = sapply(coeftest(M.MPQA.SUM.3)[,4],sigfun),
             "SWN.AVG" = sapply(coeftest(M.SWN.AVG.3)[,4],sigfun),
             "HL.AVG" = sapply(coeftest(M.HL.AVG.3)[,4],sigfun),
             "JKR.AVG" = sapply(coeftest(M.JKR.AVG.3)[,4],sigfun),
             "STN.AVG" = sapply(coeftest(M.STN.AVG.3)[,4],sigfun),
             "MPQA.AVG" = sapply(coeftest(M.MPQA.AVG.3)[,4],sigfun),
             "SWN.PCTG" = sapply(coeftest(M.SWN.PCTG.3)[,4],sigfun),
             "HL.PCTG" = sapply(coeftest(M.HL.PCTG.3)[,4],sigfun),
             "JKR.PCTG" = sapply(coeftest(M.JKR.PCTG.3)[,4],sigfun),
             "STN.PCTG" = sapply(coeftest(M.STN.PCTG.3)[,4],sigfun),
             "MPQA.PCTG" = sapply(coeftest(M.MPQA.PCTG.3)[,4],sigfun))
M.3.DF <- sapply(1:15, function(x){paste0(M.3.DF[,x],M.3.DF.S[,x])})
M.3.DF <- as.data.frame(M.3.DF)
names(M.3.DF) <- names(M.3.DF.S)
row.names(M.3.DF) <- row.names(M.3.DF.S)
M.3.DF

M.3.DF <- rbind(M.3.DF,AIC.M.3)
row.names(M.3.DF)[3] <- "AIC"

M.3.DF <- rbind(M.3.DF, ORDER.M.3)
row.names(M.3.DF)[4] <- "(r,s,b,p,d,q)"

row.names(M.3.DF)[2] <- "T1-MA0"

# View(M.3.DF)
# View(M.3.DF.S)

# Print Model Spec : Final Model (Arima)
 xtable(t(M.3.DF), digits=5)

# Residual Analysis: Ljung Box Test ---------------------------------------
BT.SWN.SUM <- unlist(Box.test(M.SWN.SUM.3$residuals)[1:3])
BT.HL.SUM <- unlist(Box.test(M.HL.SUM.3$residuals)[1:3])
BT.JKR.SUM <- unlist(Box.test(M.JKR.SUM.3$residuals)[1:3])
BT.STN.SUM <- unlist(Box.test(M.STN.SUM.3$residuals)[1:3])
BT.MPQA.SUM <- unlist(Box.test(M.MPQA.SUM.3$residuals)[1:3])
BT.SWN.AVG <- unlist(Box.test(M.SWN.AVG.3$residuals)[1:3])
BT.HL.AVG <- unlist(Box.test(M.HL.AVG.3$residuals)[1:3])
BT.JKR.AVG <- unlist(Box.test(M.JKR.AVG.3$residuals)[1:3])
BT.STN.AVG <- unlist(Box.test(M.STN.AVG.3$residuals)[1:3])
BT.MPQA.AVG <- unlist(Box.test(M.MPQA.AVG.3$residuals)[1:3])
BT.SWN.PCTG <- unlist(Box.test(M.SWN.PCTG.3$residuals)[1:3])
BT.HL.PCTG <- unlist(Box.test(M.HL.PCTG.3$residuals)[1:3])
BT.JKR.PCTG <- unlist(Box.test(M.JKR.PCTG.3$residuals)[1:3])
BT.STN.PCTG <- unlist(Box.test(M.STN.PCTG.3$residuals)[1:3])
BT.MPQA.PCTG <- unlist(Box.test(M.MPQA.PCTG.3$residuals)[1:3])

BT <- data.frame(STAT = c(BT.SWN.SUM[[1]], BT.HL.SUM[[1]], BT.JKR.SUM[[1]],
                          BT.STN.SUM[[1]], BT.MPQA.SUM[[1]],
                          BT.SWN.AVG[[1]], BT.HL.AVG[[1]], BT.JKR.AVG[[1]],
                          BT.STN.AVG[[1]], BT.MPQA.AVG[[1]],
                          BT.SWN.PCTG[[1]], BT.HL.PCTG[[1]], BT.JKR.PCTG[[1]],
                          BT.STN.PCTG[[1]], BT.MPQA.PCTG[[1]]),
                 row.names = c("SWN.SUM", "HL.SUM", "JKR.SUM",
                               "STN.SUM", "MPQA.SUM",
                               "SWN.AVG", "HL.AVG", "JKR.AVG",
                               "STN.AVG", "MPQA.AVG",
                               "SWN.PCTG", "HL.PCTG", "JKR.PCTG",
                               "STN.PCTG", "MPQA.PCTG"))
is.num <- sapply(BT, is.numeric)
BT[is.num] <- sapply(BT[is.num], round,5)
BT[is.num] <- sapply(BT[is.num], format, scientific=F)

BT.S <- data.frame(S = c(sigfun(BT.SWN.SUM[[3]]),
                         sigfun(BT.HL.SUM[[3]]),
                         sigfun(BT.JKR.SUM[[3]]),
                         sigfun(BT.STN.SUM[[3]]),
                         sigfun(BT.MPQA.SUM[[3]]),
                         sigfun(BT.SWN.AVG[[3]]),
                         sigfun(BT.HL.AVG[[3]]),
                         sigfun(BT.JKR.AVG[[3]]),
                         sigfun(BT.STN.AVG[[3]]),
                         sigfun(BT.MPQA.AVG[[3]]),
                         sigfun(BT.SWN.PCTG[[3]]),
                         sigfun(BT.HL.PCTG[[3]]),
                         sigfun(BT.JKR.PCTG[[3]]),
                         sigfun(BT.STN.PCTG[[3]]),
                         sigfun(BT.MPQA.PCTG[[3]])),
                   row.names = c("SWN.SUM", "HL.SUM", "JKR.SUM",
                                 "STN.SUM", "MPQA.SUM",
                                 "SWN.AVG", "HL.AVG", "JKR.AVG",
                                 "STN.AVG", "MPQA.AVG",
                                 "SWN.PCTG", "HL.PCTG", "JKR.PCTG",
                                 "STN.PCTG", "MPQA.PCTG"))
BT <- sapply(1:15, function(x){paste0(BT[x,],BT.S[x,])})
BT <- as.data.frame(BT)
names(BT) <- "Ljung Box Test Stat"
row.names(BT) <- row.names(BT.S)

# Residual Analysis: Shapiro-Wilk Normality Test --------------------------
ST.SWN.SUM <- unlist(shapiro.test(M.SWN.SUM.3$residuals)[1:2])
ST.HL.SUM <- unlist(shapiro.test(M.HL.SUM.3$residuals)[1:2])
ST.JKR.SUM <- unlist(shapiro.test(M.JKR.SUM.3$residuals)[1:2])
ST.STN.SUM <- unlist(shapiro.test(M.STN.SUM.3$residuals)[1:2])
ST.MPQA.SUM <- unlist(shapiro.test(M.MPQA.SUM.3$residuals)[1:2])
ST.SWN.AVG <- unlist(shapiro.test(M.SWN.AVG.3$residuals)[1:2])
ST.HL.AVG <- unlist(shapiro.test(M.HL.AVG.3$residuals)[1:2])
ST.JKR.AVG <- unlist(shapiro.test(M.JKR.AVG.3$residuals)[1:2])
ST.STN.AVG <- unlist(shapiro.test(M.STN.AVG.3$residuals)[1:2])
ST.MPQA.AVG <- unlist(shapiro.test(M.MPQA.AVG.3$residuals)[1:2])
ST.SWN.PCTG <- unlist(shapiro.test(M.SWN.PCTG.3$residuals)[1:2])
ST.HL.PCTG <- unlist(shapiro.test(M.HL.PCTG.3$residuals)[1:2])
ST.JKR.PCTG <- unlist(shapiro.test(M.JKR.PCTG.3$residuals)[1:2])
ST.STN.PCTG <- unlist(shapiro.test(M.STN.PCTG.3$residuals)[1:2])
ST.MPQA.PCTG <- unlist(shapiro.test(M.MPQA.PCTG.3$residuals)[1:2])

ST <- data.frame(STAT = c(ST.SWN.SUM[[1]], ST.HL.SUM[[1]], ST.JKR.SUM[[1]],
                          ST.STN.SUM[[1]], ST.MPQA.SUM[[1]],
                          ST.SWN.AVG[[1]], ST.HL.AVG[[1]], ST.JKR.AVG[[1]],
                          ST.STN.AVG[[1]], ST.MPQA.AVG[[1]],
                          ST.SWN.PCTG[[1]], ST.HL.PCTG[[1]], ST.JKR.PCTG[[1]],
                          ST.STN.PCTG[[1]], ST.MPQA.PCTG[[1]]),
                 row.names = c("SWN.SUM", "HL.SUM", "JKR.SUM",
                               "STN.SUM", "MPQA.SUM",
                               "SWN.AVG", "HL.AVG", "JKR.AVG",
                               "STN.AVG", "MPQA.AVG",
                               "SWN.PCTG", "HL.PCTG", "JKR.PCTG",
                               "STN.PCTG", "MPQA.PCTG"))
is.num <- sapply(ST, is.numeric)
ST[is.num] <- sapply(ST[is.num], round,5)
ST[is.num] <- sapply(ST[is.num], format, scientific=F)

ST.S <- data.frame(S = c(sigfun(ST.SWN.SUM[[2]]),
                         sigfun(ST.HL.SUM[[2]]),
                         sigfun(ST.JKR.SUM[[2]]),
                         sigfun(ST.STN.SUM[[2]]),
                         sigfun(ST.MPQA.SUM[[2]]),
                         sigfun(ST.SWN.AVG[[2]]),
                         sigfun(ST.HL.AVG[[2]]),
                         sigfun(ST.JKR.AVG[[2]]),
                         sigfun(ST.STN.AVG[[2]]),
                         sigfun(ST.MPQA.AVG[[2]]),
                         sigfun(ST.SWN.PCTG[[2]]),
                         sigfun(ST.HL.PCTG[[2]]),
                         sigfun(ST.JKR.PCTG[[2]]),
                         sigfun(ST.STN.PCTG[[2]]),
                         sigfun(ST.MPQA.PCTG[[2]])),
                   row.names = c("SWN.SUM", "HL.SUM", "JKR.SUM",
                                 "STN.SUM", "MPQA.SUM",
                                 "SWN.AVG", "HL.AVG", "JKR.AVG",
                                 "STN.AVG", "MPQA.AVG",
                                 "SWN.PCTG", "HL.PCTG", "JKR.PCTG",
                                 "STN.PCTG", "MPQA.PCTG"))
ST <- sapply(1:15, function(x){paste0(ST[x,],ST.S[x,])})
ST <- as.data.frame(ST)
names(ST) <- "Shapiro Test Stat"
row.names(ST) <- row.names(ST.S)

# Print Residual Analysis -------------------------------------------------

# xtable(cbind(BT,ST),digits=5)
