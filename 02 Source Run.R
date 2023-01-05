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
