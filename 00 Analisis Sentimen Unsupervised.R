# Loading Packages --------------------------------------------------------

library(lexicon)      # to get SentiWordNet lexicon
library(tidyverse)    # to do simple coding
library(tidytext)     # to do simple coding

# Loading Data ------------------------------------------------------------

setwd(getwd())

unsupdat <- read.csv("00 UnSupLab 10-22.csv")

names(unsupdat)[1] <- "Title"

unsupdat <- unsupdat[,c(2,6)]

# View(unsupdat)

# Loading Lexicons --------------------------------------------------------

## Lexicon : SentiWord Net
lex.sentiwordnet <- hash_sentiment_sentiword
names(lex.sentiwordnet) <- c("word", "score")
dim(lex.sentiwordnet)

## Lexicon : Hu Liu
lex.huliu <- hash_sentiment_huliu
names(lex.huliu) <- c("word", "score")
dim(lex.huliu)

## Lexicon : Jockers
lex.jockers <- hash_sentiment_jockers
names(lex.jockers) <- c("word", "score")
dim(lex.jockers)

## Lexicon : Sentic Net
lex.senticnet <- hash_sentiment_senticnet
names(lex.senticnet) <- c("word", "score")
dim(lex.senticnet)

## Lexicon : MPQA Subjecitivity
lex.mpqa.raw <- read.csv("00 MPQA Subjectivity Lexicon.csv")
names(lex.mpqa.raw)[1] <- "type"
lex.mpqa.raw <- lex.mpqa.raw[,c(1,2,4)]
dim(lex.mpqa.raw)

lex.mpqa <- 
  lex.mpqa.raw %>%
  distinct(word,               # Remove duplicate rows
           .keep_all = T) %>%
  mutate(score = 0) 
dim(lex.mpqa)                  # From 8222 --> 6886

lex.mpqa[which(lex.mpqa[,3]=="both"),4] <- 0
lex.mpqa[which(lex.mpqa[,3]=="negative"),4] <- -1
lex.mpqa[which(lex.mpqa[,3]=="neutral"),4] <- 0
lex.mpqa[which(lex.mpqa[,3]=="positive"),4] <- 1

lex.mpqa <- lex.mpqa[,c(2,4)]

# Inner Join Data and Lexicons --------------------------------------------

## Lexicon : SentiWord Net
sentiword <-
  unsupdat %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(output = word, input = Content) %>%
  left_join(y = lex.sentiwordnet) %>%
  replace(is.na(.), 0) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(overall = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  left_join(unsupdat %>% mutate(linenumber = row_number()))
write.csv(sentiword,"00 SentiWord.csv",row.names = T)

## Lexicon : Hu Liu
huliu <-
  unsupdat %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(output = word, input = Content) %>%
  left_join(lex.huliu) %>%
  replace(is.na(.),0) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(overall = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  left_join(unsupdat %>% mutate(linenumber = row_number()))
write.csv(huliu,"00 HuLiu.csv",row.names = T)

## Lexicon : Jockers
jockers <-
  unsupdat %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(output = word, input = Content) %>%
  left_join(lex.jockers) %>%
  replace(is.na(.),0) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(overall = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  left_join(unsupdat %>% mutate(linenumber = row_number()))
write.csv(jockers,"00 Jockers.csv",row.names = T)

## Lexicon : Sentic Net
senticnet <-
  unsupdat %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(output = word, input = Content) %>%
  left_join(lex.senticnet) %>%
  replace(is.na(.),0) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(overall = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  left_join(unsupdat %>% mutate(linenumber = row_number()))
write.csv(senticnet,"00 SenticNet.csv",row.names = T)

## Lexicon : MPQA Subjectivity
mpqa <-
  unsupdat %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(output = word, input = Content) %>%
  left_join(lex.mpqa) %>%
  replace(is.na(.),0) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(overall = ifelse(sentiment > 0, "Positive", "Negative")) %>%
  left_join(unsupdat %>% mutate(linenumber = row_number()))
write.csv(mpqa,"00 MPQA Subjectivity.csv",row.names = T)