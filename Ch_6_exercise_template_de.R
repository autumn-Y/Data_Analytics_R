### Kapitel 6 Übung ###
# setup
rm(list=ls())
options(scipen = 999)
RNGkind(sample.kind = "Rounding")  

library(tidyverse)
library(Hmisc)
library(gains)
library(caret)

  
# a) Daten laden & Variablen auswählen 
#setwd("/cloud/project/R-Files/Chapter 10")
# a) Daten laden & Variablen auswählen
df <- read.csv("UniversalBank.csv")
colnames(df) <- tolower(colnames(df))
df <- subset(df, select = c(income, family, mortgage))
df$has.mortgage <- ifelse(df$mortgage > 0, 1, 0)
describe(df)
df <- subset(df, select = -c(mortgage))

# b) Einfaches Modell (nur Diskussion)


# c) Schätzung 
set.seed(2)
   train.index <- sample( c(1:dim(df)[1]) , dim(df)[1]*0.6 )  
   train.df <- df[train.index, ]
   valid.df <- df[-train.index, ]

   
# d) Vorhersage
   
# e) Performance Evaluation

  
   #decile-wise lift chart
   #gain <- gains(valid.df$has.mortgage, logit.pred, groups=10)
   #heights <- gain$mean.resp/mean(valid.df$has.mortgage)
   #midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,2), xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
   #confusionMatrix(as.factor(ifelse(logit.pred > 0.30, 1, 0)), as.factor(valid.df$has.mortgage), positive="1")
  
