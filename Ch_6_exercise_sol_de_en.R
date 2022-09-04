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
    df <- read.csv("UniversalBank.csv") # ein kurzer Name für den Datensatz bedeutet weniger Tipparbeit
    colnames(df) <- tolower(colnames(df)) #klein geschriebene Variablennamen
    df <- subset(df, select = c(income, family, mortgage))
    #i: subset wählt die Variablen aus und ordnet sie an  
    df$has.mortgage <- ifelse(df$mortgage > 0, 1, 0)
    describe(df) #30% haben eine Hypothek
    df <- subset(df, select = -c(mortgage)) #Lasst uns die originale mortgage Variable loswerden  
     
# b) Einfaches Modell (nur Diskussion)
    #beginnt mit der logistischen Antwortfunktion p = exp(a+b*income)/[1 + exp(a+b*income)], 
    #die Quoten = p/(1-p) werden zu exp(a+b*income). Wenn wir logs nehmen, erhalten wir das lineare Modell. 
    #Interpretation: Wenn x um 1 Einheit (=1000$) steigt, dann ist die RELATIVE Änderung den Odds b 
    #(d.h. die %-Änderung der Odds beträgt 100*b)

# c) Schätzung 
    set.seed(2)
    train.index <- sample( c(1:dim(df)[1]) , dim(df)[1]*0.6 )  
    train.df <- df[train.index, ]
    valid.df <- df[-train.index, ]
    
    logit <- glm(has.mortgage ~ . , data = train.df, family = "binomial")
    summary(logit)
    #ii) b_income < 0, wie wir erwarten würden. Allerdings ist der Koeffizient ziemlich klein.
    #iii) Das Intercept sagt uns, dass die Kunden standardmäßig keine Hypotheken haben. 
    # Kunden neigen dazu, Hypotheken zu haben, wenn ihre Familien wachsen. 
    # Außerdem sind alle Koeffizienten nicht signifikant.

# d) Vorhersage
    logit.pred <- predict(logit, valid.df, type = "response")
    describe(logit.pred)
    
# e) Performance Evaluation
   #decile-wise lift chart
   gain <- gains(valid.df$has.mortgage, logit.pred, groups=10)
   heights <- gain$mean.resp/mean(valid.df$has.mortgage)
   midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,2), xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
   confusionMatrix(as.factor(ifelse(logit.pred > 0.30, 1, 0)), as.factor(valid.df$has.mortgage), positive="1")
   
   #Warum haben wir einen Aufschlag von 1 für die ersten 10% ?
   valid.df$pred <- logit.pred
   head( subset(valid.df, select = c('has.mortgage', 'pred')) )   
   
   top10.df <- top_frac(valid.df, n = .1 , wt = pred)
   mean(top10.df$has.mortgage)
   mean( ifelse(top10.df$pred > 0.3, 1, 0)) #alle top10 werden als "1" vorhergesagt
   describe(top10.df$pred)
   mean( ifelse(top10.df$pred > 0.3, 1, 0) == top10.df$has.mortgage )   
