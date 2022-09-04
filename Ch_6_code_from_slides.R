rm(list=ls())
options(scipen = 999)
RNGkind(sample.kind = "Rounding")

################
## Chapter 6 ##
################

# Example: Acceptance of Personal Loan. 

# Goal: build a model that identifies customers who are most likely to 
# accept the loan offer in future mailings

# we want to construct a logistic regression model for classification of customers
#setwd("/cloud/project/R-Files/Chapter 10")
#setwd("C:/Users/nykon/Desktop/나영이/#Data Analytics/workplace")

df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/UniversalBank.csv")
colnames(df) <- tolower(colnames(df)) #lower-case variable names
df <- subset(df, select = -c(id, zip.code)) #the minus-sign drops columns!

# education is coded as integer, we want to recode it as factor
# treat education as categorical (R will create dummy variables)
df$education <- factor(df$education, levels = c(1, 2, 3), 
                        labels = c("_undergrad", "_graduate", "_advanced"))
table(df$education)
library(Hmisc)
describe(df)

# partition data
set.seed(2)
train.index <- sample( c(1:dim(df)[1]) , dim(df)[1]*0.6 )  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.simple <- glm(personal.loan ~ income, data = train.df, family = "binomial") 
summary(logit.simple)

str(logit.simple$coefficients)
( b0 <- logit.simple$coefficients[1] )
( b1 <- logit.simple$coefficients[2] )

p <- function(x) exp(b0 + b1*x) / (1 + exp(b0 + b1*x))
ggplot(train.df, aes(y= personal.loan, x=income)) + geom_point() + stat_function(fun = p) + xlim(0,250) 

logit.simple.pred <- predict(logit.simple, valid.df, type = "response")

library(caret)
library(e1071)
classifications <- as.factor(ifelse(logit.simple.pred > 0.5, 1, 0))
confusionMatrix(classifications, as.factor(valid.df$personal.loan), positive="1")


# All predictors
logit.reg <- glm(personal.loan ~ ., data = train.df, family = "binomial") 
summary(logit.reg)
table(df$education)
# dep. var.: Loan offer, indep. var.s: all variables of the dataset
# like in linear regression: one category of education is excluded because of the collinearity


# Evaluating Classification Performance 

## confusion matrix:

# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
# first 5 actual and predicted records
data.frame(actual = valid.df$personal.loan[1:5], predicted = logit.reg.pred[1:5])

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$personal.loan), positive="1")
#    133 customers are correctly specified as responders to the last personal loan campaign
#    1779 customers are correctly specified as non-responders
#    misclassification rate = 1 - 0.956 = 0.044
#    with cut-off 50%
confusionMatrix(as.factor(ifelse(logit.simple.pred > 0.33, 1, 0)), as.factor(valid.df$personal.loan), positive="1")


## Lift Charts:
library(gains)
gain <- gains(valid.df$personal.loan, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$personal.loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l", axes =  T)
lines(c(0,sum(valid.df$personal.loan))~c(0, dim(valid.df)[1]), lty=2)
axis(side = 1, at = seq(from = 250, to = 2000, by = 250))

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$personal.loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#   taking the 10% of customers that are most likely to be responders, yields 7.9 times
#   as many actual responders compared to a random draw of the 10%.
