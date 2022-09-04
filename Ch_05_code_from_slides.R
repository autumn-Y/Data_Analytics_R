rm(list=ls())
options(scipen = 999)
RNGkind(sample.kind = "Rounding")

###############
## Chapter 5 ##
###############

### Bivariate Regression

car.df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/ToyotaCorolla.csv")
library(Hmisc)
describe(car.df$HP) 

# Scatterplot from the lecture
ggplot(car.df, aes(y=Price, x=HP)) +    geom_point() + expand_limits(x = 0, y = 0) + 
                                        stat_smooth(method = 'lm', se = FALSE)

# In the following, we'll only use the first 1000 observations:
car.df <- car.df[1:1000, ]
# select variables for regression:
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

# partition data:
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)

train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on in the training set.
car.lm.simple <- lm(Price ~ HP , data = train.df) # here we use the training dataset.
options(digits=2)
summary(car.lm.simple)

Price.pred.simple <- predict(car.lm.simple, valid.df) # here we use the validation dataset
data.frame(     'HP' = valid.df$HP , 
                'price_hat' = price_hat <- Price.pred.simple, 
                'price'     = price <- valid.df$Price , 
                'error'     = price - price_hat)[82:90,]

library(forecast)
accuracy(valid.df$Price, Price.pred.simple)


### Multiple Regression

# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df) # here we use the training dataset.
# use options() to ensure numbers are not displayed in scientific notation.

summary(car.lm)
#   Shows the estimated coefficients.
#   Regression coefficients are then used to predict prices of individual used
#   Toyota Corolla cars based on the predictors.

# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm, valid.df) # here we use the validation dataset
options(scipen=999, digits = 0)

some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
# shows a sample of predicted prices for 20 cars in the validation set

options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
library(forecast)
accuracy(Price.pred.simple, valid.df$Price)
accuracy(car.lm.pred, valid.df$Price)
#  those measures can be used to compare models.

# how are residuals distributed?
# look at the histogram of residuals
all.residuals <- valid.df$Price - car.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
#   most of the errors are between -2000 and +2000


# Variable Selection in Linear Regression 

# Trade-off between too few and too many predictors:
# - using predictors that are uncorrelated with the outcome
#   variable increases the variance of predictions
#
# - dropping predictors that are actually correlated with
#   the outcome variable can increase the average error (bias) of predictions

## Reducing the Number of Predictors: Exhaustive Search

# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)

# create dummies for fuel type
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))

# replace Fuel_Type column with 2 dummies
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)

search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
# show models
sum$which
#   age
#   age weight
#   age weight HP
#   age weight HP km and so on...

# show metrics
sum$rsq   # R2
sum$adjr2 # adj. R2: increases until 8 predictors and then stabilizes
sum$cp    # Mallow's Cp: indicates that a model with 7 to 8 predictors is good
#    the model with 8 predictors then is Age km HP Automatic Tax weight CNG Diesel

## Reducing the Number of Predictors: Popular Subset Selection Algorithms

#     as we changed the train.df in the meantime, we have to define car.lm again
#     using the train.df including those changes:
car.lm <- lm(Price ~ ., data = train.df) # here we use the training dataset.

# Backward selection:

# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both".

car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 

( selected.vars <- names(car.lm.step$model) )
round( cor(train.df[, selected.vars]) , 2)

# Which variables did it drop?
# Met_Color Automatic CC Doors
# Chosen seven-predictor model is identical to the best seven predictor
# model chosen by the exhaustive search.

# apply model to validation dataset
        #   therefore, we have to generate a dummy for Fuel_Type again:

        # create dummies for fuel type in validation dataset:
        Fuel_Type_val <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=valid.df))

        # replace Fuel_Type column with 2 dummies
        valid.df <- cbind(valid.df[,-4], Fuel_Type_val[,])
        head(valid.df)

car.lm.step.pred.back <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred.back, valid.df$Price)


# Forward Selection:

# create model with no predictors
car.lm.null <- lm(Price ~ 1, data = train.df)
summary(car.lm.null)
# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)  
# Which variables were added?
# all variables 11 are included (Fuel_TypePetrol is colinear)

car.lm.step.pred.for <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred.for, valid.df$Price)

# stepwise regression:
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  
# Which variables were dropped/added?
# As in backward elimination.

car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

