rm(list=ls()) # Removes all objects (datasets variables, etc.) from the environment
options(scipen = 999) # regulates values are displayed (fixed or exponential notation)
RNGkind(sample.kind = "Rounding") #Specifies the random number generator, so that results for mac and windows user match


####################################
## Exercise - Chapter 5 ##
####################################


setwd("C:/Users/nykon/Desktop/나영이/#Data Analytics/workplace")

# 1)	You want to identify house owners to sell insurance to them.
# a)	Load and describe the data in ownerExample.csv.
owner.df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/ownerExample.csv", header=TRUE)

# 24 observations, 2 variables, class owner/nonowner
# Probability : prediction of the model (blackbox for now)


# b) Compute the confusion matrixes with cutoffs 0.25, 0.5, 0.75. Set ‘owner’
#  as the positive class. You will need the confusionMatrix() command from the 
# package “caret”. Which cutoff yields the lowest misclassification rate?

library(caret)

confusionMatrix(
  table(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner'),
  owner.df$Class),
  positive = 'owner')

confusionMatrix(
  table(ifelse(owner.df$Probability > 0.4, 'owner', 'nonowner'),
        owner.df$Class),
  positive = 'nonowner')





# two diagonal cells (upper left, lower right) gives the number of correctly classified
# records
# off-diagonal cells give the number of misclassifications
# here 10 non-owners & 11 owners were correclty classified
# 1 non-owner and 2 owners were misclassified 

#Accuracy is the share of correctly classified observations, here
(10 + 11)/24

# Misclassification rate = 1 - 0.875 = 0.125 
(1 + 2)/24

# Specificity= True Negative Rate= TN/(TN + FP)
10/(10 + 2)
# Sensitivity= True Positive Rate = TP/(TP + FN)
11/(11 + 1)

# different cut-off levels:

confusionMatrix(
  table(ifelse(owner.df$Probability > 0.25, 'owner', 'nonowner'),
        owner.df$Class),
  positive = 'owner')

# Missclassification rate is now 0.2083, before  0.125
# => more observations are missclassified


confusionMatrix(
  table(ifelse(owner.df$Probability > 0.75, 'owner', 'nonowner'),
        owner.df$Class),
  positive = 'owner')

# Missclassification rate is now 0.25
# => more observations are missclassified

# highest Accuracy with the cutoff 0.5

#Optional: short preview of loops:

for(i in c(0.25, 0.5, 0.75)){
  print(confusionMatrix(
    table(ifelse(owner.df$Probability > i, 'owner', 'nonowner'),
    owner.df$Class),
    positive = 'owner'))
}

# c)	What would happen to the accuracy, sensitivity and specificity if 
# you set ‘nonowner’ as the positive class?

# positive class owner:
confusionMatrix(
  table(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner'),
  owner.df$Class),
  positive = 'owner')

# Accuracy : 0.875           
# Sensitivity : 0.9167          
# Specificity : 0.8333  

# positive class nonowner:
confusionMatrix(
  table(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner'),
        owner.df$Class),
  positive = 'nonowner')

# Accuracy : 0.875  
# Sensitivity : 0.8333          
# Specificity : 0.9167   

# => Accuracy stays the same, but Specificity and Sensitivity are mixed up

# d)	Plot the ROC curve and calculate the AUC. 
# Load the package pROC, use the commands: roc(), auc() & plot.roc()
library(pROC)

#first recode the variable Class as factor variable
owner.df$Class <-  as.factor(owner.df$Class )
# relevel it, ensures that the lecel we need comes first in our case 
# we need owner to be the first level
owner.df$Class <- relevel(owner.df$Class, ref="owner")

#compute the numbers needed for the ROC-curve
r <- roc(owner.df$Class, owner.df$Probability)
# plot.roc() actually plots the ROC based on the numbers in the object 'r'
plot.roc(r)
auc(r)

# e)	Asses the performance of the model. 
# Compare the model to a coin toss and to the naïve rule.
# Use the option direction = ">" in the roc() command to 
# correctly asses the AUCs for randomized values.

#AUC to asses the performance of a model = our model has 0.9375
#AUC for coin toss is around 0.5

#Simulate 1000 random assignment to verify this 

r <- roc(owner.df$Class, runif(24), direction = ">")
auc(r)

coin.toss.auc <- c() #generate an empty vector
for (i in 1:1000){
  r <- roc(owner.df$Class, runif(24), direction = ">")
  coin.toss.auc[i] <- auc(r)
}
mean(coin.toss.auc)
  

#### comparing to naive rule:
r <- roc(owner.df$Class, replicate(24, 1), direction = ">")
auc(r)
r <- roc(owner.df$Class, replicate(24, 0), direction = ">")
auc(r)
r <- roc(owner.df$Class, replicate(24, 0.5), direction = ">")
auc(r)
plot.roc(r)
####2
# a)	Load and describe the data in liftExample.csv.
lift.df <- read.csv('C:/Users/nykon/Desktop/나영이/#Data Analytics/data/liftExample.csv')

# 24 observations, prob (prediction), actual (0, 1)
# other variables are not relevant

#	b)	Create a lift chart. Use the lift() command from the package “caret” 
# to compute the numbers and xyplot() for plotting.
library(caret)

lift.example <- lift(relevel(as.factor(actual), ref = "1") ~ prob, data = lift.df)
xyplot(lift.example, plot = "gain")


# c)	Interpret the lift chart.

# 20 % yields around 35% of owners
# writing to the 70% most likely customers we get a yield of 100%

# d)	Create a decile-wise lift chart with four groups. 
# Use the barplot() command for plotting and the gains() command 
# from the package “gains” in order to compute the numbers.

library(gains)

gain <- gains(lift.df$actual, lift.df$prob, groups=4)

barplot(gain$mean.resp/mean(lift.df$actual),
        names.arg = gain$depth,
        xlab = "Depth of File",
        ylab = "Mean Response",
        main = "Decile-wise lift chart")



#Optional same graph with ggplot
mean_resp <-  gain$mean.resp/mean(lift.df$actual)
gain_gg.df <- data.frame(mean_resp)
gain_gg.df$depth <- gain$depth

library(ggplot2)
ggplot(gain_gg.df, aes(y = mean_resp, x = depth)) +
  geom_bar(stat = 'identity', fill = 'navy', alpha = 0.7)


# e)	Interpret the decile-wise lift chart.
# Mailing to 25% of most likely customers yields a 2x higher response rate 
# than random mailing 

sink()
