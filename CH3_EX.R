rm(list=ls()) # Removes all objects (datasets variables, etc.) from the environment
options(scipen = 999) # regulates values are displayed (fixed or exponential notation)
RNGkind(sample.kind = "Rounding") #Specifies the random number generator, so that results for mac and windows user match


####################################
## Exercise - Chapter 3 ##
####################################

setwd("C:/Users/nykon/Desktop/나영이/#Data Analytics/workplace")

#a)	Load the data set “BostonHousing.csv” in R and inspect if the data is 
# loaded correctly.
housing.df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/BostonHousing.csv", header=TRUE)

#b)	Your main variable of interest is the median value of owner-occupied homes 
#in $1000s (MEDV).  Use the package ggplot2. Plot a histogram of MEDV to get a 
#first impression of the distribution of MEDV. Try different bin 
#widths (1, 5, 10, 15) and inspect the effect.
library(ggplot2)

ggplot(housing.df)

ggplot(housing.df, aes(MEDV)) + geom_histogram(binwidth = 1)

ggplot(housing.df, aes(MEDV)) + geom_histogram(binwidth = 5)
ggplot(housing.df, aes(MEDV)) + geom_histogram(binwidth = 10)
ggplot(housing.df, aes(MEDV)) + geom_histogram(binwidth = 15)

ggplot(housing.df, aes(MEDV)) +  geom_histogram(binwidth = 1)
ggplot(housing.df, aes(MEDV)) +  geom_histogram(binwidth = 1, fill = "navy")
ggplot(housing.df, aes(MEDV)) +  geom_histogram(binwidth = 1, fill = "pink")
ggplot(housing.df, aes(MEDV)) +  geom_histogram(binwidth = 1, fill = "red")

ggplot(housing.df, aes(MEDV)) +  
  geom_histogram(binwidth = 1, fill = "navy", alpha = 0.7)

ggplot(housing.df, aes(MEDV)) +  
  geom_histogram(binwidth = 1, fill = "navy", alpha = 0.5)


#c)	You think that in tracts that boarder the Charles river (and thus are more 
#attractive) house prices should be higher. To check this, create a boxplot of 
#MEDV for both categories of CHAS. Also create a bar chart (with the mean of  MEDV)
#to verify this. Hint: Recode the variable CHAS as a factor variable for that
#using the function as.factor().

# Boxplot of MEDV (without the Charles river)
ggplot(housing.df, aes(MEDV)) + 
  geom_boxplot(fill = "navy", alpha = 2/3) + 
  stat_boxplot(geom = "errorbar", width = 0.5 )


#recode the CHAS variable as factor variable

housing.df$CHAS <- as.factor(housing.df$CHAS)

# Boxplot of MEDV by CHAS

ggplot(housing.df, aes(y= MEDV, x= CHAS)) + 
  geom_boxplot(fill = "navy", alpha = 2/3) + 
  stat_boxplot(geom = "errorbar", width = 0.5 )



ggplot(housing.df, aes(y= MEDV, x= CHAS)) + 
  geom_bar(stat = "summary", fun = mean, fill = "navy", alpha = 0.7)


#d)	The next possible determinant is the location. Create a scatterplot of MEDV 
#and DIS. Put MEDV on the y-axis and DIS on the horizontal axis.


ggplot(housing.df, aes(y= MEDV, x= DIS)) + geom_point(color = "navy", alpha = 1/2)

#e)	Combine the analysis in c) and d) and colorcode the dots within the 
#scatterplot by CHAS. 


ggplot(housing.df, aes(y= MEDV, x= DIS, color = CHAS)) + geom_point(alpha = 1/2)



#f)	Draw a scatterplot of MEDV and LSTAT. Try out the geom_smooth()function 
#to add a layer with conditional mean to the scatter plot.

ggplot(housing.df, aes(y= MEDV, x= LSTAT)) + 
  geom_point(color = "navy", alpha = 0.7) +
  geom_smooth(method = lm, formula = 'y ~ x + I(x^2)', se = TRUE)

#2
#a)	Calculate the mean, minimum, maximum and standard deviation of MEDV using
#standard R functions and the summary() function.

summary(housing.df$MEDV)
mean(housing.df$MEDV)
min(housing.df$MEDV)
max(housing.df$MEDV)
sd(housing.df$MEDV)

#b)	Load the psych package and try the commands describeFast() and describe().
library(psych)
describeFast(housing.df)
describe(housing.df)
#c)	Rename the variable CAT..MEDV to CAT.MEDV.  using the  command names(), 
#relabel the possible outcomes using the command levels().
names(housing.df)[14] <- "CAT.MEDV"

housing.df$CAT.MEDV <- as.factor(housing.df$CAT.MEDV)
levels(housing.df$CAT.MEDV)
levels(housing.df$CAT.MEDV)[1] <- "not above $30,000"
levels(housing.df$CAT.MEDV)[2] <- "above $30,000"

#d)	Cross tabulate CAT.MEDV and CHAS.
table(housing.df$CHAS)
table(housing.df$CAT.MEDV)

table(housing.df$CAT.MEDV, housing.df$CHAS)

levels(housing.df$CHAS)[1] <- "not nearby the river"
levels(housing.df$CHAS)[2] <- "nearby the river"
table(housing.df$CAT.MEDV, housing.df$CHAS)
