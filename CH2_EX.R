rm(list=ls()) # Removes all objects (datasets variables, etc.) from the environment
options(scipen = 999) # regulates values are displayed (fixed or exponential notation)
RNGkind(sample.kind = "Rounding") #Specifies the random number generator, so that results for mac and windows user match


####################################
## Exercise - Chapter 2 ##
####################################

#5)	Use the interface of RStudio to set a working directory
setwd("C:/Users/nykon/Desktop/나영이/#Data Analytics/workplace")

#6)	Use the interface  to load the data set WestRoxbury.csv and assign the name housing.df for this data frame. 
housing.df <- read.csv("C:/Users/nykon/Desktop/data/WestRoxbury.csv", header=TRUE)


#7)	Try out the View command: view(housing.df). 
View(housing.df)

#8)	Try out to display specific rows and columns of a data set by adding with dataset.df[rows, columns] , as discussed in the slides.
housing.df[1:20, 1]
housing.df[, 1]
housing.df[1, ]
housing.df[1:5, ]

# 9) Try out the commands we discussed in the slides, and find minimal working examples to verify that you understand what the command does (type ?command to get a description what this command does). The commands are:
# mean()
x <- c(0:10, 50)
mean(x)
mean(housing.df$TOTAL.VALUE)

# median()
median(housing.df$TAX)
median(housing.df$ROOMS)

# summary()
summary(housing.df)
# dim()
dim(housing.df)
dim(housing.df)[1]
# head()
head(housing.df)
# row.names() or rownames()
row.names(housing.df)
# set.seed()
# sample()

c(1:100)
sample(c(1:100), 3)
sample(c(1:100), 3)
sample(c(1:100), 3)  # new random draws generate new sequences of numbers. Surprise!

set.seed(5)
sample(c(1:100), 3)
set.seed(5)
sample(c(1:100), 3)
set.seed(5) 
sample(c(1:100), 3)

# setdiff()
setdiff(c(1:10), c(9, 7, 2, 8))
# union()
union(c(1:10), c(9, 7, 2, 8, 42, -3))

# 10)	Normalize the data in the following table 
# a)	Generate a variable for age and a variable for income containing the values given in the table.
age <- c(25, 56, 65, 32, 41, 49)
age
income <- c(49000, 156000, 99000, 192000, 39000, 57000)

# b)	Then use the command rescale() in order to min-max normalize (between 0 and 1) the two variables. 
# Use scale() to standardize (mean=0, s.d.=1) the two variables. 
# Check whether everything worked fine by recalculating it manually (the command for the standard derivation is sd).
library(scales)
age_norm <- rescale(age, to = c(0, 1))
income_norm <- rescale(income)

(56-25)/(65-25)

age_norm2 <- (age-min(age))/(max(age)-min(age))
age_norm2
age_norm == age_norm2
income_norm2 <- (income-min(income))/(max(income)-min(income))
income_norm == income_norm2


age_std <- scale(age, center = TRUE, scale = TRUE)
income_std <- scale(income, center = TRUE, scale = TRUE)


mean(age)
sd(age)

(25-44.66667)/(14.97554)

age_std2 <- (age-mean(age))/(sd(age))
income_std2 <- (income-mean(income))/(sd(income))

age_std == age_std2

#11
#a)	Load the data set ToyotaCorolla.csv.
toyota.df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/ToyotaCorolla.csv", header=TRUE)

#b)	The dataset has two categorical attributes, “Fuel Type” and “Color”. 
# Transform the two categorical variables into dummies. 

library(dummies)

toyota.df.Fuel.Type <- dummy(toyota.df$Fuel_Type)
toyota.df.Color <- dummy(toyota.df$Color)

# c)	Prepare the dataset for data mining techniques of supervised learning by creating partitions in R. 
# Select all variables and partition the data with percentages for training (50%),
# validation (30%), and test (20%) sets. Describe the roles that these partitions will play in modelling.

set.seed(7)
train.rows <-  sample(rownames(toyota.df), dim(toyota.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(toyota.df), train.rows), dim(toyota.df)[1]*0.3)
test.rows <- setdiff(row.names(toyota.df), union(valid.rows, train.rows))


train.data <- toyota.df[train.rows, ]
valid.data <- toyota.df[valid.rows, ]
test.data <- toyota.df[test.rows, ]

