rm(list=ls()) # Removes all objects (datasets variables, etc.) from the environment
options(scipen = 999)  # regulates  values are displayed (fixed or exponential notation)
RNGkind(sample.kind = "Rounding") #Specifies the random number generator, so that results for mac and windows user match
options(warn=-1)

### Exercise 6 ###

#a 
# y = price = willingness to pay

#b
setwd("C:/Users/nykon/Desktop/나영이/#Data Analytics/workplace")

#c
df <- read.csv("C:/Users/nykon/Desktop/나영이/#Data Analytics/data/ToyotaCorolla.csv")
colnames(df) <- tolower(colnames(df))  
# Alternative: pipe Operator
#library("dplyr")
#colnames(df) <- df %>% colnames() %>% tolower()

df <- subset(df, select=c("price", "fuel_type", "km", "hp"))
df$fuel_type = tolower(df$fuel_type) 


#d
table(df$fuel_type) 
mean(df$price[df$fuel_type=="cng"]) 
   # E(price | fuel_type = cng) = 9421
   # E(price | fuel_type = petorl) = 10679
   # E(price | fuel_type = diesel) = 11294
   #buyers seem to value diesel-fueled cars the most

#e
#library("ggplot2")
ggplot(df, aes(y = price, x = fuel_type)) + geom_boxplot() 
#ggplot(df, aes(y = km, x = fuel_type)) + geom_boxplot() 

#library("Hmisc")
#describe(df$price[df$fuel_type=="cng"])

# diesel-fueled cars have the largest variance in prices, 
# presumably because there's a large variation in their characteristics


#f 
model1 <- lm(price ~ fuel_type, data = df)
summary(model1)

#g
lm(price ~ fuel_type, data = df)
lm(price ~ . , data = df)

df$diesel <- as.factor( ifelse(df$fuel_type=="diesel" , 1 , 0) )
#head(df$fuel_type , n=10)
#head(df$diesel , n=10)

#empirical test using regression: do diesel-fueled cars have more
# mileage and/or less hp than other (cng- or petrol-fueled) cars?
lm(diesel ~ km + hp , data = df)

#is petrol related to "good things"?
df$petrol <- as.factor( ifelse(df$fuel_type=="petrol" , 1 , 0) )
lm(petrol ~ km + hp , data = df)


