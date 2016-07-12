#Predict the age of the Abalone using the various information given
# Predicting the age of abalone from physical measurements.  The age of
#abalone is determined by cutting the shell through the cone, staining it,
#and counting the number of rings through a microscope -- a boring and
#time-consuming task.  Other measurements, which are easier to obtain, are
#used to predict the age. 

#Load the data
setwd("C:/Users/Sabarish/Desktop/BDAP/R/UCI_Machine_Learning")
abalone <-read.csv("abalone_age_predict.csv", header = T)
abalone <- na.omit(abalone)
summary(abalone)
#Here the predicted variable is going to be the number of rings.
pairs(abalone)
summary(abalone$Sex)
mean(abalone[which(abalone$Sex == "F"),9])
mean(abalone[which(abalone$Sex == "M"),9])
mean(abalone[which(abalone$Sex != "M" | abalone$Sex != "F"),9])
library(corrplot)
x <-cor(abalone[,-1])
corrplot(x,method = "number")


#Split the data
train <- sample(1:3783,3000)
test <- -train

training_data <- abalone[train,]
testing_data <- abalone[test,]

#build the model
model <- lm(Rings ~., data = training_data)
summary(model)


#calculate the MSE
predicted_y <- predict(model,newdata = testing_data)
actual_y <- testing_data[,9]
error <- actual_y - predicted_y
error_squared <- error^2
mean(error_squared)

###########################################
#Let's remove the Length and Diameter from the model
#build the model
model <- lm(Rings ~.-Height -Length -Diameter, data = training_data)
summary(model)


#calculate the MSE
predicted_y <- predict(model,newdata = testing_data)
actual_y <- testing_data[,9]
error <- actual_y - predicted_y
error_squared <- error^2
mean(error_squared)

#You'd have to try every possible model before you reach a final conclusion on the best possible model. Thankfully there is a package that helps you do even that!
library(leaps)
regfit.full=regsubsets(Rings~.,abalone)
sum <- summary(regfit.full)
names(sum)
sum$rsq


###########################################
#Predict the sex of a abalone given its different dimensions
#Here the predicted variable is going to be the number of rings.
pairs(abalone)
summary(abalone$Sex)
library(corrplot)
x <-cor(abalone[,-1])
corrplot(x,method = "number")


#Split the data
train <- sample(1:3783,3000)
test <- -train

training_data <- abalone[train,]
testing_data <- abalone[test,]

library(foreign)
library(nnet)
#build the model
model_sex <- multinom(Sex ~ ., data= abalone)
summary(model_sex)

predicted_sex <- predict(model_sex,newdata = testing_data)
predicted_sex <- as.factor(predicted_sex)
actual_sex <- testing_data$Sex
table(predicted_sex,actual_sex)



#######################################################

#Classifying using Naive Bayes
library(mlbench)
library(e1071)
model_nb <- naiveBayes(Sex ~ -Length -Diameter -Viscera.weight, data = abalone)
summary(model_nb)
predicted_nb <- predict(model_nb, newdata = testing_data, type = "raw")
actual_nb <- testing_data$Sex
table(predicted_nb,actual_nb)

############################################################
actual_knn <- abalone$Sex
training_knn <- actual_knn[train]
testing_knn <- actual_knn[test]
##KNN Classification
library(class)
model_knn <- knn(train = training_data[,-1], test = testing_data[,-1], cl = training_knn, k = 5)

summary(model_knn)
mean(model_knn == testing_knn)
table(model_knn, testing_knn)


