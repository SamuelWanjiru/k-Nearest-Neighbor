
library(class)
library(caret)
#setwd("C:/Users/user/Desktop/school/2.1/non parametri regression/Hospital Data.csv")
#Reading in the data
knn.data <- read.csv("C:/Users/user/Desktop/school/2.1/non parametri regression/Hospital Data.csv",header=TRUE) 
attach(knn.data)
head(knn.data)

#Sampling Training and Testing Data
set.seed(1234)
Indices.Training <- sample(1:nrow(knn.data), round((2/3)*nrow(knn.data),0),replace=FALSE )

#Testing Set
Training.set  <- knn.data[Indices.Training,]

#Testing Set
Testing.set  <- knn.data[-Indices.Training,]
#Checking for samples adequacy especially on the Default Variable

summary(Training.set)
summary(Testing.set)

#Fitting the K-NN model
training.predictors <- Training.set[,c(1,6)]
training.outcome <- Training.set[,7] 
test.predictors <- Testing.set[,c(1,6)] 
test.outcome <- Testing.set[,7]

knn.modell <- knn(training.predictors, test.predictors, training.outcome , k = 1, prob = FALSE)

#Viewing the Original data and the Predicted Outcome
View(cbind(test.predictors,test.outcome,knn.modell))

#Tables
table(knn.modell)
summary1 <- table(knn.modell,test.outcome)
summary1
summary2 <- prop.table(summary1)
summary2
# % of correct classifications
sum(diag(summary2)) #You may fine tune the model by adjusting K against the reported overall Percentage

#Confusion Matrix and associated Statistics and Tests in R 

confusionMatrix(summary1)


