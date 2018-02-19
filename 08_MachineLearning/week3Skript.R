# WEEK 3
setwd("~/Coursera/DataScience/DataScienceCoursera/08_MachineLearning")

library(caret)
library(ggplot2)

# Load Data
data(iris)

# Create Data Partitions
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

qplot(Petal.Width, Sepal.Width, color = Species, data = iris)

modelFit <- train(Species ~ ., data = iris, method= "rpart")
modelFitRF <- train(Species ~ .,data = iris, method = "rf")

plot(modelFit$finalModel, uniform = T, main="Classification Tree")
text(modelFit$finalModel, use.n=T, all = T, cex = .8)


## Question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]


