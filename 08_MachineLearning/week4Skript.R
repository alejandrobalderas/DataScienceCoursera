setwd("~/Coursera/DataScience/DataScienceCoursera/08_MachineLearning")

library(ggplot2)
library(caret)
library(tictoc)
set.seed(323)

# Load the data
training <- read.csv("./Data/pml-training.csv")
testing <- read.csv("./Data/pml-testing.csv")

# qplot(classe, data = training, fill = user_name)



any.is.na <- function(vec){any(is.na(vec))}
tmp.training <- sapply(training,any.is.na)
tmp.testing <- sapply(testing,any.is.na)
tmp <- !(tmp.training | tmp.testing)

training <- training[,tmp]
testing <- testing[,tmp]
eliminateIndex <- -c(1,3,4,5,6,7)
training <- training[,eliminateIndex]
testing <- testing[,eliminateIndex]

training$classe <- as.factor(training$classe)

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)


library(tictoc)
tic()
# Model
modelFit <- train(classe~., data = training, method = "rf", trControl = fitControl)
toc()

stopCluster(cluster)
registerDoSEQ()

pred <- predict(modelFit, testing)


