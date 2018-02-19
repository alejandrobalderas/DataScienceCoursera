

setwd("~/Coursera/DataScience/DataScienceCoursera/08_MachineLearning")

library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type,p=0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)
set.seed(323)

tic()
modelFit <- train(type ~ ., data = training, method = "glm")
modelFit
toc()

predictions <- predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$type)

folds <- createFolds(y = spam$type, k = 10, list = T, returnTrain = T)


## Quiz 2 Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

names <- names(training)
names <- names[-length(names)]

featurePlot(training[names], training$CompressiveStrength, plot = "pairs")

# featurePlot(x = training, y = training["Cement"], plot = "pairs")
qplot(training$CompressiveStrength, training$Cement)
tmp <- 1:dim(training)[1]
library(Hmisc)
cutVar<- cut2(training$FlyAsh,g = 3)
q1 <- qplot(tmp,training$CompressiveStrength, color = cutVar)
cutVar<- cut2(training$Water,g = 3)
q2 <- qplot(tmp,training$CompressiveStrength, color = cutVar)
grid.arrange(q1,q2,nrow =2)

par(mfrow = c(3,3))
ctr = 1
cutVar = matrix(data = NA, nrow = dim(training)[1], ncol = length(names(training))-1)
for (colName in names(concrete)[-9]){
    if(ctr>8){ctr = 1}
    cutVar[,ctr] <- cut2(training[,colName],g = 3) 
    assign(paste0("p",ctr),qplot(tmp,training$CompressiveStrength, color = cutVar[,ctr]))
    
    #assign(paste0("p",ctr),p)
    ctr = ctr + 1
}


cutVar1 <- cut2(training[,"Cement"], g = 3)
p1 <- qplot(tmp,training$CompressiveStrength, color = cutVar1)
cutVar2 <- cut2(training[,"BlastFurnaceSlag"], g = 3)
p2 <- qplot(tmp,training$CompressiveStrength, color = cutVar2)
cutVar3 <- cut2(training[,"FlyAsh"], g = 3)
p3 <- qplot(tmp,training$CompressiveStrength, color = cutVar3)
cutVar4 <- cut2(training[,"Water"], g = 3)
p4 <- qplot(tmp,training$CompressiveStrength, color = cutVar4)
cutVar5 <- cut2(training[,"Superplasticizer"], g = 3)
p5 <- qplot(tmp,training$CompressiveStrength, color = cutVar5)
cutVar6 <- cut2(training[,"CoarseAggregate"], g = 3)
p6 <- qplot(tmp,training$CompressiveStrength, color = cutVar6)
cutVar7 <- cut2(training[,"FineAggregate"], g = 3)
p7 <- qplot(tmp,training$CompressiveStrength, color = cutVar7)
cutVar8 <- cut2(training[,"Age"], g = 4)
p8 <- qplot(tmp,training$CompressiveStrength, color = cutVar8)

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8)



## QUESTION 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(training$Superplasticizer)


## QUESTION 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

covariates <- grep("^IL", names(training))
training.IL <- training[,covariates]

prComp <- prcomp(training.IL)
qplot(1:length(prComp$sdev), prComp$sdev/sum(prComp$sdev))

which(cumsum(prComp$sdev/sum(prComp$sdev))>=0.9)
preProc <- preProcess(training.IL,method = "pca", thresh = 0.9)

## QUESTION 5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

covariates <- grep("^IL", names(training))
training.IL <- training[,c(1,covariates)]
testing.IL <- testing[,c(1,covariates)]

preProc <- preProcess(training.IL[-1], method = "pca", thresh = 0.8)
training.preProc <- predict(preProc, training.IL[-1])

modelFit <- train(training.IL[-1],training.IL$diagnosis, method = "glm")
modelFitPC <- train(training.preProc,training$diagnosis, method = "glm")

testing.preProc <- predict(preProc, testing.IL[-1])
acc1 <- confusionMatrix( predict(modelFit,testing ), testing$diagnosis)$overall["Accuracy"]
acc2 <- confusionMatrix( predict(modelFitPC,testing.preProc ), testing$diagnosis)$overall["Accuracy"]
