library(data.table)
library(dplyr)

setwd("~/Coursera/DataScience/DataScienceCoursera/03_GettingAndCleaningData")

fileName <- "W4_CourseProject.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

#Change names


# Read Feature Names + Activities
features <- fread("./UCI HAR Dataset/features.txt",col.names = c("-","featureNames"))
features <- features$featureNames
featureIndex <- grep("mean\\(\\)|std\\(\\)",features)
featureNames <- features[featureIndex]
activities <- fread("./UCI HAR Dataset/activity_labels.txt",col.names = c("Index","Activity"))


# Test Data
test <- fread("./UCI HAR Dataset/test/X_test.txt")[,featureIndex,with = FALSE]
test <- setnames(test,names(test),featureNames)
test_subject <- fread("./UCI HAR Dataset/test/subject_test.txt",col.names = c("SubjectIndex"))
test_activity <- fread("./UCI HAR Dataset/test/y_test.txt",col.names = c("Activity"))
test <- cbind(test_subject,test_activity,test)

# Train Data
train <- fread("./UCI HAR Dataset/train/X_train.txt")[,featureIndex,with = FALSE]
train <- setnames(train,names(train),featureNames)
train_subject <- fread("./UCI HAR Dataset/train/subject_train.txt",col.names = c("SubjectIndex"))
train_activity <- fread("./UCI HAR Dataset/train/y_train.txt",col.names = c("Activity"))
train <- cbind(train_subject,train_activity,train)

dataset <- rbind(train,test)
dataset[["Activity"]] <- factor(dataset[, Activity], levels = activities[["Index"]], labels = activities[["Activity"]])
dataset[["SubjectIndex"]] <- as.factor(dataset[,SubjectIndex])

dataset_mean <- aggregate(.~Activity + SubjectIndex, dataset,mean)

