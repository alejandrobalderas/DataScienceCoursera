

setwd("~/Coursera/DataScience/DataScienceCoursera/04_ExploratoryDataAnalysis/CourseProject2")

fileName <- "data/exdata_data_NEI_data.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

# Read Data

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")