

# ======== QUESTION 1 ========
fileName <- "Quiz4_Q1.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

dt <- read.csv(fileName)
splitNames <- strsplit(names(dt),"wgtp")
splitNames[123]

#========= QUESTION 2 =======
library(dplyr)
fileName <- "Quiz4_Q2.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

dt <- read.csv(fileName,skip = 4,nrows = 190)
dt <- rename(dt,GDP = X.4)
dt$GDP <- as.numeric(as.character(gsub(",","",dt$GDP)))
mean(dt$GDP)

# ====== QUESTION 3 =======
library(dplyr)
fileName <- "Quiz4_Q2.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}
dt <- read.csv(fileName,skip = 4)
dt <- rename(dt,countryNames = X.3)


# ======= QUESTION 4 ======
# Load data from quiz3
library(dplyr)
fileName <- "q3Data1.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}
fileName <- "q3Data2.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

dt1 <- read.csv("q3Data1.csv",skip = 4,nrows = 190)
dt2 <- read.csv("q3Data2.csv")
dt1 <- rename(dt1, CountryCode = X,Ranking = X.1,Country = X.3, GDP = X.4)
mergedData <- merge(dt1,dt2,by.x = "CountryCode",all=FALSE)

#Informaton on fiscal year on special notes
infoFiscalYear <- grep("[Ff]iscal year end",dt2$Special.Notes,value=TRUE)
val1 <- regexpr(": ",infoFiscalYear) + 2
val2 <- regexpr("[0-9];",infoFiscalYear)
strVal <- substr(infoFiscalYear,val1,val2)
strVal <- strsplit(strVal," ")
getMonth <- function(x){x[1]}
months <- sapply(strVal,getMonth)
sum(months == "June")


# ========= QUESTION 5 =========
# 
library(quantmod)
library(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

boolTime <- sampleTimes >ymd("2011-12-31") & sampleTimes < ymd("2013-01-01")

#Week days
thedays <- wday(sampleTimes,label = TRUE)
sum(thedays[boolTime] == "Mo")
