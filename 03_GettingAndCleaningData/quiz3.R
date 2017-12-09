# Quiz 3
# 


# ===== QUESTION 1 =====
# 
fileName <- "dataQuestion1.csv"
if(!file.exists(fileName)){
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
              ,fileName) 
}

data <- read.csv("dataQuestion1.csv")
head(data)

agricultureLogical <- data$ACR == 3 & data$AGS ==6
which(agricultureLogical)


# ===== QUESTION 2 =====
library(jpeg)
fileName <- "q2.jpeg"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName,mode = "wb") 
}
q2 <- readJPEG(fileName,native = TRUE)
quants <- quantile(q2,probs=c(0.3,.8))
quants

# ===== QUESTION 3 =====
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
dt1$Ranking <- as.numeric(as.character(gsub(",","",dt1$Ranking)))
dt1$GDP <- as.numeric(as.character(gsub(",","",dt1$GDP)))
#dt1$GDP <- as.numeric(as.character(dt1$GDP))

mergedData <- merge(dt1,dt2,by.x = "CountryCode",all=FALSE)
dat <- data.frame(mergedData$CountryCode,mergedData$Country,mergedData$Ranking,
                  mergedData$GDP,mergedData$Income.Group)
dat <- rename(dat,CountryCode = mergedData.CountryCode, Ranking = mergedData.Ranking,
              GDP = mergedData.GDP,IncomeGroup = mergedData.Income.Group)
dat <- arrange(dat,GDP)
head(dat,13)

# ======= QUESTION 4 =====

mean(dat$Ranking[dat$IncomeGroup == "High income: OECD"])
mean(dat$Ranking[dat$IncomeGroup == "High income: nonOECD"])

# ======= QUESTION 5 =====

dat$cutRange <- cut(dat$Ranking, breaks = 5)
length(dat$GDP[dat$cutRange %in% lev[1] & dat$IncomeGroup %in% lev2[5]])
