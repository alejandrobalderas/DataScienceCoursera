setwd("~/Coursera/DataScience/DataScienceCoursera/05_ReproducibleResearch")

library(knitr)
library(ggplot2)
library(dplyr)
library(reshape2)

db_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"   
db_zip <- "StormData.csv.bz2"
if (!file.exists(db_zip)) {    
    download.file(db_url, db_zip, method="curl")    
}  

df <- read.csv("StormData.csv.bz2")

str(df)
cols <- c("EVTYPE", "FATALITIES", "INJURIES", "CROPDMG", "CROPDMGEXP", "PROPDMG", "PROPDMGEXP")
dat <- df[,cols]

head(dat)

# Show level of Expenses
levels(dat$PROPDMGEXP)
# Data Cleansing
# Digits
dat$CROPDMGEXP <- gsub("[[:digit:]]", "10", dat$CROPDMGEXP)    
dat$PROPDMGEXP <- gsub("[[:digit:]]", "10", dat$PROPDMGEXP)
## +    
dat$CROPDMGEXP <- gsub("\\+", "1", dat$CROPDMGEXP)    
dat$PROPDMGEXP <- gsub("\\+", "1", dat$PROPDMGEXP)    
## -,?  
dat$CROPDMGEXP <- gsub("[-\\?]", "0", dat$CROPDMGEXP)    
dat$PROPDMGEXP <- gsub("[-\\?]", "0", dat$PROPDMGEXP)    
# Change the levels for the H, K, M, B
dat$PROPDMGEXP <- gsub("[Hh]","100",dat$PROPDMGEXP)
dat$PROPDMGEXP <- gsub("[Kk]","1000",dat$PROPDMGEXP)
dat$PROPDMGEXP <- gsub("[Mm]","1000000",dat$PROPDMGEXP)
dat$PROPDMGEXP <- gsub("[Bb]","1000000000",dat$PROPDMGEXP)  
dat$CROPDMGEXP <- gsub("[Hh]","100",dat$CROPDMGEXP)
dat$CROPDMGEXP <- gsub("[Kk]","1000",dat$CROPDMGEXP)
dat$CROPDMGEXP <- gsub("[Mm]","1000000",dat$CROPDMGEXP)
dat$CROPDMGEXP <- gsub("[Bb]","1000000000",dat$CROPDMGEXP) 
# Empty values
dat$PROPDMGEXP[dat$PROPDMGEXP == ""] <- 0
dat$CROPDMGEXP[dat$CROPDMGEXP == ""] <- 0
# Parse values as numeric
dat$PROPDMGEXP <- as.numeric(dat$PROPDMGEXP)
dat$CROPDMGEXP <- as.numeric(dat$CROPDMGEXP)
# Calculate the total damage
dat <- mutate(dat,PROPEXPTOTAL = PROPDMGEXP*PROPDMG)
dat <- mutate(dat,CROPEXPTOTAL = CROPDMGEXP*CROPDMG)


# =====================JUNK CODE

fatalities_per_event <- aggregate(FATALITIES ~ EVTYPE, dat, sum)
injuries_per_event <- aggregate(INJURIES ~ EVTYPE, dat,sum)
fatalities_and_injuries_per_event <- merge(fatalities_per_event,injuries_per_event, by="EVTYPE")
fatalities_and_injuries_per_event <- aggregate(cbind(FATALITIES,INJURIES)~EVTYPE,dat,sum,na.rm=TRUE)

fatalities_per_event <- fatalities_per_event[order(fatalities_per_event$FATALITIES, decreasing = TRUE),]
injuries_per_event <- injuries_per_event[order(fatalities_per_event$FATALITIES, decreasing = TRUE),]
fatalities_and_injuries_per_event <-
    fatalities_and_injuries_per_event[order(fatalities_and_injuries_per_event$INJURIES, decreasing = T),]
# ==============================


fatalities_and_injuries_per_event <- aggregate(cbind(FATALITIES,INJURIES)~EVTYPE,dat,sum,na.rm=TRUE)
tmpdata <- arrange(fatalities_and_injuries_per_event, desc(FATALITIES + INJURIES))[1:10,]
tmpdata <- melt(tmpdata,id.vars = "EVTYPE")
g<- ggplot(tmpdata, aes(x = reorder(EVTYPE,-value), y = value, fill = variable))
g + geom_bar(stat="identity") + labs(title = "Fatalities + Injuries for the top 10 most harmful events",
                                     x = "Event Type", y="Number Of People Involved")+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

table_data <- mutate(fatalities_and_injuries_per_event, TOTAL = FATALITIES + INJURIES)
table_data <- arrange(table_data, desc(FATALITIES + INJURIES))[1:10,]
table_data <- format(table_data, big.mark = ",", decimial.mark = ".")

# Which types of events have the greatest economic consequences across the United States? 

head(dat)
expenses_per_event <- aggregate(cbind(PROPEXPTOTAL, CROPEXPTOTAL) ~ EVTYPE, dat, sum)
names(expenses_per_event) <- c("EVTYPE","Property.Damage","Crop.Damage")
expenses_per_event <- arrange(expenses_per_event, desc(Property.Damage, Crop.Damage))[1:15,]

tmpdata <- melt(expenses_per_event, id.vars = "EVTYPE")

g <- ggplot(tmpdata, aes(x=reorder(EVTYPE,-value),y=value,fill = variable))
g + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 30, hjust = 1))+
    scale_fill_discrete(name = "",labels = c("Property Damage","Crop Damage"))
