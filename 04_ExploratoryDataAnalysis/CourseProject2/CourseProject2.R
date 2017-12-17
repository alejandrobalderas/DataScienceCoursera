

setwd("~/Coursera/DataScience/DataScienceCoursera/04_ExploratoryDataAnalysis/CourseProject2")

fileName <- "data/exdata_data_NEI_data.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists(fileName)){
    download.file(fileUrl,fileName) 
}

# Read Data

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# =========  Plot 1  =======

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
png("plot1.png",width = 480, height = 480, units = "px")
boxplot(log10(Emissions) ~ year,NEI,xlab = "year", ylab = "log10 PM2.5")
dev.off()

# ========= PLOT 2  =======

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
png("plot2.png",width = 480, height = 480, units = "px")
boxplot(log10(Emissions)~year,NEI[which(
    NEI$fips == "24510" & (NEI$year=="1999"|NEI$year == "2008")),],
    xlab = "year", ylab = "log10 PM2.5 [tons]")
title(main="Comparison of PM2.5 Levels for the years 1999-2008 in Bufallo")
dev.off()

# ======== PLOT 3 =======
library(ggplot2)
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

subNEI <- NEI[which(NEI$fips == "24510" & (NEI$year=="1999"|NEI$year == "2008")),]
subNEI_mean <- aggregate(Emissions~type + year, subNEI,mean)

png("plot3.png",width = 480, height = 480, units = "px")
subNEI <- NEI[which(NEI$fips == "24510"),]
NEI_mean <- aggregate(Emissions~type + year, subNEI,mean)
g <- ggplot(NEI_mean,aes(year,Emissions,group = type))
g + geom_point(color = "steelblue",size = 4, alpha = 0.3) + geom_path(aes(color = type),lwd=1)+
    xlim(1996,2010)+ ylab("Mean Emissions ")
dev.off()

# ======= PLOT 4 =======

library(ggplot2)
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

indexPlot <- grep("[C-c]oal",SCC$EI.Sector)
scc_index <- SCC$SCC[indexPlot]
subNEI <- NEI[which(NEI$SCC %in% scc_index),]
subNEI_median <- aggregate(Emissions~year, subNEI,median)
subNEI_mean <- aggregate(Emissions~year, subNEI,mean)

png("plot4.png",width = 480, height = 480, units = "px")
g <- ggplot(subNEI_mean,aes(x = year, y = Emissions, group = year))
g + geom_bar(stat = "identity", fill = "steelblue") +
    scale_x_discrete(limits = unique(subNEI_mean$year)) +
    labs(x = "Year", y = "Mean Emissions", title="Mean of Coal combustion emissions across the USA")
dev.off()

# ========= PLOT 5 =======
library(ggplot2)
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

vehicleIndex <- grep("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
scc_index <- SCC$SCC[vehicleIndex]
subNEI_vehicle <- NEI[NEI$fips=="24510" & NEI$SCC %in% scc_index,]
subNEI_vehicle <- aggregate(Emissions~year,subNEI_vehicle,mean)

png("plot5.png",width = 480, height = 480, units = "px")
g <- ggplot(subNEI_vehicle, aes(factor(year),Emissions))
g + geom_bar(stat = "identity", fill = "steelblue") + 
    labs(x="Year", y = "Mean Emissions [tons]",title= "Mean Emissions vor Road Vehicles in Baltimore over the years")
dev.off()

# ======== PLOT 6 =======
library(ggplot2)
library(reshape2)

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
vehicleIndex <- grep("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
scc_index <- SCC$SCC[vehicleIndex]
motorVehicleNEI <- NEI[NEI$SCC %in% scc_index,]
baltimoreVehicleNEI <- motorVehicleNEI[motorVehicleNEI$fips =="24510",]
losAngelesVehicleNEI <- motorVehicleNEI[motorVehicleNEI$fips =="06037",]
baltimoreVehicleNEI <- aggregate(Emissions~year,baltimoreVehicleNEI,mean)
losAngelesVehicleNEI <- aggregate(Emissions~year,losAngelesVehicleNEI,mean)

mergedData <- merge(baltimoreVehicleNEI,losAngelesVehicleNEI, by ="year")
names(mergedData) <- c("year","Baltimore","LA")

dataMelt <- melt(mergedData, id = "year")

png("plot6.png",width = 480, height = 480, units = "px")
g <- ggplot(dataMelt,aes(x = year,y = value, fill = variable))
g + geom_col() + facet_grid(.~variable)  + 
    labs(x = "Year",y = "Mean Value PM2.5 [tons]", title = "Motor Vehicle Comparison between Baltimore and LA")+
     scale_x_discrete(limits = unique(dataMelt$year))
dev.off()



## Interesting plots from the quiz review

library(dplyr) 
SMVS <- NEI[(NEI$fips=="24510") & (NEI$type=="ON-ROAD"),] 
SMVS_Y <- summarise(group_by(SMVS, year), Emissions=sum(Emissions)) 

##plotting data usin ggplot library(ggplot2) 
ggplot(dataMelt, aes(x= factor(year),y=Emissions, fill = year))+ 
    geom_bar(stat = "identity", position = "dodge")+ 
    xlab("year")+ 
    ggtitle("Baltimore Motor Vehicle Emissions")
