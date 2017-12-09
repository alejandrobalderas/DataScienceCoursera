library(lubridate)

#Read the Data to the df dataframe this time all collumns are read as characters
df <- read.table("household_power_consumption.txt",sep = ";",header = TRUE,colClasses = "character")
# Change the format of the Date Variables to a Date format using lubridate
df$Date <-dmy(df$Date)
df$Time <-hms(df$Time)

# Subsetting the df just to use the dates needed
s_df <- subset(df,df$Date==dmy("01-02-2007")| df$Date==dmy("02-02-2007"))
datesForPlot <- s_df$Date + s_df$Time

#par(mfrow=c(2,2), oma=c(2,2,0,4),mar=c(3,3,2,0))
#
png("plot4.png",width = 480, height = 480, units = "px")
par(mfrow = c(2,2))
plot(datesForPlot,as.numeric(s_df$Global_active_power),type = "l",xlab="",ylab = "Global Active Power (kilowatts)")
plot(datesForPlot,as.numeric(s_df$Voltage),type = "l",xlab="datetime",ylab = "Voltage")

plot(datesForPlot,as.numeric(s_df$Sub_metering_1),type = 'l',xlab = "",ylab = "Energy sub metering")
lines(datesForPlot,as.numeric(s_df$Sub_metering_2),col="red")
lines(datesForPlot,as.numeric(s_df$Sub_metering_3),col="blue")
legend("topright",bg="transparent",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col =c("black","blue","red"),lty = 1,bty = "n")

with(s_df,plot(datesForPlot,as.numeric(Global_reactive_power),xlab = "datetime",ylab = "Global Active Power",type = 'l'))

dev.off()

