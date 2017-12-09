library(lubridate)

#Read the Data to the df dataframe this time all collumns are read as characters
df <- read.table("household_power_consumption.txt",sep = ";",header = TRUE,colClasses = "character")
# Change the format of the Date Variables to a Date format using lubridate
df$Date <-dmy(df$Date)
df$Time <-hms(df$Time)

# Subsetting the df just to use the dates needed
s_df <- subset(df,df$Date==dmy("01-02-2007")| df$Date==dmy("02-02-2007"))

# Create the time variable to plot
datesForPlot <- s_df$Date + s_df$Time

plot(datesForPlot,as.numeric(s_df$Sub_metering_1),type = 'l',xlab = "",ylab = "Energy sub metering")
lines(datesForPlot,as.numeric(s_df$Sub_metering_2),col="red")
lines(datesForPlot,as.numeric(s_df$Sub_metering_3),col="blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col =c("black","blue","red"),lty = c(1,1,1))

dev.copy(png,"plot3.png",width = 480, height = 480, units = "px")
dev.off()
