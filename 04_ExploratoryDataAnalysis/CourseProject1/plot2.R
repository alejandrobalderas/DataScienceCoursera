library(lubridate)

#Read the Data to the df dataframe
df <- read.table("household_power_consumption.txt",sep = ";",header = TRUE)
# Change the format of the Date Variables to a Date format using lubridate
df$Date <-dmy(df$Date)
df$Time <-hms(df$Time)

# Subsetting the df just to use the dates needed
s_df <- subset(df,df$Date==dmy("01-02-2007")| df$Date==dmy("02-02-2007"))

# Saving the variables that will be plotted
dateToPlot <- s_df$Date + s_df$Time
Global_active_power_Numeric <- as.numeric(levels(s_df$Global_active_power))[s_df$Global_active_power]


png("plot2.png",width = 480, height = 480, units = "px")
plot(dateToPlot,Global_active_power_Numeric,type = "l",xlab="",ylab = "Global Active Power (kilowatts)")
dev.off()
