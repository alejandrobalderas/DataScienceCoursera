library(lubridate)

#Read the Data to the df dataframe
df <- read.table("household_power_consumption.txt",sep = ";",header = TRUE)
# Change the format of the Date Variables to a Date format using lubridate
df$Date <-dmy(df$Date)

# Subsetting the df just to use the dates needed
s_df <- subset(df,df$Date==dmy("01-02-2007")| df$Date==dmy("02-02-2007"))
Global_active_power_Numeric <- as.numeric(levels(s_df$Global_active_power))[s_df$Global_active_power]

# Plot 1 Hist Global Active Power
png("plot1.png",width = 480, height = 480, units = "px")
hist(Global_active_power_Numeric,col="red",main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
dev.off()


