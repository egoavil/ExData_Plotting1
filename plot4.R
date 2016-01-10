plot4 <- function(){
    #Load and format the data so we are using dates
    df <- read.table("household_power_consumption.txt", header = TRUE, 
                     sep = ";", na.strings = "?")
    #remove ? (or na) 
    good <- complete.cases(df)
    df <- df[good, ]
    df$Date <- as.Date(df$Date, format="%d/%m/%Y")
    df$Time <- paste(df$Date, df$Time, sep=" ")
    df$Time <- strptime(df$Time, "%Y-%m-%d %H:%M:%S")
    
    #declare the dates we are going to be subsetting by
    DATE1 <- as.Date("2007-02-01")
    DATE2 <- as.Date("2007-02-02")
    
    #narrow by date
    filtered_df <- df[df$Date >= DATE1 & df$Date <= DATE2,]
    
    #Generate plot (6.4 inches equals 480 pixels)
    quartz(title = "Global Active Power",width = 6.4, height = 6.4, 
           file = "plot4.png", type = "png")
    
    par(mfcol = c(2,2))
    
    #first plot out of 4
    plot(filtered_df$Time, filtered_df$Global_active_power, 
         ylab="Global Active Power", 
         xlab = "", type = "l")
    
    #second plot out of 4
    ylimits = c(0,60)
    plot(filtered_df$Time, filtered_df$Sub_metering_1, xlab = "", 
         ylab = "Energy sub metering", type = "l", col = "black", 
         ylim = ylimits)
    par(new = TRUE)
    plot(filtered_df$Time, filtered_df$Sub_metering_2, xlab = "", 
         axes = FALSE, ylab = "", type = "l", col = "red", ylim = ylimits)
    par(new = TRUE)
    plot(filtered_df$Time, filtered_df$Sub_metering_3, xlab = "", 
         axes = FALSE, ylab = "", type = "l", col = "blue", ylim = ylimits)
    legend("topright",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           #bg = "transparent",
           #bty = "n",
           lty = c(1,1,1),
           col = c("black", "red", "blue")
    )
    
    #third plot out of 4
    plot(filtered_df$Time, filtered_df$Voltage, type = "l", ylab = "Voltage"
         , xlab = "")
    
    #4th plot
    plot(filtered_df$Time, filtered_df$Global_reactive_power, type = "l", 
         ylab = "Global_reactive_power", xlab = "")
    dev.off()
}