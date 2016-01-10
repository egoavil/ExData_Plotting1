plot2 <- function(){
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
           file = "plot2.png", type = "png")
    plot(filtered_df$Time, filtered_df$Global_active_power, 
         ylab="Global Active Power (kilowatts)", 
         xlab = "", type = "l")
    dev.off()
}