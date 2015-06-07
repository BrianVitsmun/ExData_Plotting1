## plot4
##
## Per Plot4 assignment requirements, this function:
## -Reads the data from the file
## -Selects the columns and days needed for plot4
## -Saves a set of 4 line graphs to plot4.png, three of which are similar to
##    plot2, and one which is similar to plot3.
##
## File name is hard coded, so the function fails if everything is not
## in the expected location.
##
## -Brian Vitsmun

plot4 <- function() {
    library(dplyr)
    library(tidyr)
    
    data <- read.table(".\\data\\household_power_consumption.txt",
                       header = TRUE,
                       sep = ";",
                       na.strings = "?")
    
    # use dplyr and tidyr functions to get relevant data
    data <- tbl_df(data)
    data <- data %>%
            select(Date, Time,
                   Global_active_power,
                   Global_reactive_power,
                   Voltage,
                   Sub_metering_1,
                   Sub_metering_2,
                   Sub_metering_3) %>%
            filter(Date == "1/2/2007" | Date == "2/2/2007") %>%
            unite(DateTime, Date, Time, sep=" ")
    
    # DateTime strings converted to POSIXlt
    data$DateTime <- strptime(data$DateTime, format="%d/%m/%Y %H:%M:%S")
    
    # open png device, draw graphs per project requirements, close device
    # note that png device defaults to the required 480x480 pixels
    # note "Thu  Fri  Sat" on x axis is automatic with POSIXlt DateTimes
    png(filename = "plot4.png")
    
    par(mfrow = c(2,2))
    
    # top left single line graph
    plot(data$DateTime,
         data$Global_active_power,
         type = "l",
         ylab = "Global Active Power",
         xlab = "")
    
    # top right single line graph
    plot(data$DateTime,
         data$Voltage,
         type = "l",
         ylab = "Voltage",
         xlab = "datetime")
    
    # bottom left three line graph with legend
    plot(data$DateTime,
         data$Sub_metering_1,
         type = "l",
         ylab = "Energy sub metering",
         xlab = "")
    lines(data$DateTime, data$Sub_metering_2, col = "red")
    lines(data$DateTime, data$Sub_metering_3, col = "blue")
    
    legend("topright",
           c("Sub_metering_1",
             "Sub_metering_2",
             "Sub_metering_3"),
           lty=c(1,1,1),
           col=c("black","red","blue"),
           bty="n")
    
    # bottom right single line graph
    plot(data$DateTime,
         data$Global_reactive_power,
         type = "l",
         ylab = "Global_reactive_power",
         xlab = "datetime")
    
    dev.off()
    
    rm(data)
}