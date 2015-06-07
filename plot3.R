## plot3
##
## Per Plot3 assignment requirements, this function:
## -Reads the data from the file
## -Selects the columns and days needed for plot3
## -Saves a line graph to plot3.png
##
## Data file name is hard coded, so the function fails if everything is not
## in the expected location.
##
## -Brian Vitsmun

plot3 <- function() {
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
                   Sub_metering_1,
                   Sub_metering_2,
                   Sub_metering_3) %>%
            filter(Date == "1/2/2007" | Date == "2/2/2007") %>%
            unite(DateTime, Date, Time, sep=" ")
    
    # DateTime strings converted to POSIXlt
    data$DateTime <- strptime(data$DateTime, format="%d/%m/%Y %H:%M:%S")
    
    # open png device, draw graph per project requirements, close device
    # note that png device defaults to the required 480x480 pixels
    # note "Thu  Fri  Sat" on x axis is automatic with POSIXlt DateTimes
    png(filename = "plot3.png")
    
    # base plot with black metering 1 line
    plot(data$DateTime,
         data$Sub_metering_1,
         type = "l",
         ylab = "Energy sub metering",
         xlab = "")
    
    # red metering 2 line
    lines(data$DateTime, data$Sub_metering_2, col = "red")

    # blue meterint 3 line
    lines(data$DateTime, data$Sub_metering_3, col = "blue")
    
    legend("topright",
           c("Sub_metering_1",
             "Sub_metering_2",
             "Sub_metering_3"),
           lty=c(1,1,1),
           col=c("black","red","blue"))
    
    dev.off()
    
    rm(data)
}