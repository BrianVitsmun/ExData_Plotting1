## plot2
##
## Per Plot2 assignment requirements, this function:
## -Reads the data from the file
## -Selects the columns and days needed for plot2
## -Saves a line graph to plot2.png
##
## Data file name is hard coded, so the function fails if everything is not
## in the expected location.
##
## -Brian Vitsmun

plot2 <- function() {
    library(dplyr)
    library(tidyr)
    
    data <- read.table(".\\data\\household_power_consumption.txt",
                       header = TRUE,
                       sep = ";",
                       na.strings = "?")
    
    # use dplyr and tidyr functions to get relevant data
    data <- tbl_df(data)
    data <- data %>%
            select(Date, Time, Global_active_power) %>%
            filter(Date == "1/2/2007" | Date == "2/2/2007") %>%
            unite(DateTime, Date, Time, sep=" ")
    
    # DateTime strings converted to POSIXlt
    data$DateTime <- strptime(data$DateTime, format="%d/%m/%Y %H:%M:%S")
    
    # open png device, draw graph per project requirements, close device
    # note that png device defaults to the required 480x480 pixels
    # note "Thu  Fri  Sat" on x axis is automatic with POSIXlt DateTimes
    png(filename = "plot2.png")
    
    plot(data$DateTime,
         data$Global_active_power,
         type = "l",
         ylab = "Global Active Power (kilowatts)",
         xlab = "")
    
    dev.off()
    
    rm(data)
}