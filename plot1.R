## plot1 
##
## Per Plot1 assignment requirements, this function:
## -Reads the data from the file
## -Selects the columns and days needed for plot1
## -Saves a histogram to plot1.png
##
## Data file name is hard coded, so the function fails if everything is not
## in the expected location.
##
## -Brian Vitsmun

plot1 <- function() {
    library(dplyr)
    
    data <- read.table(".\\data\\household_power_consumption.txt",
                       header = TRUE,
                       sep = ";",
                       na.strings = "?")
    
    # use dplyr functions to selecting relevant data
    data <- tbl_df(data)
    data <- data %>%
            select(Date, Global_active_power) %>%
            filter(Date == "1/2/2007" | Date == "2/2/2007")
    
    # open png device, draw hist per project requirements, close device
    # note that png device defaults to the required 480x480 pixels
    png(filename = "plot1.png")
    
    hist(data$Global_active_power,
         col = "red",
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)")
    
    dev.off()
    
    rm(data)
}
