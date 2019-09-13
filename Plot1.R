library(lubridate)
library(dplyr)

#read data and clean dates
data <- read.csv(file = "exdata_data_household_power_consumption/household_power_consumption.txt",stringsAsFactors=FALSE,
header = TRUE, sep = ";")
data$Date <- dmy(data$Date)
#cut out only the dates we need
datadates <- subset(data, Date == "2007-2-2")
datadates2 <- subset(data, Date == "2007-2-1")
#combine two needed dates
cdata <- rbind(datadates, datadates2)
#fix active power variable
cdata$Global_active_power <- as.numeric(cdata$Global_active_power)
#create histogram
png(filename = "Plot1.png", width = 480, height = 480)
hist(cdata$Global_active_power,xlim=c(0,6), col = "red", 
     xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
dev.off()
