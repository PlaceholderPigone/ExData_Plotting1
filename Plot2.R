library(lubridate)
library(dplyr)
library(ggplot2)

#read data
data <- read.csv(file = "exdata_data_household_power_consumption/household_power_consumption.txt",stringsAsFactors=FALSE,
                 header = TRUE, sep = ";")
#clean dates and select or the two dates we need
data$Date <- dmy(data$Date)
datadates <- subset(data, Date == "2007-2-2")
datadates2 <- subset(data, Date == "2007-2-1")
#combine the two dates
cdata <- rbind(datadates, datadates2)
#clean time variable and create combined date/time
cdata$Time <- hms(cdata$Time)
cdata$Datetime <- with(cdata, ymd(Date) + hms(Time))
cdata$Datetime <- as.POSIXct(cdata$Datetime)
#make the variable numeric
cdata$Global_active_power <- as.numeric(cdata$Global_active_power)

#plot and export to png
png(filename = "Plot2.png", width = 480, height = 480)
ggplot(data = cdata, aes(x=Datetime, y = Global_active_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90))+
  ylab ("Global Active Power (kilowatts)")+
  xlab("")
dev.off()

