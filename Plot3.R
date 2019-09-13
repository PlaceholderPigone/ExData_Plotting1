library(lubridate)
library(dplyr)
library(ggplot2)
#read data
data <- read.csv(file = "exdata_data_household_power_consumption/household_power_consumption.txt",stringsAsFactors=FALSE,
                 header = TRUE, sep = ";")

#clean date format and select for needed dates
data$Date <- dmy(data$Date)
datadates <- subset(data, Date == "2007-2-2")
datadates2 <- subset(data, Date == "2007-2-1")

#rebind the two needed dates
cdata <- rbind(datadates, datadates2)

#clean time variable and create combined datetime variable
cdata$Time <- hms(cdata$Time)
cdata$Datetime <- with(cdata, ymd(Date) + hms(Time))
cdata$Datetime <- as.POSIXct(cdata$Datetime)

#make all required variables numeric
cdata$Sub_metering_1 <- as.numeric(cdata$Sub_metering_1)
cdata$Sub_metering_2 <- as.numeric(cdata$Sub_metering_2)
cdata$Sub_metering_3 <- as.numeric(cdata$Sub_metering_3)

#plot and export to png
png(filename = "Plot3.png", width = 480, height = 480)
ggplot(cdata, aes(x=Datetime)) +
  geom_line(aes(y=Sub_metering_1, colour = "Sub_metering_1")) +
  geom_line(aes(y=Sub_metering_2, colour = "Sub_metering_2")) +
  geom_line(aes(y=Sub_metering_3, colour = "Sub_metering_3")) +
  scale_colour_manual("",
                     breaks = c("Sub_metering_1", "Sub_metering_2",
                                "Sub_metering_3"),
                     values = c("Sub_metering_1" = "black","Sub_metering_2"= "red",
                                "Sub_metering_3"= "blue")) +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        axis.text.y = element_text(angle = 90),
        legend.margin=margin(t=-.5, r=1, b=0, l=0, unit="cm"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  ) +
  ylab("Energy sub metering") +
  xlab("") 
dev.off()
