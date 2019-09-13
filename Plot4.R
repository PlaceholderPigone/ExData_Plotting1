library(lubridate)
library(dplyr)
library(ggplot2)
#Rmisc required for multiplot
library(Rmisc)

#Read data
data <- read.csv(file = "exdata_data_household_power_consumption/household_power_consumption.txt",stringsAsFactors=FALSE,
                 header = TRUE, sep = ";")
#fix dates and cut out all but the needed dates
data$Date <- dmy(data$Date)
datadates <- subset(data, Date == "2007-2-2")
datadates2 <- subset(data, Date == "2007-2-1")

#rebind dates
cdata <- rbind(datadates, datadates2)

#clean time data
cdata$Time <- hms(cdata$Time)

#create datetime variable for graphing
cdata$Datetime <- with(cdata, ymd(Date) + hms(Time))
cdata$Datetime <- as.POSIXct(cdata$Datetime)

#turn everything into numeric for graphing
cdata$Global_active_power <- as.numeric(cdata$Global_active_power)
cdata$Global_reactive_power <- as.numeric(cdata$Global_reactive_power)
cdata$Sub_metering_1 <- as.numeric(cdata$Sub_metering_1)
cdata$Sub_metering_2 <- as.numeric(cdata$Sub_metering_2)
cdata$Sub_metering_3 <- as.numeric(cdata$Sub_metering_3)
cdata$Voltage <- as.numeric(cdata$Voltage)

#interleave function for making every other tickmark work in ggplot
interleave <- function(x,y){
  lx <- length(x)
  ly <- length(y)
  n <- max(lx,ly)
  as.vector(rbind(rep(x, length.out=n), rep(y, length.out=n)))
}

#plot all 4 graphs
p1 <- ggplot(data = cdata, aes(x=Datetime, y = Global_active_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90)) +
  ylab ("Global Active Power (kilowatts)")+
  xlab("")

my_breaks <- seq(234,248,by=2)
my_labs <- interleave(seq(234,248,by=4), "")

p2 <- ggplot(data = cdata, aes(x=Datetime, y =Voltage))+
  geom_line()+
  scale_y_continuous(breaks = my_breaks,
                     labels = my_labs) +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90))+
  ylab ("Voltage")+
  xlab("datetime")

p3 <- ggplot(cdata, aes(x=Datetime)) +
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
        legend.margin=margin(t=-.5, r=1, b=0, l=0, unit="cm"),
        axis.text.y = element_text(angle = 90),
        legend.background = element_rect(fill = "transparent"),
  ) +
  ylab("Energy sub metering") +
  xlab("") 

p4 <-  ggplot(data = cdata, aes(x=Datetime, y = Global_reactive_power))+
  geom_line()+
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 90))+
  xlab("datetime")

#export multiplot to png
png(filename = "Plot4.png", width = 480, height = 480)
multiplot(p1, p3, p2, p4, cols=2)
dev.off()
