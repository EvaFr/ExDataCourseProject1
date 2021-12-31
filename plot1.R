
fileName <- "household_power_consumption.txt"

#prediction of the dataset size
top.size <- object.size(read.table(fileName, nrow=1000))
lines <- as.numeric(gsub("[^0-9]", "", system("wc -l household_power_consumption.txt", intern=T)))
size.estimate <- lines / 1000 * top.size
size.estimate
#331892181.3 bytes

#reading of the dataset
eData <- read.table(fileName, sep = ";",  header=TRUE, na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
eData$Date <- as.Date(eData$Date, "%d/%m/%Y")
bData <- subset(eData, (eData$Date >= "2007-2-1" & eData$Date <= "2007-2-2"))

#correction of the date and time
dateTime <- paste(bData$Date, bData$Time)
dateTime <- setNames(dateTime, "DateTime")
bData <- bData[ ,!(names(bData) %in% c("Date","Time"))]
bData <- cbind(dateTime, bData)
bData$dateTime <- as.POSIXct(dateTime)

#plots
png(filename = "plot1.png")
hist(bData$Global_active_power, col = "red",ylab = "Frequency", 
     xlab = "Global Active Power (kilowatts)", main = "Global Active Power",
     xlim = c(0, 6), ylim = c(0, 1200), xaxp = c(0, 6, 3))
dev.off()

png(filename = "plot2.png")
plot(bData$Global_active_power~bData$dateTime, type = "l", ylab = "Global Active Power (kilowatts)", 
     xlab = "")
dev.off()

png(filename = "plot3.png")
with(bData, {
  plot(Sub_metering_1~dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()

png(filename = "plot4.png")
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(bData, {
  plot(Global_active_power~dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  plot(Global_reactive_power~dateTime, type="l", ylab="Global Rective Power (kilowatts)",xlab="")
})
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()
