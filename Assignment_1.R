#==============================================================================
#Load the required packages

library(dplyr)
library(magrittr)
library(lubridate)

#==============================================================================
#Set the working directory and import the data
setwd("C:\\Users\\monisha\\Desktop\\currentfolders\\Exploratory_Data_Analysis")
ec = read.table(file.choose(), header = TRUE, sep = ";")
#ec = read.csv(file.choose(), header = TRUE, sep = ",")


#==============================================================================
#Create a copy of data ec_copy
ec_copy = ec
str(ec_copy)


#ec_copy$Date = as.Date(ec_copy$Date, "%d/%m/%y")
#write.csv(ec, "electricty_consumption_data.csv")

#==============================================================================
#Extract data dated "1/2/2007" or "2/2/2007"

plot_data = subset(ec_copy, Date == "1/2/2007" | Date == "2/2/2007")
#write.csv(plot_data, "plot_data.csv")
str(plot_data)
#library(dplyr)
#plot_data = filter(ec_copy, Date >= "2007-02-01" & Date <= "2007-02-01")
#ec_copy = ec_copy[ec_copy$Date >= "2007-02-01" & ec_copy$Date <= "2007-02-01"]
#str(ec_copy)


#=============================================================================
#Replace "?" as NA and omit them

plot_data[plot_data == "?"] = NA
nrow(plot_data)
plot_data = na.omit(plot_data)


plot_data_copy = plot_data

#=============================================================================
#Convert Date column to Date format

plot_data$Date = as.Date(plot_data$Date, "%d/%m/%y")
str(plot_data$Date)

plot_data$DateTime <- strptime(paste(plot_data$Date, plot_data$Time, sep=" "), "%d/%m/%Y %H:%M:%S")




#============================================================================
#Convert the Required factor columns to numeric

factor_cols = names(plot_data)[sapply(plot_data, is.factor)]
factor_cols = factor_cols[-c(1, 2)]
plot_data[factor_cols] = as.numeric(as.matrix(plot_data[factor_cols]))
#plot_data %<>% mutate_if(is.factor, as.numeric)
#sapply(plot_data, class)
str(plot_data)
#============================================================================
#Crete a column named days

plot_data$days = as.factor(weekdays(plot_data$Date, abbreviate = T))
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
plot_data$days = as.numeric.factor(plot_data$days)


#============================================================================
#Histogram of Global_active_power

png("plot1.png", height = 480, width = 480)
with(plot_data, hist(Global_active_power, col = "red", xlab = "Global_active_power(kilowatts)",
                          main = "Global Active Power"))
dev.off()


png("plot2.png", height = 480, width = 480)
with(plot_data, plot(DateTime, Global_active_power, type = "l"))
dev.off()




#===========================================================================
#Submetering 

install.packages("tidyverse")
install.packages("reshape")
library(tidyverse)
library(reshape)
submetering = plot_data %>% select(Sub_metering_1:Sub_metering_3, DateTime)
melt_submetering = melt(submetering, id = "DateTime")
colnames(melt_submetering)

png("plot3.png", height = 480, width = 480)
with(melt_submetering, plot(DateTime, value, type = "n", xlab = NA,
                       ylab = "Energy sub metering"))
with(subset(melt_submetering, variable == "Sub_metering_1"), 
     lines(DateTime, value))
with(subset(melt_submetering, variable == "Sub_metering_2"), 
     lines(DateTime, value, col = "red"))
with(subset(melt_submetering, variable == "Sub_metering_3"), 
     lines(DateTime, value, col = "blue"))
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1",
                                                               "Sub_metering_2",
                                                               "Sub_metering_2"),
        lty = 1, cex = 0.8)
dev.off()


png("plot4.png", height = 480, width = 480)
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
with(plot_data, plot(DateTime, Global_active_power, type = "l",xlab = NA))
with(plot_data, plot(DateTime, Voltage, type = "l", xlab = "datetime",
                ylim = c(234, 246)))

with(melt_submetering, plot(DateTime, value, type = "n", xlab = NA,
                       ylab = "Energy sub metering"))
with(subset(melt_submetering, variable == "Sub_metering_1"), 
     lines(DateTime, value))
with(subset(melt_submetering, variable == "Sub_metering_2"), 
     lines(DateTime, value, col = "red"))
with(subset(melt_submetering, variable == "Sub_metering_3"), 
     lines(DateTime, value, col = "blue"))
legend("topright", col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_2"),
       lty = 1, cex = 0.6)

with(plot_data, plot(DateTime, Global_reactive_power, type = "l",
                xlab = "datetime"))
                
dev.off()

