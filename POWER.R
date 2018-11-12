
rm(list = ls())

#### ._LIBRARIES####

#install.packages("dpylr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("tidyselect")
library(tidyselect)
#install.packages("lubridate")
library(lubridate)
#install.packages("devtools")
#devtools::install_github("tidyverse/lubridate")
#install.packages("tidyverse")
library(tidyverse)


#### A_SETTING FILES####

setwd("C:/Users/pilar/Google Drive/A_DATA/UBIQUM/TASK3.1/task-3-1-define-a-data-science-process-PCANALS")

powero<-read.csv(file = "household_power_consumption.txt", header = TRUE, sep =";", na.strings=c("NA", "-", "?"), stringsAsFactors=FALSE)

#"na.strings=c("NA", "-", "?")" changes in all data set all teorical "missing values" as NA in R
#"stringsAsFactors=FALSE" don't converts the variables in factors


# str(powero)
# dim(powero)
# summary(powero)
# apply(powero, 2, class)

power<-powero # to keep origen#


power$DateTime<-lubridate::dmy_hms(paste(power$Date,power$Time))
power$Date<-lubridate::dmy(power$Date)
power$Time<-lubridate::hms(power$Time)


#each attribute it's different but it works correctly for time series#

power <- power[,c(ncol(power), 1:(ncol(power)-1))] #order 


power %>% head() %>% str()


#power_na<-dplyr::filter(is.na(power))

#power_na<-which(is.na(power))

#power_na<-subset(is.na(power))

power_na <- power[rowSums(is.na(power)) > 0,]

#### PLOTS ####
hist(power$Global_active_power, main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)", col = "mediumturquoise")

plot(power$Global_active_power ~ power$DateTime, 
     ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")

plot(power$Sub_metering_1 ~ power$DateTime, ylab = "Energy sub metering", xlab = "", type = "l")
lines(power$Sub_metering_2 ~ power$DateTime, col = 'Red')
lines(power$Sub_metering_3 ~ power$DateTime, col = 'Blue')
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)


####~~~~NOTES - NEXT STEPS~~~~####


#transform txt in a r file#

#dyplr tratar NA, ver dataframe solo na#

#lubridate tratar fechas#