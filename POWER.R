#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####

library(tidyselect)
library(lubridate)
#install.packages("tidyverse")
library(tidyverse)
#####includes:
#library(dplyr)
#library(ggplot2)
#library(tidyr)
tidyverse_update()
library(tidyselect)
library(lubridate)
library(imputeTS)

#### A._SETTING FILES####

setwd("C:/Users/pilar/Google Drive/A_DATA/UBIQUM/TASK3.1/task-3-1-define-a-data-science-process-PCANALS")

#powero<-read.csv(file = "household_power_consumption.txt", header = TRUE, sep =";", na.strings=c("NA", "-", "?"), stringsAsFactors=FALSE)

#"na.strings=c("NA", "-", "?")" changes in all data set all teorical "missing values" as NA in R
#"stringsAsFactors=FALSE" don't converts the variables in factors

#save(powero, file = "DFpowero.Rdata")

load(file = "DFpowero.Rdata")

# str(powero)
# dim(powero)
# summary(powero)
# apply(powero, 2, class)

power<-powero # to keep original#

#### B._ ATTRIBUTES ####

##changing class##

power$DateTime<-lubridate::dmy_hms(paste(power$Date,power$Time))
power$Date<-lubridate::dmy(power$Date)
power$Time<-lubridate::hms(power$Time)


#each attribute it's different but it works correctly for time series#



power <- power[,c(ncol(power), 1:(ncol(power)-1))] #order #

power %>% head() %>% str()



##renaming adding units##

names(power)<-c("DateTime", "Date", "Time",
                "Global_active_power_kWm", "Global_reactive_power_kWm", 
                "Voltage_V", "Global_intensity_A", 
                "Sub_metering_1_Wh", "Sub_metering_2_Wh", "Sub_metering_3_Wh")


##extracting observations with MISSING VALUES##


power_na <- power[rowSums(is.na(power)) > 0,]


##create attributes##


power_new<-dplyr::mutate(power, NotSubMet_Wh = ((Global_active_power_kWm*1000)/60)
                         -Sub_metering_3_Wh-Sub_metering_2_Wh-Sub_metering_1_Wh, 
                         GlobalApparent_Wh = (Global_active_power_kWm+Global_reactive_power_kWm)*1000/60, 
                         Global_active_power_Wh=((Global_active_power_kWm*1000)/60))



#### C.- SUBSETS ####

##100 observations for first easy analysis, tests#

power_100<-power_new[1:100,]

power_100<-power_100%>%group_by(hour(DateTime),minute(DateTime))%>%summarise_all(mean)



#change de NA for 0##  its need a value to do the sums in the group-by or remove de rows#

power_new[is.na(power_new)] <- 0


#group by ##
power_ym<-power_new %>% select(DateTime, Global_active_power_kWm, Global_reactive_power_kWm,
                               GlobalApparent_Wh, Voltage_V, Global_intensity_A, NotSubMet_Wh, 
                               Sub_metering_1_Wh, Sub_metering_2_Wh, Sub_metering_3_Wh, Global_active_power_Wh)%>%
  mutate(YearP=year(DateTime), MonthP=month(DateTime, label = TRUE, abbr = FALSE))%>%
  group_by(YearP, MonthP)%>%summarise_all(sum)%>%ungroup()%>%filter(YearP!=2006)



#### PLOTS ####

##histo global##

# hist(power$Global_active_power_kWm, main = "Global Active Power",
#      xlab = "Global Active Power (kilowatts)", col = "mediumturquoise")
# 
# 
# plot(power$Global_active_power_kWm ~ power$DateTime,
# ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")

<<<<<<< Updated upstream
#meterings#
=======
##sub-meterings##
>>>>>>> Stashed changes

# plot(power$Sub_metering_1 ~ power$DateTime, ylab = "Energy sub metering", xlab = "", type = "l")
# lines(power$Sub_metering_2 ~ power$DateTime, col = 'Red')
# lines(power$Sub_metering_3 ~ power$DateTime, col = 'Blue')
# legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)

#hist(power_na$DataTime, breaks = )##pendiente de ver como funcionan los breaks

#dev.off()

####GGPLOTs####

theme_set(theme_light())

PlConsuption<-ggplot(power_ym, aes(x=MonthP, group= 1))+
  geom_line(aes(y=NotSubMet_Wh, col="Unkown"), size=1.1) + 
  geom_line(aes(y=Global_active_power_Wh, col="Global Household"), size=1.1) + 
  geom_line(aes(y=Sub_metering_1_Wh, col="Kitchen"), size=1.1) + 
  geom_line(aes(y=Sub_metering_2_Wh, col="Laundry Room"), size=1.1) + 
  geom_line(aes(y=Sub_metering_3_Wh, col="Electric water-heater & Air Con"), size=1.1) + 
  facet_grid(~YearP, scales = "free_x")+
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face='bold'))+
  labs(title = "Consuption Active Power by Month",
       subtitle = "(2007-10)",
       tag = "",
       x = "",
       y = "Consuption Wh",
       colour = "Legend") 

PlConsuption

####Time Series tests####

plot(tsAirgap, main="AirPassenger data with missing values")


statsNA(tsAirgap)

par(mfrow=c(2,2))

plot(na.mean(tsAirgap, option = "mean") - AirPassengers, ylim = c(-mean(AirPassengers), mean(AirPassengers)), ylab = "Difference", main = "Mean")
mean((na.mean(tsAirgap, option = "mean") - AirPassengers)^2)

####~~~~NOTES - NEXT STEPS~~~~####

#identify missing days#
