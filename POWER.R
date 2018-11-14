#### DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


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
library(ggplot2)

#### A_SETTING FILES####

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

power<-powero # to keep origen#

#### B._ ATTRIBUTES ####

#change kind#
power$DateTime<-lubridate::dmy_hms(paste(power$Date,power$Time))
power$Date<-lubridate::dmy(power$Date)
power$Time<-lubridate::hms(power$Time)


#each attribute it's different but it works correctly for time series#

power <- power[,c(ncol(power), 1:(ncol(power)-1))] #order 

power %>% head() %>% str()

#renaming adding units#

names(power)<-c("DateTime", "Date", "Time",
                "Global_active_power_kWm", "Global_reactive_power_kWm", 
                "Voltage_V", "Global_intensity_A", 
                "Sub_metering_1_Wh", "Sub_metering_2_Wh", "Sub_metering_3_Wh")

#extracting observations with MISSING VALUES#

#power_na<-dplyr::filter(is.na(power))

#power_na<-which(is.na(power))

#power_na<-subset(is.na(power))

power_na <- power[rowSums(is.na(power)) > 0,]

##create attributes##


power_new<-dplyr::mutate(power, NotSubMet_Wh = ((Global_active_power_kWm*1000)/60)
                         -Sub_metering_3_Wh-Sub_metering_2_Wh-Sub_metering_1_Wh, 
                         GlobalApparent_Wh = (Global_active_power_kWm+Global_reactive_power_kWm)*1000/60)






#### C.- SUBSETS ####

power_100<-power_new[1:100,]

power_100<-power_100%>%group_by(hour(DateTime),minute(DateTime))%>%summarise_all(mean)

#change de NA for 0##
power_new[is.na(power_new)] <- 0

#group by ##
power_ym<-power_new %>% select(DateTime, Global_active_power_kWm, Global_reactive_power_kWm,
                               GlobalApparent_Wh, Voltage_V, Global_intensity_A, NotSubMet_Wh, 
                               Sub_metering_1_Wh, Sub_metering_2_Wh, Sub_metering_3_Wh)%>%
  mutate(YearP=year(DateTime), MonthP=month(DateTime, label = TRUE, abbr = FALSE))%>%
  group_by(YearP, MonthP)%>%summarise_all(sum)%>%ungroup()%>%filter(YearP!=2006)


# 
# power2007<-power_new %>% dplyr::filter(year(DateTime)!==2007)
# 
# power2008<-power_new%>% filter(DateTime>="2008-01-01 00:00:00")
# 
# power2009<-power_new%>%filter((Date > as.Date("2007/12/10")
#                                & Date <= as.Date("2008/12/05")))

#### PLOTS ####

#histo global#
# hist(power$Global_active_power_kWm, main = "Global Active Power",
#      xlab = "Global Active Power (kilowatts)", col = "mediumturquoise")
# 
# 
# plot(power$Global_active_power_kWm ~ power$DateTime,
# ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")

#meterings#

# plot(power$Sub_metering_1 ~ power$DateTime, ylab = "Energy sub metering", xlab = "", type = "l")
# lines(power$Sub_metering_2 ~ power$DateTime, col = 'Red')
# lines(power$Sub_metering_3 ~ power$DateTime, col = 'Blue')
# legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)

#hist(power_na$DataTime, breaks = )##pendiente de ver como funcionan los breaks

dev.off()

####GGPLOT####

# df<-power_new[, c("Date","Sub_metering_1_Wh", "Sub_metering_2_Wh", "Sub_metering_3_Wh",
#                   "GlobalApparent_Wh")]
# df<-df[lubridate::year(df$Date) %in% c(2007:2007), ]
# 
# 
# brks <- power_ym$DateTime[seq(1, length(power_ym$DateTime), 12)]
# lbls <- lubridate::month(brks)

#power_ym<-power_ym%>%rename(YearP='year(DateTime)')%>%rename(MonthP='month(DateTime)')

#power_ym$YearP<-as.factor(power_ym$YearP)
#power_ym$MonthP<-as.numeric(power_ym$MonthP)



theme_set(theme_light())

ggplot(power_ym, aes(x=MonthP, group= 1))+
  geom_line(aes(y=NotSubMet_Wh, col="NotSubMet_Wh")) + 
  geom_line(aes(y=GlobalApparent_Wh, col="GlobalApparent_Wh")) + 
  geom_line(aes(y=Sub_metering_1_Wh, col="Sub_metering_1_Wh")) + 
  geom_line(aes(y=Sub_metering_2_Wh, col="Sub_metering_2_Wh")) + 
  geom_line(aes(y=Sub_metering_3_Wh, col="Sub_metering_3_Wh")) + 
  facet_wrap(~YearP, scales = "free_x")+
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))+
  labs(title = "Consuption by Month",
       subtitle = "(2007-10)",
       tag = "",
       x = "Consuption Wh",
       y = "",
       colour = "Legend")


yearplot <-ggplot(power_ym, aes(x=MonthP, group= 1))+
  geom_line(aes(y=NotSubMet_Wh, col="NotSubMet_Wh")) + 
  geom_line(aes(y=GlobalApparent_Wh, col="GlobalApparent_Wh")) + 
  geom_line(aes(y=Sub_metering_1_Wh, col="Sub_metering_1_Wh")) + 
  geom_line(aes(y=Sub_metering_2_Wh, col="Sub_metering_2_Wh")) + 
  geom_line(aes(y=Sub_metering_3_Wh, col="Sub_metering_3_Wh")) + 
  #scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
  #scale_x_discrete(labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
  facet_wrap(~YearP, facets = c(2007:2008), scales = "free_x")




ggplot(subset(df, x != 2006), aes(x = z, y = y, fill = z))

plot1 <- ggplot(power_ym, aes(x = MonthP)) +
  geom_point(aes(y=Sub_metering_1_Wh, col="Sub_metering_1_Wh")) +
  labs(x = "Month",
       title = "xxx")

plot2 <- ggplot(power_ym, aes(x = MonthP)) +
  geom_line(aes(y=Sub_metering_1_Wh, col="Sub_metering_1_Wh")) +
  labs(x = "Month",
       title = "xxx")+
  facet_wrap(~YearP)



plot_df <- melt(power_ym[, c("YearP", "Sub_metering_1_Wh")], id="YearP")  # melt by date. 
gg <- ggplot(plot_df, aes(x=YearP))  # setup
gg + geom_line(aes(y=value, color=variable), size=1)
  
####~~~~NOTES - NEXT STEPS~~~~####


#transform txt in a r file#

#dyplr tratar NA, ver dataframe solo na#

#lubridate tratar fechas#