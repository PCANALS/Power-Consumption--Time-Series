#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####
pacman::p_load(tidyverse,dplyr,lubridate, forecast, tseries, gridExtra)
# library(dplyr)
# # library(ggplot2)
# # library(tidyr)
# library(lubridate)
# library(stats)
# library(magrittr)
# library(gridExtra)
# library(GGally)
# library(forecast)
# library(imputeTS)
# library(TTR)
# library(ggfortify)


# library(imputeTS)




#### A._ SETTING FILES####

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


##create attributes##


power_new<-dplyr::mutate(power, NotSubMet_Wh = ((Global_active_power_kWm*1000)/60)
                         -Sub_metering_3_Wh-Sub_metering_2_Wh-Sub_metering_1_Wh, 
                         GlobalApparent_Wh = (Global_active_power_kWm+Global_reactive_power_kWm)*1000/60, 
                         Global_active_power_Wh=((Global_active_power_kWm*1000)/60))



#### C.- SUBSETS - NEW DATA FRAMES ####


#### C.-1 Subset 100 Observations ####

##100 observations for first easy analysis, tests#

power_100<-power_new[1:100,]

power_100<-power_100%>%group_by(hour(DateTime),minute(DateTime))%>%summarise_all(mean)


#### C.-2 MISSING VALUES NA####

#### C.-2.1 Identify  NA####

##extracting observations with MISSING VALUES##


power_na <- power[rowSums(is.na(power)) > 0,] #blackout or unpowering#

power_na2 <- power[rowSums(is.na(power)) >= 1 & rowSums(is.na(power)) < length(colnames(power))-3,] #detect partial missing data, error in a submeters#


#### C.-2.2 Remove NA####

#change de NA for 0##  its need a value to do the sums in the group-by or remove de rows#


#power_new[is.na(power_new)] <- 0
power_new<-na.locf(power_new)


#### C.-2.3 Change NA with the closest values - INTERPOLATE####

#power_new2<-na.interp(power_new) # is only working for time series

#power_new2 <-na.approx(power)

power_new2<-dplyr::mutate(power, NotSubMet_Wh = ((Global_active_power_kWm*1000)/60)
                          -Sub_metering_3_Wh-Sub_metering_2_Wh-Sub_metering_1_Wh, 
                          GlobalApparent_Wh = (Global_active_power_kWm+Global_reactive_power_kWm)*1000/60, 
                          Global_active_power_Wh=((Global_active_power_kWm*1000)/60))

anyNA(power_new2)
power_new2<-na.locf(power_new2)



# na.approx(Cz, x=Cz$time)

#### C.-3 Group by Month and Year####

#group by ##

power_ym<-power_new %>% select(DateTime, Global_active_power_kWm, Global_reactive_power_kWm,
                               GlobalApparent_Wh, Voltage_V, Global_intensity_A, NotSubMet_Wh, 
                               Sub_metering_1_Wh, Sub_metering_2_Wh, Sub_metering_3_Wh, Global_active_power_Wh)%>%
  mutate(YearP=year(DateTime), MonthP=month(DateTime, label = TRUE, abbr = FALSE))%>%
  group_by(YearP, MonthP)%>%summarise_all(sum)%>%ungroup()%>%filter(YearP!=2006)

power_ym2<-power_new2 %>% select(DateTime, Global_active_power_kWm, Global_reactive_power_kWm,
                               GlobalApparent_Wh, Voltage_V, Global_intensity_A, NotSubMet_Wh, 
                               Sub_metering_1_Wh, Sub_metering_2_Wh, Sub_metering_3_Wh, Global_active_power_Wh)%>%
  mutate(YearP=year(DateTime), MonthP=month(DateTime, label = TRUE, abbr = FALSE))%>%
  group_by(YearP, MonthP)%>%summarise_all(sum)%>%ungroup()%>%filter(YearP!=2006)


#### C.-4 Non consecutive days/time####

##MISSING DAYS##

power_day<-power_new%>%select(Date, Global_active_power_kWm)%>%
  #mutate(Year2=year(Date), Month2=month(Date), Day2= day(Date))%>%
  group_by(Date)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))
  
power_day$consecutiveDay <- c(NA,diff(ymd(power_day$Date))==1)


head(power_day)

nrow(power_day)-1==sum(power_day$consecutiveDay, na.rm = T)

#we can confirm that all dates registered are consecutive#

##MISSING TIME##

power_time<-power_new%>%select(DateTime, Global_active_power_kWm)%>%
  mutate(Day2=ymd_hm(DateTime))%>%
  group_by(Day2)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))


# Warning message:
#   All formats failed to parse. No formats found. 

power_time$consecutivetime <- c(NA,diff(ymd_hm(power_time$DateTime))==1)


head(power_time)

nrow(power_time)-1==sum(power_time$consecutivetime, na.rm = T)



 #group by weeks#

power_week<-power_new%>%select(Date, Global_active_power_kWm)%>%
  mutate(Year2=year(Date),WeekP=week(Date))%>%
  group_by(Year2, WeekP)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

#### PLOTS ####

##histo global##

# hist(power$Global_active_power_kWm, main = "Global Active Power",
#      xlab = "Global Active Power (kilowatts)", col = "mediumturquoise")
# 
# 
# plot(power$Global_active_power_kWm ~ power$DateTime,
# ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")


##sub-meterings##


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
  labs(title = "Consuption Active Power by Month (NA replaced by 0)",
       subtitle = "(2007-10)",
       tag = "",
       x = "",
       y = "Consuption Wh",
       colour = "Legend") 

PlConsuption

PlConsuption2<-ggplot(power_ym2, aes(x=MonthP, group= 1))+
  geom_line(aes(y=NotSubMet_Wh, col="Unkown"), size=1.1) + 
  geom_line(aes(y=Global_active_power_Wh, col="Global Household"), size=1.1) + 
  geom_line(aes(y=Sub_metering_1_Wh, col="Kitchen"), size=1.1) + 
  geom_line(aes(y=Sub_metering_2_Wh, col="Laundry Room"), size=1.1) + 
  geom_line(aes(y=Sub_metering_3_Wh, col="Electric water-heater & Air Con"), size=1.1) + 
  facet_grid(~YearP, scales = "free_x")+
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face='bold'))+
  labs(title = "Consuption Active Power by Month (NA interpolated)",
       subtitle = "(2007-10)",
       tag = "",
       x = "",
       y = "Consuption Wh",
       colour = "Legend") 

PlConsuption2

pl_na_dff<-grid.arrange(PlConsuption, PlConsuption2)
pl_na_dff




######################################################################################################

#### TIME SERIES IN POWER####


##Frame by month#

power_df_m<-power_new %>% select(Date, Global_active_power_kWm)%>%
  transmute(YeTs=year(Date), MoTs=month(Date, label = TRUE, abbr = FALSE), 
            Global_active_power_kWm)%>%
  group_by(YeTs, MoTs)%>%summarise_all(sum)

power_m_ts<-ts(power_df_m$Global_active_power_kWm, frequency = 12, start=c(2006,12))

# train #

power_train_m<-power_df_m%>%ungroup()%>%filter(YeTs!=2010)

#test #

power_test_m<-power_df_m%>%ungroup()%>%filter(YeTs==2010)



#Frame by week#

      # power_week<-power_new%>%select(Date, Global_active_power_kWm)%>%
      #   mutate(Year2=year(Date),WeekP=week(Date))%>%
      #   group_by(Year2, WeekP)%>%
      #   summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

power_df_w<-power_week
power_w_ts<-ts(power_df_w$Global_active_power_kWm, frequency = 53, start=c(2006,12))


### train 

power_train_w<-power_df_w%>%ungroup()%>%filter(Year2!=2010)

###test #

power_test_w<-power_df_w%>%ungroup()%>%filter(Year2==2010)




##Frame by day##

#power_day
# power_day<-power_new%>%select(Date, Global_active_power_kWm)%>%
#   group_by(Date)%>%
#   summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

anyNA(power_day[,"Global_active_power_kWm"])

power_df_d<-power_day%>%select(Date, Global_active_power_kWm)

power_d_ts<-ts(power_df_d$Global_active_power_kWm, frequency = 365, start=c(2006,12))


### train 

power_train_d<-power_df_d%>%ungroup()%>%filter(year(Date)!=2010)

###test #

power_test_d<-power_df_d%>%ungroup()%>%filter(year(Date)==2010)

####time series with train & test####

power_train_m_ts<-ts(power_train_m$Global_active_power_kWm, frequency = 12, start=c(2006,12))
power_train_w_ts<-ts(power_train_w$Global_active_power_kWm, frequency = 53, start=c(2006,12))
power_train_d_ts<-ts(power_train_d$Global_active_power_kWm, frequency = 365, start=c(2006,12))


power_test_m_ts<-ts(power_test_m$Global_active_power_kWm, frequency = 12, start=c(2010,01))
power_test_w_ts<-ts(power_test_w$Global_active_power_kWm, frequency = 53, start=c(2010,01))
power_test_d_ts<-ts(power_test_d$Global_active_power_kWm, frequency = 365, start=c(2010,01))

#par(mfrow=c(3,1)) #number of plots
# plot.ts(power_train_m_ts)
# plot.ts(power_train_w_ts)
# plot.ts(power_train_d_ts)
# 
# plot.ts(power_test_m_ts)
# plot.ts(power_test_w_ts)
# plot.ts(power_test_d_ts)

#decompose
power_train_m_dec<-decompose(power_train_m_ts)
power_train_w_dec<-decompose(power_train_w_ts)
power_train_d_dec<-decompose(power_train_d_ts)


# par(mfrow=c(3,1)) #number of plots
# plot(power_train_m_dec)
# plot(power_train_w_dec)
# plot(power_train_d_dec)



#HW#

power_train_m_HW<-HoltWinters(power_train_m_ts)
power_m_HW<-HoltWinters(power_m_ts)

power_train_w_HW<-HoltWinters(power_train_w_ts)
power_w_HW<-HoltWinters(power_w_ts)

power_train_d_HW<-HoltWinters(power_train_d_ts)
power_d_HW<-HoltWinters(power_d_ts)


#power_train_m_HW$fitted

# plot(power_train_m_HW)
# plot(power_train_w_HW)
# plot(power_train_d_HW)



####HW FORECASTING####

power_train_m_HW_for<-forecast(power_train_m_HW, h = 11)
power_m_HW_for<-forecast(power_m_HW, h = 48)

power_train_w_HW_for<-forecast(power_train_w_HW, h = 48)
power_w_HW_for<-forecast(power_w_HW, h = 210)

power_train_d_HW_for<-forecast(power_train_d_HW, h = 330)
power_d_HW_for<-forecast(power_d_HW, h = 1441)

#### RUN WITHOUT ISSUES####


####HW ERROR####
hist(power_train_m_HW_for$residuals)

acf(power_train_m_HW_for$residuals, na.action = na.pass)
checkresiduals(power_train_m_HW)
#PENDING - REMOVE OUTLIERS#
Box.test(power_train_m_HW_for$residuals, lag = 6 )

accuracy(power_train_m_HW_for,power_test_m_ts)
accuracy(power_train_w_HW_for,power_test_w_ts)

##error#
accuracy(power_train_d_HW_for,power_test_d_ts)


autoplot(power_test_m_ts) + autolayer(power_train_m_HW_for, PI=FALSE)

autoplot(power_train_m_HW_for)


#plot prediction by month#
p_hw_m<-autoplot(power_train_m_ts) + 
  autolayer(power_train_m_HW_for, PI=FALSE, col= "coral", size=1)+
  autolayer(power_m_HW_for, PI=FALSE, col="firebrick2", size=1)


#plot prediction by week#
p_hw_w<-autoplot(power_train_w_ts) + 
  autolayer(power_train_w_HW_for, PI=FALSE, col= "green", size=1)+
  autolayer(power_w_HW_for, PI=FALSE, col="mediumspringgreen", size=1)

#plot prediction by week#
p_hw_d<-autoplot(power_train_d_ts) + 
  autolayer(power_train_d_HW_for, PI=FALSE, col= "cyan", size=1)+
  autolayer(power_d_HW_for, PI=FALSE, col="blue", size=1)


plot_HW_for<-grid.arrange(p_hw_w, p_hw_d)

####ARIMA####

# autoplot(power_train_m_ts)
# autoplot(power_train_w_ts)
# autoplot(power_train_d_ts)

power_train_m_arima<-auto.arima(power_train_m_ts)
power_train_w_arima<-auto.arima(power_train_w_ts)
power_train_d_arima<-auto.arima(power_train_d_ts)
#all data##
power_m_arima<-auto.arima(power_m_ts)
power_w_arima<-auto.arima(power_w_ts)
power_d_arima<-auto.arima(power_d_ts)
power_d_arima_manu<-arima(power_d_ts,order = c(2,1,3), seasonal = c(0,0,0))

power_train_m_arima_for<-forecast:::forecast.Arima(power_train_m_arima, h=11)
power_train_w_arima_for<-forecast:::forecast.Arima(power_train_w_arima, h=48)
power_train_d_arima_for<-forecast:::forecast.Arima(power_train_d_arima, h=330)

power_m_arima_for<-forecast(power_m_arima, h = 48)
power_w_arima_for<-forecast(power_w_arima, h = 210)
power_d_arima_for<-forecast(power_d_arima, h = 1440)
power_d_arima_for_manu<-forecast(power_d_arima_manu, h = 1440)

autoplot(power_d_arima_for_manu)

power_train_m_arima_fore$fitted

acf(power_train_m_arima$residuals, na.action = na.pass)
Box.test(power_train_m_arima$residuals, lag = 6 )
checkresiduals(power_train_m_arima)

acf(power_train_w_arima$residuals, na.action = na.pass)
acf(power_train_d_arima$residuals, na.action = na.pass)
checkresiduals(power_train_w_arima)
checkresiduals(power_train_d_arima)

accuracy(power_train_m_arima_for,power_test_m_ts)
accuracy(power_train_w_arima_for,power_test_w_ts)

###error#
accuracy(power_train_d_arima_for,power_test_d_ts)

autoplot(power_train_m_ts)+autolayer(power_train_m_arima_for, PI=FALSE)



#plot prediction by month#
p_arima_m<-autoplot(power_train_m_ts) + 
  autolayer(power_train_m_arima_for, PI=FALSE, col= "coral", size=1)+
  autolayer(power_m_arima_for, PI=FALSE, col="firebrick2", size=1)


#plot prediction by week#
p_arima_w<-autoplot(power_train_w_ts) + 
  autolayer(power_train_w_arima_for, PI=FALSE, col= "green", size=1)+
  autolayer(power_w_arima_for, PI=FALSE, col="mediumspringgreen", size=1)

#plot prediction by week#
p_arima_d<-autoplot(power_train_d_ts) + 
  autolayer(power_train_d_arima_for, PI=FALSE, col= "cyan", size=1)+
  autolayer(power_d_arima_for, PI=FALSE, col="blue", size=1)


plot_arima_for<-grid.arrange(p_arima_m, p_arima_w, p_arima_d)

#### LINEAR REGRESION ####


power_train_m_tslm<-tslm(power_train_m_ts~ trend + season)
power_train_w_tslm<-tslm(power_train_w_ts~ trend + season)
power_train_d_tslm<-tslm(power_train_d_ts~ trend + season)


#all data##
power_m_tslm<-tslm(power_m_ts~ trend + season)
power_w_tslm<-tslm(power_w_ts~ trend + season)
power_d_tslm<-tslm(power_d_ts~ trend + season)

##forecast#

power_train_m_tslm_for<-forecast(power_train_m_tslm, h=11)
power_train_w_tslm_for<-forecast(power_train_w_tslm, h=48)
power_train_d_tslm_for<-forecast(power_train_d_tslm, h=330)

power_m_tslm_for<-forecast(power_m_tslm, h = 48)
power_w_tslm_for<-forecast(power_w_tslm, h = 210)
power_d_tslm_for<-forecast(power_d_tslm, h = 1440)

autoplot(power_d_tslm_for)

accuracy(power_train_m_tslm_for,power_test_m_ts)
accuracy(power_train_w_tslm_for,power_test_w_ts)
accuracy(power_train_d_tslm_for,power_test_d_ts)

#plot prediction by month#
p_tslm_m<-autoplot(power_train_m_ts) + 
  autolayer(power_train_m_tslm_for, PI=FALSE, col= "coral", size=1)+
  autolayer(power_m_tslm_for, PI=FALSE, col="firebrick2", size=1)


#plot prediction by week#
p_tslm_w<-autoplot(power_train_w_ts) + 
  autolayer(power_train_w_tslm_for, PI=FALSE, col= "green", size=1)+
  autolayer(power_w_tslm_for, PI=FALSE, col="mediumspringgreen", size=1)

#plot prediction by week#
p_tslm_d<-autoplot(power_train_d_ts) + 
  autolayer(power_train_d_tslm_for, PI=FALSE, col= "cyan", size=1)+
  autolayer(power_d_tslm_for, PI=FALSE, col="blue", size=1)

plot_tslm_for<-grid.arrange(p_tslm_m, p_tslm_w, p_tslm_d)



####MODEL SELECTION RESUMEN####

plot_HW_for
plot_arima_for
plot_tslm_for

accuracy(power_train_m_HW_for,power_test_m_ts)
accuracy(power_train_w_HW_for,power_test_w_ts)
accuracy(power_train_d_HW_for,power_test_d_ts)#error#

accuracy(power_train_m_arima_for,power_test_m_ts)
accuracy(power_train_w_arima_for,power_test_w_ts)
accuracy(power_train_d_arima_for,power_test_d_ts)#error#

accuracy(power_train_m_tslm_for,power_test_m_ts)
accuracy(power_train_w_tslm_for,power_test_w_ts)
accuracy(power_train_d_tslm_for,power_test_d_ts)#error#

## ALL FORECASTING ###

###############################################################################################3


####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####
## pendiente de revisar la derivada de la linea 311#
#identify missing minutess#
#na f¡values with prior#
#na with the #
#replace outliers with regresion model or spline, pending to analize#

#decompose without logarith#


