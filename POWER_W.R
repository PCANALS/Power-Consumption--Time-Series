#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####

if(require("pacman")=="FALSE"){install.packages("pacman")}

pacman::p_load(tidyverse,dplyr,lubridate, forecast, tseries, gridExtra, zoo)

# library(dplyr)
# library(ggplot2)
# library(tidyr)
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
power_new$Date<-as.Date(power_new$Date, format="%Y-%m-%d")

power_df_m<-power_new %>% select(Date, Global_active_power_kWm)%>%
  transmute(YeMo=as.yearmon(Date),Global_active_power_kWm) %>%
  group_by(YeMo)%>%summarise_all(sum)%>%ungroup()

power_m_ts<-ts(power_df_m$Global_active_power_kWm, frequency = 12, start=c(2007))

# train #

power_train_m<-power_df_m%>%ungroup()%>%filter(YeMo<=as.yearmon("Nov 2009"))

#test #

power_test_m<-power_df_m%>%ungroup()%>%filter(YeMo>as.yearmon("Nov 2009"))



#Frame by week#

      # power_week<-power_new%>%select(Date, Global_active_power_kWm)%>%
      #   mutate(Year2=year(Date),WeekP=week(Date))%>%
      #   group_by(Year2, WeekP)%>%
      #   summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

power_new$Date<-as_date(power_new$Date)

power_df_w<-power_new%>%
  select(Date, Global_active_power_kWm)%>%filter(year(Date)!=2006)%>%
  mutate(Year2=year(Date),WeekP=week(Date))%>%
  group_by(Year2, WeekP)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

  # power_new%>%select(Date, Global_active_power_kWm)%>%
  # filter(year(Date)!=2006)%>%
  # transmute(month = as.Date(cut(Date, breaks = "month")),week = as.Date(cut(Date, breaks = "week", start.on.monday = T)), Global_active_power_kWm)%>% 
  # group_by(month, week)%>% 
  # summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

  

power_w_ts<-ts(power_df_w$Global_active_power_kWm, frequency = 52, start=c(2007,1))

power_train_w_ts_2<-window(power_w_ts, end=c(2009,47))
power_test_w_ts_2<-window(power_w_ts, start=c(2009,47))

autoplot(power_w_ts)+autolayer(power_train_w_ts_2)+autolayer(power_test_w_ts_2)
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

power_df_d<-power_day%>%select(Date, Global_active_power_kWm)%>%filter(year(Date)!=2006)

power_d_ts<-ts(power_df_d$Global_active_power_kWm, frequency = 365, start=c(2007,1))


### train 

power_train_d<-power_df_d%>%ungroup()%>%filter(year(Date)!=2010)

###test #

power_test_d<-power_df_d%>%ungroup()%>%filter(year(Date)==2010)



####time series with train & test####

power_train_m_ts<-ts(power_train_m$Global_active_power_kWm, frequency = 12, start=c(2006,12))
power_train_w_ts<-ts(power_train_w$Global_active_power_kWm, frequency = 52, start=c(2006,12))
power_train_d_ts<-ts(power_train_d$Global_active_power_kWm, frequency = 365, start=c(2006,12), end = c(2008,12))


power_test_m_ts<-ts(power_test_m$Global_active_power_kWm, frequency = 12, start=c(2009,01))
power_test_w_ts<-ts(power_test_w$Global_active_power_kWm, frequency = 52, start=c(2009,01))
power_test_d_ts<-ts(power_test_d$Global_active_power_kWm, frequency = 365, start=c(2009,01))

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


#HW#

power_train_m_HW<-HoltWinters(power_train_d_ts)
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

power_train_m_HW_for<-forecast(power_train_m_HW, h = 12)
power_m_HW_for<-forecast(power_m_HW, h = 48)

power_train_w_HW_for<-forecast(power_train_w_HW, h = 52)
power_w_HW_for<-forecast(power_w_HW, h = 104)

power_train_d_HW_for<-forecast(power_train_d_HW, h = 365)
power_d_HW_for<-forecast(power_d_HW, h = 365)



####HW ERROR####
hist(power_train_m_HW_for$residuals)

acf(power_train_m_HW_for$residuals, na.action = na.pass)
checkresiduals(power_train_m_HW)
#PENDING - REMOVE OUTLIERS#
Box.test(power_train_m_HW_for$residuals, lag = 6 )

accuracy(power_train_m_HW_for,power_test_m_ts)
accuracy(power_train_w_HW_for,power_test_w_ts)
accuracy(power_train_d_HW_for,power_test_d_ts)



#plot prediction by month#
p_hw_m<-autoplot(power_train_m_ts) + 
  autolayer(power_train_m_HW_for, PI=FALSE, col= "coral", size=1)+
  autolayer(power_m_HW_for, PI=FALSE, col="firebrick2", size=1)


#plot prediction by week#
p_hw_w<-autoplot(power_train_w_ts) + 
  autolayer(power_train_w_HW_for, PI=FALSE, col= "green", size=1)+
  autolayer(power_w_HW_for, PI=FALSE, col="mediumspringgreen", size=1)

#plot prediction by day#
p_hw_d<-autoplot(power_train_d_ts) + 
  autolayer(power_train_d_HW_for, PI=FALSE, col= "cyan", size=1)+
  autolayer(power_d_HW_for, PI=FALSE, col="blue", size=1)


plot_HW_for<-grid.arrange(p_hw_m,p_hw_w, p_hw_d)


####arima####

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

power_train_m_arima_for<-forecast(power_train_m_arima, h=11)
power_train_w_arima_for<-forecast(power_train_w_arima, h=48)
power_train_d_arima_for<-forecast(power_train_d_arima, h=330)

power_m_arima_for<-forecast(power_m_arima, h = 48)
power_w_arima_for<-forecast(power_w_arima, h = 210)
power_d_arima_for<-forecast(power_d_arima, h = 1440)
power_d_arima_for_manu<-forecast(power_d_arima_manu, h = 1440)

autoplot(power_d_arima_for_manu)

power_train_m_arima_for$fitted

acf(power_train_m_arima$residuals, na.action = na.pass)
Box.test(power_train_m_arima$residuals, lag = 6 )
#checkresiduals(power_train_m_arima)

acf(power_train_w_arima$residuals, na.action = na.pass)
acf(power_train_d_arima$residuals, na.action = na.pass)
#checkresiduals(power_train_w_arima)
#checkresiduals(power_train_d_arima)

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


#### ~~~~DAY~~~~####

power_df_d<-power_day%>%select(Date, Global_active_power_kWm)%>%filter(year(Date)!=2006)

power_d_ts<-ts(power_df_d$Global_active_power_kWm, frequency = 365, start=c(2007,1))

power_train_d_ts_2<-window(power_d_ts, end=c(2009,330))
power_test_d_ts_2<-window(power_d_ts, start=c(2009,330))


autoplot(power_d_ts)+autolayer(power_train_d_ts_2)+autolayer(power_test_d_ts_2)

power_train_d_dec2<-decompose(power_train_d_ts_2)

power_train_d_dec_stl <- stl(power_train_d_ts_2, s.window=7, t.window=1000, s.degree=0)
autoplot(power_train_d_dec_stl)

#HW#
power_train_d_HW_2<-HoltWinters(power_train_d_ts_2)
power_d_HW_2<-HoltWinters(power_d_ts)

power_train_d_HW_for<-forecast(power_train_d_HW_2, h = 365)
power_d_HW_for_2<-forecast(power_d_HW, h = 730)

MHW2<-accuracy(power_train_d_HW_for,power_test_d_ts_2)

#plot prediction by day2#
p_hw_d2<-autoplot(power_d_ts) + 
  autolayer(power_d_HW_for, PI=FALSE, col="blue", size=1)

p_hw_d3<-autoplot(power_test_d_ts_2) + 
  autolayer(power_train_d_HW_for, PI=FALSE, col= "cyan", size=1)

#arima# ### arima is discarded because only is accepting a maximum frequency of 350

# power_train_d_arima_2<-auto.arima(power_train_d_ts_2)
# power_d_arima_2<-auto.arima(power_d_ts)
# 
# power_train_d_arima_for_2<-forecast(power_train_d_arima_2, h = 365)
# power_d_arima_for_2<-forecast(power_d_arima_2, h = 365)
# 
# Marima2<-accuracy(power_train_d_arima_for_2,power_test_d_ts_2)
# 
# #plot prediction by day2#
# p_arima_d2<-autoplot(power_d_ts) + 
#   autolayer(power_d_arima_for_2, PI=FALSE, col="blue", size=1)
# p_arima_d3<-autoplot(power_test_d_ts_2) + 
#   autolayer(power_train_d_arima_for_2, PI=FALSE, col= "cyan", size=1)

###linear###


power_train_d_tslm_2<-tslm(power_train_d_ts_2~ trend + season)
power_d_tslm_2<-tslm(power_d_ts~ trend + season)

power_train_d_tslm_for_2<-forecast(power_train_d_tslm_2, h = 365)
power_d_tslm_for_2<-forecast(power_d_tslm_2, h = 365)

Mtslm2<-accuracy(power_train_d_tslm_for_2,power_test_d_ts_2)

#plot prediction by day2#
p_tslm_d2<-autoplot(power_d_ts) + 
  autolayer(power_d_tslm_for_2, PI=FALSE, col="blue", size=1)
p_tslm_d3<-autoplot(power_test_d_ts_2) + 
  autolayer(power_train_d_tslm_for_2, PI=FALSE, col= "cyan", size=1)


####~~~~ BY MONTH ~~~~####

power_new$Date<-as.Date(power_new$Date, format="%Y-%m-%d")

power_df_m<-power_new %>% select(Date, Global_active_power_kWm)%>%
  filter(year(Date)!=2006) %>%
  transmute(YeMo=as.yearmon(Date),Global_active_power_kWm) %>%
  group_by(YeMo)%>%summarise_all(sum)%>%ungroup()

power_m_ts<-ts(power_df_m$Global_active_power_kWm, frequency = 12, start=c(2007))

# train & test #

power_train_m_ts_2<-window(power_m_ts, end=c(2009,11))
power_test_m_ts_2<-window(power_m_ts, start=c(2009,11))


autoplot(power_m_ts)+autolayer(power_train_m_ts_2)+autolayer(power_test_m_ts_2)

#decompose#

power_train_m_dec2<-decompose(power_train_m_ts_2)

power_train_m_dec_stl <- stl(power_train_m_ts_2, s.window=30, t.window=12, s.degree=0)
autoplot(power_train_m_dec_stl)

#HW#
power_train_m_HW_2<-HoltWinters(power_train_m_ts_2)
power_m_HW_2<-HoltWinters(power_m_ts)

power_train_m_HW_for_2<-forecast(power_train_m_HW_2, h = 12)
power_m_HW_for_2<-forecast(power_m_HW, h = 48)

MHW2_m<-accuracy(power_train_m_HW_for_2,power_test_m_ts_2)

#plot prediction by month2#
p_hw_m2<-autoplot(power_m_ts) + 
  autolayer(power_m_HW_for, PI=FALSE, col="blue", size=1)

p_hw_m3<-autoplot(power_test_m_ts_2) + 
  autolayer(power_train_m_HW_for, PI=FALSE, col= "cyan", size=1)

#arima# ### 

power_train_m_arima_2<-auto.arima(power_train_m_ts_2)
power_m_arima_2<-auto.arima(power_m_ts)

power_train_m_arima_for_2<-forecast(power_train_m_arima_2, h = 12)
power_m_arima_for_2<-forecast(power_m_arima_2, h = 48)

Marima2_m<-accuracy(power_train_m_arima_for_2,power_test_m_ts_2)

#plot prediction by month2#
p_arima_m2<-autoplot(power_m_ts) +
  autolayer(power_m_arima_for_2, PI=FALSE, col="blue", size=1)
p_arima_m3<-autoplot(power_test_m_ts_2) +
  autolayer(power_train_m_arima_for_2, PI=FALSE, col= "cyan", size=1)

#linear#


power_train_m_tslm_2<-tslm(power_train_m_ts_2~ trend + season)
power_m_tslm_2<-tslm(power_m_ts~ trend + season)

power_train_m_tslm_for_2<-forecast(power_train_m_tslm_2, h = 12)
power_m_tslm_for_2<-forecast(power_m_tslm_2, h = 48)

Mtslm2_m<-accuracy(power_train_m_tslm_for_2,power_test_m_ts_2)

#plot prediction by month2#
p_tslm_m2<-autoplot(power_m_ts) + 
  autolayer(power_m_tslm_for_2, PI=FALSE, col="blue", size=1)
p_tslm_m3<-autoplot(power_test_m_ts_2) + 
  autolayer(power_train_m_tslm_for_2, PI=FALSE, col= "cyan", size=1)


####MODEL SELECTION RESUMEN####

###BY DAY###
p_d_train_models<-grid.arrange(p_tslm_d3, p_hw_d3)
p_d_pred_models<-grid.arrange(p_tslm_d2, p_hw_d2)

p_all_d<-autoplot(power_d_ts)+ 
  autolayer(power_train_d_tslm_for_2, PI=FALSE, col= "cyan", size=1)+
  + autolayer(power_train_d_HW_for_2, PI=FALSE, col= "red", size=1)

Mtslm2 ###choosed###
MHW2

## BY MONTH ##

p_m_train_models<-grid.arrange(p_tslm_m3, p_hw_m3,p_arima_m3)
p_m_pred_models<-grid.arrange(p_tslm_m2, p_hw_m2, p_arima_m2)

p_all_m<-autoplot(power_m_ts)+ 
  autolayer(power_train_m_tslm_for_2, PI=FALSE, col= "cyan", size=1, series = "tsml")+
  autolayer(power_train_m_HW_for_2, PI=FALSE, col= "red", size=1, series = "hw")+
  autolayer(power_train_m_arima_for_2, PI=FALSE, col= "orange", size=1, series = "arima")
  

Mtslm2_m #blue##choosen#
MHW2_m#red#
Marima2_m#orange#
## ALL FORECASTING ###

###############################################################################################


####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####
## pendiente de revisar la derivada de la linea 311#
#identify missing minutess#
#na f¡values with prior#
#na with the #
#replace outliers with regresion model or spline, pending to analize#

#decompose without logarith#


