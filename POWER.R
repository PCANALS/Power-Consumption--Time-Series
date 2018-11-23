#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")


#### ._LIBRARIES####

library(tidyselect)
#install.packages("tidyverse")
#library(tidyverse)
#####includes:###
library(dplyr)
library(ggplot2)
library(tidyr)
#tidyverse_update()
#library(tidyselect)
library(lubridate)
library(imputeTS)
library(R.utils)
library(TTR)
library(ggfortify)
library(magrittr)
library(forecast)
library(gridExtra)
library(GGally)
library(imputeTS)
library(forecast)
library(stats)
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
power_new[is.na(power_new)] <- 0


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
  group_by(YeTs, MoTs)%>%summarise_all(mean)
power_df_m_ts<-ts(power_df_m)

anyNA(power_df_m)
power_df_m<-na.locf(power_df_m)
power_m_ts<-ts(power_df_m, frequency = 12, start=c(2006,12))

# train #

power_train_m<-power_df_m%>%ungroup()%>%filter(YeTs!=2010)

#test #

power_test_m<-power_df_m%>%ungroup()%>%filter(YeTs==2010)



#Frame by week#

      # power_week<-power_new%>%select(Date, Global_active_power_kWm)%>%
      #   mutate(Year2=year(Date),WeekP=week(Date))%>%
      #   group_by(Year2, WeekP)%>%
      #   summarise(Global_active_power_kWm = mean(Global_active_power_kWm))

anyNA(power_week)
power_df_w<-na.locf(power_week)
power_w_ts<-ts(power_df_w, frequency = 53, start=c(2006,12))


### train 

power_train_w<-power_df_w%>%ungroup()%>%filter(Year2!=2010)

###test #

power_test_w<-power_df_w%>%ungroup()%>%filter(Year2==2010)




##Frame by day##

#power_day
# power_day<-power_new%>%select(Date, Global_active_power_kWm)%>%
#   group_by(Date)%>%
#   summarise(Global_active_power_kWm = mean(Global_active_power_kWm))
anyNA(power_day)
power_df_d<-na.locf(power_day)
power_df_d<-power_df_d%>%select(Date, Global_active_power_kWm)

power_d_ts<-ts(power_df_d, frequency = 365, start=c(2006,12))


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

par(mfrow=c(3,1)) #number of plots
plot.ts(power_train_m_ts)
plot.ts(power_train_w_ts)
plot.ts(power_train_d_ts)

plot.ts(power_test_m_ts)
plot.ts(power_test_w_ts)
plot.ts(power_test_d_ts)

#decompose
power_train_m_dec<-decompose(power_train_m_ts)
power_train_w_dec<-decompose(power_train_w_ts)
power_train_d_dec<-decompose(power_train_d_ts)


par(mfrow=c(3,1)) #number of plots
plot(power_train_m_dec)
plot(power_train_w_dec)
plot(power_train_d_dec)

#HW#

power_train_m_HW<-HoltWinters(power_train_m_ts)
power_train_w_HW<-HoltWinters(power_train_w_ts)
power_train_d_HW<-HoltWinters(power_train_d_ts)


  #power_train_m_HW$fitted

plot(power_train_m_HW)
plot(power_train_w_HW)
plot(power_train_d_HW)

power_train_m_HW_for<-forecast(power_train_m_HW, h = 6)
hist(power_train_m_HW_for$residuals)

acf(power_train_m_HW_for$residuals, na.action = na.pass)
Box.test(power_train_m_HW_for$residuals, lag = 6 )

power_train_m_HW_for2<-
forecast:::forecast.HoltWinters(power_train_m_HW_for, h = 12)

accuracy(power_train_m_HW_for,power_test_m_ts)

plot(power_train_m_HW_for)

autoplot(power_test_m_ts) + autolayer(power_train_m_HW_for, PI=FALSE)

autoplot(power_train_m_HW_for)


####ARIMA####

autoplot(power_train_m_ts)
autoplot(power_train_w_ts)
autoplot(power_train_d_ts)

power_train_m_arima<-auto.arima(power_train_m_ts)
power_train_w_arima<-auto.arima(power_train_w_ts)
power_train_d_arima<-auto.arima(power_train_d_ts)

power_train_m_arima_fore<-forecast:::forecast.Arima(power_train_m_arima, h=10)
power_train_w_arima_fore<-forecast:::forecast.Arima(power_train_w_arima, h=40)
power_train_d_arima_fore<-forecast:::forecast.Arima(power_train_d_arima, h=300)

power_train_m_arima_fore$fitted

acf(power_train_m_arima$residuals, na.action = na.pass)
Box.test(power_train_m_arima$residuals, lag = 6 )
checkresiduals(power_train_m_arima)

acf(power_train_w_arima$residuals, na.action = na.pass)
acf(power_train_d_arima$residuals, na.action = na.pass)
checkresiduals(power_train_w_arima)
checkresiduals(power_train_d_arima)



autoplot(power_train_m_ts)+autolayer(power_train_m_arima_fore)

## LINEAR REGRESION ##



tslm(power_train_m_ts~ trend + season)



## ALL FORECASTING ###

###############################################################################################3


#### tries with all values ####
pwm_ts<-ts(power_ts, frequency = 12, start=c(2006,12))

pwd_ts<-ts(power_day[,1:2], frequency = 365, start=c(2006,12))

plot.ts(pwm_ts[,3])
plot.ts(pwd_ts[,2])


##### pruebas TS plots - plots utiles ####

par(mfrow=c(2,1)) #number of plots#

ptsm<-autoplot(pwm_ts[,3])+ ggtitle("Global Consuption by month")
ptsd<-autoplot(pwd_ts[,2])+ ggtitle("Global Consuption by day")

ptsmd<-grid.arrange(ptsm, ptsd)

#plot to see differences with seasons#
pseam<-ggseasonplot(pwm_ts[,3], year.labels=TRUE, year.labels.left=TRUE)+ ylab(" Consuption ")+
                      ggtitle("Seasonal plot: Global Consuption by month")

psead<-ggseasonplot(pwd_ts[,2], year.labels=TRUE, year.labels.left=TRUE)+ ylab(" Consuption ")+
  ggtitle("Seasonal plot: Global Consuption by day")

pseamd<-grid.arrange(pseam, psead)


#ggseasonplot(pwm_ts[,3], polar=TRUE)

ggsubseriesplot(pwm_ts[,3])+  ggtitle("Seasonal subseries plot: by month") 
#season by month the horizontal line means the mean of subplot#


autoplot(pwm_ts[,3:8], facets=TRUE) #Active vs Reactive

qplot(Global_active_power_kWm, Global_reactive_power_kWm, data=as.data.frame(pwm_ts)) +
  ylab("Global reactive power_kWm") + xlab("Global active power kWm") #relations betwenn attributes

autoplot(pwm_ts[,3:8], facets=TRUE)

GGally::ggpairs(as.data.frame(pwm_ts[,3:8])) ###correlation with features### 

ggAcf(pwm_ts[,3], lag=20) ### pendiente arreglar la derivada###

lambda<-BoxCox.lambda(pwm_ts)

autoplot(BoxCox(pwm_ts[,3],lambda))

# res <- residuals(naive(pwm_ts))
# autoplot(res)
####rework time series_all values####



#not log#

#pwm_ts_log<-log(pwm_ts)
pwm_ts3<-pwm_ts[,3]

pwm_ts_dec<-decompose(pwm_ts3) #additive# there are NA values in trend and random#

pwm_ts_dec 

pwm_ts3 %>% decompose %>% autoplot

pwm_ts3 %>% stl(s.window='periodic') %>% autoplot  #smoothing is effectively replaced by taking the mean. 

plot(pwm_ts_dec)

#### holtwinters####

pwm_fore<-HoltWinters(pwm_ts3)

pwm_fore$fitted

plot(pwm_fore)
autoplot(pwm_fore)


x<-forecast(pwm_fore, h = 6)
hist(x$residuals)

acf(x$residuals, na.action = na.pass)
Box.test(x$residuals, lag = 6 )

pwm_for_hw<-forecast:::forecast.HoltWinters(pwm_fore, h=6)
plot(forecast:::forecast.HoltWinters(pwm_fore,h=12))

plot(pwm_for_hw)

###CLOSE TO ONE##



pwm_ts %>% changepoint::cpt.meanvar() %>% autoplot()
strucchange::breakpoints(pwm_ts~1) %>% autoplot()

####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####
## pendiente de revisar la derivada de la linea 311#
#identify missing minutess#
#na f¡values with prior#
#na with the #
#replace outliers with regresion model or spline, pending to analize#

#decompose without logarith#


