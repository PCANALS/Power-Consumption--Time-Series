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

#### C.-2 MISSING VALUES NA####



#### C.-2.3 Change NA with the closest values - INTERPOLATE####

power_new<-na.locf(power_new)

power_new2<-dplyr::mutate(power, NotSubMet_Wh = ((Global_active_power_kWm*1000)/60)
                          -Sub_metering_3_Wh-Sub_metering_2_Wh-Sub_metering_1_Wh, 
                          GlobalApparent_Wh = (Global_active_power_kWm+Global_reactive_power_kWm)*1000/60, 
                          Global_active_power_Wh=((Global_active_power_kWm*1000)/60))

anyNA(power_new2)
power_new2<-na.locf(power_new2)


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

####C.-5 group by day####
power_day<-power_new%>%select(Date, Global_active_power_kWm)%>%
  #mutate(Year2=year(Date), Month2=month(Date), Day2= day(Date))%>%
  group_by(Date)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))
  
power_day$consecutiveDay <- c(NA,diff(ymd(power_day$Date))==1)


nrow(power_day)-1==sum(power_day$consecutiveDay, na.rm = T)

#we can confirm that all dates registered are consecutive#

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






#### ~~~~BY DAY~~~~#### FALTA CHEK RESIDUALS####

power_df_d<-power_day%>%select(Date, Global_active_power_kWm)%>%filter(year(Date)!=2006)

power_d_ts<-ts(power_df_d$Global_active_power_kWm, frequency = 365, start=c(2007,1))

power_train_d_ts_2<-window(power_d_ts, end=c(2009,330))
power_test_d_ts_2<-window(power_d_ts, start=c(2009,330))


autoplot(power_d_ts)+autolayer(power_train_d_ts_2)+autolayer(power_test_d_ts_2)
#decompose

power_train_d_dec2<-decompose(power_train_d_ts_2)

power_train_d_dec_stl <- stl(power_train_d_ts_2, s.window=7, t.window=1000, s.degree=0)
autoplot(power_train_d_dec_stl)

#HW#
power_train_d_HW_2<-HoltWinters(power_train_d_ts_2)
power_d_HW_2<-HoltWinters(power_d_ts)

power_train_d_HW_for_2<-forecast(power_train_d_HW_2, h = 365)
power_d_HW_for_2<-forecast(power_d_HW_2, h = 730)

MHW2<-accuracy(power_train_d_HW_for,power_test_d_ts_2)

#plot prediction by day2#
p_hw_d2<-autoplot(power_d_ts) + 
  autolayer(power_d_HW_for_2, PI=FALSE, col="blue", size=1)

p_hw_d3<-autoplot(power_test_d_ts_2) + 
  autolayer(power_train_d_HW_for_2, PI=FALSE, col= "cyan", size=1)

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


####~~~~BY WEEK ~~~~~####

power_new$Date<-as_date(power_new$Date)

power_df_w<-power_new%>%
  select(Date, Global_active_power_kWm)%>%filter(year(Date)!=2006)%>%
  mutate(Year2=year(Date),WeekP=week(Date))%>%
  group_by(Year2, WeekP)%>%
  summarise(Global_active_power_kWm = mean(Global_active_power_kWm))


power_w_ts<-ts(power_df_w$Global_active_power_kWm, frequency = 52, start=c(2007,1))

power_train_w_ts_2<-window(power_w_ts, start=c(2007,1), end=c(2009,47))
power_test_w_ts_2<-window(power_w_ts, start=c(2009,48))

autoplot(power_w_ts)+autolayer(power_train_w_ts_2)+autolayer(power_test_w_ts_2)

#decompose

power_train_w_dec2<-decompose(power_train_w_ts_2)

power_train_w_dec_stl <- stl(power_train_w_ts_2, s.window=7, t.window=150, s.degree=0)
autoplot(power_train_w_dec_stl)

#HW#
power_train_w_HW_2<-HoltWinters(power_train_w_ts_2)
power_w_HW_2<-HoltWinters(power_w_ts)

power_train_w_HW_for_2<-forecast(power_train_w_HW_2, h = 52)
power_w_HW_for_2<-forecast(power_w_HW_2, h = 105)

MHW2_w<-accuracy(power_train_w_HW_for_2,power_test_w_ts_2)

#plot prediction by week2#
p_hw_w2<-autoplot(power_w_ts) + 
  autolayer(power_w_HW_for_2, PI=FALSE, col="blue", size=1)

p_hw_w3<-autoplot(power_test_w_ts_2) + 
  autolayer(power_train_w_HW_for_2, PI=FALSE, col= "cyan", size=1)

#ARIMA#

power_train_w_arima_2<-auto.arima(power_train_w_ts_2)
power_w_arima_2<-auto.arima(power_w_ts)

power_train_w_arima_for_2<-forecast(power_train_w_arima_2, h = 52)
power_w_arima_for_2<-forecast(power_w_arima_2, h = 105)

Marima2_w<-accuracy(power_train_w_arima_for_2,power_test_w_ts_2)

#plot prediction by week2#
p_arima_w2<-autoplot(power_w_ts) +
  autolayer(power_w_arima_for_2, PI=FALSE, col="blue", size=1)
p_arima_w3<-autoplot(power_test_w_ts_2) +
  autolayer(power_train_w_arima_for_2, PI=FALSE, col= "cyan", size=1)

#linear#


power_train_w_tslm_2<-tslm(power_train_w_ts_2~ trend + season)
power_w_tslm_2<-tslm(power_w_ts~ trend + season)

power_train_w_tslm_for_2<-forecast(power_train_w_tslm_2, h = 52)
power_w_tslm_for_2<-forecast(power_w_tslm_2, h = 105)

Mtslm2_w<-accuracy(power_train_w_tslm_for_2,power_test_w_ts_2)

#plot prediction by week2#
p_tslm_w2<-autoplot(power_w_ts) + 
  autolayer(power_w_tslm_for_2, PI=FALSE, col="blue", size=1)
p_tslm_w3<-autoplot(power_test_w_ts_2) + 
  autolayer(power_train_w_tslm_for_2, PI=FALSE, col= "cyan", size=1)




####~~~~BY MONTH ~~~~####

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
power_m_HW_for_2<-forecast(power_m_HW_2, h = 48)

MHW2_m<-accuracy(power_train_m_HW_for_2,power_test_m_ts_2)

#plot prediction by month2#
p_hw_m2<-autoplot(power_m_ts) + 
  autolayer(power_m_HW_for_2, PI=FALSE, col="blue", size=1)

p_hw_m3<-autoplot(power_test_m_ts_2) + 
  autolayer(power_train_m_HW_for_2, PI=FALSE, col= "cyan", size=1)

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
## BY WEEK ##

p_w_train_models<-grid.arrange(p_tslm_w3, p_hw_w3,p_arima_w3)
p_w_pred_models<-grid.arrange(p_tslm_w2, p_hw_w2, p_arima_w2)

p_all_w<-autoplot(power_w_ts)+ 
  autolayer(power_train_w_tslm_for_2, PI=FALSE, col= "cyan", size=1, series = "tsml")+
  autolayer(power_train_w_HW_for_2, PI=FALSE, col= "red", size=1, series = "hw")+
  autolayer(power_train_w_arima_for_2, PI=FALSE, col= "orange", size=1, series = "arima")


Mtslm2_w #blue##choosen#
MHW2_w#red#
Marima2_w#orange#

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


####ALL FORECASTING ####

p_all_for<-grid.arrange(p_tslm_m2, p_tslm_w2, p_tslm_d2)


# #plot prediction by month# 
# p_hw_m<-autoplot(power_train_m_ts) + 
#   autolayer(power_train_m_HW_for, PI=FALSE, col= "coral", size=1)+
#   autolayer(power_m_HW_for, PI=FALSE, col="firebrick2", size=1)
# 
# 
# #plot prediction by week#
# p_hw_w<-autoplot(power_train_w_ts) + 
#   autolayer(power_train_w_HW_for, PI=FALSE, col= "green", size=1)+
#   autolayer(power_w_HW_for, PI=FALSE, col="mediumspringgreen", size=1)
# 
# #plot prediction by day#
# p_hw_d<-autoplot(power_train_d_ts) + 
#   autolayer(power_train_d_HW_for, PI=FALSE, col= "cyan", size=1)+
#   autolayer(power_d_HW_for, PI=FALSE, col="blue", size=1)
# 
# 
# plot_HW_for<-grid.arrange(p_hw_m,p_hw_w, p_hw_d)


####~~~~~~~~   NOTES - NEXT STEPS    ~~~~~~~~####
## pendiente de revisar la derivada de la linea 311#
#identify missing minutess#
#na f¡values with prior#
#na with the #
#replace outliers with regresion model or spline, pending to analize#
#MISSING TIME#
#decompose without logarith#


