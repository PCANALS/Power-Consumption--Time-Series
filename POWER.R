
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

setwd("C:/Users/pilar/Google Drive/A_DATA/UBIQUM/TASK3.1/git/task-3-1-define-a-data-science-process-PCANALS")

powero<-read.csv(file = "household_power_consumption.txt", header = TRUE, sep =";", na.strings=c("NA", "-", "?"), stringsAsFactors=FALSE)

#"na.strings=c("NA", "-", "?")" changes in all data set all teorical "missing values" as NA in R
#"stringsAsFactors=FALSE" don't converts the variables in factors


str(powero)
dim(powero)
summary(powero)
apply(powero, 2, class)

power<-powero

power<-cbind(power,paste(power$Date,power$Time), stringsAsFactors=FALSE)
colnames(power)[10] <-"DateTime"
power <- power[,c(ncol(power), 1:(ncol(power)-1))]
head(power)


power$Date<-lubridate::dmy(power$Date)
power$Time<-lubridate::hms(power$Time)
power$DateTime<-lubridate::dmy_hms(power$DateTime)### error#

###warning error Warning messages:
# 1: All formats failed to parse. No formats found. 
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation

#'data.frame':	2075259 obs. of  10 variables:
# $ DateTime             : POSIXct, format: "2006-12-16 17:24:00" "2006-12-16 17:25:00" "2006-12-16 17:26:00" "2006-12-16 17:27:00" ...
# $ Date                 : Date, format: "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
# $ Time                 :Formal class 'Period' [package "lubridate"] with 6 slots


str(power)

####~~~~NOTES - NEXT STEPS~~~~####

#dyplr tratar NA, ver dataframe solo na#

#lubridate tratar fechas#