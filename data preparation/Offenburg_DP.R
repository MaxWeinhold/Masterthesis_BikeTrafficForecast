#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Offenburg

library(lubridate)
library(dplyr)
library(plyr)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Offenburg
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Baden Württemberg")

#Read Bycicle Counting Data----------------------------------------------
countingData_2021_1 = read.csv(file = "eco_counter_fahrradzaehler_202101.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_2 = read.csv(file = "eco_counter_fahrradzaehler_202102.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_3 = read.csv(file = "eco_counter_fahrradzaehler_202103.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_4 = read.csv(file = "eco_counter_fahrradzaehler_202104.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_5 = read.csv(file = "eco_counter_fahrradzaehler_202105.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_6 = read.csv(file = "eco_counter_fahrradzaehler_202106.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_7 = read.csv(file = "eco_counter_fahrradzaehler_202107.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_8 = read.csv(file = "eco_counter_fahrradzaehler_202108.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_9 = read.csv(file = "eco_counter_fahrradzaehler_202109.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_10 = read.csv(file = "eco_counter_fahrradzaehler_202110.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_11 = read.csv(file = "eco_counter_fahrradzaehler_202111.csv",sep=",", encoding="ISO-8859-1")
countingData_2021_12 = read.csv(file = "eco_counter_fahrradzaehler_202112.csv",sep=",", encoding="ISO-8859-1")

#Connect Years

rawData = rbind(countingData_2021_1,countingData_2021_2)
rawData = rbind(rawData,countingData_2021_3)
rawData = rbind(rawData,countingData_2021_4)
rawData = rbind(rawData,countingData_2021_5)
rawData = rbind(rawData,countingData_2021_6)
rawData = rbind(rawData,countingData_2021_7)
rawData = rbind(rawData,countingData_2021_8)
rawData = rbind(rawData,countingData_2021_9)
rawData = rbind(rawData,countingData_2021_10)
rawData = rbind(rawData,countingData_2021_11)
rawData = rbind(rawData,countingData_2021_12)

#Select correct cities

rawData<-rawData[(rawData$standort=="Stadt Offenburg"),]
rawData$standort = "Offenburg"
names(rawData)
rawData$iso_timestamp = NULL
rawData$stand = NULL
rawData$channel_id = NULL
rawData$channel_name = NULL
#rawData$counter_site = NULL
rawData$counter_site_id = NULL
rawData$domain_name = NULL
rawData$timezone = NULL
rawData$interval = NULL
rawData$counter_serial = NULL
rawData$domain_id = NULL

names(rawData)[1] = "Timestamp"
names(rawData)[2] = "Value"
names(rawData)[3] = "Town"
names(rawData)[4] = "Station"
names(rawData)[5] = "Lon"
names(rawData)[6] = "Lat"

summary(rawData$Value)

levels(as.factor(rawData$Lon))
levels(as.factor(rawData$Station))

#Change count frequency to hourly data----------------------------------------------

rawData$Timestamp = gsub("T", " ", rawData$Timestamp)
rawData$Timestamp = gsub("+0000", "", rawData$Timestamp)
rawData$Timestamp = substring(rawData$Timestamp,1, nchar(rawData$Timestamp)-1)
rawData$Timestamp=cut(strptime(rawData$Timestamp,"%Y-%m-%d %H:%M"),"hour")

raw = rawData
names(raw)
stations=matrix(1:3*nlevels(as.factor(raw$Lon)), nrow = nlevels(as.factor(raw$Lon)), ncol = 3)

for(i in 1:nlevels(as.factor(raw$Lon))){
  d=raw[raw$Lon %in% toString(levels(as.factor(raw$Lon))[i]),]
  
  stations[i,1]=d$Lon[i]
  stations[i,2]=d$Station[i]
  stations[i,3]=d$Town[i]
  
}

stations=as.data.frame(stations)
names(stations)[1]="Lon"
names(stations)[2]="Station"
names(stations)[3]="Town"

rawData = ddply(rawData,.(Timestamp,Lon,Lat),summarize,Value=sum(Value))

rawData = merge(x = rawData,y = stations,
                by = c("Lon"),
                all = FALSE)

rawData = na.omit(rawData)
summary(rawData)
nlevels(as.factor(rawData$Station))
rm(list=setdiff(ls(), "rawData"))

#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays

#TimeStamp Configurations
rawData$Timestamp

#rawData$Timestamp=cut(strptime(rawData$Timestamp,"%Y-%m-%d %H:%M"),"hour")
rawData$Timestamp=as.POSIXlt(rawData$Timestamp,format="%Y-%m-%d %H:%M")

rawData$Year	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%Y"))
rawData$Months=month(as.POSIXlt(rawData$Timestamp))
rawData$Day	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%d"))
rawData$Summer = ifelse(rawData$Months == "6" | rawData$Months == "7"| rawData$Months == "8", 1, 0)
rawData$Winter = ifelse(rawData$Months == "12" | rawData$Months == "1"| rawData$Months == "2", 1, 0)
rawData$Weekday	= format(as.POSIXlt(rawData$Timestamp),"%a")
rawData$Weekend <- ifelse(rawData$Weekday == "So" | rawData$Weekday == "Sa", 1, 0)
rawData$Hour	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%H"))
rawData$Night = ifelse(rawData$Hour<7,1,0)

#Load data for public holidays
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
publicHolidays = read.csv(file = "Feiertage.csv",sep=";")

pH=publicHolidays[publicHolidays$BWB %in% TRUE,]
rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)

#Load data for school holidays
schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")

sH=schoolHolidays[schoolHolidays$Bundesland %in% "BWB",]
x <- vector()
for(i in 1:length(sH$Startdatum)){
  x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
}
rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)

summary(rawData)

#Add Data on the Youth Inhabitant Ratio

#Add Weather Data (Source: Deutscher Wetterdienst)

rm(list=setdiff(ls(), "rawData"))

#Import Weather Data
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Offenburg")
Weather_Wind  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_F.csv",sep=",", skip = 1, header = F)
Weather_CloudCover  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_N.csv",sep=",", skip = 1, header = F)
Weather_Humidity  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RF.csv",sep=",", skip = 1, header = F)
Weather_Rain  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RR.csv",sep=",", skip = 1, header = F)
Weather_Temperature  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_T2M.csv",sep=",", skip = 1, header = F)

Weather_Wind[1] <- NULL
Weather_Wind[1] <- NULL
Weather_Wind[3] <- NULL
Weather_Wind[3] <- NULL
Weather_Wind[3] <- NULL

Weather_CloudCover[1] <- NULL
Weather_CloudCover[1] <- NULL
Weather_CloudCover[3] <- NULL
Weather_CloudCover[3] <- NULL
Weather_CloudCover[3] <- NULL

Weather_Humidity[1] <- NULL
Weather_Humidity[1] <- NULL
Weather_Humidity[3] <- NULL
Weather_Humidity[3] <- NULL
Weather_Humidity[3] <- NULL

Weather_Rain[1] <- NULL
Weather_Rain[1] <- NULL
Weather_Rain[3] <- NULL
Weather_Rain[3] <- NULL
Weather_Rain[3] <- NULL

Weather_Temperature[1] <- NULL
Weather_Temperature[1] <- NULL
Weather_Temperature[3] <- NULL
Weather_Temperature[3] <- NULL
Weather_Temperature[3] <- NULL

names(Weather_Wind)[1]="Timestamp"
names(Weather_Wind)[2]="Wind"

names(Weather_CloudCover)[1]="Timestamp"
names(Weather_CloudCover)[2]="CloudCover"

names(Weather_Humidity)[1]="Timestamp"
names(Weather_Humidity)[2]="Humidity"

names(Weather_Rain)[1]="Timestamp"
names(Weather_Rain)[2]="Rain"

names(Weather_Temperature)[1]="Timestamp"
names(Weather_Temperature)[2]="Temperature"

Weather_Wind$Timestamp = gsub("T", " ", Weather_Wind$Timestamp)
Weather_Wind$Timestamp=as.POSIXlt(Weather_Wind$Timestamp,format="%Y-%m-%d %H:%M:%S")
Weather_Wind$Year	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%Y"))
Weather_Wind$Months=month(as.POSIXlt(Weather_Wind$Timestamp))
Weather_Wind$Day	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%d"))
Weather_Wind$Hour	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%H"))

Weather_CloudCover$Timestamp = gsub("T", " ", Weather_CloudCover$Timestamp)
Weather_CloudCover$Timestamp=as.POSIXlt(Weather_CloudCover$Timestamp,format="%Y-%m-%d %H:%M:%S")
Weather_CloudCover$Year	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%Y"))
Weather_CloudCover$Months=month(as.POSIXlt(Weather_CloudCover$Timestamp))
Weather_CloudCover$Day	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%d"))
Weather_CloudCover$Hour	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%H"))

Weather_Humidity$Timestamp = gsub("T", " ", Weather_Humidity$Timestamp)
Weather_Humidity$Timestamp=as.POSIXlt(Weather_Humidity$Timestamp,format="%Y-%m-%d %H:%M:%S")
Weather_Humidity$Year	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%Y"))
Weather_Humidity$Months=month(as.POSIXlt(Weather_Humidity$Timestamp))
Weather_Humidity$Day	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%d"))
Weather_Humidity$Hour	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%H"))

Weather_Rain$Timestamp = gsub("T", " ", Weather_Rain$Timestamp)
Weather_Rain$Timestamp=as.POSIXlt(Weather_Rain$Timestamp,format="%Y-%m-%d %H:%M:%S")
Weather_Rain$Year	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%Y"))
Weather_Rain$Months=month(as.POSIXlt(Weather_Rain$Timestamp))
Weather_Rain$Day	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%d"))
Weather_Rain$Hour	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%H"))

Weather_Temperature$Timestamp = gsub("T", " ", Weather_Temperature$Timestamp)
Weather_Temperature$Timestamp=as.POSIXlt(Weather_Temperature$Timestamp,format="%Y-%m-%d %H:%M:%S")
Weather_Temperature$Year	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%Y"))
Weather_Temperature$Months=month(as.POSIXlt(Weather_Temperature$Timestamp))
Weather_Temperature$Day	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%d"))
Weather_Temperature$Hour	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%H"))

Weather_Wind$Timestamp <- NULL
Weather_CloudCover$Timestamp <- NULL
Weather_Humidity$Timestamp <- NULL
Weather_Rain$Timestamp <- NULL
Weather_Temperature$Timestamp <- NULL

rawData = merge(x = rawData,y = Weather_Wind,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

rawData=na.omit(rawData)
rm(Weather_Wind)

rawData = merge(x = rawData,y = Weather_CloudCover,
                by = c("Year","Months","Day","Hour"),
                all = TRUE)

rawData=na.omit(rawData)
rm(Weather_CloudCover)

rawData = merge(x = rawData,y = Weather_Humidity,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

rawData=na.omit(rawData)
rm(Weather_Humidity)

rawData = merge(x = rawData,y = Weather_Rain,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

rawData=na.omit(rawData)
rm(Weather_Rain)

rawData = merge(x = rawData,y = Weather_Temperature,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

rawData=na.omit(rawData)
rm(Weather_Temperature)
summary(rawData)
#setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
#write.csv(rawData,"Tübingen.csv")



rawData$young18 = 16
rawData$young20 = 18  

summary(rawData)

# Adding ADFC-Fahrradklima Values

Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
ADFC_Index=c(3.4,3.4,3.4,3.4,3.6,3.6,3.6,3.6,3.6,3.6)

ADFC=as.data.frame(cbind(Year,ADFC_Index))

rawData = merge(x = rawData,y = ADFC,
                by = c("Year"),
                all = FALSE)

rm(list=setdiff(ls(), "rawData"))

#Add Data about number of inhabitants, city size, city center and male and female inhabitant ratio
#Also calculate distance to city ratio

#Load data (source: Destatis)

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Einwohner_Destatis")
Destatis12 = read.csv(file = "31122012_Auszug_GV.csv",sep=";")
Destatis13 = read.csv(file = "31122013_Auszug_GV.csv",sep=";")
Destatis14 = read.csv(file = "31122014_Auszug_GV.csv",sep=";")
Destatis15 = read.csv(file = "31122015_Auszug_GV.csv",sep=";")
Destatis16 = read.csv(file = "31122016_Auszug_GV.csv",sep=";")
Destatis17 = read.csv(file = "31122017_Auszug_GV.csv",sep=";")
Destatis18 = read.csv(file = "31122018_Auszug_GV.csv",sep=";")
Destatis19 = read.csv(file = "31122019_Auszug_GV.csv",sep=";")
Destatis20 = read.csv(file = "31122020_Auszug_GV.csv",sep=";")
Destatis21 = read.csv(file = "31122021_Auszug_GV.csv",sep=";")

title=", Stadt" #This differs, there are cities and also hanseatic cities

test12=as.data.frame(Destatis12[Destatis12$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test12[17] <- NULL
test12[17] <- NULL
test12 <- test12 %>% mutate_all(na_if,"")
names(test12)[1]="number"
test12=na.omit(test12)
test12$Year=2012

test13=as.data.frame(Destatis13[Destatis13$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test13[17] <- NULL
test13[17] <- NULL
test13 <- test13 %>% mutate_all(na_if,"")
names(test13)[1]="number"
test13=na.omit(test13)
test13$Year=2013

test14=as.data.frame(Destatis14[Destatis14$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test14[17] <- NULL
test14[17] <- NULL
test14 <- test14 %>% mutate_all(na_if,"")
names(test14)[1]="number"
test14=na.omit(test14)
test14$Year=2014

test15=as.data.frame(Destatis15[Destatis15$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test15[17] <- NULL
test15[17] <- NULL
test15 <- test15 %>% mutate_all(na_if,"")
names(test15)[1]="number"
test15=na.omit(test15)
test15$Year=2015

test16=as.data.frame(Destatis16[Destatis16$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test16[17] <- NULL
test16[17] <- NULL
test16 <- test16 %>% mutate_all(na_if,"")
names(test16)[1]="number"
test16=na.omit(test16)
test16$Year=2016

test17=as.data.frame(Destatis17[Destatis17$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test17[17] <- NULL
test17[17] <- NULL
test17 <- test17 %>% mutate_all(na_if,"")
names(test17)[1]="number"
test17=na.omit(test17)
test17$Year=2017

test18=as.data.frame(Destatis18[Destatis18$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test18[17] <- NULL
test18[17] <- NULL
test18 <- test18 %>% mutate_all(na_if,"")
names(test18)[1]="number"
test18=na.omit(test18)
test18$Year=2018

test19=as.data.frame(Destatis19[Destatis19$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test19[17] <- NULL
test19[17] <- NULL
test19 <- test19 %>% mutate_all(na_if,"")
names(test19)[1]="number"
test19=na.omit(test19)
test19$Year=2019

test20=as.data.frame(Destatis20[Destatis20$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test20[17] <- NULL
test20[17] <- NULL
test20 <- test20 %>% mutate_all(na_if,"")
names(test20)[1]="number"
test20=na.omit(test20)
test20$Year=2020

test21=as.data.frame(Destatis21[Destatis21$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
test21[17] <- NULL
test21[17] <- NULL
test21 <- test21 %>% mutate_all(na_if,"")
names(test21)[1]="number"
test21=na.omit(test21)
test21$Year=2021

test=rbind(test12,test13)
test=rbind(test,test14)
test=rbind(test,test15)
test=rbind(test,test16)
test=rbind(test,test17)
test=rbind(test,test18)
test=rbind(test,test19)
test=rbind(test,test20)
test=rbind(test,test21)

test$X.7 = gsub(" ", "", test$X.7)
test$X.7 = gsub(",", ".", test$X.7)
test$X.7 = as.numeric(test$X.7)
names(test)[9]="Area"

test$X.8 = gsub(" ", "", test$X.8)
test$X.8 = as.numeric(test$X.8)
names(test)[10]="Inhabitants"

test$X.9 = gsub(" ", "", test$X.9)
test$X.9 = as.numeric(test$X.9) / test$Inhabitants
names(test)[11]="Male_Ratio"

#City Longitude and Latidtude

test$X.13 = gsub(" ", "", test$X.13)
test$X.13 = gsub(",", ".", test$X.13)
test$X.13 = as.numeric(test$X.13)
names(test)[15]="City_Lon"

test$X.14 = gsub(" ", "", test$X.14)
test$X.14 = gsub(",", ".", test$X.14)
test$X.14 = as.numeric(test$X.14)
names(test)[16]="City_Lat"

names(test)[18]="Density"

test$number <- NULL
test$X <- NULL
test$X.1 <- NULL
test$X.2 <- NULL
test$X.3 <- NULL
test$X.4 <- NULL
test$X.5 <- NULL
test$X.6 <- NULL
test$X.10 <- NULL
test$X.11 <- NULL
test$X.12 <- NULL
test$X.17 <- NULL

rawData = merge(x = rawData,y = test,
                by = c("Year"),
                all = FALSE)

rm(list=setdiff(ls(), "rawData"))

#calculate distance to city center for every station

#create a matrix, that later will contaion needed information
distmat=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance and add this in a data frame
for(i in 1:nlevels(as.factor(rawData$Station))){
  print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  #calculate distance for station
  dist=distm(c(d$Lon[i],d$Lat[i]), c(d$City_Lon[i],d$City_Lat[i]), fun=distGeo)
  print(paste("Distance from",d[1,8]," station to city center is",dist,"meters"))
  
  distmat[i,1]=d$Station[1]
  distmat[i,2]=dist
  
  rm(d)
}

distmat=as.data.frame(distmat)
names(distmat)[1]="Station"
names(distmat)[2]="Distance_to_Center"
distmat$Distance_to_Center=as.numeric(distmat$Distance_to_Center)

rawData = merge(x = rawData,y = distmat,
                by = c("Station"),
                all = FALSE)

summary(rawData)

rm(list=setdiff(ls(), "rawData"))


#Reading POI from Open Street Map________________________________________________________________________________________________________________________________________-
#Using the overpass API 

#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#Build a query asking for cinemas
#building the query
q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure

cinema <- osmdata_sf(q)

#c1lon=cinema$osm_points$geometry[[7]][1]
#c1lat=cinema$osm_points$geometry[[7]][2]

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)

for(i in 1:length(cinema$osm_points$name)){
  
  cinmat[i,1]=cinema$osm_points$name[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
  print(cinema$osm_points$name[i])
  print(cinema$osm_points$geometry[[i]][])
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
    print(cindist)
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 1000)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 3000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestCinema"
distmat_closest$ClosestCinema=as.numeric(distmat_closest$ClosestCinema)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="Cinemas1kmRadius"
distmat_1kmradius$Cinemas1kmRadius=as.numeric(distmat_1kmradius$Cinemas1kmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="Cinemas3kmRadius"
distmat_3kmradius$Cinemas3kmRadius=as.numeric(distmat_3kmradius$Cinemas3kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

#Get all schools________________________________________________________________

#Build a query asking for cinemas
#building the query
q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("amenity", "school")

str(q) #query structure

cinema <- osmdata_sf(q)

#c1lon=cinema$osm_points$geometry[[7]][1]
#c1lat=cinema$osm_points$geometry[[7]][2]

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_polygons$osm_id)){
  
  cinmat[i,1]=cinema$osm_polygons$osm_id[i]
  cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
  cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
  
  #print(cinema$osm_points$name[i])
  #print(cinema$osm_points$geometry[[i]][])
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  #print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
    #print(cindist)
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 500)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 2000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestSchool"
distmat_closest$ClosestSchool=as.numeric(distmat_closest$ClosestSchool)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="Schools500mmRadius"
distmat_1kmradius$Schools500mmRadius=as.numeric(distmat_1kmradius$Schools500mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="Schools2kmRadius"
distmat_3kmradius$Schools2kmRadius=as.numeric(distmat_3kmradius$Schools2kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

#Get university Buildings_______________________________________________________

#Build a query asking for cinemas
#building the query
q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("amenity", "university")

str(q) #query structure

cinema <- osmdata_sf(q)

#c1lon=cinema$osm_points$geometry[[7]][1]
#c1lat=cinema$osm_points$geometry[[7]][2]

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_polygons$osm_id)){
  
  cinmat[i,1]=cinema$osm_polygons$osm_id[i]
  cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
  cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
  
  #print(cinema$osm_points$name[i])
  #print(cinema$osm_points$geometry[[i]][])
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  #print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
    #print(cindist)
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 500)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 2000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestUniBuild"
distmat_closest$ClosestUniBuild=as.numeric(distmat_closest$ClosestUniBuild)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="UniBuild500mmRadius"
distmat_1kmradius$UniBuild500mmRadius=as.numeric(distmat_1kmradius$UniBuild500mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="UniBuild2kmRadius"
distmat_3kmradius$UniBuild2kmRadius=as.numeric(distmat_3kmradius$UniBuild2kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))


#Use Coordinates of Shops and Supermarkets___________________________________________________________________________________


#Build a query asking for cinemas
#building the query
q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("shop", "supermarket")

str(q) #query structure

cinema <- osmdata_sf(q)

#c1lon=cinema$osm_points$geometry[[7]][1]
#c1lat=cinema$osm_points$geometry[[7]][2]

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_polygons$osm_id)){
  
  cinmat[i,1]=cinema$osm_polygons$osm_id[i]
  cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
  cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
  
  #print(cinema$osm_points$name[i])
  #print(cinema$osm_points$geometry[[i]][])
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  #print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
    #print(cindist)
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 500)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 1000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestSuperMarket"
distmat_closest$ClosestSuperMarket=as.numeric(distmat_closest$ClosestSuperMarket)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="SuperMarket500mmRadius"
distmat_1kmradius$SuperMarket500mmRadius=as.numeric(distmat_1kmradius$SuperMarket500mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="SuperMarket1kmRadius"
distmat_3kmradius$SuperMarket1kmRadius=as.numeric(distmat_3kmradius$SuperMarket1kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

available_tags("shop")

#Clothingshops__________________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("shop", "clothes")

str(q) #query structure

cinema <- osmdata_sf(q)

#c1lon=cinema$osm_points$geometry[[7]][1]
#c1lat=cinema$osm_points$geometry[[7]][2]

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
  #print(cinema$osm_points$name[i])
  #print(cinema$osm_points$geometry[[i]][])
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  #print(levels(as.factor(rawData$Station))[i])
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
    #print(cindist)
  }
  
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 500)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 2000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestClothesShop"
distmat_closest$ClosestClothesShop=as.numeric(distmat_closest$ClosestClothesShop)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="ClothesShop500mmRadius"
distmat_1kmradius$ClothesShop500mmRadius=as.numeric(distmat_1kmradius$ClothesShop500mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="ClothesShop2kmRadius"
distmat_3kmradius$ClothesShop2kmRadius=as.numeric(distmat_3kmradius$ClothesShop2kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

available_tags("amenity")

#Get Data about public transport by OSM____________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("highway", "bus_stop")

str(q) #query structure

cinema <- osmdata_sf(q)

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)

for(i in 1:length(cinema$osm_points$name)){
  
  cinmat[i,1]=cinema$osm_points$name[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 250)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 1000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestBusStop"
distmat_closest$ClosestBusStop=as.numeric(distmat_closest$ClosestBusStop)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="BusStop250mmRadius"
distmat_1kmradius$BusStop250mmRadius=as.numeric(distmat_1kmradius$BusStop250mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="BusStop1kmRadius"
distmat_3kmradius$BusStop1kmRadius=as.numeric(distmat_3kmradius$BusStop1kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

#Crossing Signals_______________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("highway", "traffic_signals")

str(q) #query structure

cinema <- osmdata_sf(q)

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 250)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 1000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestSignals"
distmat_closest$ClosestSignals=as.numeric(distmat_closest$ClosestSignals)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="Signals250mmRadius"
distmat_1kmradius$Signals250mmRadius=as.numeric(distmat_1kmradius$Signals250mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="Signals1kmRadius"
distmat_3kmradius$Signals1kmRadius=as.numeric(distmat_3kmradius$Signals1kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

#Crossing Unmarked_______________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("crossing", "unmarked")

str(q) #query structure

cinema <- osmdata_sf(q)

#cinema$osm_points$osm_id

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance to the closest cinema
for(i in 1:nlevels(as.factor(rawData$Station))){
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  distc=c(1:length(cinmat$name))
  
  #Start loops for each cinemar
  for (j in 1:length(cinmat$name)) {
    cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
    distc[j]=cindist
  }
  
  
  distmat_closest[i,1]=d[1,1]
  distmat_closest[i,2]=min(distc)
  
  distmat_1kmradius[i,1]=d[1,1]
  distmat_1kmradius[i,2]=sum(distc < 250)
  
  distmat_3kmradius[i,1]=d[1,1]
  distmat_3kmradius[i,2]=sum(distc < 1000)
  
}

distmat_closest=as.data.frame(distmat_closest)
names(distmat_closest)[1]="Station"
names(distmat_closest)[2]="ClosestUnmCross"
distmat_closest$ClosestUnmCross=as.numeric(distmat_closest$ClosestUnmCross)

distmat_1kmradius=as.data.frame(distmat_1kmradius)
names(distmat_1kmradius)[1]="Station"
names(distmat_1kmradius)[2]="UnmCross250mmRadius"
distmat_1kmradius$UnmCross250mmRadius=as.numeric(distmat_1kmradius$UnmCross250mmRadius)

distmat_3kmradius=as.data.frame(distmat_3kmradius)
names(distmat_3kmradius)[1]="Station"
names(distmat_3kmradius)[2]="UnmCross1kmRadius"
distmat_3kmradius$UnmCross1kmRadius=as.numeric(distmat_3kmradius$UnmCross1kmRadius)

rawData = merge(x = rawData,y = distmat_closest,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_1kmradius,
                by = c("Station"),
                all = FALSE)

rawData = merge(x = rawData,y = distmat_3kmradius,
                by = c("Station"),
                all = FALSE)



summary(rawData)

rm(list=setdiff(ls(), "rawData"))

#Get Tram Stattions_____________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("railway", "tram_stop")

str(q) #query structure

cinema <- osmdata_sf(q)

#cinema$osm_points$osm_id

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

#Not every city has tramstations, so we make a if question for this

if(length(cinmat$name)>0){
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestTram"
  distmat_closest$ClosestTram=as.numeric(distmat_closest$ClosestTram)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Tram250mmRadius"
  distmat_1kmradius$Tram250mmRadius=as.numeric(distmat_1kmradius$Tram250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Tram1kmRadius"
  distmat_3kmradius$Tram1kmRadius=as.numeric(distmat_3kmradius$Tram1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
}else{
  
  rawData$ClosestTram=50000
  rawData$Tram250mmRadius=0
  rawData$Tram1kmRadius=0
}

summary(rawData)


#Get Subway Entrance_____________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("railway", "subway_entrance")

str(q) #query structure

cinema <- osmdata_sf(q)

#cinema$osm_points$osm_id

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

#Not every city has tramstations, so we make a if question for this

if(length(cinmat$name)>0){
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSubway"
  distmat_closest$ClosestSubway=as.numeric(distmat_closest$ClosestSubway)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Subway250mmRadius"
  distmat_1kmradius$Subway250mmRadius=as.numeric(distmat_1kmradius$Subway250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Subway1kmRadius"
  distmat_3kmradius$Subway1kmRadius=as.numeric(distmat_3kmradius$Subway1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
}else{
  
  rawData$ClosestSubway=50000
  rawData$Subway250mmRadius=0
  rawData$Subway1kmRadius=0
}

summary(rawData)

#Railway Station operated by the DB Netz AG_____________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("railway", "station")%>%
  add_osm_feature("operator", "DB Netz AG")

str(q) #query structure

cinema <- osmdata_sf(q)

#cinema$osm_points$osm_id

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

#Not every city has tramstations, so we make a if question for this

if(length(cinmat$name)>0){
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 1000)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 3000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestTrainS"
  distmat_closest$ClosestTrainS=as.numeric(distmat_closest$ClosestTrainS)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="TrainS1kmRadius"
  distmat_1kmradius$TrainS1kmRadius=as.numeric(distmat_1kmradius$TrainS1kmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="TrainS3kmRadius"
  distmat_3kmradius$TrainS3kmRadius=as.numeric(distmat_3kmradius$TrainS3kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
}else{
  
  rawData$ClosestTrainS=50000
  rawData$TrainS1kmRadius=0
  rawData$TrainS3kmRadius=0
}

summary(rawData)

#Bike Shops_____________________________________________________________

q <- getbb(toString(rawData$Town[1])) %>%
  opq() %>%
  add_osm_feature("shop", "bicycle")

str(q) #query structure

cinema <- osmdata_sf(q)

#cinema$osm_points$osm_id

#create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
}

#cinmat=na.omit(cinmat)
cinmat=as.data.frame(cinmat)
names(cinmat)[1]="name"
names(cinmat)[2]="lon"
names(cinmat)[3]="lat"
cinmat$lon=as.numeric(cinmat$lon)
cinmat$lat=as.numeric(cinmat$lat)

#Not every city has tramstations, so we make a if question for this

if(length(cinmat$name)>0){
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 1000)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 3000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestBikeShop"
  distmat_closest$ClosestBikeShop=as.numeric(distmat_closest$ClosestBikeShop)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="BikeShop1kmRadius"
  distmat_1kmradius$BikeShop1kmRadius=as.numeric(distmat_1kmradius$BikeShop1kmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="BikeShop3kmRadius"
  distmat_3kmradius$BikeShop3kmRadius=as.numeric(distmat_3kmradius$BikeShop3kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  #rm(list=setdiff(ls(), "rawData"))
  
}else{
  
  rawData$ClosestTrainS=50000
  rawData$TrainS1kmRadius=0
  rawData$TrainS3kmRadius=0
}

summary(rawData)

rm(list=setdiff(ls(), "rawData"))



#RoadNetwork

city=rawData$Town[1]

q1 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "cycleway")
q2 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "residential")
q3 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "living_street")
q4 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "path")
q5 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "secondary")
q6 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", "primary")
q7 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("bridge", "yes")

#str(q1) #query structure

cycleways <- osmdata_sf(q1)
residential <- osmdata_sf(q2)
living_street <- osmdata_sf(q3)
path <- osmdata_sf(q4)
secondary <- osmdata_sf(q5)
primary <- osmdata_sf(q6)
bridge <- osmdata_sf(q7)

dist_mat=as.data.frame(levels(as.factor(rawData$Station)))
dist_mat$cycleways = 9999
dist_mat$residential = 9999
dist_mat$living_street = 9999
dist_mat$path = 9999
dist_mat$secondary = 9999
dist_mat$primary = 9999

bool_mat=as.data.frame(levels(as.factor(rawData$Station)))
bool_mat$cycleways = 0
bool_mat$residential = 0
bool_mat$living_street = 0
bool_mat$path = 0
bool_mat$secondary = 0
bool_mat$primary = 0

bridge_mat=levels(as.factor(rawData$Station))
bridge_mat=as.data.frame(bridge_mat)
bridge_mat$ClosestBridge = 9999
bridge_mat$isBridge = 0

i=1

for(i in 1:nlevels(as.factor(rawData$Station))){
  
  d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
  
  DT = as.data.frame(cbind(d$Lon[i],d$Lat[i]))
  names(DT)[1]="long1"
  names(DT)[2]="lat1"
  DT2 = st_as_sf(DT, coords = c("long1","lat1"))
  DT2 <- st_set_crs(DT2, 4269)
  st_crs(DT2) <- 4269 
  DT3cycleways = st_transform(cycleways$osm_lines$geometry,4269)
  DT3residential = st_transform(residential$osm_lines$geometry,4269)
  DT3living_street = st_transform(living_street$osm_lines$geometry,4269)
  DT3path = st_transform(path$osm_lines$geometry,4269)
  DT3secondary = st_transform(secondary$osm_lines$geometry,4269)
  DT3primary = st_transform(primary$osm_lines$geometry,4269)
  DT3bridge = st_transform(bridge$osm_lines$geometry,4269)
  
  dist_mat$cycleways[i]=min(st_distance(DT2$geometry, DT3cycleways))
  dist_mat$residential[i]=min(st_distance(DT2$geometry, DT3residential))
  dist_mat$living_street[i]=min(st_distance(DT2$geometry, DT3living_street))
  dist_mat$path[i]=min(st_distance(DT2$geometry, DT3path))
  dist_mat$secondary[i]=min(st_distance(DT2$geometry, DT3secondary))
  dist_mat$primary[i]=min(st_distance(DT2$geometry, DT3primary))
  
  bridge_mat$ClosestBridge[i]=min(st_distance(DT2$geometry, DT3primary))
  if(bridge_mat$ClosestBridge[i]<5){bridge_mat$isBridge[i]=1}
  
  if(dist_mat$cycleways[i]<5){bool_mat$cycleways[i]=1}
  if(dist_mat$residential[i]<5){bool_mat$residential[i]=1}
  if(dist_mat$living_street[i]<5){bool_mat$living_street[i]=1}
  if(dist_mat$path[i]<5){bool_mat$path[i]=1}
  if(dist_mat$secondary[i]<5){bool_mat$secondary[i]=1}
  if(dist_mat$primary[i]<5){bool_mat$primary[i]=1}
}

dist_mat
bool_mat
bridge_mat

names(bool_mat)[1]="Station"
names(bridge_mat)[1]="Station"

rawData = merge(x = rawData,y = bool_mat,
                by = c("Station"),
                all = FALSE)


rawData = merge(x = rawData,y = bridge_mat,
                by = c("Station"),
                all = FALSE)

summary(rawData)

citation ("osmdata")
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
write.csv(rawData,paste(toString(rawData$Town[1]),".csv",sep=""))



