#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Heidelberg

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
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Tübingen
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
  
  rawData<-rawData[(rawData$standort=="Stadt Heidelberg"),]
  rawData$standort = gsub("Stadt", "", rawData$standort)
  names(rawData)
  rawData$iso_timestamp = NULL
  rawData$stand = NULL
  rawData$channel_id = NULL
  rawData$counter_site = NULL
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
  
  levels(as.factor(rawData$Lon))
  levels(as.factor(rawData$Station))
  
  #Change count frequency to hourly data----------------------------------------------
  
  rawData$Timestamp = gsub("T", " ", rawData$Timestamp)
  rawData$Timestamp = gsub("+0000", "", rawData$Timestamp)
  rawData$Timestamp = substring(rawData$Timestamp,1, nchar(rawData$Timestamp)-1)
  rawData$Timestamp=cut(strptime(rawData$Timestamp,"%Y-%m-%d %H:%M"),"hour")
  
  raw = rawData
  
  ddply(rawData,.(Timestamp,Lon,Lat),summarize,Value=sum(Value))
  