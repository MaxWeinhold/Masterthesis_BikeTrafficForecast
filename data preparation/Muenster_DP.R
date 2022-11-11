#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Muenster

library(plyr)
library(dplyr)
library(lubridate)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Münster
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Münster")

#Read Bycicle Counting Data----------------------------------------------
  countingData_GartenStr_18 = read.csv(file = "zaehlstelle_gartenstrasse_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_GartenStr_20 = read.csv(file = "Zaehlstelle_Gartenstrasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_GartenStr_19 = read.csv(file = "Zaehlstelle_Gartenstrasse_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_GartenStr_21 = read.csv(file = "Zaehlstelle_Gartenstrasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_HafenStr_20 = read.csv(file = "Zaehlstelle_Hafenstrasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_HafenStr_21 = read.csv(file = "Zaehlstelle_Hafenstrasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_HammerStr_20 = read.csv(file = "Zaehlstelle_Hammer_Strasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_HammerStr_19 = read.csv(file = "Zaehlstelle_Hammer_Strasse_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_HammerStr_21 = read.csv(file = "Zaehlstelle_Hammer_Strasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_HammerStr_18 = read.csv(file = "zaehlstelle_hammer-strasse_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_HuefferStr_18 = read.csv(file = "zaehlstelle_huefferstrasse_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_HuefferStr_20 = read.csv(file = "Zaehlstelle_Huefferstrasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_HuefferStr_19 = read.csv(file = "Zaehlstelle_Huefferstrasse_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_HuefferStr_21 = read.csv(file = "Zaehlstelle_Huefferstrasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_Kanalpromenade_20 = read.csv(file = "Zaehlstelle_Kanalpromenade_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_Kanalpromenade_21 = read.csv(file = "Zaehlstelle_Kanalpromenade_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_Neutor_18 = read.csv(file = "zaehlstelle_neutor_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_Neutor_20 = read.csv(file = "Zaehlstelle_Neutor_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_Neutor_19 = read.csv(file = "Zaehlstelle_Neutor_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  #countingData_Neutor_21 = read.csv(file = "Zaehlstelle_Neutor_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_Promenade_18 = read.csv(file = "zaehlstelle_promenade_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_Promenade_20 = read.csv(file = "Zaehlstelle_Promenade_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_Promenade_19 = read.csv(file = "Zaehlstelle_Promenade_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_Promenade_21 = read.csv(file = "Zaehlstelle_Promenade_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_WarendorferStr_18 = read.csv(file = "zaehlstelle_warendorfer_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_WarendorferStr_20 = read.csv(file = "Zaehlstelle_Warendorfer_Strasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  #ountingData_WarendorferStr_21 = read.csv(file = "Zaehlstelle_Warendorfer_Strasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_WarendorferStr_19 = read.csv(file = "Zaehlstelle_Warendorfer_Strasse_Tagesauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_WeselerStr_18 = read.csv(file = "zaehlstelle_weseler_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_WeselerStr_20 = read.csv(file = "Zaehlstelle_Weseler_Strasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_WeselerStr_19 = read.csv(file = "Zaehlstelle_Weseler_Strasse_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  #countingData_WeselerStr_21 = read.csv(file = "Zaehlstelle_Weseler_Strasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_WolbeckerStr_20 = read.csv(file = "Zaehlstelle_Wolbecker_Strasse_2020_Stundenauswertung.csv",sep=";", skip = 1, header = F)
  countingData_WolbeckerStr_19 = read.csv(file = "Zaehlstelle_Wolbecker_Strasse_Stundenauswertung_2019.csv",sep=";", skip = 1, header = F)
  countingData_WolbeckerStr_21 = read.csv(file = "Zaehlstelle_Wolbecker_Strasse_Stundenauswertung_2021.csv",sep=";", skip = 1, header = F)
  countingData_WolbeckerStr_18 = read.csv(file = "zahelstelle_wolbecker_2018_stundenauswertung.csv",sep=";", skip = 1, header = F)
  
#Convert Timestamps (Timestamps differ in stations)  

  #names(countingData_GartenStr_18)
  
  #countingData_GartenStr_18[1] = gsub(" Jan. ", "01.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Febr. ", "02.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Mrz. ", "03.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Apr. ", "04.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Mai. ", "05.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Jun. ", "06.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Jul. ", "07.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Aug. ", "08.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Sep. ", "09.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Okt. ", "10.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Nov. ", "11.", countingData_GartenStr_18[1])
  #countingData_GartenStr_18[1] = gsub(" Dez. ", "12.", countingData_GartenStr_18[1])
  
  #countingData_GartenStr_18[1]=as.POSIXlt(countingData_GartenStr_18[1],format="%d.%m.%Y %H:%M")
  
#Connect all years per Station----------------------------------------------
  
  countingData_GartenStr=rbind(countingData_GartenStr_19)
  countingData_GartenStr=rbind(countingData_GartenStr,countingData_GartenStr_20)
  countingData_GartenStr=rbind(countingData_GartenStr,countingData_GartenStr_21)
  
  countingData_HafenStr=rbind(countingData_HafenStr_20,countingData_HafenStr_21)
  
  countingData_HammerStr=rbind(countingData_HammerStr_19)
  countingData_HammerStr=rbind(countingData_HammerStr,countingData_HammerStr_20)
  countingData_HammerStr=rbind(countingData_HammerStr,countingData_HammerStr_21)
  
  countingData_HuefferStr=rbind(countingData_HuefferStr_19)
  countingData_HuefferStr=rbind(countingData_HuefferStr,countingData_HuefferStr_20)
  countingData_HuefferStr=rbind(countingData_HuefferStr,countingData_HuefferStr_21)
  
  countingData_Kanalpromenade=rbind(countingData_Kanalpromenade_20,countingData_Kanalpromenade_21)
  
  countingData_Neutor=rbind(countingData_Neutor_19)
  countingData_Neutor=rbind(countingData_Neutor,countingData_Neutor_20)
  
  countingData_Promenade=rbind(countingData_Promenade_19)
  countingData_Promenade=rbind(countingData_Promenade,countingData_Promenade_20)
  countingData_Promenade=rbind(countingData_Promenade,countingData_Promenade_21)
  
  #countingData_WarendorferStr=countingData_WarendorferStr_18
  countingData_WarendorferStr=rbind(countingData_WarendorferStr_20)
  
  countingData_WeselerStr=rbind(countingData_WeselerStr_19)
  countingData_WeselerStr=rbind(countingData_WeselerStr,countingData_WeselerStr_20)
  
  countingData_WolbeckerStr=rbind(countingData_WolbeckerStr_19)
  countingData_WolbeckerStr=rbind(countingData_WolbeckerStr,countingData_WolbeckerStr_20)
  countingData_WolbeckerStr=rbind(countingData_WolbeckerStr,countingData_WolbeckerStr_21)
  
#Rename Columns----------------------------------------------
  
  names(countingData_GartenStr)[1]="Timestamp"
  names(countingData_GartenStr)[2]="Value"
  
  names(countingData_HafenStr)[1]="Timestamp"
  names(countingData_HafenStr)[2]="Value"
  
  names(countingData_HammerStr)[1]="Timestamp"
  names(countingData_HammerStr)[2]="Value"
  
  names(countingData_HuefferStr)[1]="Timestamp"
  names(countingData_HuefferStr)[2]="Value"
  
  names(countingData_Kanalpromenade)[1]="Timestamp"
  names(countingData_Kanalpromenade)[2]="Value"
  
  names(countingData_Neutor)[1]="Timestamp"
  names(countingData_Neutor)[2]="Value"
  
  names(countingData_Promenade)[1]="Timestamp"
  names(countingData_Promenade)[2]="Value"
  
  names(countingData_WarendorferStr)[1]="Timestamp"
  names(countingData_WarendorferStr)[2]="Value"
  
  names(countingData_WeselerStr)[1]="Timestamp"
  names(countingData_WeselerStr)[2]="Value"
  
  names(countingData_WolbeckerStr)[1]="Timestamp"
  names(countingData_WolbeckerStr)[2]="Value"
  
#Delete Columns we don't need----------------------------------------------
  countingData_GartenStr[3:9] <- NULL
  countingData_HafenStr[3:9] <- NULL
  countingData_HammerStr[3:9] <- NULL
  countingData_HuefferStr[3:9] <- NULL
  countingData_Kanalpromenade[3:9] <- NULL
  countingData_Neutor[3:9] <- NULL
  countingData_Promenade[3:9] <- NULL
  countingData_WarendorferStr[3:9] <- NULL
  countingData_WeselerStr[3:9] <- NULL
  countingData_WolbeckerStr[3:9] <- NULL
  
#Add Location Columns----------------------------------------------
  
  countingData_GartenStr$Town = "Muenster"
  countingData_GartenStr$Station = "GartenStr"
  countingData_GartenStr$Lon = 7.635686416805148
  countingData_GartenStr$Lat = 51.97154102512242
  countingData_GartenStr$Oneway = FALSE
  #countingData_GartenStr$Road_type = "Street"
  
  countingData_HafenStr$Town = "Muenster"
  countingData_HafenStr$Station = "HafenStr"
  countingData_HafenStr$Lon = 7.62974
  countingData_HafenStr$Lat = 51.95426
  countingData_HafenStr$Oneway = FALSE
  #countingData_HafenStr$Road_type = "Street"  
  
  countingData_HammerStr$Town = "Muenster"
  countingData_HammerStr$Station = "HammerStr"
  countingData_HammerStr$Lon = 7.62631
  countingData_HammerStr$Lat = 51.95462
  countingData_HammerStr$Oneway = FALSE
  #countingData_HammerStr$Road_type = "Street"  
  
  countingData_HuefferStr$Town = "Muenster"
  countingData_HuefferStr$Station = "HuefferStr"
  countingData_HuefferStr$Lon = 7.61094
  countingData_HuefferStr$Lat = 51.9617
  countingData_HuefferStr$Oneway = FALSE
  #countingData_HuefferStr$Road_type = "Street"  
  
  countingData_Kanalpromenade$Town = "Muenster"
  countingData_Kanalpromenade$Station = "Kanalpromenade"
  countingData_Kanalpromenade$Lon = 7.64931603023393
  countingData_Kanalpromenade$Lat = 51.91728273862377
  countingData_Kanalpromenade$Oneway = FALSE
  #countingData_Kanalpromenade$Road_type = "Pathway"  
  
  countingData_Neutor$Town = "Muenster"
  countingData_Neutor$Station = "Neutor"
  countingData_Neutor$Lon = 7.615573751539727
  countingData_Neutor$Lat = 51.967021135540726
  countingData_Neutor$Oneway = FALSE
  #countingData_Neutor$Road_type = "large_Street"  
  
  countingData_Promenade$Town = "Muenster"
  countingData_Promenade$Station = "Promenade"
  countingData_Promenade$Lon = 7.634015197473532
  countingData_Promenade$Lat = 51.960595599885565
  countingData_Promenade$Oneway = FALSE
  #countingData_Promenade$Road_type = "Pathway"  
  
  countingData_WarendorferStr$Town = "Muenster"
  countingData_WarendorferStr$Station = "WarendorferStr"
  countingData_WarendorferStr$Lon = 7.637658504624373
  countingData_WarendorferStr$Lat = 51.96180915463516
  countingData_WarendorferStr$Oneway = FALSE
  #countingData_WarendorferStr$Road_type = "Street"  
  
  countingData_WeselerStr$Town = "Muenster"
  countingData_WeselerStr$Station = "WeselerStr"
  countingData_WeselerStr$Lon = 7.617581706511294
  countingData_WeselerStr$Lat = 51.95063640185317
  countingData_WeselerStr$Oneway = FALSE
  #countingData_WeselerStr$Road_type = "large_Street"  
  
  countingData_WolbeckerStr$Town = "Muenster"
  countingData_WolbeckerStr$Station = "WolbeckerStr"
  countingData_WolbeckerStr$Lon = 7.636358065438691
  countingData_WolbeckerStr$Lat = 51.959244978875326
  countingData_WolbeckerStr$Oneway = FALSE
  #countingData_WolbeckerStr$Road_type = "Street"  
  
#Connect the Stations----------------------------------------------

  rawData=rbind(countingData_GartenStr,countingData_HafenStr)
  rawData=rbind(rawData,countingData_HammerStr) 
  rawData=rbind(rawData,countingData_HuefferStr) 
  rawData=rbind(rawData,countingData_Kanalpromenade) 
  rawData=rbind(rawData,countingData_Neutor) 
  rawData=rbind(rawData,countingData_Promenade) 
  rawData=rbind(rawData,countingData_WarendorferStr) 
  rawData=rbind(rawData,countingData_WeselerStr)  
  rawData=rbind(rawData,countingData_WolbeckerStr)  
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
  rawData$Timestamp=as.POSIXlt(rawData$Timestamp,format="%d-%m-%Y %H:%M:%S")
  
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
  
  pH=publicHolidays[publicHolidays$NRW %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "NRW",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Münster")
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
                  all = FALSE)
  
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
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(rawData,"Muenster.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(2.6,2.6,2.5,2.5,3.1,3.1,3.3,3.3,3.2,3.2)
  
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
  
  test12=as.data.frame(Destatis12[Destatis12$X.6 == paste("Münster",title,sep=""),])
  test12[17] <- NULL
  test12[17] <- NULL
  test12 <- test12 %>% mutate_all(na_if,"")
  names(test12)[1]="number"
  test12=na.omit(test12)
  test12$Year=2012
  
  test13=as.data.frame(Destatis13[Destatis13$X.6 == paste("Münster",title,sep=""),])
  test13[17] <- NULL
  test13[17] <- NULL
  test13 <- test13 %>% mutate_all(na_if,"")
  names(test13)[1]="number"
  test13=na.omit(test13)
  test13$Year=2013
  
  test14=as.data.frame(Destatis14[Destatis14$X.6 == paste("Münster",title,sep=""),])
  test14[17] <- NULL
  test14[17] <- NULL
  test14 <- test14 %>% mutate_all(na_if,"")
  names(test14)[1]="number"
  test14=na.omit(test14)
  test14$Year=2014
  
  test15=as.data.frame(Destatis15[Destatis15$X.6 == paste("Münster",title,sep=""),])
  test15[17] <- NULL
  test15[17] <- NULL
  test15 <- test15 %>% mutate_all(na_if,"")
  names(test15)[1]="number"
  test15=na.omit(test15)
  test15$Year=2015
  
  test16=as.data.frame(Destatis16[Destatis16$X.6 == paste("Münster",title,sep=""),])
  test16[17] <- NULL
  test16[17] <- NULL
  test16 <- test16 %>% mutate_all(na_if,"")
  names(test16)[1]="number"
  test16=na.omit(test16)
  test16$Year=2016
  
  test17=as.data.frame(Destatis17[Destatis17$X.6 == paste("Münster",title,sep=""),])
  test17[17] <- NULL
  test17[17] <- NULL
  test17 <- test17 %>% mutate_all(na_if,"")
  names(test17)[1]="number"
  test17=na.omit(test17)
  test17$Year=2017
  
  test18=as.data.frame(Destatis18[Destatis18$X.6 == paste("Münster",title,sep=""),])
  test18[17] <- NULL
  test18[17] <- NULL
  test18 <- test18 %>% mutate_all(na_if,"")
  names(test18)[1]="number"
  test18=na.omit(test18)
  test18$Year=2018
  
  test19=as.data.frame(Destatis19[Destatis19$X.6 == paste("Münster",title,sep=""),])
  test19[17] <- NULL
  test19[17] <- NULL
  test19 <- test19 %>% mutate_all(na_if,"")
  names(test19)[1]="number"
  test19=na.omit(test19)
  test19$Year=2019
  
  test20=as.data.frame(Destatis20[Destatis20$X.6 == paste("Münster",title,sep=""),])
  test20[17] <- NULL
  test20[17] <- NULL
  test20 <- test20 %>% mutate_all(na_if,"")
  names(test20)[1]="number"
  test20=na.omit(test20)
  test20$Year=2020
  
  test21=as.data.frame(Destatis21[Destatis21$X.6 == paste("Münster",title,sep=""),])
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
    
    distmat[i,1]=d[1,8]
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
  
  