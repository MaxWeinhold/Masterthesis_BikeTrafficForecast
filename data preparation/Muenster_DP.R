#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Muenster

library(plyr)

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
  
  
  #Connect all years per Station----------------------------------------------
  
  countingData_GartenStr=rbind(countingData_GartenStr_18,countingData_GartenStr_19)
  countingData_GartenStr=rbind(countingData_GartenStr,countingData_GartenStr_20)
  countingData_GartenStr=rbind(countingData_GartenStr,countingData_GartenStr_21)
  
  countingData_HafenStr=rbind(countingData_HafenStr_20,countingData_HafenStr_21)
  
  countingData_HammerStr=rbind(countingData_HammerStr_18,countingData_HammerStr_19)
  countingData_HammerStr=rbind(countingData_HammerStr,countingData_HammerStr_20)
  countingData_HammerStr=rbind(countingData_HammerStr,countingData_HammerStr_21)
  
  countingData_HuefferStr=rbind(countingData_HuefferStr_18,countingData_HuefferStr_19)
  countingData_HuefferStr=rbind(countingData_HuefferStr,countingData_HuefferStr_20)
  countingData_HuefferStr=rbind(countingData_HuefferStr,countingData_HuefferStr_21)
  
  countingData_Kanalpromenade=rbind(countingData_Kanalpromenade_20,countingData_Kanalpromenade_21)
  
  countingData_Neutor=rbind(countingData_Neutor_18,countingData_Neutor_19)
  countingData_Neutor=rbind(countingData_Neutor,countingData_Neutor_20)
  
  countingData_Promenade=rbind(countingData_Promenade_18,countingData_Promenade_19)
  countingData_Promenade=rbind(countingData_Promenade,countingData_Promenade_20)
  countingData_Promenade=rbind(countingData_Promenade,countingData_Promenade_21)
  
  countingData_WarendorferStr=countingData_WarendorferStr_18
  countingData_WarendorferStr=rbind(countingData_WarendorferStr,countingData_WarendorferStr_20)
  
  countingData_WeselerStr=rbind(countingData_WeselerStr_18,countingData_WeselerStr_19)
  countingData_WeselerStr=rbind(countingData_WeselerStr,countingData_WeselerStr_20)
  
  countingData_WolbeckerStr=rbind(countingData_WolbeckerStr_18,countingData_WolbeckerStr_19)
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
  
  #TimeStamp Configurations
  
  rawData$Timestamp = gsub(" Jan. ", "01.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Feb. ", "02.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mrz. ", "03.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Apr. ", "04.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mai. ", "05.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jun. ", "06.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jul. ", "07.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Aug. ", "08.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Sep. ", "09.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Okt. ", "10.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Nov. ", "11.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Dez. ", "12.", rawData$Timestamp)
  
  rawData$Timestamp=as.POSIXlt(rawData$Timestamp,format="%d.%m.%Y %H:%M")
  
  rawData$Year	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%Y"))
  rawData$Months=month(as.POSIXlt(rawData$Timestamp))
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
  