#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Muenchen

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
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\München
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/München")

#Read Bycicle Counting Data----------------------------------------------
  countingData_1 = read.csv(file = "rad_2022_01_15min.csv",sep=",")
  countingData_2 = read.csv(file = "rad_2022_02_15min.csv",sep=",")
  countingData_3 = read.csv(file = "rad_2022_03_15min.csv",sep=",")
  countingData_4 = read.csv(file = "rad_2022_04_15min.csv",sep=",")
  countingData_5 = read.csv(file = "rad_2022_05_15min.csv",sep=",")
  countingData_6 = read.csv(file = "rad_2022_06_15min.csv",sep=",")
  countingData_7 = read.csv(file = "rad_2022_07_15min.csv",sep=",")
  countingData_8 = read.csv(file = "rad_2022_08_15min.csv",sep=",")
  countingData_9 = read.csv(file = "rad_2022_09_15min.csv",sep=",")
  countingData_10 = read.csv(file = "rad20210115min.csv",sep=",")
  countingData_12 = read.csv(file = "rad20210215min.csv",sep=",")
  countingData_13 = read.csv(file = "rad20210315min.csv",sep=",")
  countingData_14 = read.csv(file = "rad20210415min.csv",sep=",")
  countingData_15 = read.csv(file = "rad20210515min.csv",sep=",")
  countingData_16 = read.csv(file = "rad_2021_06_15min.csv",sep=",")
  countingData_17 = read.csv(file = "rad_2021_07_15min.csv",sep=",")
  countingData_18 = read.csv(file = "rad_2021_08_15min.csv",sep=",")
  countingData_19 = read.csv(file = "rad_2021_09_15min.csv",sep=",")
  countingData_20 = read.csv(file = "rad_2021_10_15min.csv",sep=",")
  countingData_21 = read.csv(file = "rad_2021_11_15min.csv",sep=",")
  countingData_22 = read.csv(file = "rad_2021_12_15min.csv",sep=",")
  countingData_23 = read.csv(file = "rad20200115min.csv",sep=",")
  countingData_24 = read.csv(file = "rad20200215min.csv",sep=",")
  countingData_25 = read.csv(file = "rad20200315min.csv",sep=",")
  countingData_26 = read.csv(file = "rad20200415min.csv",sep=",")
  countingData_27 = read.csv(file = "rad20200515min.csv",sep=",")
  countingData_28 = read.csv(file = "rad20200615min.csv",sep=",")
  countingData_29 = read.csv(file = "rad20200715min.csv",sep=",")
  countingData_30 = read.csv(file = "rad20200815min.csv",sep=",")
  countingData_31 = read.csv(file = "rad20200915min.csv",sep=",")
  countingData_32 = read.csv(file = "rad20201015min.csv",sep=",")
  countingData_33 = read.csv(file = "rad20201115min.csv",sep=",")
  countingData_34 = read.csv(file = "rad20201215minbearbeitet.csv",sep=",")
  countingData_35 = read.csv(file = "rad20190115min.csv",sep=",")
  countingData_36 = read.csv(file = "rad20190215min.csv",sep=",")
  countingData_37 = read.csv(file = "rad20190315min.csv",sep=",")
  countingData_38 = read.csv(file = "rad20190415min.csv",sep=",")
  countingData_39 = read.csv(file = "rad20190515min.csv",sep=",")
  countingData_40 = read.csv(file = "rad20190615min.csv",sep=",")
  countingData_41 = read.csv(file = "rad20190715min.csv",sep=",")
  countingData_42 = read.csv(file = "rad20190815min.csv",sep=",")
  countingData_43 = read.csv(file = "rad20190915min.csv",sep=",")
  countingData_44 = read.csv(file = "rad20191015min.csv",sep=",")
  countingData_45 = read.csv(file = "rad20191115min.csv",sep=",")
  countingData_46 = read.csv(file = "rad20191215min.csv",sep=",")
  countingData_47 = read.csv(file = "rad20180115min.csv",sep=",")
  countingData_48 = read.csv(file = "rad20180215min.csv",sep=",")
  countingData_49 = read.csv(file = "rad_2018_03_15min.csv",sep=",")
  countingData_50 = read.csv(file = "rad20180415min.csv",sep=",")
  countingData_51 = read.csv(file = "rad20180515min.csv",sep=",")
  countingData_52 = read.csv(file = "rad20180615min.csv",sep=",")
  countingData_53 = read.csv(file = "rad20180715min.csv",sep=",")
  countingData_54 = read.csv(file = "rad20180815min.csv",sep=",")
  countingData_55 = read.csv(file = "rad20180915min.csv",sep=",")
  countingData_56 = read.csv(file = "rad20181015min.csv",sep=",")
  countingData_57 = read.csv(file = "rad20181115min.csv",sep=",")
  countingData_58 = read.csv(file = "rad20181215min.csv",sep=",")
  countingData_59 = read.csv(file = "rad20170115min.csv",sep=",")
  countingData_60 = read.csv(file = "rad20170315min.csv",sep=",")
  countingData_61 = read.csv(file = "rad20170415min.csv",sep=",")
  countingData_62 = read.csv(file = "rad20170515min.csv",sep=",")
  countingData_63 = read.csv(file = "rad20170615min.csv",sep=",")
  countingData_64 = read.csv(file = "rad20170715min.csv",sep=",")
  countingData_65 = read.csv(file = "rad20170815min.csv",sep=",")
  countingData_66 = read.csv(file = "rad20170915min.csv",sep=",")
  countingData_67 = read.csv(file = "rad20171015min.csv",sep=",")
  countingData_68 = read.csv(file = "rad20171115min.csv",sep=",")
  countingData_69 = read.csv(file = "rad20171215min.csv",sep=",")

#Connect all years----------------------------------------------
  
  countingData=rbind(countingData_1,countingData_2)
  countingData=rbind(countingData,countingData_3)
  countingData=rbind(countingData,countingData_4)
  countingData=rbind(countingData,countingData_5)
  countingData=rbind(countingData,countingData_6)
  countingData=rbind(countingData,countingData_7)
  countingData=rbind(countingData,countingData_8)
  countingData=rbind(countingData,countingData_9)
  countingData=rbind(countingData,countingData_10)
  countingData=rbind(countingData,countingData_12)
  countingData=rbind(countingData,countingData_13)
  countingData=rbind(countingData,countingData_14)
  countingData=rbind(countingData,countingData_15)
  countingData=rbind(countingData,countingData_16)
  countingData=rbind(countingData,countingData_17)
  countingData=rbind(countingData,countingData_18)
  countingData=rbind(countingData,countingData_19)
  countingData=rbind(countingData,countingData_20)
  countingData=rbind(countingData,countingData_21)
  countingData=rbind(countingData,countingData_22)
  countingData=rbind(countingData,countingData_23)
  countingData=rbind(countingData,countingData_24)
  countingData=rbind(countingData,countingData_25)
  countingData=rbind(countingData,countingData_26)
  countingData=rbind(countingData,countingData_27)
  countingData=rbind(countingData,countingData_28)
  countingData=rbind(countingData,countingData_29)
  countingData=rbind(countingData,countingData_30)
  countingData=rbind(countingData,countingData_31)
  countingData=rbind(countingData,countingData_32)
  countingData=rbind(countingData,countingData_33)
  countingData=rbind(countingData,countingData_34)
  countingData=rbind(countingData,countingData_35)
  countingData=rbind(countingData,countingData_36)
  countingData=rbind(countingData,countingData_37)
  countingData=rbind(countingData,countingData_38)
  countingData=rbind(countingData,countingData_39)
  countingData=rbind(countingData,countingData_40)
  countingData=rbind(countingData,countingData_41)
  countingData=rbind(countingData,countingData_42)
  countingData=rbind(countingData,countingData_43)
  countingData=rbind(countingData,countingData_44)
  countingData=rbind(countingData,countingData_45)
  countingData=rbind(countingData,countingData_46)
  countingData=rbind(countingData,countingData_47)
  countingData=rbind(countingData,countingData_48)
  countingData=rbind(countingData,countingData_49)
  countingData=rbind(countingData,countingData_50)
  countingData=rbind(countingData,countingData_51)
  countingData=rbind(countingData,countingData_52)
  countingData=rbind(countingData,countingData_53)
  countingData=rbind(countingData,countingData_54)
  countingData=rbind(countingData,countingData_55)
  countingData=rbind(countingData,countingData_56)
  countingData=rbind(countingData,countingData_57)
  countingData=rbind(countingData,countingData_58)
  countingData=rbind(countingData,countingData_59)
  countingData=rbind(countingData,countingData_60)
  countingData=rbind(countingData,countingData_61)
  countingData=rbind(countingData,countingData_62)
  countingData=rbind(countingData,countingData_63)
  countingData=rbind(countingData,countingData_64)
  countingData=rbind(countingData,countingData_65)
  countingData=rbind(countingData,countingData_66)
  countingData=rbind(countingData,countingData_67)
  countingData=rbind(countingData,countingData_68)
  countingData=rbind(countingData,countingData_69)

#Delete Columns we don't need----------------------------------------------
  
  names(countingData)
  countingData$uhrzeit_ende <- NULL
  countingData$richtung_1 <- NULL
  countingData$richtung_2 <- NULL
  
#Change count frequency to hourly data----------------------------------------------
  
  countingData$Uhrzeit=paste(countingData$datum,countingData$uhrzeit_start, sep=" ")
  countingData$Uhrzeit=cut(strptime(countingData$Uhrzeit,"%Y.%m.%d %H:%M"),"hour")
  countingData=ddply(countingData,.(Uhrzeit,zaehlstelle),summarize,Value=sum(gesamt))
  
#Divide Data Set for Stations
  
  nlevels(as.factor(countingData$zaehlstelle))
  levels(as.factor(countingData$zaehlstelle))
  
  Arnulf <- countingData[ which(countingData$zaehlstelle=='Arnulf'),]
  Erhardt <- countingData[ which(countingData$zaehlstelle=='Erhardt'),]
  Hirsch <- countingData[ which(countingData$zaehlstelle=='Hirsch'),]
  Kreuther <- countingData[ which(countingData$zaehlstelle=='Kreuther'),]
  Margareten <- countingData[ which(countingData$zaehlstelle=='Margareten'),]
  Olympia <- countingData[ which(countingData$zaehlstelle=='Olympia'),]
  
#Delete Columns we don't need----------------------------------------------
  Arnulf$zaehlstelle <- NULL
  Erhardt$zaehlstelle <- NULL
  Hirsch$zaehlstelle <- NULL
  Kreuther$zaehlstelle <- NULL
  Margareten$zaehlstelle <- NULL
  Olympia$zaehlstelle <- NULL
  
#Rename Columns----------------------------------------------
  names(Arnulf)[1]="Timestamp"
  names(Erhardt)[1]="Timestamp"
  names(Hirsch)[1]="Timestamp"
  names(Kreuther)[1]="Timestamp"
  names(Margareten)[1]="Timestamp"
  names(Olympia)[1]="Timestamp"
  
#Add Location Columns----------------------------------------------
  
  Arnulf$Town = "Muenchen"
  Arnulf$Station = "Arnulf"
  Arnulf$Lon = 11.55534
  Arnulf$Lat = 48.14205	
  Arnulf$Oneway = FALSE
  #Arnulf$Road_type = "Street"  
  
  Erhardt$Town = "Muenchen"
  Erhardt$Station = "Erhardt"
  Erhardt$Lon = 11.58469
  Erhardt$Lat = 48.13192	
  Erhardt$Oneway = FALSE
  #Erhardt$Road_type = "Street"  
  
  Hirsch$Town = "Muenchen"
  Hirsch$Station = "Hirsch"
  Hirsch$Lon = 11.51794
  Hirsch$Lat = 48.14438	
  Hirsch$Oneway = FALSE
  #Hirsch$Road_type = "Pathway"  
  
  Kreuther$Town = "Muenchen"
  Kreuther$Station = "Kreuther"
  Kreuther$Lon = 11.62417
  Kreuther$Lat = 48.12194	
  Kreuther$Oneway = FALSE
  #Kreuther$Road_type = "Street"  
  
  Margareten$Town = "Muenchen"
  Margareten$Station = "Margareten"
  Margareten$Lon = 11.53599
  Margareten$Lat = 48.12032	
  Margareten$Oneway = FALSE
  #Margareten$Road_type = "Pathway"  
  
  Olympia$Town = "Muenchen"
  Olympia$Station = "Olympia"
  Olympia$Lon = 11.55005
  Olympia$Lat = 48.16887	
  Olympia$Oneway = FALSE
  #Olympia$Road_type = "Pathway"
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(Arnulf,Erhardt)
  rawData=rbind(rawData,Hirsch)
  rawData=rbind(rawData,Kreuther)
  rawData=rbind(rawData,Margareten)
  rawData=rbind(rawData,Olympia)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
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
  
  pH=publicHolidays[publicHolidays$BAY %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "BAY",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/München")
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
  write.csv(rawData,"Munchen.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(3.7,3.7,3.7,3.7,3.8,3.8,4,4,3.8,3.8)
  
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
  
  title=", Landeshauptstadt" #This differs, there are cities and also hanseatic cities
  
  test12=as.data.frame(Destatis12[Destatis12$X.6 == paste("München",title,sep=""),])
  test12[17] <- NULL
  test12[17] <- NULL
  test12 <- test12 %>% mutate_all(na_if,"")
  names(test12)[1]="number"
  test12=na.omit(test12)
  test12$Year=2012
  
  test13=as.data.frame(Destatis13[Destatis13$X.6 == paste("München",title,sep=""),])
  test13[17] <- NULL
  test13[17] <- NULL
  test13 <- test13 %>% mutate_all(na_if,"")
  names(test13)[1]="number"
  test13=na.omit(test13)
  test13$Year=2013
  
  test14=as.data.frame(Destatis14[Destatis14$X.6 == paste("München",title,sep=""),])
  test14[17] <- NULL
  test14[17] <- NULL
  test14 <- test14 %>% mutate_all(na_if,"")
  names(test14)[1]="number"
  test14=na.omit(test14)
  test14$Year=2014
  
  test15=as.data.frame(Destatis15[Destatis15$X.6 == paste("München",title,sep=""),])
  test15[17] <- NULL
  test15[17] <- NULL
  test15 <- test15 %>% mutate_all(na_if,"")
  names(test15)[1]="number"
  test15=na.omit(test15)
  test15$Year=2015
  
  test16=as.data.frame(Destatis16[Destatis16$X.6 == paste("München",title,sep=""),])
  test16[17] <- NULL
  test16[17] <- NULL
  test16 <- test16 %>% mutate_all(na_if,"")
  names(test16)[1]="number"
  test16=na.omit(test16)
  test16$Year=2016
  
  test17=as.data.frame(Destatis17[Destatis17$X.6 == paste("München",title,sep=""),])
  test17[17] <- NULL
  test17[17] <- NULL
  test17 <- test17 %>% mutate_all(na_if,"")
  names(test17)[1]="number"
  test17=na.omit(test17)
  test17$Year=2017
  
  test18=as.data.frame(Destatis18[Destatis18$X.6 == paste("München",title,sep=""),])
  test18[17] <- NULL
  test18[17] <- NULL
  test18 <- test18 %>% mutate_all(na_if,"")
  names(test18)[1]="number"
  test18=na.omit(test18)
  test18$Year=2018
  
  test19=as.data.frame(Destatis19[Destatis19$X.6 == paste("München",title,sep=""),])
  test19[17] <- NULL
  test19[17] <- NULL
  test19 <- test19 %>% mutate_all(na_if,"")
  names(test19)[1]="number"
  test19=na.omit(test19)
  test19$Year=2019
  
  test20=as.data.frame(Destatis20[Destatis20$X.6 == paste("München",title,sep=""),])
  test20[17] <- NULL
  test20[17] <- NULL
  test20 <- test20 %>% mutate_all(na_if,"")
  names(test20)[1]="number"
  test20=na.omit(test20)
  test20$Year=2020
  
  test21=as.data.frame(Destatis21[Destatis21$X.6 == paste("München",title,sep=""),])
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
  
  