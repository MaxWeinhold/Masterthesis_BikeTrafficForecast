#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Bremen

library(lubridate)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Bochum
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Bremen")

#Read Bycicle Counting Data----------------------------------------------
  countingData = read.csv(file = "2022-10-27-10-49-35_Radzaehler_VMZ-Bremen_Values.csv",sep=";")
  names(countingData)[1]="Timestamp"
  names(countingData)
  
#Divide raw data per stations----------------------------------------------
  
  MoltkeStr_Ost=cbind(countingData$Timestamp,countingData[2])
  MoltkeStr_West=cbind(countingData$Timestamp,countingData[3])
  HastedterBr=cbind(countingData$Timestamp,countingData[4])
  LangemarckStr_Ost=cbind(countingData$Timestamp,countingData[5])
  LangemarckStr_West=cbind(countingData$Timestamp,countingData[6])
  Osterdeich=cbind(countingData$Timestamp,countingData[7])
  Radweg_Kleine_Weser=cbind(countingData$Timestamp,countingData[8])
  Schwachhauser_Ring=cbind(countingData$Timestamp,countingData[9])
  WachmannStr_Sued=cbind(countingData$Timestamp,countingData[10])
  WachmannStr_Nord=cbind(countingData$Timestamp,countingData[11])
  WilhelmKaiserBr_Ost=cbind(countingData$Timestamp,countingData[12])
  WilhelmKaiserBr_West=cbind(countingData$Timestamp,countingData[13])
  
#Rename Columns----------------------------------------------
  
  names(MoltkeStr_Ost)[1]="Timestamp"
  names(MoltkeStr_West)[1]="Timestamp"
  names(HastedterBr)[1]="Timestamp"
  names(LangemarckStr_Ost)[1]="Timestamp"
  names(LangemarckStr_West)[1]="Timestamp"
  names(Osterdeich)[1]="Timestamp"
  names(Radweg_Kleine_Weser)[1]="Timestamp"
  names(Schwachhauser_Ring)[1]="Timestamp"
  names(WachmannStr_Sued)[1]="Timestamp"
  names(WachmannStr_Nord)[1]="Timestamp"
  names(WilhelmKaiserBr_Ost)[1]="Timestamp"
  names(WilhelmKaiserBr_West)[1]="Timestamp"
  
  names(MoltkeStr_Ost)[2]="Value"
  names(MoltkeStr_West)[2]="Value"
  names(HastedterBr)[2]="Value"
  names(LangemarckStr_Ost)[2]="Value"
  names(LangemarckStr_West)[2]="Value"
  names(Osterdeich)[2]="Value"
  names(Radweg_Kleine_Weser)[2]="Value"
  names(Schwachhauser_Ring)[2]="Value"
  names(WachmannStr_Sued)[2]="Value"
  names(WachmannStr_Nord)[2]="Value"
  names(WilhelmKaiserBr_Ost)[2]="Value"
  names(WilhelmKaiserBr_West)[2]="Value"
  
#Add Location Columns----------------------------------------------
  
  MoltkeStr_Ost$Town = "Bremen"
  MoltkeStr_West$Town = "Bremen"
  HastedterBr$Town = "Bremen"
  LangemarckStr_Ost$Town = "Bremen"
  LangemarckStr_West$Town = "Bremen"
  Osterdeich$Town = "Bremen"
  Radweg_Kleine_Weser$Town = "Bremen"
  Schwachhauser_Ring$Town = "Bremen"
  WachmannStr_Sued$Town = "Bremen"
  WachmannStr_Nord$Town = "Bremen"
  WilhelmKaiserBr_Ost$Town = "Bremen"
  WilhelmKaiserBr_West$Town = "Bremen"
  
  MoltkeStr_Ost$Station = "MoltkeStr"
  MoltkeStr_West$Station = "MoltkeStr"
  HastedterBr$Station = "HastedterBr"
  LangemarckStr_Ost$Station = "LangemarckStr"
  LangemarckStr_West$Station = "LangemarckStr"
  Osterdeich$Station = "Osterdeich"
  Radweg_Kleine_Weser$Station = "Radweg_Kleine_Weser"
  Schwachhauser_Ring$Station = "Schwachhauser"
  WachmannStr_Sued$Station = "WachmannStr"
  WachmannStr_Nord$Station = "WachmannStr"
  WilhelmKaiserBr_Ost$Station = "WilhelmKaiserBr"
  WilhelmKaiserBr_West$Station = "WilhelmKaiserBr"
  
  MoltkeStr_Ost$Lon = 8.8330
  MoltkeStr_West$Lon = 8.8330
  HastedterBr$Lon = 8.8528
  LangemarckStr_Ost$Lon = 8.7974
  LangemarckStr_West$Lon = 8.7969
  Osterdeich$Lon = 8.8198
  Radweg_Kleine_Weser$Lon = 8.8073
  Schwachhauser_Ring$Lon = 8.8409
  WachmannStr_Sued$Lon = 8.8263
  WachmannStr_Nord$Lon = 8.8264
  WilhelmKaiserBr_Ost$Lon = 8.8040
  WilhelmKaiserBr_West$Lon = 8.8040
  
  MoltkeStr_Ost$Lat = 53.0778
  MoltkeStr_West$Lat = 53.0778
  HastedterBr$Lat = 53.0612
  LangemarckStr_Ost$Lat = 53.0764
  LangemarckStr_West$Lat = 53.0765
  Osterdeich$Lat = 53.0693
  Radweg_Kleine_Weser$Lat = 53.0660
  Schwachhauser_Ring$Lat = 53.0891
  WachmannStr_Sued$Lat = 53.0845
  WachmannStr_Nord$Lat = 53.0847
  WilhelmKaiserBr_Ost$Lat = 53.0722
  WilhelmKaiserBr_West$Lat = 53.0726
  
  MoltkeStr_Ost$Oneway = FALSE
  MoltkeStr_West$Oneway = FALSE
  HastedterBr$Oneway = FALSE
  LangemarckStr_Ost$Oneway = FALSE
  LangemarckStr_West$Oneway = FALSE
  Osterdeich$Oneway = FALSE
  Radweg_Kleine_Weser$Oneway = FALSE
  Schwachhauser_Ring$Oneway = FALSE
  WachmannStr_Sued$Oneway = FALSE
  WachmannStr_Nord$Oneway = FALSE
  WilhelmKaiserBr_Ost$Oneway = FALSE
  WilhelmKaiserBr_West$Oneway = FALSE
  
#Summarize Directions----------------------------------------------
  
  MoltkeStr=MoltkeStr_Ost
  MoltkeStr$Value = as.numeric(MoltkeStr_Ost$Value) + as.numeric(MoltkeStr_West$Value)
  LangemarckStr=LangemarckStr_Ost
  LangemarckStr$Value = as.numeric(LangemarckStr_Ost$Value) + as.numeric(LangemarckStr_West$Value)
  WachmannStr=WachmannStr_Sued
  WachmannStr$Value = as.numeric(WachmannStr_Sued$Value) + as.numeric(WachmannStr_Nord$Value)
  WilhelmKaiserBr=WilhelmKaiserBr_Ost
  WilhelmKaiserBr$Value = as.numeric(WilhelmKaiserBr$Value) + as.numeric(WilhelmKaiserBr_West$Value)
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(MoltkeStr,HastedterBr)
  rawData=rbind(rawData,LangemarckStr)
  rawData=rbind(rawData,Osterdeich)
  rawData=rbind(rawData,Radweg_Kleine_Weser)
  rawData=rbind(rawData,Schwachhauser_Ring)
  rawData=rbind(rawData,WachmannStr)
  rawData=rbind(rawData,WilhelmKaiserBr)
  
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
  
  pH=publicHolidays[publicHolidays$BRE %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "BRE",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
  #Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(publicHolidays)
  rm(schoolHolidays)
  rm(sH)
  rm(pH)
  rm(x)
  rm(i)
  rm(countingData)
  rm(HastedterBr)
  rm(LangemarckStr)
  rm(LangemarckStr_Ost)
  rm(LangemarckStr_West)
  rm(MoltkeStr)
  rm(MoltkeStr_Ost)
  rm(MoltkeStr_West)
  rm(Osterdeich)
  rm(Radweg_Kleine_Weser)
  rm(Schwachhauser_Ring)
  rm(WachmannStr)
  rm(WachmannStr_Nord)
  rm(WachmannStr_Sued)
  rm(WilhelmKaiserBr)
  rm(WilhelmKaiserBr_Ost)
  rm(WilhelmKaiserBr_West)
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Bremen")
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
  write.csv(rawData,"Bremen.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.5,3.6,3.6)
  
  ADFC=as.data.frame(cbind(Year,ADFC_Index))
  
  rawData = merge(x = rawData,y = ADFC,
                   by = c("Year"),
                   all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))