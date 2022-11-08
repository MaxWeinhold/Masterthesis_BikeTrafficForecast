#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Bochum

library(lubridate)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Bochum
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Bochum")

#Read Bycicle Counting Data----------------------------------------------
  countingData_Springorumtrasse = read.csv(file = "Springorumtrasse_2019_KumuliertAufStunden_Gesamt_Richtungen.csv",sep=",")
  countingData_Wittener_Strasse = read.csv(file = "Wittener_Strasse_2019_KumuliertAufStunden_Gesamt_Richtungen.csv",sep=",")

  names(countingData_Springorumtrasse)
  names(countingData_Wittener_Strasse)
  
#Rename Columns----------------------------------------------
  names(countingData_Springorumtrasse)[1]="Timestamp"
  names(countingData_Springorumtrasse)[2]="Value"
  
  names(countingData_Wittener_Strasse)[1]="Timestamp"
  names(countingData_Wittener_Strasse)[2]="Value"
  
#Delete Columns we don't need----------------------------------------------
  countingData_Springorumtrasse$X <- NULL
  countingData_Springorumtrasse$Springorumtrasse.stadteinwärts.Fahrräder <- NULL
  countingData_Springorumtrasse$Springorumtrasse.stadtauswärts.Fahrräder <- NULL
  
  countingData_Wittener_Strasse$X <- NULL
  countingData_Wittener_Strasse$Wittener.Str..Gesamt.Wittener.Str..stadteinwärts <- NULL
  countingData_Wittener_Strasse$Wittener.Str..Gesamt.Wittener.Str..stadtauswärts <- NULL
  
#Add Location Columns----------------------------------------------
  
  countingData_Springorumtrasse$Town = "Bochum"
  countingData_Springorumtrasse$Station = "Springorumtr"
  countingData_Springorumtrasse$Lon = 7.229305
  countingData_Springorumtrasse$Lat = 51.468975
  countingData_Springorumtrasse$Oneway = FALSE
  
  countingData_Wittener_Strasse$Town = "Bochum"
  countingData_Wittener_Strasse$Station = "Wittenerstr"
  countingData_Wittener_Strasse$Lon = 7.225124568907597
  countingData_Wittener_Strasse$Lat = 51.479342160683515
  countingData_Wittener_Strasse$Oneway = FALSE
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(countingData_Springorumtrasse,countingData_Wittener_Strasse)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
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
  