#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Darmstadt

library(lubridate)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Bochum
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Darmstadt")

#Read Bycicle Counting Data----------------------------------------------
  countingData = read.csv(file = "Zaehlstellen_Export_gesamt.csv",sep=";")
  names(countingData)
  
#Divide raw data per stations  
  
  ArheilgerStr = cbind(countingData$Time,countingData[2])
  Lincoln_Siedlung = cbind(countingData$Time,countingData[3])
  Woogsweg = cbind(countingData$Time,countingData[4])
  
#Rename Columns----------------------------------------------
  
  names(ArheilgerStr)[1]="Timestamp"
  names(Lincoln_Siedlung)[1]="Timestamp"
  names(Woogsweg)[1]="Timestamp"
  
  names(ArheilgerStr)[2]="Value"
  names(Lincoln_Siedlung)[2]="Value"
  names(Woogsweg)[2]="Value"
  
#Add Location Columns----------------------------------------------
  
  ArheilgerStr$Town = "Darmstadt"
  Lincoln_Siedlung$Town = "Darmstadt"
  Woogsweg$Town = "Darmstadt"
  
  ArheilgerStr$Station = "ArheilgerStr"
  Lincoln_Siedlung$Station = "Lincoln_Siedlung"
  Woogsweg$Station = "Woogsweg"
  
  ArheilgerStr$Lon = 8.661602121836408
  Lincoln_Siedlung$Lon = 8.64616682805755
  Woogsweg$Lon = 8.672674199853052
  
  ArheilgerStr$Lat = 49.89193924770451
  Lincoln_Siedlung$Lat = 49.84762934355626
  Woogsweg$Lat = 49.90125634416516
  
  ArheilgerStr$Oneway = FALSE
  Lincoln_Siedlung$Oneway = FALSE
  Woogsweg$Oneway = FALSE
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(ArheilgerStr,Lincoln_Siedlung)
  rawData=rbind(rawData,Woogsweg)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
  #TimeStamp Configurations
  rawData$Timestamp
  
  rawData$Timestamp = gsub(" Jan ", "01.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Feb ", "02.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mrz ", "03.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Apr ", "04.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mai ", "05.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jun ", "06.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jul ", "07.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Aug ", "08.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Sep ", "09.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Okt ", "10.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Nov ", "11.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Dez ", "12.", rawData$Timestamp)
  
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
  
  pH=publicHolidays[publicHolidays$HES %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "HES",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  