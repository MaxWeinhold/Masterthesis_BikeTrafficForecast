#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Mannheim

library(plyr)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Mannheim
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Mannheim")

#Read Bycicle Counting Data----------------------------------------------
  countingData_Jungbusch = read.csv(file = "jungbuschbrucke-eco-counter-daten-kopieren.csv",sep=";")
  countingData_AdenauerBr = read.csv(file = "konrad_adenau_bruecke_sued-gesamt-eco-counter-daten.csv",sep=";")
  countingData_KurpfalzBr = read.csv(file = "kurpfalzbrucke-gesamt-eco-counter-daten.csv",sep=";")
  countingData_SchumacherBr = read.csv(file = "kurt-schumacher-brucke-sud-hafenstr-eco-counter-daten.csv",sep=";")
  countingData_Lindenhof = read.csv(file = "lindenhofuberfuhrung-eco-counter-verkehrszahler.csv",sep=";")
  countingData_NeckarauerUebergang = read.csv(file = "neckarauer-ubergang-eco-counter-verkehrszahler.csv",sep=";")
  countingData_Renzstrasse = read.csv(file = "renzstr-eco-counter-daten.csv",sep=";")
  countingData_Schlosspark = read.csv(file = "schlosspark-lindenhof-eco-counter-verkehrszahler.csv",sep=";")
  
  names(countingData_Jungbusch)
  
#Rename Columns----------------------------------------------
  names(countingData_Jungbusch)[1]="Timestamp"
  names(countingData_Jungbusch)[2]="Value"#
  
  names(countingData_AdenauerBr)[1]="Timestamp"
  names(countingData_AdenauerBr)[2]="Value"
  
  names(countingData_KurpfalzBr)[1]="Timestamp"
  names(countingData_KurpfalzBr)[2]="Value"
  
  names(countingData_Lindenhof)[1]="Timestamp"
  names(countingData_Lindenhof)[2]="Value"
  
  names(countingData_NeckarauerUebergang)[1]="Timestamp"
  names(countingData_NeckarauerUebergang)[2]="Value"
  
  names(countingData_Renzstrasse)[1]="Timestamp"
  names(countingData_Renzstrasse)[2]="Value"
  
  names(countingData_Schlosspark)[1]="Timestamp"
  names(countingData_Schlosspark)[2]="Value"
  
  names(countingData_SchumacherBr)[1]="Timestamp"
  names(countingData_SchumacherBr)[2]="Value"
  
#Delete Columns we don't need----------------------------------------------
  countingData_Jungbusch$ZÃ.hlernummer <- NULL
  countingData_Jungbusch$Standort <- NULL
  countingData_Jungbusch$Koordinaten <- NULL
  countingData_Jungbusch$Foto <- NULL
  
  countingData_AdenauerBr$ZÃ.hlernummer <- NULL
  countingData_AdenauerBr$Standort <- NULL
  countingData_AdenauerBr$Koordinaten <- NULL
  countingData_AdenauerBr$Foto <- NULL
  
  countingData_KurpfalzBr$ZÃ.hlernummer <- NULL
  countingData_KurpfalzBr$Standort <- NULL
  countingData_KurpfalzBr$Koordinaten <- NULL
  countingData_KurpfalzBr$Foto <- NULL
  
  countingData_Lindenhof$ZÃ.hlernummer <- NULL
  countingData_Lindenhof$Standort <- NULL
  countingData_Lindenhof$Koordinaten <- NULL
  countingData_Lindenhof$Foto <- NULL
  
  countingData_NeckarauerUebergang$ZÃ.hlernummer <- NULL
  countingData_NeckarauerUebergang$Standort <- NULL
  countingData_NeckarauerUebergang$Koordinaten <- NULL
  countingData_NeckarauerUebergang$Foto <- NULL
  
  countingData_Renzstrasse$ZÃ.hlernummer <- NULL
  countingData_Renzstrasse$Standort <- NULL
  countingData_Renzstrasse$Koordinaten <- NULL
  countingData_Renzstrasse$Foto <- NULL
  
  countingData_Schlosspark$ZÃ.hlernummer <- NULL
  countingData_Schlosspark$Standort <- NULL
  countingData_Schlosspark$Koordinaten <- NULL
  countingData_Schlosspark$Foto <- NULL
  
  countingData_SchumacherBr$ZÃ.hlernummer <- NULL
  countingData_SchumacherBr$Standort <- NULL
  countingData_SchumacherBr$Koordinaten <- NULL
  countingData_SchumacherBr$Foto <- NULL
  
#Add Location Columns----------------------------------------------

  countingData_Jungbusch$Town = "Mannheim"
  countingData_Jungbusch$Station = "Jungbusch"
  countingData_Jungbusch$Lon = 8.45931945094
  countingData_Jungbusch$Lat = 49.497220385
  countingData_Jungbusch$Oneway = FALSE  
  #countingData_Jungbusch$Road_type = "Bridge"  
  
  countingData_AdenauerBr$Town = "Mannheim"
  countingData_AdenauerBr$Station = "AdenauerBr"
  countingData_AdenauerBr$Lon = 8.45943311526
  countingData_AdenauerBr$Lat = 49.4828619472
  countingData_AdenauerBr$Oneway = TRUE 
  #countingData_AdenauerBr$Road_type = "Bridge"  
  
  countingData_KurpfalzBr$Town = "Mannheim"
  countingData_KurpfalzBr$Station = "KurpfalzBr"
  countingData_KurpfalzBr$Lon = 8.47202644207
  countingData_KurpfalzBr$Lat = 49.4940003429
  countingData_KurpfalzBr$Oneway = FALSE  
  #countingData_KurpfalzBr$Road_type = "Bridge"  
  
  countingData_Lindenhof$Town = "Mannheim"
  countingData_Lindenhof$Station = "Lindenhof"
  countingData_Lindenhof$Lon = 8.46539838729
  countingData_Lindenhof$Lat = 49.4799213023
  countingData_Lindenhof$Oneway = FALSE  
  #countingData_Lindenhof$Road_type = "Pathway"  
  
  countingData_NeckarauerUebergang$Town = "Mannheim"
  countingData_NeckarauerUebergang$Station = "NeckarauerUebergang"
  countingData_NeckarauerUebergang$Lon = 8.4829557336
  countingData_NeckarauerUebergang$Lat = 49.4733000861
  countingData_NeckarauerUebergang$Oneway = FALSE  
  #countingData_NeckarauerUebergang$Road_type = "Bridge"  
  
  countingData_Renzstrasse$Town = "Mannheim"
  countingData_Renzstrasse$Station = "Renzstrasse"
  countingData_Renzstrasse$Lon = 8.481114
  countingData_Renzstrasse$Lat = 49.49027
  countingData_Renzstrasse$Oneway = FALSE  
  #countingData_Renzstrasse$Road_type = "Street"  
  
  countingData_Schlosspark$Town = "Mannheim"
  countingData_Schlosspark$Station = "Schlosspark"
  countingData_Schlosspark$Lon = 8.46035466079
  countingData_Schlosspark$Lat = 49.4816324402
  countingData_Schlosspark$Oneway = FALSE  
  #countingData_Schlosspark$Road_type = "Pathway"  
  
  countingData_SchumacherBr$Town = "Mannheim"
  countingData_SchumacherBr$Station = "SchumacherBr"
  countingData_SchumacherBr$Lon = 8.45659112993
  countingData_SchumacherBr$Lat = 49.4904831811
  countingData_SchumacherBr$Oneway = TRUE
  #countingData_SchumacherBr$Road_type = "Bridge"  
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(countingData_Jungbusch,countingData_AdenauerBr)
  rawData=rbind(rawData,countingData_KurpfalzBr)
  rawData=rbind(rawData,countingData_Lindenhof)
  rawData=rbind(rawData,countingData_NeckarauerUebergang)
  rawData=rbind(rawData,countingData_Renzstrasse)
  rawData=rbind(rawData,countingData_Schlosspark)
  rawData=rbind(rawData,countingData_SchumacherBr)
  
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
  
  