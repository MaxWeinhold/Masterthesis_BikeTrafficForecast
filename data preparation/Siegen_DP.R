#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Siegen

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Siegen
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Siegen")

#Read Bycicle Counting Data----------------------------------------------
  countingData = read.csv(file = "Siegen_Radzaehler.csv",sep=";")

#Divide raw data per stations----------------------------------------------

  An_der_Alche=cbind(countingData$Time,countingData[2])
  Siegarena=cbind(countingData$Time,countingData[3])
  TiergartenStr=cbind(countingData$Time,countingData[4])

#Rename Columns----------------------------------------------
  names(An_der_Alche)[1]="Timestamp"
  names(An_der_Alche)[2]="Value"

  names(Siegarena)[1]="Timestamp"
  names(Siegarena)[2]="Value"

  names(TiergartenStr)[1]="Timestamp"
  names(TiergartenStr)[2]="Value"


#Add Location Columns----------------------------------------------

  An_der_Alche$Town = "Siegen"
  An_der_Alche$Station = "An_der_Alche"
  An_der_Alche$Lon = 8.01022430544296
  An_der_Alche$Lat = 50.88037007619345
  An_der_Alche$Oneway = FALSE
  #An_der_Alche$Road_type = "Pathway

  Siegarena$Town = "Siegen"
  Siegarena$Station = "Siegarena"
  Siegarena$Lon = 7.994474468725039
  Siegarena$Lat = 50.850877961808834
  Siegarena$Oneway = FALSE
  #Siegarena$Road_type = "Pathway

  TiergartenStr$Town = "Siegen"
  TiergartenStr$Station = "TiergartenStr"
  TiergartenStr$Lon = 8.02456057840836
  TiergartenStr$Lat = 50.888904046268614
  TiergartenStr$Oneway = FALSE
  #TiergartenStr$Road_type = "Pathway

#Connect the Stations----------------------------------------------

  rawData=rbind(An_der_Alche,Siegarena)
  rawData=rbind(rawData,TiergartenStr)

  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
  #TimeStamp Configurations
  rawData$Timestamp
  
  rawData$Timestamp = gsub("So. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Mo. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Di. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Mi. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Do. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Fr. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Sa. ", "", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jan. ", "01.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Febr. ", "02.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mrz. ", "03.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Apr. ", "04.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mai ", "05.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jun. ", "06.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jul. ", "07.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Aug. ", "08.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Sept. ", "09.", rawData$Timestamp)
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
  
