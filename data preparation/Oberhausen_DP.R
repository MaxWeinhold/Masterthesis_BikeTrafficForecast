#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Oberhausen

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Oberhausen
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Oberhausen")

#Read Bycicle Counting Data----------------------------------------------
  countingData = read.csv(file = "Oberhausen_Daten.csv",sep=";")
  names(countingData)

#Divide raw data per stations----------------------------------------------

  GruenerPfad=cbind(countingData$Time,countingData[2])
  HOG_Trasse=cbind(countingData$Time,countingData[3])
  Centro=cbind(countingData$Time,countingData[4])
  RWagnerAlee=cbind(countingData$Time,countingData[5])

#Rename Columns----------------------------------------------
  names(GruenerPfad)[1]="Timestamp"
  names(GruenerPfad)[2]="Value"
  
  names(HOG_Trasse)[1]="Timestamp"
  names(HOG_Trasse)[2]="Value"
  
  names(Centro)[1]="Timestamp"
  names(Centro)[2]="Value"
  
  names(RWagnerAlee)[1]="Timestamp"
  names(RWagnerAlee)[2]="Value"
  
#Add Location Columns----------------------------------------------
  
  GruenerPfad$Town = "Oberhausen"
  GruenerPfad$Station = "GruenerPfad"
  GruenerPfad$Lon = 6.824089142142462
  GruenerPfad$Lat = 51.49666835412627
  GruenerPfad$Oneway = FALSE
  #GruenerPfad$Road_type = "Pathway
  
  HOG_Trasse$Town = "Oberhausen"
  HOG_Trasse$Station = "HOG_Trasse"
  HOG_Trasse$Lon = 6.80824204357636
  HOG_Trasse$Lat = 51.53360061124719
  HOG_Trasse$Oneway = FALSE
  #HOG_Trasse$Road_type = "Pathway
  
  Centro$Town = "Oberhausen"
  Centro$Station = "Centro"
  Centro$Lon = 6.87436792895911
  Centro$Lat = 51.490976539804
  Centro$Oneway = FALSE
  #HOG_Trasse$Road_type = "Pathway
  
  RWagnerAlee$Town = "Oberhausen"
  RWagnerAlee$Station = "RWagnerAlee"
  RWagnerAlee$Lon = 6.869515590156982
  RWagnerAlee$Lat = 51.505586384633986
  RWagnerAlee$Oneway = FALSE
  #HOG_Trasse$Road_type = "Pathway
  
 #Connect the Stations----------------------------------------------
  
  rawData=rbind(GruenerPfad,HOG_Trasse)
  rawData=rbind(rawData,Centro)
  rawData=rbind(rawData,RWagnerAlee)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
  #TimeStamp Configurations
  rawData$Timestamp
  
  rawData$Timestamp = gsub(" Jan. ", "01.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Feb. ", "02.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mrz. ", "03.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Apr. ", "04.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mai ", "05.", rawData$Timestamp)
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
  
  
  