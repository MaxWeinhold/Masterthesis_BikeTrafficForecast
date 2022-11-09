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

