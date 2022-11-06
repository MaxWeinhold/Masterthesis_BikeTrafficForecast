#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Darmstadt

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