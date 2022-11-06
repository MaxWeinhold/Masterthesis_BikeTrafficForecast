#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Bochum

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



  

