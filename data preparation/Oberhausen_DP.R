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
  
  
  