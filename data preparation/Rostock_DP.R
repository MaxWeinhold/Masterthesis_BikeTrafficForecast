#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Rostock

library(plyr)
library(lubridate)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Rostock
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Rostock")

#Read Bycicle Counting Data----------------------------------------------
  countingData = read.csv(file = "Zaehldaten_Rostock.csv",sep=",")
  names(countingData)

#Change count frequency to hourly data----------------------------------------------

  countingData$zeitpunkt = gsub("T", " ", countingData$zeitpunkt)
  countingData$Timestamp	= as.POSIXct(countingData$zeitpunkt)
  countingData$Timestamp=cut(strptime(countingData$Timestamp,"%Y-%m-%d %H:%M"),"hour")
  countingData=ddply(countingData,.(Timestamp,standort_id),summarize,Value=sum(summe))

#Divide Data Set for Stations

  nlevels(as.factor(countingData$standort_id))
  levels(as.factor(countingData$standort_id))
  
  HambStrLiningw = countingData[ which(countingData$standort_id=='100005392'),]
  HambStrBraesig = countingData[ which(countingData$standort_id=='100005393'),]
  Lange_Str_Sued = countingData[ which(countingData$standort_id=='100005394'),]
  Am_Strande = countingData[ which(countingData$standort_id=='100005395'),]
  Muehlendamm = countingData[ which(countingData$standort_id=='100011605'),]
  Warnemuende = countingData[ which(countingData$standort_id=='100017341'),]
  Hundertmaenner = countingData[ which(countingData$standort_id=='100034887'),]
  Hinrichshagen = countingData[ which(countingData$standort_id=='100037010'),]
  Markgrafenheid = countingData[ which(countingData$standort_id=='100037011'),]
  UniCampus = countingData[ which(countingData$standort_id=='100056900'),]
  
#Delete Columns we don't need----------------------------------------------
  HambStrLiningw$standort_id <- NULL
  HambStrBraesig$standort_id <- NULL
  Lange_Str_Sued$standort_id <- NULL
  Am_Strande$standort_id <- NULL
  Muehlendamm$standort_id <- NULL
  Warnemuende$standort_id <- NULL
  Hundertmaenner$standort_id <- NULL
  Hinrichshagen$standort_id <- NULL
  Markgrafenheid$standort_id <- NULL
  UniCampus$standort_id <- NULL

#Add Location Columns----------------------------------------------
  
  HambStrLiningw$Town = "Rostock"
  HambStrLiningw$Station = "HamburgerStr"
  HambStrLiningw$Lon = 12.0751943454518
  HambStrLiningw$Lat = 54.1028104661795
  HambStrLiningw$Oneway = TRUE
  #HambStrLiningw$Road_type = "large_Street"  
  
  HambStrBraesig$Town = "Rostock"
  HambStrBraesig$Station = "HamburgerStr"
  HambStrBraesig$Lon = 12.0751943454518
  HambStrBraesig$Lat = 54.1028104661795
  HambStrBraesig$Oneway = TRUE
  #HambStrBraesig$Road_type = "large_Street"  
  
  Lange_Str_Sued$Town = "Rostock"
  Lange_Str_Sued$Station = "Lange_Str_Sued"
  Lange_Str_Sued$Lon = 12.1358127462484
  Lange_Str_Sued$Lat = 54.0898467513184	
  Lange_Str_Sued$Oneway = FALSE
  #Lange_Str_Sued$Road_type = "large_Street"  
  
  Am_Strande$Town = "Rostock"
  Am_Strande$Station = "Am_Strande"
  Am_Strande$Lon = 12.1489283407518
  Am_Strande$Lat = 54.0916592166559	
  Am_Strande$Oneway = FALSE
  #Am_Strande$Road_type = "large_Street"  
  
  Muehlendamm$Town = "Rostock"
  Muehlendamm$Station = "Muehlendamm"
  Muehlendamm$Lon = 12.1529337045542
  Muehlendamm$Lat = 54.0837175276464
  Muehlendamm$Oneway = FALSE
  #Muehlendamm$Road_type = "Street"  
  
  Warnemuende$Town = "Rostock"
  Warnemuende$Station = "Warnemuende"
  Warnemuende$Lon = 12.0583319524552
  Warnemuende$Lat = 54.1765990012892	
  Warnemuende$Oneway = FALSE
  #Warnemuende$Road_type = "Street"  
  
  Hundertmaenner$Town = "Rostock"
  Hundertmaenner$Station = "Hundertmaenner"
  Hundertmaenner$Lon = 12.1140381032233
  Hundertmaenner$Lat = 54.0819465093574	
  Hundertmaenner$Oneway = FALSE
  #Hundertmaenner$Road_type = "Bridge"  
  
  Hinrichshagen$Town = "Rostock"
  Hinrichshagen$Station = "Hinrichshagen"
  Hinrichshagen$Lon = 12.1821342229307
  Hinrichshagen$Lat = 54.1908838808145	
  Hinrichshagen$Oneway = FALSE
  #Hinrichshagen$Road_type = "Pathway"  
  
  Markgrafenheid$Town = "Rostock"
  Markgrafenheid$Station = "Markgrafenheid"
  Markgrafenheid$Lon = 12.180946347738
  Markgrafenheid$Lat = 54.1910583813394	
  Markgrafenheid$Oneway = FALSE
  #Markgrafenheid$Road_type = "Pathway"  
  
  UniCampus$Town = "Rostock"
  UniCampus$Station = "UniCampus"
  UniCampus$Lon = 12.1019789944482
  UniCampus$Lat = 54.0783813146185	
  UniCampus$Oneway = FALSE
  #UniCampus$Road_type = "Pathway"  
  
#Summarize Directions----------------------------------------------
  
  #HamburgerStr=HambStrLiningw
  #HamburgerStr$Value = as.numeric(HambStrLiningw$Value) + as.numeric(HambStrBraesig$Value)

  Markgrafenheid$Value = as.numeric(Markgrafenheid$Value) + as.numeric(Hinrichshagen$Value)
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(HambStrLiningw,Markgrafenheid)
  rawData=rbind(rawData,Lange_Str_Sued)
  rawData=rbind(rawData,Am_Strande)
  rawData=rbind(rawData,Muehlendamm)
  rawData=rbind(rawData,Hundertmaenner)
  rawData=rbind(rawData,UniCampus)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)
