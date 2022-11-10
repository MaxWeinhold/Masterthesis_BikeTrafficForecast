#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Hamburg

library(lubridate)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Hamburg
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Hamburg")

#Read Bycicle Counting Data----------------------------------------------
  countingData_Dauer = read.csv(file = "export_radverkehr.csv",sep=";")
  countingData_6399 = read.csv(file = "Anzahl_Fahrraeder_6399_Querschnitt 1-Stunde_2022-11-07_11-27-35.csv",sep=";")
  countingData_6397 = read.csv(file = "Anzahl_Fahrraeder_6397_Nord nach Süd 1-Stunde_2022-11-07_11-24-09.csv",sep=";")
  countingData_6393 = read.csv(file = "Anzahl_Fahrraeder_6393_Querschnitt 1-Stunde_2022-11-07_11-25-07.csv",sep=";")
  countingData_6344 = read.csv(file = "Anzahl_Fahrraeder_6344_Nordwest nach Südost 1-Stunde_2022-11-07_11-21-19.csv",sep=";")
  countingData_5901 = read.csv(file = "Anzahl_Fahrraeder_5901_Querschnitt 1-Stunde_2022-11-07_11-27-25.csv",sep=";")
  countingData_5883 = read.csv(file = "Anzahl_Fahrraeder_5883_Querschnitt 1-Stunde_2022-11-07_11-24-31.csv",sep=";")
  countingData_5869 = read.csv(file = "Anzahl_Fahrraeder_5869_Querschnitt 1-Stunde_2022-11-07_11-21-05.csv",sep=";")
  countingData_5866 = read.csv(file = "Anzahl_Fahrraeder_5866_Querschnitt 1-Stunde_2022-11-07_11-27-11.csv",sep=";")
  countingData_5862 = read.csv(file = "Anzahl_Fahrraeder_5862_Querschnitt 1-Stunde_2022-11-07_11-24-21.csv",sep=";")
  countingData_5856 = read.csv(file = "Anzahl_Fahrraeder_5856_Querschnitt 1-Stunde_2022-11-07_11-23-54.csv",sep=";")
  countingData_5854 = read.csv(file = "Anzahl_Fahrraeder_5854_Querschnitt 1-Stunde_2022-11-07_11-23-40.csv",sep=";")
  countingData_5812 = read.csv(file = "Anzahl_Fahrraeder_5812_Querschnitt 1-Stunde_2022-11-07_11-22-19.csv",sep=";")
  countingData_5809 = read.csv(file = "Anzahl_Fahrraeder_5809_Querschnitt 1-Stunde_2022-11-07_11-23-02.csv",sep=";")
  countingData_5804 = read.csv(file = "Anzahl_Fahrraeder_5804_Querschnitt 1-Stunde_2022-11-07_11-20-08.csv",sep=";")
  countingData_5801 = read.csv(file = "Anzahl_Fahrraeder_5801_Querschnitt 1-Stunde_2022-11-07_11-26-48.csv",sep=";")
  countingData_5636 = read.csv(file = "Anzahl_Fahrraeder_5636_Ost nach West 1-Stunde_2022-11-07_11-22-37.csv",sep=";")
  countingData_5629 = read.csv(file = "Anzahl_Fahrraeder_5629_Querschnitt 1-Stunde_2022-11-07_11-26-20.csv",sep=";")
  countingData_5623 = read.csv(file = "Anzahl_Fahrraeder_5623_Querschnitt 1-Stunde_2022-11-07_11-20-41.csv",sep=";")
  countingData_5620 = read.csv(file = "Anzahl_Fahrraeder_5620_Querschnitt 1-Stunde_2022-11-07_11-25-31.csv",sep=";")
  countingData_5617 = read.csv(file = "Anzahl_Fahrraeder_5617_Querschnitt 1-Stunde_2022-11-07_11-27-56.csv",sep=";")
  countingData_5616 = read.csv(file = "Anzahl_Fahrraeder_5616_Südwest nach Nordost 1-Stunde_2022-11-07_11-25-45.csv",sep=";")
  countingData_5606 = read.csv(file = "Anzahl_Fahrraeder_5606_Ost nach West 1-Stunde_2022-11-07_11-22-32.csv",sep=";")
  countingData_5593 = read.csv(file = "Anzahl_Fahrraeder_5593_Querschnitt 1-Stunde_2022-11-07_11-23-16.csv",sep=";")
  countingData_5590 = read.csv(file = "Anzahl_Fahrraeder_5590_Querschnitt 1-Stunde_2022-11-07_11-22-01.csv",sep=";")
  countingData_5584 = read.csv(file = "Anzahl_Fahrraeder_5584_Querschnitt 1-Stunde_2022-11-07_11-28-06.csv",sep=";")
  countingData_5583 = read.csv(file = "Anzahl_Fahrraeder_5583_Südost nach Nordwest 1-Stunde_2022-11-07_11-22-50.csv",sep=";")
  countingData_5575 = read.csv(file = "Anzahl_Fahrraeder_5575_Querschnitt 1-Stunde_2022-11-07_11-26-05.csv",sep=";")
  countingData_5573 = read.csv(file = "Anzahl_Fahrraeder_5573_Querschnitt 1-Stunde_2022-11-07_11-26-35.csv",sep=";")
  #countingData_5567 = read.csv(file = "Anzahl_Fahrraeder_5567_Querschnitt 1-Stunde_2022-11-07_11-21-40.csv",sep=";")
  countingData_5564 = read.csv(file = "Anzahl_Fahrraeder_5564_Querschnitt 1-Stunde_2022-11-07_11-24-49.csv",sep=";")
  
#Create Timestamp----------------------------------------------
  
  names(countingData_Dauer)
  countingData_Dauer$Timestamp=paste(countingData_Dauer$Datum,countingData_Dauer$Zeitraum..von., sep=" ")
  countingData_Dauer$Timestamp = gsub("U", "", countingData_Dauer$Timestamp)
  countingData_Dauer$Timestamp = gsub("h", "", countingData_Dauer$Timestamp)
  countingData_Dauer$Timestamp = gsub("r", "", countingData_Dauer$Timestamp)
  countingData_Dauer$Timestamp = substring(countingData_Dauer$Timestamp,1, nchar(countingData_Dauer$Timestamp)-1)
  countingData_Dauer$Timestamp	= as.POSIXct(countingData_Dauer$Timestamp, format = "%d.%m.%Y %H:%M")
  
  names(countingData_6399)
  countingData_6399$Timestamp=paste(countingData_6399$ï..Datum,countingData_6399$Uhrzeit.von, sep=" ")
  countingData_6397$Timestamp=paste(countingData_6397$ï..Datum,countingData_6397$Uhrzeit.von, sep=" ")
  countingData_6393$Timestamp=paste(countingData_6393$ï..Datum,countingData_6393$Uhrzeit.von, sep=" ")
  countingData_6344$Timestamp=paste(countingData_6344$ï..Datum,countingData_6344$Uhrzeit.von, sep=" ")
  countingData_5901$Timestamp=paste(countingData_5901$ï..Datum,countingData_5901$Uhrzeit.von, sep=" ")
  countingData_5883$Timestamp=paste(countingData_5883$ï..Datum,countingData_5883$Uhrzeit.von, sep=" ")
  countingData_5869$Timestamp=paste(countingData_5869$ï..Datum,countingData_5869$Uhrzeit.von, sep=" ")
  countingData_5866$Timestamp=paste(countingData_5866$ï..Datum,countingData_5866$Uhrzeit.von, sep=" ")
  countingData_5862$Timestamp=paste(countingData_5862$ï..Datum,countingData_5862$Uhrzeit.von, sep=" ")
  countingData_5856$Timestamp=paste(countingData_5856$ï..Datum,countingData_5856$Uhrzeit.von, sep=" ")
  countingData_5854$Timestamp=paste(countingData_5854$ï..Datum,countingData_5854$Uhrzeit.von, sep=" ")
  countingData_5812$Timestamp=paste(countingData_5812$ï..Datum,countingData_5812$Uhrzeit.von, sep=" ")
  countingData_5809$Timestamp=paste(countingData_5809$ï..Datum,countingData_5809$Uhrzeit.von, sep=" ")
  countingData_5804$Timestamp=paste(countingData_5804$ï..Datum,countingData_5804$Uhrzeit.von, sep=" ")
  countingData_5801$Timestamp=paste(countingData_5801$ï..Datum,countingData_5801$Uhrzeit.von, sep=" ")
  countingData_5636$Timestamp=paste(countingData_5636$ï..Datum,countingData_5636$Uhrzeit.von, sep=" ")
  countingData_5629$Timestamp=paste(countingData_5629$ï..Datum,countingData_5629$Uhrzeit.von, sep=" ")
  countingData_5623$Timestamp=paste(countingData_5623$ï..Datum,countingData_5623$Uhrzeit.von, sep=" ")
  countingData_5620$Timestamp=paste(countingData_5620$ï..Datum,countingData_5620$Uhrzeit.von, sep=" ")
  countingData_5617$Timestamp=paste(countingData_5617$ï..Datum,countingData_5617$Uhrzeit.von, sep=" ")
  countingData_5616$Timestamp=paste(countingData_5616$ï..Datum,countingData_5616$Uhrzeit.von, sep=" ")
  countingData_5606$Timestamp=paste(countingData_5606$ï..Datum,countingData_5606$Uhrzeit.von, sep=" ")
  countingData_5593$Timestamp=paste(countingData_5593$ï..Datum,countingData_5593$Uhrzeit.von, sep=" ")
  countingData_5590$Timestamp=paste(countingData_5590$ï..Datum,countingData_5590$Uhrzeit.von, sep=" ")
  countingData_5584$Timestamp=paste(countingData_5584$ï..Datum,countingData_5584$Uhrzeit.von, sep=" ")
  countingData_5583$Timestamp=paste(countingData_5583$ï..Datum,countingData_5583$Uhrzeit.von, sep=" ")
  countingData_5575$Timestamp=paste(countingData_5575$ï..Datum,countingData_5575$Uhrzeit.von, sep=" ")
  countingData_5573$Timestamp=paste(countingData_5573$ï..Datum,countingData_5573$Uhrzeit.von, sep=" ")
  countingData_5564$Timestamp=paste(countingData_5564$ï..Datum,countingData_5564$Uhrzeit.von, sep=" ")
  
#Rename Columns----------------------------------------------
  names(countingData_Dauer)[3]="Value"
  names(countingData_6399)[3]="Value"
  names(countingData_6397)[3]="Value"
  names(countingData_6393)[3]="Value"
  names(countingData_6344)[3]="Value"
  names(countingData_5901)[3]="Value"
  names(countingData_5883)[3]="Value"
  names(countingData_5869)[3]="Value"
  names(countingData_5866)[3]="Value"
  names(countingData_5862)[3]="Value"
  names(countingData_5856)[3]="Value"
  names(countingData_5854)[3]="Value"
  names(countingData_5812)[3]="Value"
  names(countingData_5809)[3]="Value"
  names(countingData_5804)[3]="Value"
  names(countingData_5801)[3]="Value"
  names(countingData_5636)[3]="Value"
  names(countingData_5629)[3]="Value"
  names(countingData_5623)[3]="Value"
  names(countingData_5620)[3]="Value"
  names(countingData_5617)[3]="Value"
  names(countingData_5616)[3]="Value"
  names(countingData_5606)[3]="Value"
  names(countingData_5593)[3]="Value"
  names(countingData_5590)[3]="Value"
  names(countingData_5584)[3]="Value"
  names(countingData_5575)[3]="Value"
  names(countingData_5573)[3]="Value"
  names(countingData_5564)[3]="Value"
  
#Delete Columns----------------------------------------------
  AcountingData_Dauer=as.data.frame(cbind(countingData_Dauer[,'Timestamp',drop = FALSE],countingData_Dauer[3]))
  AcountingData_6399=as.data.frame(cbind(countingData_6399[,'Timestamp',drop = FALSE],countingData_6399[3]))
  AcountingData_6397=as.data.frame(cbind(countingData_6397[,'Timestamp',drop = FALSE],countingData_6397[3]))
  AcountingData_6393=as.data.frame(cbind(countingData_6393[,'Timestamp',drop = FALSE],countingData_6393[3]))
  AcountingData_6344=as.data.frame(cbind(countingData_6344[,'Timestamp',drop = FALSE],countingData_6344[3]))
  AcountingData_5901=as.data.frame(cbind(countingData_5901[,'Timestamp',drop = FALSE],countingData_5901[3]))
  AcountingData_5883=as.data.frame(cbind(countingData_5883[,'Timestamp',drop = FALSE],countingData_5883[3]))
  AcountingData_5869=as.data.frame(cbind(countingData_5869[,'Timestamp',drop = FALSE],countingData_5869[3]))
  AcountingData_5866=as.data.frame(cbind(countingData_5866[,'Timestamp',drop = FALSE],countingData_5866[3]))
  AcountingData_5862=as.data.frame(cbind(countingData_5862[,'Timestamp',drop = FALSE],countingData_5862[3]))
  AcountingData_5856=as.data.frame(cbind(countingData_5856[,'Timestamp',drop = FALSE],countingData_5856[3]))
  AcountingData_5854=as.data.frame(cbind(countingData_5854[,'Timestamp',drop = FALSE],countingData_5854[3]))
  AcountingData_5812=as.data.frame(cbind(countingData_5812[,'Timestamp',drop = FALSE],countingData_5812[3]))
  AcountingData_5809=as.data.frame(cbind(countingData_5809[,'Timestamp',drop = FALSE],countingData_5809[3]))
  AcountingData_5804=as.data.frame(cbind(countingData_5804[,'Timestamp',drop = FALSE],countingData_5804[3]))
  AcountingData_5801=as.data.frame(cbind(countingData_5801[,'Timestamp',drop = FALSE],countingData_5801[3]))
  AcountingData_5636=as.data.frame(cbind(countingData_5636[,'Timestamp',drop = FALSE],countingData_5636[3]))
  AcountingData_5629=as.data.frame(cbind(countingData_5629[,'Timestamp',drop = FALSE],countingData_5629[3]))
  AcountingData_5623=as.data.frame(cbind(countingData_5623[,'Timestamp',drop = FALSE],countingData_5623[3]))
  AcountingData_5620=as.data.frame(cbind(countingData_5620[,'Timestamp',drop = FALSE],countingData_5620[3]))
  AcountingData_5617=as.data.frame(cbind(countingData_5617[,'Timestamp',drop = FALSE],countingData_5617[3]))
  AcountingData_5616=as.data.frame(cbind(countingData_5616[,'Timestamp',drop = FALSE],countingData_5616[3]))
  AcountingData_5606=as.data.frame(cbind(countingData_5606[,'Timestamp',drop = FALSE],countingData_5606[3]))
  AcountingData_5593=as.data.frame(cbind(countingData_5593[,'Timestamp',drop = FALSE],countingData_5593[3]))
  AcountingData_5590=as.data.frame(cbind(countingData_5590[,'Timestamp',drop = FALSE],countingData_5590[3]))
  AcountingData_5584=as.data.frame(cbind(countingData_5584[,'Timestamp',drop = FALSE],countingData_5584[3]))
  AcountingData_5575=as.data.frame(cbind(countingData_5575[,'Timestamp',drop = FALSE],countingData_5575[3]))
  AcountingData_5573=as.data.frame(cbind(countingData_5573[,'Timestamp',drop = FALSE],countingData_5573[3]))
  AcountingData_5564=as.data.frame(cbind(countingData_5564[,'Timestamp',drop = FALSE],countingData_5564[3]))
  
#Add Location Columns----------------------------------------------
  
  AcountingData_Dauer$Town = "Hamburg"
  AcountingData_6399$Town = "Hamburg"
  AcountingData_6397$Town = "Hamburg"
  AcountingData_6393$Town = "Hamburg"
  AcountingData_6344$Town = "Hamburg"
  AcountingData_5901$Town = "Hamburg"
  AcountingData_5883$Town = "Hamburg"
  AcountingData_5869$Town = "Hamburg"
  AcountingData_5866$Town = "Hamburg"
  AcountingData_5862$Town = "Hamburg"
  AcountingData_5856$Town = "Hamburg"
  AcountingData_5854$Town = "Hamburg"
  AcountingData_5812$Town = "Hamburg"
  AcountingData_5809$Town = "Hamburg"
  AcountingData_5804$Town = "Hamburg"
  AcountingData_5801$Town = "Hamburg"
  AcountingData_5636$Town = "Hamburg"
  AcountingData_5629$Town = "Hamburg"
  AcountingData_5623$Town = "Hamburg"
  AcountingData_5620$Town = "Hamburg"
  AcountingData_5617$Town = "Hamburg"
  AcountingData_5616$Town = "Hamburg"
  AcountingData_5606$Town = "Hamburg"
  AcountingData_5593$Town = "Hamburg"
  AcountingData_5590$Town = "Hamburg"
  AcountingData_5584$Town = "Hamburg"
  AcountingData_5575$Town = "Hamburg"
  AcountingData_5573$Town = "Hamburg"
  AcountingData_5564$Town = "Hamburg"
  
  AcountingData_Dauer$Station = "Dauer"
  AcountingData_6399$Station = "6399"
  AcountingData_6397$Station = "6397"
  AcountingData_6393$Station = "6393"
  AcountingData_6344$Station = "6344"
  AcountingData_5901$Station = "5901"
  AcountingData_5883$Station = "5883"
  AcountingData_5869$Station = "5869"
  AcountingData_5866$Station = "5866"
  AcountingData_5862$Station = "5862"
  AcountingData_5856$Station = "5856"
  AcountingData_5854$Station = "5854"
  AcountingData_5812$Station = "5812"
  AcountingData_5809$Station = "5809"
  AcountingData_5804$Station = "5804"
  AcountingData_5801$Station = "5801"
  AcountingData_5636$Station = "5636"
  AcountingData_5629$Station = "5629"
  AcountingData_5623$Station = "5623"
  AcountingData_5620$Station = "5620"
  AcountingData_5617$Station = "5617"
  AcountingData_5616$Station = "5616"
  AcountingData_5606$Station = "5606"
  AcountingData_5593$Station = "5593"
  AcountingData_5590$Station = "5590"
  AcountingData_5584$Station = "5584"
  AcountingData_5575$Station = "5575"
  AcountingData_5573$Station = "5573"
  AcountingData_5564$Station = "5564"
  
  AcountingData_Dauer$Lon = 10.008669172906453
  AcountingData_6399$Lon = 10.146688411688647
  AcountingData_6397$Lon = 9.993367823584439
  AcountingData_6393$Lon = 10.01023161756706
  AcountingData_6344$Lon = 9.93186233867988
  AcountingData_5901$Lon = 10.11932195882699
  AcountingData_5883$Lon = 9.995809691297193
  AcountingData_5869$Lon = 9.890416762248307
  AcountingData_5866$Lon = 10.09403346915416
  AcountingData_5862$Lon = 9.997965099044128
  AcountingData_5856$Lon = 9.990068357987
  AcountingData_5854$Lon = 9.985728100234075
  AcountingData_5812$Lon = 9.966519900765514
  AcountingData_5809$Lon = 9.970488169393583
  AcountingData_5804$Lon = 9.868750224484131
  AcountingData_5801$Lon = 10.042970431615835
  AcountingData_5636$Lon = 9.967630836087585
  AcountingData_5629$Lon = 10.044645281900348
  AcountingData_5623$Lon = 9.934183898139135
  AcountingData_5620$Lon = 10.019980707310843
  AcountingData_5617$Lon = 10.186461425761824
  AcountingData_5616$Lon = 10.027614480879572
  AcountingData_5606$Lon = 9.966894405516138
  AcountingData_5593$Lon = 9.972622254952231
  AcountingData_5590$Lon = 9.950668211364697
  AcountingData_5584$Lon = 10.207709781979963
  AcountingData_5575$Lon = 10.018682002768545
  AcountingData_5573$Lon = 10.03667528596795
  AcountingData_5564$Lon = 9.999136277341934
  
  AcountingData_Dauer$Lat = 53.5592769530185
  AcountingData_6399$Lat = 53.59493314192152
  AcountingData_6397$Lat = 53.55814969100046
  AcountingData_6393$Lat = 53.577179331761826
  AcountingData_6344$Lat = 53.62868575141001
  AcountingData_5901$Lat = 53.6057406549951
  AcountingData_5883$Lat = 53.559398186032425
  AcountingData_5869$Lat = 53.602150303397124
  AcountingData_5866$Lat = 53.65671517825859
  AcountingData_5862$Lat = 53.55780440673376
  AcountingData_5856$Lat = 53.55972754858469
  AcountingData_5854$Lat = 53.456459453468
  AcountingData_5812$Lat = 53.540860367824294
  AcountingData_5809$Lat = 53.575983287931116
  AcountingData_5804$Lat = 53.57125989061707
  AcountingData_5801$Lat = 53.59875888978793
  AcountingData_5636$Lat = 53.557448730765685
  AcountingData_5629$Lat = 53.56391938217152
  AcountingData_5623$Lat = 53.55026374717966
  AcountingData_5620$Lat = 53.56618470258496
  AcountingData_5617$Lat = 53.48963913729209
  AcountingData_5616$Lat = 53.53318403563522
  AcountingData_5606$Lat = 53.5574290149294
  AcountingData_5593$Lat = 53.58139857569912
  AcountingData_5590$Lat = 53.617009031586775
  AcountingData_5584$Lat = 53.491376960565574
  AcountingData_5575$Lat = 53.49168002033256
  AcountingData_5573$Lat = 53.57578384115247
  AcountingData_5564$Lat = 53.579928806935726
  
  AcountingData_Dauer$Oneway = FALSE
  AcountingData_6399$Oneway = FALSE
  AcountingData_6397$Oneway = FALSE
  AcountingData_6393$Oneway = FALSE
  AcountingData_6344$Oneway = FALSE
  AcountingData_5901$Oneway = FALSE
  AcountingData_5883$Oneway = FALSE
  AcountingData_5869$Oneway = FALSE
  AcountingData_5866$Oneway = FALSE
  AcountingData_5862$Oneway = FALSE
  AcountingData_5856$Oneway = FALSE
  AcountingData_5854$Oneway = FALSE
  AcountingData_5812$Oneway = FALSE
  AcountingData_5809$Oneway = FALSE
  AcountingData_5804$Oneway = FALSE
  AcountingData_5801$Oneway = FALSE
  AcountingData_5636$Oneway = FALSE
  AcountingData_5629$Oneway = FALSE
  AcountingData_5623$Oneway = FALSE
  AcountingData_5620$Oneway = FALSE
  AcountingData_5617$Oneway = FALSE
  AcountingData_5616$Oneway = FALSE
  AcountingData_5606$Oneway = FALSE
  AcountingData_5593$Oneway = FALSE
  AcountingData_5590$Oneway = FALSE
  AcountingData_5584$Oneway = FALSE
  AcountingData_5575$Oneway = FALSE
  AcountingData_5573$Oneway = FALSE
  AcountingData_5564$Oneway = FALSE
  
  #AcountingData_Dauer$Road_type = "large_Street"  
  #AcountingData_6399$Road_type = "Street"  
  #AcountingData_6397$Road_type = "large_Street"  
  #AcountingData_6393$Road_type = "Bridge"  
  #AcountingData_6344$Road_type = "large_Street"  
  #AcountingData_5901$Road_type = "large_Street"  
  #AcountingData_5883$Road_type = "Street"  
  #AcountingData_5869$Road_type = "Tunnel"  
  #AcountingData_5866$Road_type = "Street"  
  #AcountingData_5862$Road_type = "Bridge"  
  #AcountingData_5856$Road_type = "large_Street"  
  #AcountingData_5854$Road_type = "Street"  
  #AcountingData_5812$Road_type = "Street"  
  #AcountingData_5809$Road_type = "Bridge"  
  #AcountingData_5804$Road_type = "Street"  
  #AcountingData_5801$Road_type = "Street"  
  #AcountingData_5636$Road_type = "large_Street"  
  #AcountingData_5629$Road_type = "Street"  
  #AcountingData_5623$Road_type = "Street"  
  #AcountingData_5620$Road_type = "Street"  
  #AcountingData_5617$Road_type = "large_Street"  
  #AcountingData_5616$Road_type = "Bridge"  
  #AcountingData_5606$Road_type = "large_Street"  
  #AcountingData_5593$Road_type = "large_Street"  
  #AcountingData_5590$Road_type = "large_Street"  
  #AcountingData_5584$Road_type = "Street"  
  #AcountingData_5575$Road_type = "Street"  
  #AcountingData_5573$Road_type = "large_Street"  
  #AcountingData_5564$Road_type = "Bridge"  
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(AcountingData_Dauer,AcountingData_6399)
  rawData=rbind(rawData,AcountingData_6397)
  rawData=rbind(rawData,AcountingData_6393)
  rawData=rbind(rawData,AcountingData_6344)
  rawData=rbind(rawData,AcountingData_5901)
  rawData=rbind(rawData,AcountingData_5883)
  rawData=rbind(rawData,AcountingData_5869)
  rawData=rbind(rawData,AcountingData_5866)
  rawData=rbind(rawData,AcountingData_5862)
  rawData=rbind(rawData,AcountingData_5856)
  rawData=rbind(rawData,AcountingData_5854)
  rawData=rbind(rawData,AcountingData_5812)
  rawData=rbind(rawData,AcountingData_5809)
  rawData=rbind(rawData,AcountingData_5804)
  rawData=rbind(rawData,AcountingData_5801)
  rawData=rbind(rawData,AcountingData_5636)
  rawData=rbind(rawData,AcountingData_5629)
  rawData=rbind(rawData,AcountingData_5623)
  rawData=rbind(rawData,AcountingData_5620)
  rawData=rbind(rawData,AcountingData_5617)
  rawData=rbind(rawData,AcountingData_5616)
  rawData=rbind(rawData,AcountingData_5606)
  rawData=rbind(rawData,AcountingData_5590)
  rawData=rbind(rawData,AcountingData_5584)
  rawData=rbind(rawData,AcountingData_5575)
  rawData=rbind(rawData,AcountingData_5573)
  rawData=rbind(rawData,AcountingData_5564)
  
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
  
  pH=publicHolidays[publicHolidays$HAM %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "HAM",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Hamburg")
  Weather_Wind  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_F.csv",sep=",", skip = 1, header = F)
  Weather_CloudCover  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_N.csv",sep=",", skip = 1, header = F)
  Weather_Humidity  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RF.csv",sep=",", skip = 1, header = F)
  Weather_Rain  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RR.csv",sep=",", skip = 1, header = F)
  Weather_Temperature  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_T2M.csv",sep=",", skip = 1, header = F)
  
  Weather_Wind[1] <- NULL
  Weather_Wind[1] <- NULL
  Weather_Wind[3] <- NULL
  Weather_Wind[3] <- NULL
  Weather_Wind[3] <- NULL
  
  Weather_CloudCover[1] <- NULL
  Weather_CloudCover[1] <- NULL
  Weather_CloudCover[3] <- NULL
  Weather_CloudCover[3] <- NULL
  Weather_CloudCover[3] <- NULL
  
  Weather_Humidity[1] <- NULL
  Weather_Humidity[1] <- NULL
  Weather_Humidity[3] <- NULL
  Weather_Humidity[3] <- NULL
  Weather_Humidity[3] <- NULL
  
  Weather_Rain[1] <- NULL
  Weather_Rain[1] <- NULL
  Weather_Rain[3] <- NULL
  Weather_Rain[3] <- NULL
  Weather_Rain[3] <- NULL
  
  Weather_Temperature[1] <- NULL
  Weather_Temperature[1] <- NULL
  Weather_Temperature[3] <- NULL
  Weather_Temperature[3] <- NULL
  Weather_Temperature[3] <- NULL
  
  names(Weather_Wind)[1]="Timestamp"
  names(Weather_Wind)[2]="Wind"
  
  names(Weather_CloudCover)[1]="Timestamp"
  names(Weather_CloudCover)[2]="CloudCover"
  
  names(Weather_Humidity)[1]="Timestamp"
  names(Weather_Humidity)[2]="Humidity"
  
  names(Weather_Rain)[1]="Timestamp"
  names(Weather_Rain)[2]="Rain"
  
  names(Weather_Temperature)[1]="Timestamp"
  names(Weather_Temperature)[2]="Temperature"
  
  Weather_Wind$Timestamp = gsub("T", " ", Weather_Wind$Timestamp)
  Weather_Wind$Timestamp=as.POSIXlt(Weather_Wind$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Wind$Year	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%Y"))
  Weather_Wind$Months=month(as.POSIXlt(Weather_Wind$Timestamp))
  Weather_Wind$Day	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%d"))
  Weather_Wind$Hour	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%H"))
  
  Weather_CloudCover$Timestamp = gsub("T", " ", Weather_CloudCover$Timestamp)
  Weather_CloudCover$Timestamp=as.POSIXlt(Weather_CloudCover$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_CloudCover$Year	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%Y"))
  Weather_CloudCover$Months=month(as.POSIXlt(Weather_CloudCover$Timestamp))
  Weather_CloudCover$Day	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%d"))
  Weather_CloudCover$Hour	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%H"))
  
  Weather_Humidity$Timestamp = gsub("T", " ", Weather_Humidity$Timestamp)
  Weather_Humidity$Timestamp=as.POSIXlt(Weather_Humidity$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Humidity$Year	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%Y"))
  Weather_Humidity$Months=month(as.POSIXlt(Weather_Humidity$Timestamp))
  Weather_Humidity$Day	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%d"))
  Weather_Humidity$Hour	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%H"))
  
  Weather_Rain$Timestamp = gsub("T", " ", Weather_Rain$Timestamp)
  Weather_Rain$Timestamp=as.POSIXlt(Weather_Rain$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Rain$Year	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%Y"))
  Weather_Rain$Months=month(as.POSIXlt(Weather_Rain$Timestamp))
  Weather_Rain$Day	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%d"))
  Weather_Rain$Hour	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%H"))
  
  Weather_Temperature$Timestamp = gsub("T", " ", Weather_Temperature$Timestamp)
  Weather_Temperature$Timestamp=as.POSIXlt(Weather_Temperature$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Temperature$Year	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%Y"))
  Weather_Temperature$Months=month(as.POSIXlt(Weather_Temperature$Timestamp))
  Weather_Temperature$Day	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%d"))
  Weather_Temperature$Hour	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%H"))
  
  Weather_Wind$Timestamp <- NULL
  Weather_CloudCover$Timestamp <- NULL
  Weather_Humidity$Timestamp <- NULL
  Weather_Rain$Timestamp <- NULL
  Weather_Temperature$Timestamp <- NULL
  
  rawData = merge(x = rawData,y = Weather_Wind,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Wind)
  
  rawData = merge(x = rawData,y = Weather_CloudCover,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_CloudCover)
  
  rawData = merge(x = rawData,y = Weather_Humidity,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Humidity)
  
  rawData = merge(x = rawData,y = Weather_Rain,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Rain)
  
  rawData = merge(x = rawData,y = Weather_Temperature,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Temperature)
  summary(rawData)
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(rawData,"Hamburg.csv")
  
  