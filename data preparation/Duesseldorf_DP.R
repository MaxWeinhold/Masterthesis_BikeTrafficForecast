#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Duesseldorf

library(lubridate)
library(dplyr)
library(plyr)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Bochum
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Düsseldorf")
#Read Bycicle Counting Data----------------------------------------------
  countingData2012 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2012_3.csv",sep=";", encoding="UTF-8")
  countingData2013 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2013_1.csv",sep=";", encoding="UTF-8")
  countingData2014 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2014_1.csv",sep=";", encoding="UTF-8")
  countingData2015 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2015_1.csv",sep=";", encoding="UTF-8")
  countingData2016 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2016_1.csv",sep=";", encoding="UTF-8")
  countingData2017 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2017_1.csv",sep=";", encoding="UTF-8")
  countingData2018 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2018_1.csv",sep=";", encoding="UTF-8")
  countingData2019 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2019_1.csv",sep=";", encoding="UTF-8")
  countingData2020 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2020.csv",sep=";", encoding="UTF-8")
  countingData2021 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2021_2.csv",sep=";", encoding="UTF-8")
  names(countingData2021)
  
#Change count frequency to hourly data----------------------------------------------
  
  names(countingData2012)
  countingData2012$Uhrzeit=paste(countingData2012$Datum,countingData2012$Uhrzeit, sep=" ")
  countingData2012$Uhrzeit=cut(strptime(countingData2012$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2012=ddply(countingData2012,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker_Allee_IN),
                         Bilker.Allee.OUT=sum(Bilker_Allee_OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher_Deich_Ost_stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher_Deich_west_stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer_einwaerts_nach_TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer_einwaerts_vor_TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer_stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer_stadtauswaertss),
                         Koe_Steinstr=sum(Koe_Steinstr),
                         Lohauser_Deich=sum(Lohauser_Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann_Ufer.Totem_01),
                         OKB_Nord=sum(OKB_Nord),
                         OKB_Sued=sum(OKB_Sued))
  
  names(countingData2013)
  countingData2013$Uhrzeit=paste(countingData2013$Datum,countingData2013$Uhrzeit, sep=" ")
  countingData2013$Uhrzeit=cut(strptime(countingData2013$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2013=ddply(countingData2013,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwärts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwärts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwärts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwärts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswärts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswärtss),
                         Koe_Steinstr=sum(KÖ.Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Süd))
  
  
  countingData2014$Uhrzeit=paste(countingData2014$Datum,countingData2014$Uhrzeit, sep=" ")
  countingData2014$Uhrzeit=cut(strptime(countingData2014$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2014=ddply(countingData2014,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwärts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwärts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwärts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwärts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswärts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswärtss),
                         Koe_Steinstr=sum(KÖ.Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Süd))
  
  countingData2015$Uhrzeit=paste(countingData2015$Datum,countingData2015$Uhrzeit, sep=" ")
  countingData2015$Uhrzeit=cut(strptime(countingData2015$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2015=ddply(countingData2015,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2016$Uhrzeit=paste(countingData2016$Datum,countingData2016$Uhrzeit, sep=" ")
  countingData2016$Uhrzeit=cut(strptime(countingData2016$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2016=ddply(countingData2016,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2017$Uhrzeit=paste(countingData2017$Datum,countingData2017$Uhrzeit, sep=" ")
  countingData2017$Uhrzeit=cut(strptime(countingData2017$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2017=ddply(countingData2017,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(Kirchfeldstraße),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2018$Uhrzeit=paste(countingData2018$Datum,countingData2018$Uhrzeit, sep=" ")
  countingData2018$Uhrzeit=cut(strptime(countingData2018$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2018=ddply(countingData2018,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstrasse),
                         Elisabethstr=sum(Elisabethstrasse),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstrasse),
                         Kirchfeldstrasse=sum(Kirchfeldstrasse),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstrasse),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2019$Uhrzeit=paste(countingData2019$Datum,countingData2019$Uhrzeit, sep=" ")
  countingData2019$Uhrzeit=cut(strptime(countingData2019$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2019=ddply(countingData2019,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstrasse),
                         Elisabethstr=sum(Elisabethstrasse),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstrasse),
                         Kirchfeldstrasse=sum(Kirchfeldstrasse),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstrasse),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2020$Uhrzeit=paste(countingData2020$Datum,countingData2020$Uhrzeit, sep=" ")
  countingData2020$Uhrzeit=cut(strptime(countingData2020$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2020=ddply(countingData2020,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstrasse),
                         Elisabethstr=sum(Elisabethstrasse),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstrasse),
                         Kirchfeldstrasse=sum(Kirchfeldstrasse),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwaerts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwaerts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(Koe.Steinstrasse),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.Sued))
  
  countingData2021$Uhrzeit=paste(countingData2021$datum,countingData2021$uhrzeit, sep=" ")
  countingData2021$Uhrzeit=cut(strptime(countingData2021$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2021=ddply(countingData2021,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(bilker.allee.in),
                         Bilker.Allee.OUT=sum(bilker.allee.out),
                         Christophstr=sum(christophstrasse),
                         Elisabethstr=sum(elisabethstrasse),
                         Fleher_Deich_Ost_stromaufwaerts=sum(fleher.deich.ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(fleher.deich.west.stromabwaerts),
                         Friedrichstr=sum(friedrichstrasse),
                         Kirchfeldstrasse=sum(kirchfeldstrasse),
                         Koblenzer_einwaerts_nach_TLS=sum(koblenzer.einwaerts.nach.tls),
                         Koblenzer_einwaerts_vor_TLS=sum(koblenzer.einwaerts.vor.tls),
                         Koblenzer_stadtauswaerts=sum(koblenzer.stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswaertss),
                         Koe_Steinstr=sum(koe.steinstrasse),
                         Lohauser_Deich=sum(lohauser.deich),
                         Mannesmann_Ufer.Totem_01=sum(mannesmann.ufer),
                         OKB_Nord=sum(okb.nord),
                         OKB_Sued=sum(okb.sued))
  
#Connect all years----------------------------------------------
  
  countingData=rbind(countingData2012,countingData2013)
  countingData=rbind(countingData,countingData2014)
  countingData=rbind(countingData,countingData2015)
  countingData=rbind(countingData,countingData2016)
  countingData=rbind(countingData,countingData2017)
  countingData=rbind(countingData,countingData2018)
  countingData=rbind(countingData,countingData2019)
  countingData=rbind(countingData,countingData2020)
  countingData=rbind(countingData,countingData2021)
  countingData=na.omit(countingData)
  
#Divide raw data per stations----------------------------------------------
  
  Data_bilkeralleein=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Bilker_Allee_IN))
  Data_bilkeralleeout=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Bilker.Allee.OUT))
  Data_Christophstr=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Christophstr))
  Data_Elisabethstr=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Elisabethstr))
  Data_Fleher_Deich_Ost=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Fleher_Deich_Ost_stromaufwaerts))
  Data_Fleher_Deich_west=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Fleher_Deich_west_stromabwaerts))
  Data_Friedrichstr=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Friedrichstr))
  Data_Kirchfeldstrasse=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Kirchfeldstrasse))
  Data_Koblenzer_einwaerts_nach_TLS=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Koblenzer_einwaerts_nach_TLS))
  Data_Koblenzer_einwaerts_vor_TLS=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Koblenzer_einwaerts_vor_TLS))
  Data_Koblenzer_stadtauswaerts=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Koblenzer_stadtauswaerts))
  Data_Koe_Steinstr=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Koe_Steinstr))
  Data_Lohauser_Deich=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Lohauser_Deich))
  Data_Mannesmann_Ufer=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$Mannesmann_Ufer.Totem_01))
  Data_OKB_Nord=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$OKB_Nord))
  Data_OKB_Sued=as.data.frame(cbind(countingData[,'Uhrzeit',drop = FALSE],countingData$OKB_Sued))
  
#Rename Columns----------------------------------------------
  
  names(Data_bilkeralleein)[1]="Timestamp"
  names(Data_bilkeralleeout)[1]="Timestamp"
  names(Data_Christophstr)[1]="Timestamp"
  names(Data_Elisabethstr)[1]="Timestamp"
  names(Data_Fleher_Deich_Ost)[1]="Timestamp"
  names(Data_Fleher_Deich_west)[1]="Timestamp"
  names(Data_Friedrichstr)[1]="Timestamp"
  names(Data_Kirchfeldstrasse)[1]="Timestamp"
  names(Data_Koblenzer_einwaerts_nach_TLS)[1]="Timestamp"
  names(Data_Koblenzer_einwaerts_vor_TLS)[1]="Timestamp"
  names(Data_Koblenzer_stadtauswaerts)[1]="Timestamp"
  names(Data_Koe_Steinstr)[1]="Timestamp"
  names(Data_Lohauser_Deich)[1]="Timestamp"
  names(Data_Mannesmann_Ufer)[1]="Timestamp"
  names(Data_OKB_Nord)[1]="Timestamp"
  names(Data_OKB_Sued)[1]="Timestamp"
  
  names(Data_bilkeralleein)[2]="Value"
  names(Data_bilkeralleeout)[2]="Value"
  names(Data_Christophstr)[2]="Value"
  names(Data_Elisabethstr)[2]="Value"
  names(Data_Fleher_Deich_Ost)[2]="Value"
  names(Data_Fleher_Deich_west)[2]="Value"
  names(Data_Friedrichstr)[2]="Value"
  names(Data_Kirchfeldstrasse)[2]="Value"
  names(Data_Koblenzer_einwaerts_nach_TLS)[2]="Value"
  names(Data_Koblenzer_einwaerts_vor_TLS)[2]="Value"
  names(Data_Koblenzer_stadtauswaerts)[2]="Value"
  names(Data_Koe_Steinstr)[2]="Value"
  names(Data_Lohauser_Deich)[2]="Value"
  names(Data_Mannesmann_Ufer)[2]="Value"
  names(Data_OKB_Nord)[2]="Value"
  names(Data_OKB_Sued)[2]="Value"
  
#Add Location Columns----------------------------------------------
  
  Data_bilkeralleein$Town = "Düsseldorf"
  Data_bilkeralleeout$Town = "Düsseldorf"
  Data_Christophstr$Town = "Düsseldorf"
  Data_Elisabethstr$Town = "Düsseldorf"
  Data_Fleher_Deich_Ost$Town = "Düsseldorf"
  Data_Fleher_Deich_west$Town = "Düsseldorf"
  Data_Friedrichstr$Town = "Düsseldorf"
  Data_Kirchfeldstrasse$Town = "Düsseldorf"
  Data_Koblenzer_einwaerts_nach_TLS$Town = "Düsseldorf"
  Data_Koblenzer_einwaerts_vor_TLS$Town = "Düsseldorf"
  Data_Koblenzer_stadtauswaerts$Town = "Düsseldorf"
  Data_Koe_Steinstr$Town = "Düsseldorf"
  Data_Lohauser_Deich$Town = "Düsseldorf"
  Data_Mannesmann_Ufer$Town = "Düsseldorf"
  Data_OKB_Nord$Town = "Düsseldorf"
  Data_OKB_Sued$Town = "Düsseldorf"
  
  Data_bilkeralleein$Station = "bilkeralleein"
  Data_bilkeralleeout$Station = "bilkeralleeout"
  Data_Christophstr$Station = "Christophstr"
  Data_Elisabethstr$Station = "Elisabethstr"
  Data_Fleher_Deich_Ost$Station = "Fleher_Deich_Ost"
  Data_Fleher_Deich_west$Station = "Fleher_Deich_west"
  Data_Friedrichstr$Station = "Friedrichstr"
  Data_Kirchfeldstrasse$Station = "Kirchfeldstrasse"
  Data_Koblenzer_einwaerts_nach_TLS$Station = "Koblenzer_einwaerts_nach_TLS"
  Data_Koblenzer_einwaerts_vor_TLS$Station = "Koblenzer_einwaerts_vor_TLS"
  Data_Koblenzer_stadtauswaerts$Station = "Koblenzer_stadtauswaerts"
  Data_Koe_Steinstr$Station = "Koe_Steinstr"
  Data_Lohauser_Deich$Station = "Lohauser_Deich"
  Data_Mannesmann_Ufer$Station = "Mannesmann_Ufer"
  Data_OKB_Nord$Station = "OKB_Nord"
  Data_OKB_Sued$Station = "OKB_Sued"
  
  Data_bilkeralleein$Lon = 6.77131
  Data_bilkeralleeout$Lon = 6.77151
  Data_Christophstr$Lon = 6.79654
  Data_Elisabethstr$Lon = 6.77528
  Data_Fleher_Deich_Ost$Lon = 6.77571
  Data_Fleher_Deich_west$Lon = 6.77514
  Data_Friedrichstr$Lon = 6.77685
  Data_Kirchfeldstrasse$Lon = 6.77076
  Data_Koblenzer_einwaerts_nach_TLS$Lon = 6.88603
  Data_Koblenzer_einwaerts_vor_TLS$Lon = 6.88668
  Data_Koblenzer_stadtauswaerts$Lon = 6.88611
  Data_Koe_Steinstr$Lon = 6.77918
  Data_Lohauser_Deich$Lon = 6.71409
  Data_Mannesmann_Ufer$Lon = 6.76712
  Data_OKB_Nord$Lon = 6.76320
  Data_OKB_Sued$Lon = 6.76324
  
  Data_bilkeralleein$Lat = 51.21074
  Data_bilkeralleeout$Lat = 51.21074
  Data_Christophstr$Lat = 51.19623
  Data_Elisabethstr$Lat = 51.21641
  Data_Fleher_Deich_Ost$Lat = 51.18834
  Data_Fleher_Deich_west$Lat = 51.18774
  Data_Friedrichstr$Lat = 51.21461
  Data_Kirchfeldstrasse$Lat = 51.21246
  Data_Koblenzer_einwaerts_nach_TLS$Lat = 51.14922
  Data_Koblenzer_einwaerts_vor_TLS$Lat = 51.14898
  Data_Koblenzer_stadtauswaerts$Lat = 51.14911
  Data_Koe_Steinstr$Lat = 51.22324
  Data_Lohauser_Deich$Lat = 51.27754
  Data_Mannesmann_Ufer$Lat = 51.22015
  Data_OKB_Nord$Lat = 51.23166
  Data_OKB_Sued$Lat = 51.23146
  
  Data_bilkeralleein$Oneway = FALSE
  Data_bilkeralleeout$Oneway = FALSE
  Data_Christophstr$Oneway = FALSE
  Data_Elisabethstr$Oneway = FALSE
  Data_Fleher_Deich_Ost$Oneway = FALSE
  Data_Fleher_Deich_west$Oneway = FALSE
  Data_Friedrichstr$Oneway = FALSE
  Data_Kirchfeldstrasse$Oneway = FALSE
  Data_Koblenzer_einwaerts_nach_TLS$Oneway = FALSE
  Data_Koblenzer_einwaerts_vor_TLS$Oneway = FALSE
  Data_Koblenzer_stadtauswaerts$Oneway = FALSE
  Data_Koe_Steinstr$Oneway = FALSE
  Data_Lohauser_Deich$Oneway = FALSE
  Data_Mannesmann_Ufer$Oneway = FALSE
  Data_OKB_Nord$Oneway = FALSE
  Data_OKB_Sued$Oneway = FALSE
  
#Summarize Directions----------------------------------------------
  
  Data_bilkerallee=Data_bilkeralleein
  Data_bilkerallee$Value = as.numeric(Data_bilkeralleein$Value) + as.numeric(Data_bilkeralleeout$Value)
  Data_Fleher_Deich=Data_Fleher_Deich_Ost
  Data_Fleher_Deich$Value = as.numeric(Data_Fleher_Deich_Ost$Value) + as.numeric(Data_Fleher_Deich_west$Value)
  Data_Koblenzer=Data_Koblenzer_einwaerts_nach_TLS
  Data_Koblenzer$Value = as.numeric(Data_Koblenzer_einwaerts_nach_TLS$Value) + as.numeric(Data_Koblenzer_einwaerts_vor_TLS$Value)
  Data_OKB=Data_OKB_Nord
  Data_OKB$Value = as.numeric(Data_OKB_Nord$Value) + as.numeric(Data_OKB_Sued$Value)
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(Data_bilkerallee,Data_Christophstr)
  rawData=rbind(rawData,Data_Elisabethstr)
  rawData=rbind(rawData,Data_Fleher_Deich)
  rawData=rbind(rawData,Data_Friedrichstr)
  rawData=rbind(rawData,Data_Kirchfeldstrasse)
  rawData=rbind(rawData,Data_Koblenzer)
  rawData=rbind(rawData,Data_Koblenzer_stadtauswaerts)
  rawData=rbind(rawData,Data_Koe_Steinstr)
  rawData=rbind(rawData,Data_Lohauser_Deich)
  rawData=rbind(rawData,Data_Mannesmann_Ufer)
  rawData=rbind(rawData,Data_OKB)
  
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
  
  
  #Add Data on the Youth Inhabitant Ratio
  
  youthRatios = read.csv(file = "Altersgruppen.csv",sep=";", encoding="UTF-8")
  names(youthRatios)
  
  youthRatios$Code <- NULL
  youthRatios$Kreis <- NULL
  youthRatios$unter.3.Jahre <- NULL
  youthRatios$X3.bis.unter.6.Jahre <- NULL
  youthRatios$X6.bis.unter.10.Jahre <- NULL
  youthRatios$X10.bis.unter.15.Jahre <- NULL
  youthRatios$X15.bis.unter.18.Jahre <- NULL
  youthRatios$X18.bis.unter.20.Jahre <- NULL
  youthRatios$Insgesamt <- NULL
  youthRatios$Unter.18 <- NULL
  youthRatios$Unter.20 <- NULL
  
  names(youthRatios)[1]="Year"
  names(youthRatios)[2]="Town"
  names(youthRatios)[3]="young18"
  names(youthRatios)[4]="young20"
  
  youthRatios$young18 = gsub(",", ".", youthRatios$young18)
  youthRatios$young20 = gsub(",", ".", youthRatios$young20)
  
  youthRatios$young18 = as.numeric(youthRatios$young18)
  youthRatios$young20 = as.numeric(youthRatios$young20)
  
  rawData = merge(x = rawData,y = youthRatios,
                  by = c("Year","Town"),
                  all = FALSE)
  
  summary(rawData)
  
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Düsseldorf")
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
  write.csv(rawData,"Duesseldorf.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(4.4,4.4,4.3,4.3,4.2,4.2,4.2,4.2,4.2,4.2)
  
  ADFC=as.data.frame(cbind(Year,ADFC_Index))
  
  rawData = merge(x = rawData,y = ADFC,
                   by = c("Year"),
                   all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
#Add Data about number of inhabitants, city size, city center and male and female inhabitant ratio
#Also calculate distance to city ratio
  
  #Load data (source: Destatis)
  
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Einwohner_Destatis")
  Destatis12 = read.csv(file = "31122012_Auszug_GV.csv",sep=";")
  Destatis13 = read.csv(file = "31122013_Auszug_GV.csv",sep=";")
  Destatis14 = read.csv(file = "31122014_Auszug_GV.csv",sep=";")
  Destatis15 = read.csv(file = "31122015_Auszug_GV.csv",sep=";")
  Destatis16 = read.csv(file = "31122016_Auszug_GV.csv",sep=";")
  Destatis17 = read.csv(file = "31122017_Auszug_GV.csv",sep=";")
  Destatis18 = read.csv(file = "31122018_Auszug_GV.csv",sep=";")
  Destatis19 = read.csv(file = "31122019_Auszug_GV.csv",sep=";")
  Destatis20 = read.csv(file = "31122020_Auszug_GV.csv",sep=";")
  Destatis21 = read.csv(file = "31122021_Auszug_GV.csv",sep=";")
  
  title=", Stadt" #This differs, there are cities and also hanseatic cities
  
  test12=as.data.frame(Destatis12[Destatis12$X.6 == "Düsseldorf, Stadt",])
  test12[17] <- NULL
  test12[17] <- NULL
  test12 <- test12 %>% mutate_all(na_if,"")
  names(test12)[1]="number"
  test12=na.omit(test12)
  test12$Year=2012
  
  test13=as.data.frame(Destatis13[Destatis13$X.6 == "Düsseldorf, Stadt",])
  test13[17] <- NULL
  test13[17] <- NULL
  test13 <- test13 %>% mutate_all(na_if,"")
  names(test13)[1]="number"
  test13=na.omit(test13)
  test13$Year=2013
  
  test14=as.data.frame(Destatis14[Destatis14$X.6 == "Düsseldorf, Stadt",])
  test14[17] <- NULL
  test14[17] <- NULL
  test14 <- test14 %>% mutate_all(na_if,"")
  names(test14)[1]="number"
  test14=na.omit(test14)
  test14$Year=2014
  
  test15=as.data.frame(Destatis15[Destatis15$X.6 == "Düsseldorf, Stadt",])
  test15[17] <- NULL
  test15[17] <- NULL
  test15 <- test15 %>% mutate_all(na_if,"")
  names(test15)[1]="number"
  test15=na.omit(test15)
  test15$Year=2015
  
  test16=as.data.frame(Destatis16[Destatis16$X.6 == "Düsseldorf, Stadt",])
  test16[17] <- NULL
  test16[17] <- NULL
  test16 <- test16 %>% mutate_all(na_if,"")
  names(test16)[1]="number"
  test16=na.omit(test16)
  test16$Year=2016
  
  test17=as.data.frame(Destatis17[Destatis17$X.6 == "Düsseldorf, Stadt",])
  test17[17] <- NULL
  test17[17] <- NULL
  test17 <- test17 %>% mutate_all(na_if,"")
  names(test17)[1]="number"
  test17=na.omit(test17)
  test17$Year=2017
  
  test18=as.data.frame(Destatis18[Destatis18$X.6 == "Düsseldorf, Stadt",])
  test18[17] <- NULL
  test18[17] <- NULL
  test18 <- test18 %>% mutate_all(na_if,"")
  names(test18)[1]="number"
  test18=na.omit(test18)
  test18$Year=2018
  
  test19=as.data.frame(Destatis19[Destatis19$X.6 == "Düsseldorf, Stadt",])
  test19[17] <- NULL
  test19[17] <- NULL
  test19 <- test19 %>% mutate_all(na_if,"")
  names(test19)[1]="number"
  test19=na.omit(test19)
  test19$Year=2019
  
  test20=as.data.frame(Destatis20[Destatis20$X.6 == "Düsseldorf, Stadt",])
  test20[17] <- NULL
  test20[17] <- NULL
  test20 <- test20 %>% mutate_all(na_if,"")
  names(test20)[1]="number"
  test20=na.omit(test20)
  test20$Year=2020
  
  test21=as.data.frame(Destatis21[Destatis21$X.6 == "Düsseldorf, Stadt",])
  test21[17] <- NULL
  test21[17] <- NULL
  test21 <- test21 %>% mutate_all(na_if,"")
  names(test21)[1]="number"
  test21=na.omit(test21)
  test21$Year=2021
  
  test=rbind(test12,test13)
  test=rbind(test,test14)
  test=rbind(test,test15)
  test=rbind(test,test16)
  test=rbind(test,test17)
  test=rbind(test,test18)
  test=rbind(test,test19)
  test=rbind(test,test20)
  test=rbind(test,test21)
  
  test$X.7 = gsub(" ", "", test$X.7)
  test$X.7 = gsub(",", ".", test$X.7)
  test$X.7 = as.numeric(test$X.7)
  names(test)[9]="Area"
  
  test$X.8 = gsub(" ", "", test$X.8)
  test$X.8 = as.numeric(test$X.8)
  names(test)[10]="Inhabitants"
  
  test$X.9 = gsub(" ", "", test$X.9)
  test$X.9 = as.numeric(test$X.9) / test$Inhabitants
  names(test)[11]="Male_Ratio"
  
  #City Longitude and Latidtude
  
  test$X.13 = gsub(" ", "", test$X.13)
  test$X.13 = gsub(",", ".", test$X.13)
  test$X.13 = as.numeric(test$X.13)
  names(test)[15]="City_Lon"
  
  test$X.14 = gsub(" ", "", test$X.14)
  test$X.14 = gsub(",", ".", test$X.14)
  test$X.14 = as.numeric(test$X.14)
  names(test)[16]="City_Lat"
  
  names(test)[18]="Density"
  
  test$number <- NULL
  test$X <- NULL
  test$X.1 <- NULL
  test$X.2 <- NULL
  test$X.3 <- NULL
  test$X.4 <- NULL
  test$X.5 <- NULL
  test$X.6 <- NULL
  test$X.10 <- NULL
  test$X.11 <- NULL
  test$X.12 <- NULL
  test$X.17 <- NULL
  
  rawData = merge(x = rawData,y = test,
                  by = c("Year"),
                  all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #calculate distance to city center for every station
  
  #create a matrix, that later will contaion needed information
  distmat=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance and add this in a data frame
  for(i in 1:nlevels(as.factor(rawData$Station))){
    print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    #calculate distance for station
    dist=distm(c(d$Lon[i],d$Lat[i]), c(d$City_Lon[i],d$City_Lat[i]), fun=distGeo)
    print(paste("Distance from",d[1,8]," station to city center is",dist,"meters"))
    
    distmat[i,1]=d[1,8]
    distmat[i,2]=dist
    
    rm(d)
  }
  
  distmat=as.data.frame(distmat)
  names(distmat)[1]="Station"
  names(distmat)[2]="Distance_to_Center"
  distmat$Distance_to_Center=as.numeric(distmat$Distance_to_Center)
  
  rawData = merge(x = rawData,y = distmat,
                  by = c("Station"),
                  all = FALSE)
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  
  #Reading POI from Open Street Map________________________________________________________________________________________________________________________________________-
  #Using the overpass API 
  
  #install the osmdata, sf, tidyverse and ggmap package
  if(!require("osmdata")) install.packages("osmdata")
  if(!require("tidyverse")) install.packages("tidyverse")
  if(!require("sf")) install.packages("sf")
  if(!require("ggmap")) install.packages("ggmap")
  
  #load packages
  library(tidyverse)
  library(osmdata)
  library(sf)
  library(ggmap)
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("amenity", "cinema")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    print(cinema$osm_points$name[i])
    print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 1000)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 3000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestCinema"
  distmat_closest$ClosestCinema=as.numeric(distmat_closest$ClosestCinema)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Cinemas1kmRadius"
  distmat_1kmradius$Cinemas1kmRadius=as.numeric(distmat_1kmradius$Cinemas1kmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Cinemas3kmRadius"
  distmat_3kmradius$Cinemas3kmRadius=as.numeric(distmat_3kmradius$Cinemas3kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get all schools________________________________________________________________
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("amenity", "school")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSchool"
  distmat_closest$ClosestSchool=as.numeric(distmat_closest$ClosestSchool)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Schools500mmRadius"
  distmat_1kmradius$Schools500mmRadius=as.numeric(distmat_1kmradius$Schools500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Schools2kmRadius"
  distmat_3kmradius$Schools2kmRadius=as.numeric(distmat_3kmradius$Schools2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get university Buildings_______________________________________________________
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("amenity", "university")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestUniBuild"
  distmat_closest$ClosestUniBuild=as.numeric(distmat_closest$ClosestUniBuild)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="UniBuild500mmRadius"
  distmat_1kmradius$UniBuild500mmRadius=as.numeric(distmat_1kmradius$UniBuild500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="UniBuild2kmRadius"
  distmat_3kmradius$UniBuild2kmRadius=as.numeric(distmat_3kmradius$UniBuild2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  
  #Use Coordinates of Shops and Supermarkets___________________________________________________________________________________
  
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("shop", "supermarket")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSuperMarket"
  distmat_closest$ClosestSuperMarket=as.numeric(distmat_closest$ClosestSuperMarket)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="SuperMarket500mmRadius"
  distmat_1kmradius$SuperMarket500mmRadius=as.numeric(distmat_1kmradius$SuperMarket500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="SuperMarket1kmRadius"
  distmat_3kmradius$SuperMarket1kmRadius=as.numeric(distmat_3kmradius$SuperMarket1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  available_tags("shop")
  
  #Clothingshops__________________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("shop", "clothes")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestClothesShop"
  distmat_closest$ClosestClothesShop=as.numeric(distmat_closest$ClosestClothesShop)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="ClothesShop500mmRadius"
  distmat_1kmradius$ClothesShop500mmRadius=as.numeric(distmat_1kmradius$ClothesShop500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="ClothesShop2kmRadius"
  distmat_3kmradius$ClothesShop2kmRadius=as.numeric(distmat_3kmradius$ClothesShop2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  available_tags("amenity")
  #Get Data about public transport by OSM____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("highway", "bus_stop")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestBusStop"
  distmat_closest$ClosestBusStop=as.numeric(distmat_closest$ClosestBusStop)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="BusStop250mmRadius"
  distmat_1kmradius$BusStop250mmRadius=as.numeric(distmat_1kmradius$BusStop250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="BusStop1kmRadius"
  distmat_3kmradius$BusStop1kmRadius=as.numeric(distmat_3kmradius$BusStop1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Crossing Signals_______________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("highway", "traffic_signals")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSignals"
  distmat_closest$ClosestSignals=as.numeric(distmat_closest$ClosestSignals)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Signals250mmRadius"
  distmat_1kmradius$Signals250mmRadius=as.numeric(distmat_1kmradius$Signals250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Signals1kmRadius"
  distmat_3kmradius$Signals1kmRadius=as.numeric(distmat_3kmradius$Signals1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Crossing Unmarked_______________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("crossing", "unmarked")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestUnmCross"
  distmat_closest$ClosestUnmCross=as.numeric(distmat_closest$ClosestUnmCross)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="UnmCross250mmRadius"
  distmat_1kmradius$UnmCross250mmRadius=as.numeric(distmat_1kmradius$UnmCross250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="UnmCross1kmRadius"
  distmat_3kmradius$UnmCross1kmRadius=as.numeric(distmat_3kmradius$UnmCross1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get Tram Stattions_____________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("railway", "tram_stop")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 250)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 1000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestTram"
    distmat_closest$ClosestTram=as.numeric(distmat_closest$ClosestTram)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="Tram250mmRadius"
    distmat_1kmradius$Tram250mmRadius=as.numeric(distmat_1kmradius$Tram250mmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="Tram1kmRadius"
    distmat_3kmradius$Tram1kmRadius=as.numeric(distmat_3kmradius$Tram1kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTram=50000
    rawData$Tram250mmRadius=0
    rawData$Tram1kmRadius=0
  }
  
  summary(rawData)
  
  
  #Get Subway Entrance_____________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("railway", "subway_entrance")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 250)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 1000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestSubway"
    distmat_closest$ClosestSubway=as.numeric(distmat_closest$ClosestSubway)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="Subway250mmRadius"
    distmat_1kmradius$Subway250mmRadius=as.numeric(distmat_1kmradius$Subway250mmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="Subway1kmRadius"
    distmat_3kmradius$Subway1kmRadius=as.numeric(distmat_3kmradius$Subway1kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestSubway=50000
    rawData$Subway250mmRadius=0
    rawData$Subway1kmRadius=0
  }
  
  summary(rawData)
  
  #Railway Station operated by the DB Netz AG_____________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("railway", "station")%>%
    add_osm_feature("operator", "DB Netz AG")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 1000)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 3000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestTrainS"
    distmat_closest$ClosestTrainS=as.numeric(distmat_closest$ClosestTrainS)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="TrainS1kmRadius"
    distmat_1kmradius$TrainS1kmRadius=as.numeric(distmat_1kmradius$TrainS1kmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="TrainS3kmRadius"
    distmat_3kmradius$TrainS3kmRadius=as.numeric(distmat_3kmradius$TrainS3kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTrainS=50000
    rawData$TrainS1kmRadius=0
    rawData$TrainS3kmRadius=0
  }
  
  summary(rawData)
  
  
  #Bike Shops_____________________________________________________________
  
  q <- getbb("Düsseldorf") %>%
    opq() %>%
    add_osm_feature("shop", "bicycle")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 1000)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 3000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestBikeShop"
    distmat_closest$ClosestBikeShop=as.numeric(distmat_closest$ClosestBikeShop)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="BikeShop1kmRadius"
    distmat_1kmradius$BikeShop1kmRadius=as.numeric(distmat_1kmradius$BikeShop1kmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="BikeShop3kmRadius"
    distmat_3kmradius$BikeShop3kmRadius=as.numeric(distmat_3kmradius$BikeShop3kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    #rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTrainS=50000
    rawData$TrainS1kmRadius=0
    rawData$TrainS3kmRadius=0
  }
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #RoadNetwork
  
  city="Düsseldorf"
  
  q1 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "cycleway")
  q2 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "residential")
  q3 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "living_street")
  q4 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "path")
  q5 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "secondary")
  q6 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "primary")
  q7 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("bridge", "yes")
  
  #str(q1) #query structure
  
  cycleways <- osmdata_sf(q1)
  residential <- osmdata_sf(q2)
  living_street <- osmdata_sf(q3)
  path <- osmdata_sf(q4)
  secondary <- osmdata_sf(q5)
  primary <- osmdata_sf(q6)
  bridge <- osmdata_sf(q7)
  
  dist_mat=as.data.frame(levels(as.factor(rawData$Station)))
  dist_mat$cycleways = 9999
  dist_mat$residential = 9999
  dist_mat$living_street = 9999
  dist_mat$path = 9999
  dist_mat$secondary = 9999
  dist_mat$primary = 9999
  
  bool_mat=as.data.frame(levels(as.factor(rawData$Station)))
  bool_mat$cycleways = 0
  bool_mat$residential = 0
  bool_mat$living_street = 0
  bool_mat$path = 0
  bool_mat$secondary = 0
  bool_mat$primary = 0
  
  bridge_mat=levels(as.factor(rawData$Station))
  bridge_mat=as.data.frame(bridge_mat)
  bridge_mat$ClosestBridge = 9999
  bridge_mat$isBridge = 0
  
  i=1
  
  for(i in 1:nlevels(as.factor(rawData$Station))){
    
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    DT = as.data.frame(cbind(d$Lon[i],d$Lat[i]))
    names(DT)[1]="long1"
    names(DT)[2]="lat1"
    DT2 = st_as_sf(DT, coords = c("long1","lat1"))
    DT2 <- st_set_crs(DT2, 4269)
    st_crs(DT2) <- 4269 
    DT3cycleways = st_transform(cycleways$osm_lines$geometry,4269)
    DT3residential = st_transform(residential$osm_lines$geometry,4269)
    DT3living_street = st_transform(living_street$osm_lines$geometry,4269)
    DT3path = st_transform(path$osm_lines$geometry,4269)
    DT3secondary = st_transform(secondary$osm_lines$geometry,4269)
    DT3primary = st_transform(primary$osm_lines$geometry,4269)
    DT3bridge = st_transform(bridge$osm_lines$geometry,4269)
    
    dist_mat$cycleways[i]=min(st_distance(DT2$geometry, DT3cycleways))
    dist_mat$residential[i]=min(st_distance(DT2$geometry, DT3residential))
    dist_mat$living_street[i]=min(st_distance(DT2$geometry, DT3living_street))
    dist_mat$path[i]=min(st_distance(DT2$geometry, DT3path))
    dist_mat$secondary[i]=min(st_distance(DT2$geometry, DT3secondary))
    dist_mat$primary[i]=min(st_distance(DT2$geometry, DT3primary))
    
    bridge_mat$ClosestBridge[i]=min(st_distance(DT2$geometry, DT3primary))
    if(bridge_mat$ClosestBridge[i]<5){bridge_mat$isBridge[i]=1}
    
    if(dist_mat$cycleways[i]<5){bool_mat$cycleways[i]=1}
    if(dist_mat$residential[i]<5){bool_mat$residential[i]=1}
    if(dist_mat$living_street[i]<5){bool_mat$living_street[i]=1}
    if(dist_mat$path[i]<5){bool_mat$path[i]=1}
    if(dist_mat$secondary[i]<5){bool_mat$secondary[i]=1}
    if(dist_mat$primary[i]<5){bool_mat$primary[i]=1}
  }
  
  
  names(bool_mat)[1]="Station"
  names(bridge_mat)[1]="Station"
  
  rawData = merge(x = rawData,y = bool_mat,
                  by = c("Station"),
                  all = FALSE)
  
  
  rawData = merge(x = rawData,y = bridge_mat,
                  by = c("Station"),
                  all = FALSE)
  
  summary(rawData)
  
  
  citation ("osmdata")
  
  
  rm(list=setdiff(ls(), "rawData"))
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(rawData,paste(toString(rawData$Town[1]),".csv",sep=""))