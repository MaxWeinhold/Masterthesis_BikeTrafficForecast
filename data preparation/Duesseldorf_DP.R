#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Duesseldorf

library(plyr)

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
  countingData2012 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2012_3.csv",sep=";")
  countingData2013 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2013_1.csv",sep=";")
  countingData2014 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2014_1.csv",sep=";")
  countingData2015 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2015_1.csv",sep=";")
  countingData2016 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2016_1.csv",sep=";")
  countingData2017 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2017_1.csv",sep=";")
  countingData2018 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2018_1.csv",sep=";")
  countingData2019 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2019_1.csv",sep=";")
  countingData2020 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2020.csv",sep=";")
  countingData2021 = read.csv(file = "Jahresübersicht aller Dauerzählstellen 2021_2.csv",sep=";")
  names(countingData2021)
  
#Change count frequency to hourly data----------------------------------------------
  
  countingData2012$Uhrzeit=paste(countingData2012$Datum,countingData2012$Uhrzeit, sep=" ")
  countingData2012$Uhrzeit=cut(strptime(countingData2012$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2012=ddply(countingData2012,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker_Allee_IN),
                         Bilker.Allee.OUT=sum(Bilker_Allee_OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher_Deich_Ost_stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher_Deich_west_stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer_einwaerts_nach_TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer_einwaerts_vor_TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer_stadtauswaerts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer_stadtauswaertss),
                         Koe_Steinstr=sum(Koe_Steinstr),
                         Lohauser_Deich=sum(Lohauser_Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann_Ufer.Totem_01),
                         OKB_Nord=sum(OKB_Nord),
                         OKB_Sued=sum(OKB_Sued))
  
  countingData2013$Uhrzeit=paste(countingData2013$Datum,countingData2013$Uhrzeit, sep=" ")
  countingData2013$Uhrzeit=cut(strptime(countingData2013$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2013=ddply(countingData2013,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwÃ.rts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwÃ.rts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwÃ.rts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwÃ.rts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswÃ.rts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswÃ.rtss),
                         Koe_Steinstr=sum(KÃ..Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.SÃ.d))
  
  
  countingData2014$Uhrzeit=paste(countingData2014$Datum,countingData2014$Uhrzeit, sep=" ")
  countingData2014$Uhrzeit=cut(strptime(countingData2014$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2014=ddply(countingData2014,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwÃ.rts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwÃ.rts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
                         Koblenzer_einwaerts_nach_TLS=sum(Koblenzer.einwÃ.rts.nach.TLS),
                         Koblenzer_einwaerts_vor_TLS=sum(Koblenzer.einwÃ.rts.vor.TLS),
                         Koblenzer_stadtauswaerts=sum(Koblenzer.stadtauswÃ.rts),
                         #Koblenzer_stadtauswaertss=sum(Koblenzer.stadtauswÃ.rtss),
                         Koe_Steinstr=sum(KÃ..Steinstr),
                         Lohauser_Deich=sum(Lohauser.Deich),
                         Mannesmann_Ufer.Totem_01=sum(Mannesmann.Ufer.Totem.01),
                         OKB_Nord=sum(OKB.Nord),
                         OKB_Sued=sum(OKB.SÃ.d))
  
  countingData2015$Uhrzeit=paste(countingData2015$Datum,countingData2015$Uhrzeit, sep=" ")
  countingData2015$Uhrzeit=cut(strptime(countingData2015$Uhrzeit,"%d.%m.%Y %H:%M"),"hour")
  countingData2015=ddply(countingData2015,.(Uhrzeit),summarize,Bilker_Allee_IN=sum(Bilker.Allee.IN),
                         Bilker.Allee.OUT=sum(Bilker.Allee.OUT),
                         Christophstr=sum(Christophstr.),
                         Elisabethstr=sum(Elisabethstr),
                         Fleher_Deich_Ost_stromaufwaerts=sum(Fleher.Deich.Ost.stromaufwaerts),
                         Fleher_Deich_west_stromabwaerts=sum(Fleher.Deich.west.stromabwaerts),
                         Friedrichstr=sum(Friedrichstr),
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
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
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
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
                         Kirchfeldstrasse=sum(KirchfeldstraÃYe),
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
  
  Data_bilkeralleein$Town = "Duesseldorf"
  Data_bilkeralleeout$Town = "Duesseldorf"
  Data_Christophstr$Town = "Duesseldorf"
  Data_Elisabethstr$Town = "Duesseldorf"
  Data_Fleher_Deich_Ost$Town = "Duesseldorf"
  Data_Fleher_Deich_west$Town = "Duesseldorf"
  Data_Friedrichstr$Town = "Duesseldorf"
  Data_Kirchfeldstrasse$Town = "Duesseldorf"
  Data_Koblenzer_einwaerts_nach_TLS$Town = "Duesseldorf"
  Data_Koblenzer_einwaerts_vor_TLS$Town = "Duesseldorf"
  Data_Koblenzer_stadtauswaerts$Town = "Duesseldorf"
  Data_Koe_Steinstr$Town = "Duesseldorf"
  Data_Lohauser_Deich$Town = "Duesseldorf"
  Data_Mannesmann_Ufer$Town = "Duesseldorf"
  Data_OKB_Nord$Town = "Duesseldorf"
  Data_OKB_Sued$Town = "Duesseldorf"
  
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
  
  Ddorf_rawData=rbind(Data_bilkerallee,Data_Christophstr)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Elisabethstr)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Fleher_Deich)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Friedrichstr)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Kirchfeldstrasse)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Koblenzer)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Koblenzer_stadtauswaerts)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Koe_Steinstr)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Lohauser_Deich)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_Mannesmann_Ufer)
  Ddorf_rawData=rbind(Ddorf_rawData,Data_OKB)
  
  Ddorf_rawData$Value=as.numeric(Ddorf_rawData$Value)
  summary(Ddorf_rawData)