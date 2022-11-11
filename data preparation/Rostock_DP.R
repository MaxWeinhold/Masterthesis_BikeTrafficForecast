#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Rostock

library(plyr)
library(dplyr)
library(lubridate)
library(geosphere)#package for calculating distance using longitude and latitude

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
  
  pH=publicHolidays[publicHolidays$MVP %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "MVP",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Rostock")
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
  write.csv(rawData,"Rostock.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(3.6,3.6,3.6,3.6,3.7,3.7,3.9,3.9,3.9,3.9)
  
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
  
  title=", Hansestadt" #This differs, there are cities and also hanseatic cities
  
  test12=as.data.frame(Destatis12[Destatis12$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test12[17] <- NULL
  test12[17] <- NULL
  test12 <- test12 %>% mutate_all(na_if,"")
  names(test12)[1]="number"
  test12=na.omit(test12)
  test12$Year=2012
  
  test13=as.data.frame(Destatis13[Destatis13$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test13[17] <- NULL
  test13[17] <- NULL
  test13 <- test13 %>% mutate_all(na_if,"")
  names(test13)[1]="number"
  test13=na.omit(test13)
  test13$Year=2013
  
  test14=as.data.frame(Destatis14[Destatis14$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test14[17] <- NULL
  test14[17] <- NULL
  test14 <- test14 %>% mutate_all(na_if,"")
  names(test14)[1]="number"
  test14=na.omit(test14)
  test14$Year=2014
  
  test15=as.data.frame(Destatis15[Destatis15$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test15[17] <- NULL
  test15[17] <- NULL
  test15 <- test15 %>% mutate_all(na_if,"")
  names(test15)[1]="number"
  test15=na.omit(test15)
  test15$Year=2015
  
  test16=as.data.frame(Destatis16[Destatis16$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test16[17] <- NULL
  test16[17] <- NULL
  test16 <- test16 %>% mutate_all(na_if,"")
  names(test16)[1]="number"
  test16=na.omit(test16)
  test16$Year=2016
  
  test17=as.data.frame(Destatis17[Destatis17$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test17[17] <- NULL
  test17[17] <- NULL
  test17 <- test17 %>% mutate_all(na_if,"")
  names(test17)[1]="number"
  test17=na.omit(test17)
  test17$Year=2017
  
  test18=as.data.frame(Destatis18[Destatis18$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test18[17] <- NULL
  test18[17] <- NULL
  test18 <- test18 %>% mutate_all(na_if,"")
  names(test18)[1]="number"
  test18=na.omit(test18)
  test18$Year=2018
  
  test19=as.data.frame(Destatis19[Destatis19$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test19[17] <- NULL
  test19[17] <- NULL
  test19 <- test19 %>% mutate_all(na_if,"")
  names(test19)[1]="number"
  test19=na.omit(test19)
  test19$Year=2019
  
  test20=as.data.frame(Destatis20[Destatis20$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test20[17] <- NULL
  test20[17] <- NULL
  test20 <- test20 %>% mutate_all(na_if,"")
  names(test20)[1]="number"
  test20=na.omit(test20)
  test20$Year=2020
  
  test21=as.data.frame(Destatis21[Destatis21$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
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
  