#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model Map Projection: Projection Set Generator

if(!require("beepr")) install.packages("beepr")
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(beepr)
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(lubridate)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_1.csv",sep=",", encoding="ISO-8859-1")

beep("coin")

Variables_you_need = names(BikeData)
summaryBikeData = summary(BikeData)
rm(BikeData)

#Choose Values you are interested in -------------------------------------------

Year = 2023
Town = "Münster"
ProjectionData = as.data.frame(cbind(Year,Town))

#ProjectionData$Station = "Projection"

Months = 6
ProjectionData = merge(x = ProjectionData,y = Months,all = FALSE)
names(ProjectionData)[3] = "Months"

Day = c(14,15)
ProjectionData = merge(x = ProjectionData,y = Day,all = FALSE)
names(ProjectionData)[4] = "Day"

Hour = c(12:14)
ProjectionData = merge(x = ProjectionData,y = Hour,all = FALSE)
names(ProjectionData)[5] = "Hour"

ProjectionData$Value = 1

Bundesland = "NRW"

rm(list=setdiff(ls(), c("ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

#Ad the different Variables-----------------------------------------------------

ProjectionData$Timestamp = as.POSIXlt(paste(ProjectionData$Day,".",ProjectionData$Months,".",ProjectionData$Year,sep=""),format="%d.%m.%Y")

ProjectionData$Oneway = FALSE
ProjectionData$Summer = ifelse(ProjectionData$Months == "6" | ProjectionData$Months == "7"| ProjectionData$Months == "8", 1, 0)
ProjectionData$Winter = ifelse(ProjectionData$Months == "12" | ProjectionData$Months == "1"| ProjectionData$Months == "2", 1, 0)
ProjectionData$Weekday	= format(as.POSIXlt(ProjectionData$Timestamp),"%a")
ProjectionData$Weekend <- ifelse(ProjectionData$Weekday == "So" | ProjectionData$Weekday == "Sa", 1, 0)
ProjectionData$Night = ifelse(ProjectionData$Hour<7,1,0)

#Load data for public holidays
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
publicHolidays = read.csv(file = "Feiertage.csv",sep=";")

pH=publicHolidays[publicHolidays$NRW %in% TRUE,]
ProjectionData$publicHoliday = ifelse(as.Date(ProjectionData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)

#Load data for school holidays
schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")

sH=schoolHolidays[schoolHolidays$Bundesland %in% Bundesland,]
x <- vector()
for(i in 1:length(sH$Startdatum)){
  x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
}
ProjectionData$schoolHoliday = ifelse(as.numeric(as.Date(ProjectionData$Timestamp)) %in% x,1,0)

summary(ProjectionData)

#Add Weather Data (Source: Deutscher Wetterdienst)------------------------------

#Import Weather Data
setwd(paste("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/",Town,sep=""))
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

if(Year %in% levels(as.factor(Weather_Temperature$Year))){
  Weather_Wind = Weather_Wind[(Weather_Wind$Year==Year),]
  Weather_CloudCover = Weather_CloudCover[(Weather_CloudCover$Year==Year),]
  Weather_Humidity = Weather_Humidity[(Weather_Humidity$Year==Year),]
  Weather_Rain = Weather_Rain[(Weather_Rain$Year==Year),]
  Weather_Temperature = Weather_Temperature[(Weather_Temperature$Year==Year),]
}else {
  Weather_Wind = Weather_Wind[(Weather_Wind$Year==2022),]
  Weather_CloudCover = Weather_CloudCover[(Weather_CloudCover$Year==2022),]
  Weather_Humidity = Weather_Humidity[(Weather_Humidity$Year==2022),]
  Weather_Rain = Weather_Rain[(Weather_Rain$Year==2022),]
  Weather_Temperature = Weather_Temperature[(Weather_Temperature$Year==2022),]
  Weather_Wind$Year = Year
  Weather_CloudCover$Year = Year
  Weather_Humidity$Year = Year
  Weather_Rain$Year = Year
  Weather_Temperature$Year = Year
}

ProjectionData = merge(x = ProjectionData,y = Weather_Wind,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

ProjectionData=na.omit(ProjectionData)
rm(Weather_Wind)

ProjectionData = merge(x = ProjectionData,y = Weather_CloudCover,
                by = c("Year","Months","Day","Hour"),
                all = TRUE)

ProjectionData=na.omit(ProjectionData)
rm(Weather_CloudCover)

ProjectionData = merge(x = ProjectionData,y = Weather_Humidity,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

ProjectionData=na.omit(ProjectionData)
rm(Weather_Humidity)

ProjectionData = merge(x = ProjectionData,y = Weather_Rain,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

ProjectionData=na.omit(ProjectionData)
rm(Weather_Rain)

ProjectionData = merge(x = ProjectionData,y = Weather_Temperature,
                by = c("Year","Months","Day","Hour"),
                all = FALSE)

ProjectionData=na.omit(ProjectionData)
rm(Weather_Temperature)
summary(ProjectionData)
rm(list=setdiff(ls(), c("ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

#Destatis Data -----------------------------------------------------------------

ProjectionData$ADFC_Index = 3.2

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

test12=as.data.frame(Destatis12[Destatis12$X.6 == paste(Town,title,sep=""),])
test12[17] <- NULL
test12[17] <- NULL
test12 <- test12 %>% mutate_all(na_if,"")
names(test12)[1]="number"
test12=na.omit(test12)
test12$Year=2012

test13=as.data.frame(Destatis13[Destatis13$X.6 == paste(Town,title,sep=""),])
test13[17] <- NULL
test13[17] <- NULL
test13 <- test13 %>% mutate_all(na_if,"")
names(test13)[1]="number"
test13=na.omit(test13)
test13$Year=2013

test14=as.data.frame(Destatis14[Destatis14$X.6 == paste(Town,title,sep=""),])
test14[17] <- NULL
test14[17] <- NULL
test14 <- test14 %>% mutate_all(na_if,"")
names(test14)[1]="number"
test14=na.omit(test14)
test14$Year=2014

test15=as.data.frame(Destatis15[Destatis15$X.6 == paste(Town,title,sep=""),])
test15[17] <- NULL
test15[17] <- NULL
test15 <- test15 %>% mutate_all(na_if,"")
names(test15)[1]="number"
test15=na.omit(test15)
test15$Year=2015

test16=as.data.frame(Destatis16[Destatis16$X.6 == paste(Town,title,sep=""),])
test16[17] <- NULL
test16[17] <- NULL
test16 <- test16 %>% mutate_all(na_if,"")
names(test16)[1]="number"
test16=na.omit(test16)
test16$Year=2016

test17=as.data.frame(Destatis17[Destatis17$X.6 == paste(Town,title,sep=""),])
test17[17] <- NULL
test17[17] <- NULL
test17 <- test17 %>% mutate_all(na_if,"")
names(test17)[1]="number"
test17=na.omit(test17)
test17$Year=2017

test18=as.data.frame(Destatis18[Destatis18$X.6 == paste(Town,title,sep=""),])
test18[17] <- NULL
test18[17] <- NULL
test18 <- test18 %>% mutate_all(na_if,"")
names(test18)[1]="number"
test18=na.omit(test18)
test18$Year=2018

test19=as.data.frame(Destatis19[Destatis19$X.6 == paste(Town,title,sep=""),])
test19[17] <- NULL
test19[17] <- NULL
test19 <- test19 %>% mutate_all(na_if,"")
names(test19)[1]="number"
test19=na.omit(test19)
test19$Year=2019

test20=as.data.frame(Destatis20[Destatis20$X.6 == paste(Town,title,sep=""),])
test20[17] <- NULL
test20[17] <- NULL
test20 <- test20 %>% mutate_all(na_if,"")
names(test20)[1]="number"
test20=na.omit(test20)
test20$Year=2020

test21=as.data.frame(Destatis21[Destatis21$X.6 == paste(Town,title,sep=""),])
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

if(Year %in% levels(as.factor(test$Year))){
  test = test[(test$Year==Year),]
}else {
  
  p = as.data.frame(c(2012:2030))
  names(p)[1]="Year"
  p$Year2 = p$Year^2
  p$Year3 = p$Year^3
  
  model1 <- lm(log(Inhabitants) ~ Year, data=test)
  predict1 <- as.data.frame(predict(model1,p))
  predict1$Year = p$Year
  names(predict1)[1]="Inhabitants"
  predict1$Inhabitants = exp(predict1$Inhabitants)
  
  model2 <- lm(log(Male_Ratio) ~ Year, data=test)
  predict2 <- as.data.frame(predict(model2,p))
  predict2$Year = p$Year
  names(predict2)[1]="Male_Ratio "
  predict2$Male_Ratio  = exp(predict2$Male_Ratio )
  
  plot1 = ggplot(data = test,aes(x = Year, y = Inhabitants)) +
    geom_point(size=1.5)+
    labs(title = paste("Prognose zur Stadtentwicklung:",Town),color="Formel:") +
    xlab("Jahr") +
    ylab("Einwohneranzahl") +
    theme_bw() +
    xlim(2012, 2030) +
    geom_line(data = predict1, aes(x = Year, y = Inhabitants),color = "red", size = 1.5)
  
  plot2 = ggplot(data = test,aes(x = Year, y = Male_Ratio)) +
    geom_point(size=1.5)+
    labs(title = paste("Prognose zur Stadtentwicklung:",Town),color="Formel:") +
    xlab("Jahr") +
    ylab("Geschlechterverhältnis") +
    theme_bw() +
    xlim(2012, 2030) +
    geom_line(data = predict2, aes(x = Year, y = Male_Ratio),color = "red", size = 1.5)
  
  
  test[nrow(test) + 1,] = c(test$Area[10],predict1[predict1$Year == Year, ]$Inhabitants,predict2[predict1$Year == Year, ]$Male_Ratio,test$City_Lon[10],test$City_Lat[10],"dicht besiedelt",Year)
  
  setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
  png(file=paste("Predictions_",Town,"_plot01.png",sep=""),width=800, height=800)
  plot1
  dev.off()
  
  png(file=paste("Predictions_",Town,"_plot02.png",sep=""),width=800, height=800)
  plot2
  dev.off()
  
}

ProjectionData = merge(x = ProjectionData,y = test,
                by = c("Year"),
                all = FALSE)

ProjectionData$Area = as.numeric(ProjectionData$Area)
ProjectionData$Inhabitants = as.numeric(ProjectionData$Inhabitants)
ProjectionData$Male_Ratio = as.numeric(ProjectionData$Male_Ratio)
ProjectionData$City_Lon = as.numeric(ProjectionData$City_Lon)
ProjectionData$City_Lat = as.numeric(ProjectionData$City_Lat)
summary(ProjectionData)

rm(list=setdiff(ls(), c("ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

Variables_you_need

#Load data (source: Destatis)

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Destatis")
Destatis12 = read.csv(file = "Altersgruppen 2012.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis13 = read.csv(file = "Altersgruppen 2013.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis14 = read.csv(file = "Altersgruppen 2014.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis15 = read.csv(file = "Altersgruppen 2015.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis16 = read.csv(file = "Altersgruppen 2016.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis17 = read.csv(file = "Altersgruppen 2017.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis18 = read.csv(file = "Altersgruppen 2018.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis19 = read.csv(file = "Altersgruppen 2019.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis20 = read.csv(file = "Altersgruppen 2020.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis21 = read.csv(file = "Altersgruppen 2021.csv",sep=";", encoding="ISO-8859-1", skip = 5)
Destatis22 = read.csv(file = "Altersgruppen 2021.csv",sep=";", encoding="ISO-8859-1", skip = 5)

Altersgruppen = rbind(Destatis12,Destatis13)
Altersgruppen = rbind(Altersgruppen,Destatis14)
Altersgruppen = rbind(Altersgruppen,Destatis15)
Altersgruppen = rbind(Altersgruppen,Destatis16)
Altersgruppen = rbind(Altersgruppen,Destatis17)
Altersgruppen = rbind(Altersgruppen,Destatis18)
Altersgruppen = rbind(Altersgruppen,Destatis19)
Altersgruppen = rbind(Altersgruppen,Destatis20)
Altersgruppen = rbind(Altersgruppen,Destatis21)
names(Altersgruppen)
Altersgruppen$X.1 = NULL
Altersgruppen$Timestamp=as.POSIXlt(Altersgruppen$X,format="%d.%m.%Y")
Altersgruppen$Year	= as.numeric(format(as.POSIXlt(Altersgruppen$Timestamp), format = "%Y"))
Altersgruppen$Timestamp = NULL
names(Altersgruppen)[2]="Town"
Altersgruppen$X = NULL
for(i in 1: nrow(Altersgruppen)){
  if(Altersgruppen$Town[i] == "Berlin, kreisfreie Stadt"){Altersgruppen$Town[i] = "Berlin"}
  else if(Altersgruppen$Town[i] == "Bochum, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bochum"}
  else if(Altersgruppen$Town[i] == "Bonn, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bonn"}
  else if(Altersgruppen$Town[i] == "Bremen, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bremen"}
  else if(Altersgruppen$Town[i] == "Darmstadt, kreisfreie Stadt"){Altersgruppen$Town[i] = "Darmstadt"}
  else if(Altersgruppen$Town[i] == "Düsseldorf, kreisfreie Stadt"){Altersgruppen$Town[i] = "Düsseldorf"}
  else if(Altersgruppen$Town[i] == "Hamburg, kreisfreie Stadt"){Altersgruppen$Town[i] = "Hamburg"}
  else if(Altersgruppen$Town[i] == "Leipzig, kreisfreie Stadt"){Altersgruppen$Town[i] = "Leipzig"}
  else if(Altersgruppen$Town[i] == "Mannheim, kreisfreie Stadt"){Altersgruppen$Town[i] = "Mannheim"}
  else if(Altersgruppen$Town[i] == "München, kreisfreie Stadt"){Altersgruppen$Town[i] = "München"}
  else if(Altersgruppen$Town[i] == "Münster, kreisfreie Stadt"){Altersgruppen$Town[i] = "Münster"}
  else if(Altersgruppen$Town[i] == "Oberhausen, kreisfreie Stadt"){Altersgruppen$Town[i] = "Oberhausen"}
  else if(Altersgruppen$Town[i] == "Rostock, kreisfreie Stadt"){Altersgruppen$Town[i] = "Rostock"}
  else if(Altersgruppen$Town[i] == "Siegen-Wittgenstein, Landkreis"){Altersgruppen$Town[i] = "Siegen"}
  else if(Altersgruppen$Town[i] == "Erfurt, kreisfreie Stadt"){Altersgruppen$Town[i] = "Erfurt"}
  else if(Altersgruppen$Town[i] == "Tübingen, Landkreis"){Altersgruppen$Town[i] = "Tübingen"}
  else{Altersgruppen$Town[i] = NA}
}
Altersgruppen = na.omit(Altersgruppen)

names(Altersgruppen)
Altersgruppen$young18 = (as.numeric(Altersgruppen$unter.3.Jahre) +
                           as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
                           as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
                           as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
                           as.numeric(Altersgruppen$X15.bis.unter.18.Jahre)
)/as.numeric(Altersgruppen$Insgesamt) * 100
Altersgruppen$young25 = (as.numeric(Altersgruppen$unter.3.Jahre) +
                           as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
                           as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
                           as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
                           as.numeric(Altersgruppen$X15.bis.unter.18.Jahre) +
                           as.numeric(Altersgruppen$X18.bis.unter.20.Jahre) + 
                           as.numeric(Altersgruppen$X20.bis.unter.25.Jahre)
)/as.numeric(Altersgruppen$Insgesamt) * 100
Altersgruppen$young30 = (as.numeric(Altersgruppen$unter.3.Jahre) +
                           as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
                           as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
                           as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
                           as.numeric(Altersgruppen$X15.bis.unter.18.Jahre) +
                           as.numeric(Altersgruppen$X18.bis.unter.20.Jahre) + 
                           as.numeric(Altersgruppen$X20.bis.unter.25.Jahre) +
                           as.numeric(Altersgruppen$X25.bis.unter.30.Jahre)
)/as.numeric(Altersgruppen$Insgesamt) * 100
Altersgruppen$older40 = (as.numeric(Altersgruppen$X40.bis.unter.45.Jahre) +
                           as.numeric(Altersgruppen$X45.bis.unter.50.Jahre) +
                           as.numeric(Altersgruppen$X50.bis.unter.55.Jahre) +
                           as.numeric(Altersgruppen$X55.bis.unter.60.Jahre) + 
                           as.numeric(Altersgruppen$X60.bis.unter.65.Jahre) +
                           as.numeric(Altersgruppen$X65.bis.unter.75.Jahre) + 
                           as.numeric(Altersgruppen$X75.Jahre.und.mehr)
)/as.numeric(Altersgruppen$Insgesamt) * 100
Altersgruppen$older60 = (as.numeric(Altersgruppen$X60.bis.unter.65.Jahre) +
                           as.numeric(Altersgruppen$X65.bis.unter.75.Jahre) + 
                           as.numeric(Altersgruppen$X75.Jahre.und.mehr)
)/as.numeric(Altersgruppen$Insgesamt) * 100
Altersgruppen$Insgesamt = as.numeric(Altersgruppen$Insgesamt)

Altersgruppen$unter.3.Jahre = NULL
Altersgruppen$X3.bis.unter.6.Jahre = NULL
Altersgruppen$X6.bis.unter.10.Jahre = NULL
Altersgruppen$X10.bis.unter.15.Jahre = NULL
Altersgruppen$X15.bis.unter.18.Jahre = NULL
Altersgruppen$X18.bis.unter.20.Jahre = NULL
Altersgruppen$X20.bis.unter.25.Jahre = NULL
Altersgruppen$X25.bis.unter.30.Jahre = NULL
Altersgruppen$X25.bis.unter.30.Jahre = NULL
Altersgruppen$X30.bis.unter.35.Jahre = NULL
Altersgruppen$X35.bis.unter.40.Jahre = NULL
Altersgruppen$X40.bis.unter.45.Jahre = NULL
Altersgruppen$X45.bis.unter.50.Jahre = NULL
Altersgruppen$X50.bis.unter.55.Jahre = NULL
Altersgruppen$X55.bis.unter.60.Jahre = NULL
Altersgruppen$X60.bis.unter.65.Jahre = NULL
Altersgruppen$X65.bis.unter.75.Jahre = NULL
Altersgruppen$X75.Jahre.und.mehr = NULL

names(Altersgruppen)[2]="InhDestrict"

d = Altersgruppen[which(Altersgruppen$Year==2021),]
d$Year = 2022
Altersgruppen = rbind(Altersgruppen,d)

Altersgruppen = Altersgruppen[(Altersgruppen$Town==Town),]

if(Year %in% levels(as.factor(Altersgruppen$Year))){
  Altersgruppen = Altersgruppen[(Altersgruppen$Year==Year),]
}else {
  Altersgruppen = Altersgruppen[(Altersgruppen$Year==2022),]
  Altersgruppen$Year = Year
}

ProjectionData = merge(x = ProjectionData,y = Altersgruppen,
                 by = c("Year","Town"),
                 all = TRUE)

Immigrants = read.csv(file = "Auslaenderstatistik 12 bis 22.csv",sep=";", encoding="ISO-8859-1", skip = 7)
names(Immigrants)
Immigrants$Timestamp=as.POSIXlt(Immigrants$X31.12.2012,format="%d.%m.%Y")
Immigrants$Year	= as.numeric(format(as.POSIXlt(Immigrants$Timestamp), format = "%Y"))
Immigrants$X31.12.2012 = NULL
Immigrants$X01003 = NULL
Immigrants$X8510 = NULL
Immigrants$X8195 = NULL
Immigrants$Timestamp = NULL
names(Immigrants)[1]="Town"
names(Immigrants)[2]="Immigrants"
for(i in 1: nrow(Immigrants)){
  if(Immigrants$Town[i] == "Berlin, kreisfreie Stadt"){Immigrants$Town[i] = "Berlin"}
  else if(Immigrants$Town[i] == "Bochum, kreisfreie Stadt"){Immigrants$Town[i] = "Bochum"}
  else if(Immigrants$Town[i] == "Bonn, kreisfreie Stadt"){Immigrants$Town[i] = "Bonn"}
  else if(Immigrants$Town[i] == "Bremen, kreisfreie Stadt"){Immigrants$Town[i] = "Bremen"}
  else if(Immigrants$Town[i] == "Darmstadt, kreisfreie Stadt"){Immigrants$Town[i] = "Darmstadt"}
  else if(Immigrants$Town[i] == "Düsseldorf, kreisfreie Stadt"){Immigrants$Town[i] = "Düsseldorf"}
  else if(Immigrants$Town[i] == "Hamburg, kreisfreie Stadt"){Immigrants$Town[i] = "Hamburg"}
  else if(Immigrants$Town[i] == "Leipzig, kreisfreie Stadt"){Immigrants$Town[i] = "Leipzig"}
  else if(Immigrants$Town[i] == "Mannheim, kreisfreie Stadt"){Immigrants$Town[i] = "Mannheim"}
  else if(Immigrants$Town[i] == "München, kreisfreie Stadt"){Immigrants$Town[i] = "München"}
  else if(Immigrants$Town[i] == "Münster, kreisfreie Stadt"){Immigrants$Town[i] = "Münster"}
  else if(Immigrants$Town[i] == "Oberhausen, kreisfreie Stadt"){Immigrants$Town[i] = "Oberhausen"}
  else if(Immigrants$Town[i] == "Rostock, kreisfreie Stadt"){Immigrants$Town[i] = "Rostock"}
  else if(Immigrants$Town[i] == "Siegen-Wittgenstein, Landkreis"){Immigrants$Town[i] = "Siegen"}
  else if(Immigrants$Town[i] == "Erfurt, kreisfreie Stadt"){Immigrants$Town[i] = "Erfurt"}
  else if(Immigrants$Town[i] == "Tübingen, Landkreis"){Immigrants$Town[i] = "Tübingen"}
  else{Immigrants$Town[i] = NA}
}
Immigrants = na.omit(Immigrants)

d = Immigrants[which(Immigrants$Year==2021),]
d$Year = 2022
Immigrants = rbind(Immigrants,d)

Immigrants$Immigrants = as.numeric(Immigrants$Immigrants)

if(Year %in% levels(as.factor(Immigrants$Year))){
  Immigrants = Immigrants[(Immigrants$Year==Year),]
}else {
  Immigrants = Immigrants[(Immigrants$Year==2022),]
  Immigrants$Year = Year
}

ProjectionData = merge(x = ProjectionData,y = Immigrants,
                 by = c("Year","Town"),
                 all = TRUE)

PKW = read.csv(file = "PKWs12 bis 22.csv",sep=";", encoding="ISO-8859-1", skip = 5)
names(PKW)
PKW$Timestamp=as.POSIXlt(PKW$X,format="%d.%m.%Y")
PKW$Year	= as.numeric(format(as.POSIXlt(PKW$Timestamp), format = "%Y"))
PKW$X = NULL
PKW$Timestamp = NULL
PKW$X.1 = NULL
names(PKW)[1]="Town"
names(PKW)[2]="PKWs"
for(i in 1: nrow(PKW)){
  if(PKW$Town[i] == "Berlin, kreisfreie Stadt"){PKW$Town[i] = "Berlin"}
  else if(PKW$Town[i] == "Bochum, kreisfreie Stadt"){PKW$Town[i] = "Bochum"}
  else if(PKW$Town[i] == "Bonn, kreisfreie Stadt"){PKW$Town[i] = "Bonn"}
  else if(PKW$Town[i] == "Bremen, kreisfreie Stadt"){PKW$Town[i] = "Bremen"}
  else if(PKW$Town[i] == "Darmstadt, kreisfreie Stadt"){PKW$Town[i] = "Darmstadt"}
  else if(PKW$Town[i] == "Düsseldorf, kreisfreie Stadt"){PKW$Town[i] = "Düsseldorf"}
  else if(PKW$Town[i] == "Hamburg, kreisfreie Stadt"){PKW$Town[i] = "Hamburg"}
  else if(PKW$Town[i] == "Leipzig, kreisfreie Stadt"){PKW$Town[i] = "Leipzig"}
  else if(PKW$Town[i] == "Mannheim, kreisfreie Stadt"){PKW$Town[i] = "Mannheim"}
  else if(PKW$Town[i] == "München, kreisfreie Stadt"){PKW$Town[i] = "München"}
  else if(PKW$Town[i] == "Münster, kreisfreie Stadt"){PKW$Town[i] = "Münster"}
  else if(PKW$Town[i] == "Oberhausen, kreisfreie Stadt"){PKW$Town[i] = "Oberhausen"}
  else if(PKW$Town[i] == "Rostock, kreisfreie Stadt"){PKW$Town[i] = "Rostock"}
  else if(PKW$Town[i] == "Siegen-Wittgenstein, Landkreis"){PKW$Town[i] = "Siegen"}
  else if(PKW$Town[i] == "Erfurt, kreisfreie Stadt"){PKW$Town[i] = "Erfurt"}
  else if(PKW$Town[i] == "Tübingen, Landkreis"){PKW$Town[i] = "Tübingen"}
  else{PKW$Town[i] = NA}
}
PKW = na.omit(PKW)
PKW$PKWs=as.numeric(PKW$PKWs)

if(Year %in% levels(as.factor(PKW$Year))){
  PKW = PKW[(PKW$Year==Year),]
}else {
  PKW = PKW[(PKW$Year==2022),]
  PKW$Year = Year
}


ProjectionData = merge(x = ProjectionData,y = PKW,
                 by = c("Year","Town"),
                 all = TRUE)

ProjectionData$PKWs = ProjectionData$PKWs/ProjectionData$InhDestrict

summary(ProjectionData)
rm(list=setdiff(ls(), c("ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

ProjectionData = na.omit(ProjectionData)

Variables_you_need

#ProjectionData=na.omit(ProjectionData)

#Get all the streetpositions----------------------------------------------------

#building the query
q <- getbb(Town) %>%
  opq() %>%
  add_osm_feature("highway")

str(q) #query structure

streets <- osmdata_sf(q)

names(streets$osm_lines)

#levels(as.factor(streets$osm_lines$maxspeed.type))

streetPoints = 0

for(i in 1:length(streets$osm_lines$geometry)){
  streetPoints = streetPoints + length(streets$osm_lines$geometry[[i]])/2 - 1
}

streetPoints

streetPositions=as.data.frame(c(1:streetPoints))
names(streetPositions)[1]="Lon"
streetPositions$Lat=50
streetPositions$Lon2=7
streetPositions$Lat2=NA
streetPositions$Station=NA

k = 1

for(i in 1:length(streets$osm_lines$geometry)){
  
  l = length(streets$osm_lines$geometry[[i]])
  
  for(j in 1:(length(streets$osm_lines$geometry[[i]])/2 - 1)){
    
    
    tryCatch({
      streetPositions$Lon[k]=streets$osm_lines$geometry[[i]][j]
      streetPositions$Lat[k]=streets$osm_lines$geometry[[i]][l/2+j]
      streetPositions$Lon2[k]=streets$osm_lines$geometry[[i]][j+1]
      streetPositions$Lat2[k]=streets$osm_lines$geometry[[i]][l/2+j+1]
      streetPositions$Station=paste("Station_",k,sep="")
      
      k = k + 1
    })
  }
  print((i)/length(streets$osm_lines$geometry)*100)
}

beep("coin")

streetPositions = na.omit(streetPositions)

#myLocation<-c(7.597514856738869,51.94573812395569,   7.652382675482133,51.9756143280805)
#mad_map <- get_stamenmap(bbox=myLocation, maptype="terrain-background", zoom=14)
#ggmap(mad_map) + geom_segment(data = streetPositions, aes(x = Lon, y = Lat, xend = Lon2, yend = Lat2), color = "red", size = 1, alpha = 0.8, lineend = "round")

ProjectionData = merge(x = ProjectionData,y = streetPositions,all = FALSE)

summary(ProjectionData)
rm(list=setdiff(ls(), c("ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

#Distance to city center--------------------------------------------------------

#create a matrix, that later will contaion needed information
distmat=matrix(1:2*nlevels(as.factor(ProjectionData$Station)), nrow = nlevels(as.factor(ProjectionData$Station)), ncol = 2)

#divide in stations in a for loop
#Each Loop is for one station
#Than calculate distance and add this in a data frame
for(i in 1:nlevels(as.factor(ProjectionData$Station))){
  print(levels(as.factor(ProjectionData$Station))[i])
  d=ProjectionData[ProjectionData$Station %in% toString(levels(as.factor(ProjectionData$Station))[i]),]
  
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

#ProjectionData = merge(x = ProjectionData,y = distmat,by = c("Station"), all = FALSE)

summary(ProjectionData)