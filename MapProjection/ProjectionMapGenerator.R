#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model Map Projection: Map Projection Generator

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

#Load pre generated map data and combine it with adjustable values
setwd("D:/STUDIUM/Münster/7. Semester/BikeProjections")
mapData = read.csv(file = "Mannheim_Innenstadt_Oststadt_2.csv",sep=",")
mapData$Hour = NULL
mapData$Months = NULL
mapData$Day = NULL
mapData$Year = NULL
mapData$Timestamp = NULL
mapData$Summer  = NULL
mapData$Winter  = NULL
mapData$Weekday  = NULL
mapData$Weekend  = NULL
mapData$Night  = NULL
mapData$publicHoliday  = NULL
mapData$schoolHoliday = NULL
mapData$Wind  = NULL
mapData$CloudCover  = NULL
mapData$Humidity  = NULL
mapData$Rain  = NULL
mapData$Temperature  = NULL
mapData$ADFC_Index    = NULL
mapData$InhDestrict   = NULL
mapData$young18 = NULL
mapData$young25 = NULL
mapData$young30   = NULL
mapData$older40   = NULL
mapData$older60  = NULL
mapData$Immigrants = NULL
mapData$PKWs = NULL
mapData$Area = NULL
mapData$Inhabitants = NULL
mapData$Male_Ratio = NULL
mapData$City_Lon = NULL
mapData$City_Lat = NULL
mapData$Density = NULL

#Adjust Values here


Year = 2023
Town = "Mannheim"
ProjectionData = as.data.frame(cbind(Year,Town))

#ProjectionData$Station = "Projection"

Months = c(6)
ProjectionData = merge(x = ProjectionData,y = Months,all = FALSE)
names(ProjectionData)[3] = "Months"

Day = c(15)
ProjectionData = merge(x = ProjectionData,y = Day,all = FALSE)
names(ProjectionData)[4] = "Day"

Hour = c(14)
ProjectionData = merge(x = ProjectionData,y = Hour,all = FALSE)
names(ProjectionData)[5] = "Hour"

ProjectionData$CorInz = 0
ProjectionData$Lockdowns = 0

ProjectionData$Value = 1

weather_mode = 2
#in mode 1 weather will be selected by the reference year 2020
#in mode 2 weather will be selected by the median in all years
#in mode 3 weather will be selected by a manualy selected data

Wind = c(0)
CloudCover = c(0)
Humidity = c(0)
Rain = c(0)
Temperature = c(-5:35)

ADFC_Index = c(3.9)
ProjectionData = merge(x = ProjectionData,y = ADFC_Index,all = FALSE)
names(ProjectionData)[ncol(ProjectionData)] = "ADFC_Index"

#Ad the time Variables-----------------------------------------------------
Bundesland = "BWB"
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

pH=publicHolidays[publicHolidays$BWB %in% TRUE,]
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

if(weather_mode==1){
  if(Year %in% levels(as.factor(Weather_Temperature$Year))){
    Weather_Wind = Weather_Wind[(Weather_Wind$Year==Year),]
    Weather_CloudCover = Weather_CloudCover[(Weather_CloudCover$Year==Year),]
    Weather_Humidity = Weather_Humidity[(Weather_Humidity$Year==Year),]
    Weather_Rain = Weather_Rain[(Weather_Rain$Year==Year),]
    Weather_Temperature = Weather_Temperature[(Weather_Temperature$Year==Year),]
  }else {
    Weather_Wind = Weather_Wind[(Weather_Wind$Year==2020),]
    Weather_CloudCover = Weather_CloudCover[(Weather_CloudCover$Year==2020),]
    Weather_Humidity = Weather_Humidity[(Weather_Humidity$Year==2020),]
    Weather_Rain = Weather_Rain[(Weather_Rain$Year==2020),]
    Weather_Temperature = Weather_Temperature[(Weather_Temperature$Year==2020),]
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
  
}
if(weather_mode==2){
  
  ProjectionData$Wind = 0
  ProjectionData$CloudCover = 0
  ProjectionData$Humidity = 0
  ProjectionData$Rain = 0
  ProjectionData$Temperature = 0
  
  for(i in 1:nrow(ProjectionData)){
    
    subsetdata = Weather_Wind[Weather_Wind$Months == ProjectionData$Months[i], ]
    subsetdata = subsetdata[subsetdata$Day == ProjectionData$Day[i], ]
    subsetdata = subsetdata[subsetdata$Hour == ProjectionData$Hour[i], ]
    ProjectionData$Wind[i] = median(subsetdata$Wind)
    
    subsetdata = Weather_CloudCover[Weather_CloudCover$Months == ProjectionData$Months[i], ]
    subsetdata = subsetdata[subsetdata$Day == ProjectionData$Day[i], ]
    subsetdata = subsetdata[subsetdata$Hour == ProjectionData$Hour[i], ]
    ProjectionData$CloudCover[i] = median(subsetdata$CloudCover)
    
    subsetdata = Weather_Humidity[Weather_Humidity$Months == ProjectionData$Months[i], ]
    subsetdata = subsetdata[subsetdata$Day == ProjectionData$Day[i], ]
    subsetdata = subsetdata[subsetdata$Hour == ProjectionData$Hour[i], ]
    ProjectionData$Humidity[i] = median(subsetdata$Humidity)
    
    subsetdata = Weather_Rain[Weather_Rain$Months == ProjectionData$Months[i], ]
    subsetdata = subsetdata[subsetdata$Day == ProjectionData$Day[i], ]
    subsetdata = subsetdata[subsetdata$Hour == ProjectionData$Hour[i], ]
    ProjectionData$Rain[i] = median(subsetdata$Rain)
    
    subsetdata = Weather_Temperature[Weather_Temperature$Months == ProjectionData$Months[i], ]
    subsetdata = subsetdata[subsetdata$Day == ProjectionData$Day[i], ]
    subsetdata = subsetdata[subsetdata$Hour == ProjectionData$Hour[i], ]
    ProjectionData$Temperature[i] = median(subsetdata$Temperature)
  }
  
}
if(weather_mode==3){
  
  ProjectionData = merge(x = ProjectionData,y = Wind,all = FALSE)
  names(ProjectionData)[ncol(ProjectionData)] = "Wind"
  
  ProjectionData = merge(x = ProjectionData,y = CloudCover,all = FALSE)
  names(ProjectionData)[ncol(ProjectionData)] = "CloudCover"
  
  ProjectionData = merge(x = ProjectionData,y = Humidity,all = FALSE)
  names(ProjectionData)[ncol(ProjectionData)] = "Humidity"
  
  ProjectionData = merge(x = ProjectionData,y = Rain,all = FALSE)
  names(ProjectionData)[ncol(ProjectionData)] = "Rain"
  
  ProjectionData = merge(x = ProjectionData,y = Temperature,all = FALSE)
  names(ProjectionData)[ncol(ProjectionData)] = "Temperature"
  
}

summary(ProjectionData)
rm(list=setdiff(ls(), c("mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

#Destatis Data -----------------------------------------------------------------


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

title=", Universitätsstadt" #This differs, there are cities and also hanseatic cities

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
  print(plot1)
  dev.off()
  
  png(file=paste("Predictions_",Town,"_plot02.png",sep=""),width=800, height=800)
  print(plot2)
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

rm(list=setdiff(ls(), c("mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

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
#foreach(i = 1: nrow(Immigrants))%dopar%{
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
#foreach (i = 1: nrow(PKW))%dopar%{
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
rm(list=setdiff(ls(), c("mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland")))

ProjectionData = na.omit(ProjectionData)

ProjectionData = merge(x = ProjectionData,y = mapData,
                       by = c("Town"),
                       all = TRUE)

names(ProjectionData)
#calculate Values --------------------------------------------------------------
setwd("D:/STUDIUM/Münster/7. Semester")

load("Modell3_RF_newDataset.rdata")

summary(model)

library(randomForest)
#projection_pred <- model %>% predict(ProjectionData, type='response')
projection_pred <- predict(model, newdata = ProjectionData, type='response')

summary(as.numeric(projection_pred))
summary(exp(as.numeric(projection_pred)))

ProjectionData$Value = exp(as.numeric(projection_pred))
summary(ProjectionData$Value)
nrow(ProjectionData)
#Create Map

#bounding box for our map
myLocation <- c(8.45440628005673,49.47735485105553,   8.497814937261264,49.49986824573402) # Mannheim Innensatdt
#myLocation <- c(9.968615748457593,53.539830498755265,   10.012409572679795,53.55974898224376) # Hamburg Innensatdt
#myLocation <- c(6.833644830296469,51.460877236637465,  6.874634203344688,51.48078438095241) # Oberhausen Innensatdt
#myLocation <- c(7.547877265931465,51.911200682602676,   7.689021424551272,52.0041202032665) # Muenster
#myLocation <- c(7.597514856738869,51.94573812395569,   7.652382675482133,51.9756143280805) # Muenster Ring
#myLocation <- c(7.613588137509167,51.955501852036285,   7.638086559861329,51.96820564471896) # Muenster Innenstadt


mad_map <- get_stamenmap(bbox=myLocation, maptype="terrain-background", zoom=15)

#write.csv(ProjectionData,"Mannheim_Innenstadt_Oststadt.csv")

for(i in 1:nlevels(as.factor(ProjectionData$Months))){
  
  for(j in 1:nlevels(as.factor(ProjectionData$Day))){
    
    for(k in 1:nlevels(as.factor(ProjectionData$Hour))){
      
      streetPositions = ProjectionData[ProjectionData$Months==levels(as.factor(ProjectionData$Months))[i],]
      streetPositions = streetPositions[streetPositions$Day==levels(as.factor(ProjectionData$Day))[j],]
      streetPositions = streetPositions[streetPositions$Hour==levels(as.factor(ProjectionData$Hour))[k],]
      nrow(streetPositions)
      
      map_plot = ggmap(mad_map) + geom_segment(data = streetPositions, aes(x = Lon, y = Lat, xend = Lon2, yend = Lat2, color = Value), size = 1.8, alpha = 1.5, lineend = "round") +
        ggtitle(paste("Fahradfahrer am ", streetPositions$Day[1],".", streetPositions$Months[1],".", streetPositions$Year[1],
                      " um ",streetPositions$Hour[1], " Uhr in: ",streetPositions$Town[1],"\n", "Temp: ",
                      streetPositions$Temperature[1]," C° , Regen: ", streetPositions$Rain[1], " mm, Wochenende: ",
                      streetPositions$Weekend[1], sep="")) + 
        scale_colour_gradientn(limits = c(0, max(ProjectionData$Value)), space = "Lab",
                               colours = c("black","darkblue","blue","violet","red","orange", "yellow")) +
        theme_bw() +
        theme(text = element_text(size = 20))     +
        labs(y = "Längengrad", x = "Breitengrad", color ="Fahrer summiert")
      
      setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/MapProjection/Plots")
      png(file=paste("map",ProjectionData$Town[1],"plot_RF_Innenstadt_",i,"_",j,"_",k,".png",sep=""),width=1200, height=1200)
      print(map_plot)
      dev.off()
      #summary(streetPositions)
      
    }
    
  }
  
}






beep("mario")







