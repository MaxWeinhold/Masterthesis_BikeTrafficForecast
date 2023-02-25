#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model Map Projection: Bike Level Forecast

#Clean up memory
rm(list=ls())

#Load pre generated map data and combine it with adjustable values
setwd("D:/STUDIUM/Münster/7. Semester/BikeProjections")
mapData = read.csv(file = "Hamburg_Innenstadt_Altona.csv",sep=",")
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

StationDots = TRUE

Year = c(2012:2030)
Town = "Hamburg"
#ProjectionData = as.data.frame(cbind(Year,Town))
ProjectionData = merge(x = Year,y = Town,all = FALSE)
names(ProjectionData)[1] = "Year"
names(ProjectionData)[2] = "Town"

#ProjectionData$Station = "Projection"

Months = c(1:12)
ProjectionData = merge(x = ProjectionData,y = Months,all = FALSE)
names(ProjectionData)[3] = "Months"

Day = c(1,10,20)
ProjectionData = merge(x = ProjectionData,y = Day,all = FALSE)
names(ProjectionData)[4] = "Day"

Hour = c(12)
ProjectionData = merge(x = ProjectionData,y = Hour,all = FALSE)
names(ProjectionData)[5] = "Hour"

ProjectionData$CorInz = 0
ProjectionData$Lockdowns = 0

ProjectionData$Value = 1

weather_mode = 2
#in mode 1 weather will be selected by the reference year 2020 #Not usable in this version
#in mode 2 weather will be selected by the median in all years
#in mode 3 weather will be selected by a manualy selected data

Wind = c(0)
CloudCover = c(0)
Humidity = c(50)
Rain = c(0)
Temperature = c(10)

ADFC_Index = c(3.9)
ProjectionData = merge(x = ProjectionData,y = ADFC_Index,all = FALSE)
names(ProjectionData)[ncol(ProjectionData)] = "ADFC_Index"

#Ad the time Variables-----------------------------------------------------
Bundesland = "HAM"
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

pH=publicHolidays[publicHolidays$HAM %in% TRUE,]
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
rm(list=setdiff(ls(), c("mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland","StationDots")))


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

title=", Freie und Hansestadt" #This differs, there are cities and also hanseatic cities

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

fc = as.data.frame(c(2022:max(ProjectionData$Year)))
names(fc)[1]="Year"
fc$Density = test$Density[1]
fc$City_Lat = test$City_Lat[1]
fc$City_Lon = test$City_Lon[1]

model1 <- lm(log(Inhabitants) ~ Year, data=test)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="Inhabitants"
predict1$Inhabitants = exp(predict1$Inhabitants)

fc$Inhabitants = predict1$Inhabitants

model2 <- lm(log(Male_Ratio) ~ Year, data=test)
predict2 <- as.data.frame(predict(model2,fc))
predict2$Year = fc$Year
names(predict2)[1]="Male_Ratio"
predict2$Male_Ratio  = exp(predict2$Male_Ratio )

fc$Male_Ratio = predict2$Male_Ratio

model3 <- lm(log(Area) ~ Year, data=test)
predict3 <- as.data.frame(predict(model3,fc))
predict3$Year = fc$Year
names(predict3)[1]="Area"
predict3$Area  = exp(predict3$Area )

fc$Area = predict3$Area

plot1 = ggplot(data = test,aes(x = Year, y = Inhabitants)) +
  geom_line(data = predict1, aes(x = Year, y = Inhabitants),color = "red", size = 1.5) +
  geom_point(size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  xlab("Jahr") +
  ylab("Einwohneranzahl") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year))

plot2 = ggplot(data = test,aes(x = Year, y = Male_Ratio)) +
  geom_point(size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  xlab("Jahr") +
  ylab("Geschlechterverhältnis") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year)) +
  geom_line(data = predict2, aes(x = Year, y = Male_Ratio),color = "red", size = 1.5)

plot3 = ggplot(data = test,aes(x = Year, y = Area)) +
  geom_point(size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  xlab("Jahr") +
  ylab("Fläche in km²") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year)) +
  geom_line(data = predict3, aes(x = Year, y = Area),color = "red", size = 1.5)


setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,"_plot01.png",sep=""),width=800, height=800)
print(plot1)
dev.off()

png(file=paste("Forecast_",Town,"_plot02.png",sep=""),width=800, height=800)
print(plot2)
dev.off()

png(file=paste("Forecast_",Town,"_plot03.png",sep=""),width=800, height=800)
print(plot3)
dev.off()

test = rbind(test,fc)

ProjectionData = merge(x = ProjectionData,y = test,
                       by = c("Year"),
                       all = FALSE)

levels(as.factor(ProjectionData$Year))
summary(ProjectionData)

rm(list=setdiff(ls(), c("test","mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland","StationDots")))





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
  else if(Altersgruppen$Town[i] == "Dresden, kreisfreie Stadt"){Altersgruppen$Town[i] = "Dresden"}
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

fc = as.data.frame(c(2022:max(ProjectionData$Year)))
names(fc)[1]="Year"
fc$Town = Altersgruppen$Town[1]
fc$InhDestrict  = test$Inhabitants[test$Year>2021]

model1 <- lm(log(young18) ~ Year, data=Altersgruppen)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="young18"
predict1$young18 = exp(predict1$young18)

fc$young18 = predict1$young18

model1 <- lm(log(young25) ~ Year, data=Altersgruppen)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="young25"
predict1$young25 = exp(predict1$young25)

fc$young25 = predict1$young25

model1 <- lm(log(young30) ~ Year, data=Altersgruppen)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="young30"
predict1$young30 = exp(predict1$young30)

fc$young30 = predict1$young30

model1 <- lm(log(older40) ~ Year, data=Altersgruppen)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="older40"
predict1$older40 = exp(predict1$older40)

fc$older40 = predict1$older40

model1 <- lm(log(older60) ~ Year, data=Altersgruppen)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="older60"
predict1$older60 = exp(predict1$older60)

fc$older60 = predict1$older60

for(i in 1:nrow(fc)){
  
  if(fc$young18[i]>fc$young25[i]){fc$young25[i] = fc$young18[i]}
  if(fc$young25[i]>fc$young30[i]){fc$young30[i] = fc$young25[i]}
  if(fc$older60[i]>fc$older40[i]){fc$older40[i] = fc$older60[i]}
  
}

for(i in 1:nrow(fc)){
  
  fc$young18[i] = mean(c(fc$young18[i],Altersgruppen$young18[nrow(Altersgruppen)]))
  fc$young25[i] = mean(c(fc$young25[i],Altersgruppen$young25[nrow(Altersgruppen)]))
  fc$young30[i] = mean(c(fc$young30[i],Altersgruppen$young30[nrow(Altersgruppen)]))
  fc$older40[i] = mean(c(fc$older40[i],Altersgruppen$older40[nrow(Altersgruppen)]))
  fc$older60[i] = mean(c(fc$older60[i],Altersgruppen$older60[nrow(Altersgruppen)]))

}

Altersgruppen = rbind(Altersgruppen,fc)

plot3 = ggplot() +
  geom_line(data = Altersgruppen,aes(x = Year, y = young18, color = "Unter 18"),size=1.5)+
  geom_line(data = Altersgruppen,aes(x = Year, y = young25, color = "Unter 25"),size=1.5)+
  geom_line(data = Altersgruppen,aes(x = Year, y = young30, color = "unter 30"),size=1.5)+
  geom_line(data = Altersgruppen,aes(x = Year, y = older40, color = "Über 40"),size=1.5)+
  geom_line(data = Altersgruppen,aes(x = Year, y = older60, color = "Über 60"),size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  labs(x = "Jahr",
       y = "Prozentanteil an \n der Gesamtbevölerung",
       color = "Altersgruppen") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year))


setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,"_plot04.png",sep=""),width=800, height=800)
print(plot3)
dev.off()

ProjectionData = merge(x = ProjectionData,y = Altersgruppen,
                       by = c("Year","Town"),
                       all = TRUE)

levels(as.factor(ProjectionData$Year))
summary(ProjectionData)
rm(list=setdiff(ls(), c("test","mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland","StationDots")))


setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Destatis")
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
  else if(Immigrants$Town[i] == "Dresden, kreisfreie Stadt"){Immigrants$Town[i] = "Dresden"}
  else{Immigrants$Town[i] = NA}
}
Immigrants = na.omit(Immigrants)

d = Immigrants[which(Immigrants$Year==2021),]
d$Year = 2022
Immigrants = rbind(Immigrants,d)

Immigrants$Immigrants = as.numeric(Immigrants$Immigrants)
Immigrants = Immigrants[(Immigrants$Town==Town),]

fc = as.data.frame(c(2022:max(ProjectionData$Year)))
names(fc)[1]="Year"
fc$Town = Immigrants$Town[1]

model1 <- lm(log(as.numeric(Immigrants)) ~ Year, data=Immigrants)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="Immigrants"
predict1$Immigrants = exp(predict1$Immigrants)

fc$Immigrants = predict1$Immigrants

plot1 = ggplot(data = Immigrants,aes(x = Year, y = as.numeric(Immigrants))) +
  geom_line(data = predict1, aes(x = Year, y = Immigrants),color = "red", size = 1.5) +
  geom_point(size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  xlab("Jahr") +
  ylab("Anzahl der Immigranten") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year))

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,"_plot05.png",sep=""),width=800, height=800)
print(plot1)
dev.off()

Immigrants = rbind(Immigrants,fc)

ProjectionData = merge(x = ProjectionData,y = Immigrants,
                       by = c("Year","Town"),
                       all = TRUE)

levels(as.factor(ProjectionData$Year))
summary(ProjectionData)
rm(list=setdiff(ls(), c("test","mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland","StationDots")))


setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Destatis")
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
  else if(PKW$Town[i] == "Dresden, kreisfreie Stadt"){PKW$Town[i] = "Dresden"}
  else{PKW$Town[i] = NA}
}
PKW = na.omit(PKW)
PKW$PKWs=as.numeric(PKW$PKWs)

PKW = PKW[(PKW$Town==Town),]

fc = as.data.frame(c(2022:max(ProjectionData$Year)))
names(fc)[1]="Year"
fc$Town = PKW$Town[1]

model1 <- lm(log(as.numeric(PKWs)) ~ Year, data=PKW)
predict1 <- as.data.frame(predict(model1,fc))
predict1$Year = fc$Year
names(predict1)[1]="PKWs"
predict1$PKWs = exp(predict1$PKWs)

fc$PKWs = predict1$PKWs

plot1 = ggplot(data = PKW,aes(x = Year, y = as.numeric(PKWs))) +
  geom_line(data = predict1, aes(x = Year, y = PKWs),color = "red", size = 1.5) +
  geom_point(size=1.5)+
  labs(title = paste("Prognose zur \n Stadtentwicklung:",Town),color="Formel:") +
  xlab("Jahr") +
  ylab("Anzahl der Autos") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  xlim(2012, max(ProjectionData$Year))

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,"_plot06.png",sep=""),width=800, height=800)
print(plot1)
dev.off()

PKW = rbind(PKW,fc)
ProjectionData = merge(x = ProjectionData,y = PKW,
                       by = c("Year","Town"),
                       all = TRUE)

ProjectionData$PKWs = ProjectionData$PKWs/ProjectionData$InhDestrict

summary(ProjectionData)
rm(list=setdiff(ls(), c("mapData","ProjectionData","Variables_you_need","summaryBikeData","Town","Year","Bundesland","StationDots")))


ProjectionData = na.omit(ProjectionData)

nrow(mapData)

mapData = mapData[mapData$Distance_to_Center<1000,]

mapData = sample_n(mapData, 1000)

ProjectionData = merge(x = ProjectionData,y = mapData,
                       by = c("Town"),
                       all = TRUE)

rm(list=setdiff(ls(), c("ProjectionData","Town")))

ProjectionData$.data_footway = 0
ProjectionData$.data_pedestrian = 0
if(is.null(ProjectionData$.data_motorway)){ProjectionData$.data_motorway = 0}
if(is.null(ProjectionData$.data_driveway)){ProjectionData$.data_driveway = 0}
if(is.null(ProjectionData$.data_sidepath)){ProjectionData$.data_sidepath = 0}
if(is.null(ProjectionData$.data_sidewalk)){ProjectionData$.data_sidewalk = 0}
if(is.null(ProjectionData$.data_pebblestone)){ProjectionData$.data_pebblestone = 0}
if(is.null(ProjectionData$.data_trunk_link)){ProjectionData$.data_trunk_link = 0}

ProjectionData$Rain2 = ProjectionData$Rain^2
ProjectionData$Temperature2 = ProjectionData$Temperature^2
ProjectionData$Inhabitants2 = ProjectionData$Inhabitants^2
ProjectionData$ADFC_Index2 = ProjectionData$ADFC_Index^2

ProjectionData$ClosestSchool2 = ProjectionData$ClosestSchool^2
ProjectionData$Schools500mmRadius2 = ProjectionData$Schools500mmRadius^2
ProjectionData$UniBuild2kmRadius2 = ProjectionData$UniBuild2kmRadius^2

ProjectionData$ClosestUniBuild2 = ProjectionData$ClosestUniBuild^2
ProjectionData$UniBuild500mmRadius2 = ProjectionData$UniBuild500mmRadius^2
ProjectionData$ClosestSchool2 = ProjectionData$ClosestSchool^2

ProjectionData$ClosestClothesShop2 = ProjectionData$ClosestClothesShop^2
ProjectionData$ClothesShop500mmRadius2 = ProjectionData$ClothesShop500mmRadius^2
ProjectionData$ClothesShop2kmRadius2 = ProjectionData$ClothesShop2kmRadius^2

ProjectionData$ClosestTrainS2 = ProjectionData$ClosestTrainS^2
ProjectionData$ClosestBridge2 = ProjectionData$ClosestBridge^2
ProjectionData$young302 = ProjectionData$young30^2
ProjectionData$PKWs2 = ProjectionData$PKWs^2

ProjectionData$CorInz2 = ProjectionData$CorInz^2
ProjectionData$stre_dist2 = ProjectionData$stre_dist^2
ProjectionData$stre_lengths2 = ProjectionData$stre_lengths^2
ProjectionData$stre_lanes2 = ProjectionData$stre_lanes^2
ProjectionData$stre_maxspeed2 = ProjectionData$stre_maxspeed^2

ProjectionData$Rain3 = ProjectionData$Rain^3
ProjectionData$Inhabitants3 = ProjectionData$Inhabitants^3
ProjectionData$UniBuild500mmRadius3 = ProjectionData$UniBuild500mmRadius^3
ProjectionData$ClothesShop500mmRadius3 = ProjectionData$ClothesShop500mmRadius^3
ProjectionData$ClosestTrainS3 = ProjectionData$ClosestTrainS^3
ProjectionData$ClosestBridge3 = ProjectionData$ClosestBridge3
ProjectionData$stre_lengths3 = ProjectionData$stre_lengths^3
ProjectionData$stre_lanes3 = ProjectionData$stre_lanes^3
ProjectionData$stre_maxspeed3 = ProjectionData$stre_maxspeed^3

ProjectionData$SignalsRatio = ProjectionData$UnmCross250mmRadius/(ProjectionData$UnmCross250mmRadius + ProjectionData$Signals250mmRadius + 1)

if(is.null(ProjectionData$stre_lengths2)){ProjectionData$stre_lengths2 = ProjectionData$stre_lengths^2}
if(is.null(ProjectionData$ClosestSchool2)){ProjectionData$ClosestSchool2 = ProjectionData$ClosestSchool^2}
if(is.null(ProjectionData$ClosestUniBuild2)){ProjectionData$ClosestUniBuild2 = ProjectionData$ClosestUniBuild^2}
if(is.null(ProjectionData$ClosestClothesShop2)){ProjectionData$ClosestClothesShop2 = ProjectionData$ClosestClothesShop^2}
if(is.null(ProjectionData$stre_lanes2)){ProjectionData$stre_lanes2 = ProjectionData$stre_lanes^2}
if(is.null(ProjectionData$stre_maxspeed2)){ProjectionData$stre_maxspeed2 = ProjectionData$stre_maxspeed^2}
if(is.null(ProjectionData$stre_lengths3)){ProjectionData$stre_lengths3 = ProjectionData$stre_lengths^3}
if(is.null(ProjectionData$stre_lanes3)){ProjectionData$stre_lanes3 = ProjectionData$stre_lanes^3}
if(is.null(ProjectionData$stre_maxspeed3)){ProjectionData$stre_maxspeed3 = ProjectionData$stre_maxspeed^3}
if(is.null(ProjectionData$stre_lanes2)){ProjectionData$stre_lanes2 = ProjectionData$stre_lanes^2}

if(is.null(ProjectionData$Rain2)){ProjectionData$Rain2 = ProjectionData$Rain^2}
if(is.null(ProjectionData$Temperature2)){ProjectionData$Temperature2 = ProjectionData$Temperature^2}
if(is.null(ProjectionData$young302)){ProjectionData$young302 = ProjectionData$young30^2}
if(is.null(ProjectionData$PKWs2)){ProjectionData$PKWs2 = ProjectionData$PKWs^2}

names(ProjectionData)
#ProjectionData = na.omit(ProjectionData)
#calculate Values --------------------------------------------------------------
setwd("D:/STUDIUM/Münster/7. Semester")

summary(ProjectionData)

load("Modell3_RF_newDataset3.rdata")
#load("Modell3_RF_newDataset2.rdata")

summary(model)

library(randomForest)
#projection_pred <- model %>% predict(ProjectionData, type='response')
projection_pred <- predict(model, newdata = ProjectionData, type='response')

summary(as.numeric(projection_pred))
summary(exp(as.numeric(projection_pred)))

ProjectionData$Value = exp(as.numeric(projection_pred))
summary(ProjectionData$Value)
nrow(ProjectionData)

ProjectionData2=ddply(ProjectionData,.(Year,Months),summarize,Value=mean(Value))
ProjectionData2$Timestamp = as.Date(paste(ProjectionData2$Year,"-",ProjectionData2$Months,"-1",sep=""))

ProjectionData3=ddply(ProjectionData,.(Year,Months),summarize,Value=median(Value))
ProjectionData3$Timestamp = as.Date(paste(ProjectionData3$Year,"-",ProjectionData3$Months,"-1",sep=""))

ProjectionData4=ddply(ProjectionData,.(Year,Months),summarize,Value=min(Value))
ProjectionData4$Timestamp = as.Date(paste(ProjectionData4$Year,"-",ProjectionData4$Months,"-1",sep=""))

ProjectionData5=ddply(ProjectionData,.(Year,Months),summarize,Value=max(Value))
ProjectionData5$Timestamp = as.Date(paste(ProjectionData5$Year,"-",ProjectionData5$Months,"-1",sep=""))

plot1 = ggplot() +
  geom_line(data = ProjectionData2, aes(x = Timestamp, y = Value,color = "mean"), size = 1.2) +
  geom_line(data = ProjectionData3, aes(x = Timestamp, y = Value,color = "median"), size = 1.2) +
  geom_line(data = ProjectionData4, aes(x = Timestamp, y = Value,color = "min"), size = 1.2) +
  geom_line(data = ProjectionData5, aes(x = Timestamp, y = Value,color = "max"), size = 1.2) +
  labs(title = paste("Prognose zur Entwicklung \n des Radverkehrs in ",Town),color="Legende:") +
  xlab("Jahr") +
  ylab("Anzahl der Fahrräder Im Schnitt") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  ylim(0, max(ProjectionData5$Value)*1.05)

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,".png",sep=""),width=800, height=800)
print(plot1)
dev.off()

ProjectionData2=ddply(ProjectionData,.(Year),summarize,Value=mean(Value))
ProjectionData2$Timestamp = as.Date(paste(ProjectionData2$Year,"-1-1",sep=""))

ProjectionData3=ddply(ProjectionData,.(Year),summarize,Value=median(Value))
ProjectionData3$Timestamp = as.Date(paste(ProjectionData3$Year,"-1-1",sep=""))

ProjectionData4=ddply(ProjectionData,.(Year),summarize,Value=min(Value))
ProjectionData4$Timestamp = as.Date(paste(ProjectionData4$Year,"-1-1",sep=""))

ProjectionData5=ddply(ProjectionData,.(Year),summarize,Value=max(Value))
ProjectionData5$Timestamp = as.Date(paste(ProjectionData5$Year,"-1-1",sep=""))

plot1 = ggplot() +
  geom_line(data = ProjectionData2, aes(x = Timestamp, y = Value,color = "mean"), size = 1.2) +
  geom_line(data = ProjectionData3, aes(x = Timestamp, y = Value,color = "median"), size = 1.2) +
  geom_line(data = ProjectionData4, aes(x = Timestamp, y = Value,color = "min"), size = 1.2) +
  geom_line(data = ProjectionData5, aes(x = Timestamp, y = Value,color = "max"), size = 1.2) +
  labs(title = paste("Prognose zur Entwicklung \n des Radverkehrs in ",Town),color="Legende:") +
  xlab("Jahr") +
  ylab("Anzahl der Fahrräder Im Schnitt") +
  theme_bw() +
  theme(text = element_text(size = 32)) +
  ylim(0, max(ProjectionData5$Value)*1.05)

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file=paste("Forecast_",Town,"_pa.png",sep=""),width=800, height=800)
print(plot1)
dev.off()


