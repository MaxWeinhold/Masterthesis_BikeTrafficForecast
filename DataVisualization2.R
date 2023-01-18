#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Data Set Visualization2
#Clean up memory
rm(list=ls())

library(tidyverse)
library(scales)
library(dplyr)
library(plyr)
library(lubridate)

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_2.csv",sep=",", encoding="ISO-8859-1")

levels(as.factor(BikeData$Town))
nlevels(as.factor(BikeData$Station))
levels(as.factor(BikeData$Station))
levels(as.factor(BikeData$Year))
nrow(BikeData)

names(BikeData)

nrow(BikeData)

#Create average Variables per city

citties = as.data.frame(matrix(1:18*nlevels(as.factor(BikeData$Town)), nrow = nlevels(as.factor(BikeData$Town)), ncol = 18))

names(citties)[1]="Town"
names(citties)[2]="Value_Mean"
names(citties)[3]="Value_Median"
names(citties)[4]="Value_Min"
names(citties)[5]="Value_Max"
names(citties)[6]="Inhabitants_Mean"
names(citties)[7]="MaleRatio_Mean"
names(citties)[8]="young18_Mean"
names(citties)[9]="young25_Mean"
names(citties)[10]="young30_Mean"
names(citties)[11]="older40_Mean"
names(citties)[12]="older60_Mean"
names(citties)[13]="Temperature_Mean"
names(citties)[14]="Rain_Mean"
names(citties)[15]="Size"
names(citties)[16]="ADFC_Mean"
names(citties)[17]="PKWs_Mean"
names(citties)[18]="Immigrants_Mean"
citties$Town=levels(as.factor(BikeData$Town))


for(i in 1:nlevels(as.factor(BikeData$Town))){
  d=BikeData[BikeData$Town %in% citties$Town[i],]
  citties$Value_Mean[i]=mean(d$Value)
  citties$Value_Median[i]=median(d$Value)
  citties$Value_Max[i]=max(d$Value)
  citties$Value_Min[i]=min(d$Value)
  citties$Inhabitants_Mean[i]=mean(d$Inhabitants)
  citties$MaleRatio_Mean[i]=mean(d$Male_Ratio)
  citties$young18_Mean[i]=mean(d$young18)
  citties$young25_Mean[i]=mean(d$young25)
  citties$young30_Mean[i]=mean(d$young30)
  citties$older40_Mean[i]=mean(d$older40)
  citties$older60_Mean[i]=mean(d$older60)
  citties$Temperature_Mean[i]=mean(d$Temperature)
  citties$Rain_Mean[i]=mean(d$Rain)
  citties$Size[i]=mean(d$Area)
  citties$ADFC_Mean[i]=mean(d$ADFC_Index)
  citties$PKWs_Mean[i]=mean(d$PKWs)
  citties$Immigrants_Mean[i]=mean(d$Immigrants)
}

if(!require("ggrepel")) install.packages("ggrepel")
library(ggrepel)

plot1 = ggplot(data=citties)+
  geom_point(aes(x=Inhabitants_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Inhabitants_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = Inhabitants_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "Einwohnergröße") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot1

plot2 = ggplot(data=citties)+
  geom_point(aes(x=MaleRatio_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=MaleRatio_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = MaleRatio_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "Anteil männl. Bevölker") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot2

plot3 = ggplot(data=citties)+
  geom_point(aes(x=young30_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=young30_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = young30_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "Anteil der Bevölker unter 30 Jahre") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot3

plot(citties$Size,citties$Value_Mean)

plot4 = ggplot(data=citties)+
  geom_point(aes(x=Inhabitants_Mean/Size, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Inhabitants_Mean/Size, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = Inhabitants_Mean/Size, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "Einwohner je km²") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot4

plot5 = ggplot(data=citties)+
  geom_point(aes(x=ADFC_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=ADFC_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = ADFC_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "ADFC Fahrradklima Index") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot5

plot6 = ggplot(data=citties)+
  geom_point(aes(x=PKWs_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=PKWs_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = PKWs_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "PKWs je Person") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot6

plot7 = ggplot(data=citties)+
  geom_point(aes(x=Immigrants_Mean, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Immigrants_Mean, y=Value_Mean),method='lm') +
  geom_label_repel(aes(x = Immigrants_Mean, y = Value_Mean, label = Town)) + 
  labs(y = "Radfahrer pro Stunde je Stadt"
       , x = "Immigrantenanteil") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot7

plot8 = ggplot(citties, aes(y=Value_Mean, x=Town, fill=Town)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , fill = "Städte")

plot8


setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot01.png",width=800, height=800)
plot1
dev.off()

png(file="plot02.png",width=800, height=800)
plot2
dev.off()

png(file="plot03.png",width=800, height=800)
plot3
dev.off()

png(file="plot04.png",width=800, height=800)
plot4
dev.off()

png(file="plot05.png",width=800, height=800)
plot5
dev.off()

png(file="plot06.png",width=800, height=800)
plot6
dev.off()

png(file="plot07.png",width=800, height=800)
plot7
dev.off()

png(file="plot08.png",width=800, height=800)
plot8
dev.off()

rm(list=setdiff(ls(), "BikeData"))

#share of observations per year

years_shares = c(1:nlevels(as.factor(BikeData$Year)))

for(i in 1: nlevels(as.factor(BikeData$Year))){
  years_shares[i] = sum(BikeData$Year==levels(as.factor(BikeData$Year))[i])
}
years_shares = as.data.frame(cbind(years_shares,levels(as.factor(BikeData$Year))))
names(years_shares)[1]="Observations"
names(years_shares)[2]="Year"
years_shares$Observations = as.numeric(years_shares$Observations)
years_shares$Observations = years_shares$Observations/sum(years_shares$Observations)*100

plot28 = ggplot(years_shares, aes(y=Observations, x=Year, fill = Year)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(title = "Anteile des Datensatzes nach Jahren"
       , y = "in %"
       , fill = "Jahre")

plot28

png(file="plot28.png",width=800, height=800)
plot28
dev.off()

#share of observations per city

city_shares = c(1:nlevels(as.factor(BikeData$Town)))

for(i in 1: nlevels(as.factor(BikeData$Town))){
  city_shares[i] = sum(BikeData$Town==levels(as.factor(BikeData$Town))[i])
}

city_shares = as.data.frame(cbind(city_shares,levels(as.factor(BikeData$Town))))
names(city_shares)[1]="Observations"
names(city_shares)[2]="Town"
city_shares$Observations = as.numeric(city_shares$Observations)
city_shares$Observations = city_shares$Observations/sum(city_shares$Observations)*100

city_shares2 = city_shares[order(city_shares$Observations,decreasing=TRUE),]

pie(city_shares2$Observations, labels = city_shares2$Town)


plot9 = ggplot(data = city_shares2, aes(x = "", y = -Observations, 
                                        fill = reorder(Town, -Observations))) + 
  geom_bar(stat = "identity", color = "black") + 
  labs(fill = "Stadt",x="",y="") +
  #geom_label_repel(aes(x = "", y = -Observations, label = round(Observations))) +
  coord_polar("y") +
  theme_void() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot9

png(file="plot09.png",width=800, height=800)
plot9
dev.off()

#share of observations per Inhabitants

city_shares = c(1:4)
city_shares[1]=sum(BikeData$Inhabitants<=100000)
city_shares[2]=sum(BikeData$Inhabitants>100000 & BikeData$Inhabitants<=300000)
city_shares[3]=sum(BikeData$Inhabitants>300000 & BikeData$Inhabitants<=1000000)
city_shares[4]=sum(BikeData$Inhabitants>1000000)
city_shares=as.data.frame(cbind(city_shares,c("1: < 100 Tsd","2: 100 Tsd - 300 Tsd","3: 300 Tsd - 1 Mio","4: > 1 Mio")))
city_shares=as.data.frame(cbind(city_shares,c("A","B","C","D")))
names(city_shares)[1]="Observations"
names(city_shares)[2]="Population"
names(city_shares)[3]="Title"
city_shares$Observations = as.numeric(city_shares$Observations)
city_shares$Observations = city_shares$Observations/sum(city_shares$Observations)*100

plot10 = ggplot(city_shares, aes(y=Observations, x=Title, fill = Population)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y = "in %"
       , fill = "Einwohnerzahl")

plot10

png(file="plot10.png",width=800, height=800)
plot10
dev.off()

#OSM Data per Counting Station

stations = as.data.frame(matrix(1:32*nlevels(as.factor(BikeData$Station)), nrow = nlevels(as.factor(BikeData$Station)), ncol = 32))

names(stations)[1]="Distance_to_Center"
names(stations)[2]="Cinemas1kmRadius"
names(stations)[3]="ClosestUniBuild"
names(stations)[4]="SuperMarket1kmRadius"
names(stations)[5]="ClothesShop500mmRadius"
names(stations)[6]="ClothesShop2kmRadius"
names(stations)[7]="BusStop1kmRadius"
names(stations)[8]="Signals1kmRadius"
names(stations)[9]="UnmCross1kmRadius"
names(stations)[10]="Tram1kmRadius"
names(stations)[11]="Subway1kmRadius"
names(stations)[12]="ClosestTrainS"
names(stations)[13]="BikeShop1kmRadius"
names(stations)[14]="secondary"
names(stations)[15]="primary"
names(stations)[16]="residential"
names(stations)[17]="living_street"
names(stations)[18]="path"
names(stations)[19]="cycleways"
names(stations)[20]="ClosestBridge"
names(stations)[21]="isBridge"
names(stations)[22]="Value_Mean"
names(stations)[23]="stre_type"
names(stations)[24]="stre_density"
names(stations)[25]="stre_lengths"
names(stations)[26]="stre_type_spec"
names(stations)[27]="stre_surface"
names(stations)[28]="stre_lanes"
names(stations)[29]="stre_maxspeed"
names(stations)[30]="os_way_to_city"
names(stations)[31]="cluster_way_to_city"
names(stations)[32]="stre_dist"
stations$Station=levels(as.factor(BikeData$Station))

for(i in 1:nlevels(as.factor(BikeData$Station))){
  d=BikeData[BikeData$Station %in% stations$Station[i],]
  stations$Distance_to_Center[i]=d$Distance_to_Center[1]
  stations$Cinemas1kmRadius[i]=d$Cinemas1kmRadius[1]
  stations$ClosestUniBuild[i]=d$ClosestUniBuild[1]
  stations$SuperMarket1kmRadius[i]=d$SuperMarket1kmRadius[1]
  stations$ClothesShop500mmRadius[i]=d$ClothesShop500mmRadius[1]
  stations$ClothesShop2kmRadius[i]=d$ClothesShop2kmRadius[1]
  stations$BusStop1kmRadius[i]=d$BusStop1kmRadius[1]
  stations$Signals1kmRadius[i]=d$Signals1kmRadius[1]
  stations$UnmCross1kmRadius[i]=d$UnmCross1kmRadius[1]
  stations$Tram1kmRadius[i]=d$Tram1kmRadius[1]
  stations$Subway1kmRadius[i]=d$Subway1kmRadius[1]
  stations$ClosestTrainS[i]=d$ClosestTrainS[1]
  stations$BikeShop1kmRadius[i]=d$BikeShop1kmRadius[1]
  stations$secondary[i]=d$secondary[1]
  stations$primary[i]=d$primary[1]
  stations$living_street[i]=d$living_street[1]
  stations$cycleways[i]=d$cycleways[1]
  stations$residential[i]=d$residential[1]
  stations$path[i]=d$path[1]
  stations$ClosestBridge[i]=d$ClosestBridge[1]
  stations$isBridge[i]=d$isBridge[1]
  stations$stre_type[i]=d$stre_type[1]
  stations$stre_density[i]=mean(d$stre_density)
  stations$stre_lengths[i]=mean(d$stre_lengths)
  stations$stre_type_spec[i]=d$stre_type_spec[1]
  stations$stre_surface[i]=d$stre_surface[1]
  stations$stre_lanes[i]=mean(d$stre_lanes)
  stations$os_way_to_city[i]=mean(d$os_way_to_city)
  stations$cluster_way_to_city[i]=mean(d$cluster_way_to_city)
  stations$stre_dist[i]=d$stre_dist[1]
  stations$stre_maxspeed[i]=d$stre_maxspeed[1]
  stations$Value_Mean[i]=mean(d$Value)
}

plot11 = ggplot(data=stations)+
  geom_point(aes(x=Distance_to_Center, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Distance_to_Center, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot11

plot12 = ggplot(data=stations)+
  geom_point(aes(x=Cinemas1kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Cinemas1kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Kinos") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot12

plot13 = ggplot(data=stations)+
  geom_point(aes(x=ClosestUniBuild, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=ClosestUniBuild, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot13

plot14 = ggplot(data=stations)+
  geom_point(aes(x=SuperMarket1kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=SuperMarket1kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Supermärkte") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot14

plot15 = ggplot(data=stations)+
  geom_point(aes(x=ClothesShop2kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=ClothesShop2kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Kleidungsgeschäfte") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot15

plot16 = ggplot(data=stations)+
  geom_point(aes(x=BusStop1kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=BusStop1kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Bushaltestellen") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot16

plot17 = ggplot(data=stations)+
  geom_point(aes(x=Signals1kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Signals1kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Ampeln") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot17

plot18 = ggplot(data=stations)+
  geom_point(aes(x=Tram1kmRadius, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=Tram1kmRadius, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der S-Bahnstationen") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot18

plot19 = ggplot(data=stations)+
  geom_point(aes(x=ClosestTrainS, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=ClosestTrainS, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot19

names(BikeData)

plot53 = ggplot(data=stations)+
  geom_point(aes(x=stre_density, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=stre_density, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Örtliche Straßendichte in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot54 = ggplot(data=stations)+
  geom_point(aes(x=stre_lengths, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=stre_lengths, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Örtliche Straßenlänge in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot54

plot55 = ggplot(data=stations)+
  geom_point(aes(x=stre_lanes, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=stre_lanes, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Anzahl der Straßenspuren") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot55

stations$stre_maxspeed

plot56 = ggplot(data=stations)+
  geom_point(aes(x=stre_maxspeed, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=stre_maxspeed, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Höchstgeschwindigkeit in km/h") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot56

plot57 = ggplot(data=stations)+
  geom_point(aes(x=os_way_to_city, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=os_way_to_city, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Straße in Richtung Stadtzentrum in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot57

plot58 = ggplot(data=stations)+
  geom_point(aes(x=cluster_way_to_city, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=cluster_way_to_city, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Wegnetz in Richtung Stadtzentrum in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot58

plot59 = ggplot(data=stations)+
  geom_point(aes(x=os_way_to_city/cluster_way_to_city, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=os_way_to_city/cluster_way_to_city, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "Richtungsverhältnis") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot59

plot60 = ggplot(data=stations)+
  geom_boxplot(aes(x=stre_type, y=Value_Mean,fill=stre_type), outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  labs(y = "Radfahrer pro Stunde je Zählstelle",fill="Straßentyp",x="") + 
  theme_bw() + 
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.text.x = element_text(angle=45)) +
  theme(axis.text.x = element_text(margin = margin(t = 20, r = 0, b = -32, l = 0))) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot60

plot61 = ggplot(data=stations)+
  geom_boxplot(aes(x=stre_surface, y=Value_Mean,fill=stre_surface), outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  labs(y = "Radfahrer pro Stunde je Zählstelle",fill="Straßenbelag",x="") + 
  theme_bw() + 
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(axis.text.x = element_text(angle=45)) +
  theme(axis.text.x = element_text(margin = margin(t = 22, r = 0, b = -32, l = 0))) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot61

png(file="plot11.png",width=800, height=800)
plot11
dev.off()

png(file="plot12.png",width=800, height=800)
plot12
dev.off()

png(file="plot13.png",width=800, height=800)
plot13
dev.off()

png(file="plot14.png",width=800, height=800)
plot14
dev.off()

png(file="plot15.png",width=800, height=800)
plot15
dev.off()

png(file="plot16.png",width=800, height=800)
plot16
dev.off()

png(file="plot17.png",width=800, height=800)
plot17
dev.off()

png(file="plot18.png",width=800, height=800)
plot18
dev.off()

png(file="plot19.png",width=800, height=800)
plot19
dev.off()

png(file="plot53.png",width=800, height=800)
plot53
dev.off()

png(file="plot54.png",width=800, height=800)
plot54
dev.off()

png(file="plot55.png",width=800, height=800)
plot55
dev.off()

png(file="plot56.png",width=800, height=800)
plot56
dev.off()

png(file="plot57.png",width=800, height=800)
plot57
dev.off()

png(file="plot58.png",width=800, height=800)
plot58
dev.off()

png(file="plot59.png",width=800, height=800)
plot59
dev.off()

png(file="plot60.png",width=800, height=800)
plot60
dev.off()

png(file="plot61.png",width=800, height=800)
plot61
dev.off()
names(stations)

a = sum(stations$secondary==1)
b = sum(stations$primary==1)
c = sum(stations$residential==1)
d = sum(stations$living_street==1)
e = sum(stations$path==1)
f = sum(stations$cycleways==1)
sum(stations$isBridge==1)

g = sum(stations$secondary==0 & stations$primary==0 & stations$living_street==0 & stations$path==0 & stations$residential==0 & stations$cycleways==0)
nrow(stations) -a -b -c -d -e -f -g
street_type1 = c(a,b,c,d,e,f,g)
street_type2 = as.data.frame(cbind(street_type1,c("secondary","primary","residential","living street","path","cycleways","unknown")))
names(street_type2)[1]="Numbers"
names(street_type2)[2]="Type"
street_type2$Numbers=as.numeric(street_type2$Numbers)

plot20 = ggplot(data = street_type2, aes(x = "", y = -Numbers, 
                                         fill = reorder(Type, -Numbers))) + 
  geom_bar(stat = "identity", color = "black") +
  #geom_label_repel(aes(x = "", y = -Numbers, label = Numbers)) +
  labs(fill = "Stadt",x="",y="") +
  coord_polar("y") +
  theme_void() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot20

plot21 = ggplot(data=stations)+
  geom_point(aes(x=ClosestBridge, y=Value_Mean), size = 2) +
  geom_smooth(aes(x=ClosestBridge, y=Value_Mean),method='lm') +
  labs(y = "Radfahrer pro Stunde je Zählstelle"
       , x = "in M") + 
  scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot21

png(file="plot20.png",width=800, height=800)
plot20
dev.off()

png(file="plot21.png",width=800, height=800)
plot21
dev.off()

#Map of Germany with all Cities


city_points = as.data.frame(matrix(1:4*nlevels(as.factor(BikeData$Town)), nrow = nlevels(as.factor(BikeData$Town)), ncol = 4))

names(city_points)[1]="Town"
names(city_points)[2]="Value_Mean"
names(city_points)[3]="lon"
names(city_points)[4]="lat"
city_points$Town=levels(as.factor(BikeData$Town))

for(i in 1:nlevels(as.factor(BikeData$Town))){
  d=BikeData[BikeData$Town %in% city_points$Town[i],]
  city_points$Value_Mean[i]=mean(d$Value)
  city_points$lon[i]=d$City_Lon[1]
  city_points$lat[i]=d$City_Lat[1]
}

library(ggmap)

myLocation<-c(5, 46.5,   16, 55.5)

myMap <- get_stamenmap(bbox=myLocation, maptype="terrain-background", zoom=9)

#ggmap(myMap)

plot22 = ggmap(myMap)+
  geom_point(aes(x=as.numeric(lon), y=as.numeric(lat), color = as.numeric(Value_Mean)), data=city_points, size = 4) + 
  geom_label_repel(data = city_points,aes(x = lon, y = lat, label = Town)) +
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  labs(y = "Längengrad"
       , x = "Breitengrad"
       , color = "Durchschnittl. \n Radverkehr")

plot22  

png(file="plot22.png",width=600, height=800)
plot22
dev.off()

rm(list=setdiff(ls(), "BikeData"))

#Weather Data Visualization

weather = as.data.frame(matrix(1:3*nlevels(as.factor(BikeData$Months)), nrow = nlevels(as.factor(BikeData$Months)), ncol = 3))

names(weather)[1]="Rain"
names(weather)[2]="Temperature"
names(weather)[3]="Value_Mean"
weather$Months=levels(as.factor(BikeData$Months))

for(i in 1:nlevels(as.factor(BikeData$Months))){
  d=BikeData[BikeData$Months %in% weather$Months[i],]
  weather$Rain[i]=max(d$Rain)
  weather$Temperature[i]=mean(d$Temperature)
  weather$Value_Mean[i]=mean(d$Value)
}

weather$Months = as.numeric(weather$Months)
weather$Months = c("01","02","03","04","05","06","07","08","09","10","11","12")

plot23 = ggplot(NULL, aes(v, p)) + 
  geom_bar(data=weather, aes(x = Months, y= Rain),stat="identity", fill = "lightblue", width=.5, position = "dodge") +
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  xlab("Monat") +
  geom_line(data=weather, mapping = aes(x = Months, y = Temperature, group = 1), size = 1 , color = "red") +
  scale_y_continuous(name = "durchschn. Temperatur in C°", 
                     sec.axis = sec_axis(~.*1, name = "maximaler Niederschlag in mm")) + 
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"))  

plot23

plot24 = ggplot(NULL, aes(v, p)) + 
  geom_line(data=weather, mapping = aes(x = Months, y = Value_Mean/5, group = 1), size = 1 , color = "blue") +
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  xlab("Monat") +
  geom_line(data=weather, mapping = aes(x = Months, y = Temperature, group = 1), size = 1 , color = "red") +
  scale_y_continuous(name = "durchschn. Temperatur in C°", 
                     sec.axis = sec_axis(~.*5, name = "Durchschnitt des Radverkehrs je Stunde")) + 
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"))  

plot24

png(file="plot23.png",width=400, height=400)
plot23
dev.off()

png(file="plot24.png",width=400, height=400)
plot24
dev.off()

summary(BikeData)
levels(as.factor(BikeData[BikeData$ClosestTrainS == 50000,]$Station))

#New Plots-----------------------------------------------------------------------------------------------------------------------------------------------------------------

names(BikeData)

city_shares = c(1:nlevels(as.factor(BikeData$stre_type)))

for(i in 1: nlevels(as.factor(BikeData$stre_type))){
  city_shares[i] = sum(BikeData$stre_type==levels(as.factor(BikeData$stre_type))[i])
}

city_shares = as.data.frame(cbind(city_shares,levels(as.factor(BikeData$stre_type))))
names(city_shares)[1]="Observations"
names(city_shares)[2]="stre_type"
city_shares$Observations = as.numeric(city_shares$Observations)
city_shares$Observations = city_shares$Observations/sum(city_shares$Observations)*100

city_shares2 = city_shares[order(city_shares$Observations,decreasing=TRUE),]

pie(city_shares2$Observations, labels = city_shares2$stre_type)

plot52 = ggplot(data = city_shares2, aes(x = "", y = -Observations, 
                                        fill = reorder(stre_type, -Observations))) + 
  geom_bar(stat = "identity", color = "black") + 
  labs(fill = "Stadt",x="",y="") +
  #geom_label_repel(aes(x = "", y = -Observations, label = round(Observations))) +
  coord_polar("y") +
  theme_void() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot52

png(file="plot52.png",width=800, height=800)
plot52
dev.off()

#Corona Plots-------------------------------------------------------------------

CoronaData = BikeData[BikeData$Year > 2018, ]
CoronaData1=ddply(CoronaData,.(Timestamp),summarize,c(Value=mean(Value)))
CoronaData2=ddply(CoronaData,.(Timestamp),summarize,c(CorInz=mean(CorInz)))
CoronaData3=ddply(CoronaData,.(Timestamp),summarize,c(Lockdowns=mean(Lockdowns)))
names(CoronaData1)[2]="Value"
names(CoronaData2)[2]="CorInz"
names(CoronaData3)[2]="Lockdowns"

CD = merge(x = CoronaData1,y = CoronaData2,
                 by = c("Timestamp"),
                 all = FALSE)

CD = merge(x = CD,y = CoronaData3,
           by = c("Timestamp"),
           all = FALSE)

rm(list=setdiff(ls(), c("BikeData","CD")))

plot62 = ggplot(data = CD) + 
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  xlab("Jahre") +
  geom_line(aes(x = Timestamp, y = CorInz/3, group = 1),color="blue",alpha = 1, size = 1.4) + 
  geom_line(aes(x = Timestamp, y = Value, group = 1),color="red",alpha = .5, size = .9) +
  scale_y_continuous(name = "durchschn. Fahrradverkehr", 
                     sec.axis = sec_axis(~.*3, name = "Corona Inzidenz")) + 
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue")) +theme(axis.text.x = element_text(angle=45))
    #theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

png(file="plot62.png",width=800, height=800)
plot62
dev.off()

CD$Weekday	= format(as.POSIXlt(CD$Timestamp),"%a")
CD$Weekend <- ifelse(CD$Weekday == "So" | CD$Weekday == "Sa", "Wochenende", "Wochentag")
CD$Lockdowns <- ifelse(CD$Lockdowns == 1, "LD", "kein LD")
CD$class = paste(as.character(CD$Weekend),as.character(CD$Lockdowns))
levels(as.factor(CD$class))


plot63 = ggplot(data=CD)+
  geom_boxplot(aes(x=class, y=Value,fill=class), outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  labs(y = "Radfahrer pro Stunde je Zählstelle",fill="Kategorien",x="") + 
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

plot63

png(file="plot63.png",width=800, height=800)
plot63
dev.off()

rm(list=setdiff(ls(), c("BikeData")))
