#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Data Set Visualization
#Clean up memory
rm(list=ls())

library(tidyverse)
library(scales)

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
  BikeData = read.csv(file = "completeDataSet_1.csv",sep=",", encoding="ISO-8859-1")
  
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
    labs(title = "Durchschnittlicher Radverkehr nach Einwohnergröße"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "Einwohnergröße") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot1
  
  plot2 = ggplot(data=citties)+
    geom_point(aes(x=MaleRatio_Mean, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=MaleRatio_Mean, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = MaleRatio_Mean, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach Geschlechterverhältnis"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "Anteil männl. Bevölker") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot2
  
  plot3 = ggplot(data=citties)+
    geom_point(aes(x=young30_Mean, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=young30_Mean, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = young30_Mean, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach Altersgruooen"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "Anteil der Bevölker unter 30 Jahre") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot3
  
  plot(citties$Size,citties$Value_Mean)
  
  plot4 = ggplot(data=citties)+
    geom_point(aes(x=Inhabitants_Mean/Size, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=Inhabitants_Mean/Size, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = Inhabitants_Mean/Size, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach Bevölkerungsdichte"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "Einwohner je km²") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot4
  
  plot5 = ggplot(data=citties)+
    geom_point(aes(x=ADFC_Mean, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=ADFC_Mean, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = ADFC_Mean, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach ADFC Fahrradklima"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "ADFC Fahrradklima Index") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot5
  
  plot6 = ggplot(data=citties)+
    geom_point(aes(x=PKWs_Mean, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=PKWs_Mean, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = PKWs_Mean, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach PKWs je Person"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "PKWs je Person") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot6
  
  plot7 = ggplot(data=citties)+
    geom_point(aes(x=Immigrants_Mean, y=Value_Mean), size = 2) +
    geom_smooth(aes(x=Immigrants_Mean, y=Value_Mean),method='lm') +
    geom_label_repel(aes(x = Immigrants_Mean, y = Value_Mean, label = Town)) + 
    labs(title = "Durchschn. Radverkehr nach Immigrantenanteil"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , x = "Immigrantenanteil") + 
    scale_x_continuous(labels = comma) + 
    theme_classic()
  
  plot7
  
  plot8 = ggplot(citties, aes(y=Value_Mean, x=Town, fill=Town)) + 
    geom_bar(position="dodge", stat="identity") +
    theme_classic()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    labs(title = "Durchschnittlicher Radverkehr nach Städten"
         , y = "Radfahrer pro Stunde je Zählstelle"
         , fill = "Städte")
  
  plot8
  
  setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/DataSet Plots")
  png(file="plot1.png",width=800, height=800)
  plot1
  dev.off()
  
  png(file="plot2.png",width=800, height=800)
  plot2
  dev.off()
  
  png(file="plot3.png",width=800, height=800)
  plot3
  dev.off()
  
  png(file="plot4.png",width=800, height=800)
  plot4
  dev.off()
  
  png(file="plot5.png",width=800, height=800)
  plot5
  dev.off()
  
  png(file="plot6.png",width=800, height=800)
  plot6
  dev.off()
  
  png(file="plot7.png",width=800, height=800)
  plot7
  dev.off()
  
  png(file="plot8.png",width=800, height=800)
  plot8
  dev.off()
  
  rm(list=setdiff(ls(), "BikeData"))

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
    labs(title = "Städte nach Anteil am Datensatz", fill = "") +
    coord_polar("y") +
    theme_void()
  
  plot9
  
  png(file="plot9.png",width=800, height=800)
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
    theme_classic()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    labs(title = "Anteile des Datensatzes nach Einwohnergröße"
         , y = "in %"
         , fill = "Einwohnerzahl")
  
  plot10

  png(file="plot10.png",width=800, height=800)
  plot10
  dev.off()
  
#OSM Data per Counting Station
  
  
  
#Weather Data Visualization