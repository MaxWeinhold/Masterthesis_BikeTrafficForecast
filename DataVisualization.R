#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Data Set Visualization
#Clean up memory
rm(list=ls())

library(tidyverse)

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

barplot(citties$Value_Mean,citties$Inhabitants_Mean)

plot(citties$Inhabitants_Mean,citties$Value_Mean)

ggplot(citties)+
  geom_point(aes(y=Value_Mean,x=Inhabitants_Mean,fill=Town))

ggplot()+
  geom_point(aes(x=Value_Mean, y=Inhabitants_Mean, color = Town), data=citties, size = 4) + 
  geom_label_repel(aes(label = Town),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()

plot(citties$MaleRatio_Mean,citties$Value_Mean)
plot(citties$young30_Mean,citties$Value_Mean)
plot(citties$Temperature_Mean,citties$Value_Mean)
plot(citties$Rain_Mean,citties$Value_Mean)
plot(citties$Size,citties$Value_Mean)
plot(citties$ADFC_Mean,citties$Value_Mean)
plot(citties$PKWs_Mean,citties$Value_Mean)
plot(citties$Immigrants_Mean,citties$Value_Mean)

ggplot(citties, aes(y=Value_Mean, x=Town, fill=Town)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(title = "Durchschnittlicher Radverkehr nach Städten"
       , y = "Radfahrer pro Stunde je Zählstelle"
       , fill = "Städte")





