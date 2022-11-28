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

names(BikeData)

nrow(BikeData)


citties = as.data.frame(matrix(1:13*nlevels(as.factor(BikeData$Town)), nrow = nlevels(as.factor(BikeData$Town)), ncol = 13))

names(citties)[1]="Town"
names(citties)[2]="Value_Mean"
names(citties)[3]="Value_Median"
names(citties)[4]="Value_Min"
names(citties)[5]="Value_Max"
names(citties)[6]="Inhabitants_Mean"
names(citties)[7]="MaleRatio_Mean"
names(citties)[8]="YouthRatio18_Mean"
names(citties)[9]="YouthRatio20_Mean"
names(citties)[10]="Temperature_Mean"
names(citties)[11]="Rain_Mean"
names(citties)[12]="Size"
names(citties)[13]="ADFC_Mean"
citties$Town=levels(as.factor(BikeData$Town))


for(i in 1:nlevels(as.factor(BikeData$Town))){
  d=BikeData[BikeData$Town %in% citties$Town[i],]
  citties$Value_Mean[i]=mean(d$Value)
  citties$Value_Median[i]=median(d$Value)
  citties$Value_Max[i]=max(d$Value)
  citties$Value_Min[i]=min(d$Value)
  citties$Inhabitants_Mean[i]=mean(d$Inhabitants)
  citties$MaleRatio_Mean[i]=mean(d$Male_Ratio)
  citties$YouthRatio18_Mean[i]=mean(d$young18)
  citties$YouthRatio20_Mean[i]=mean(d$young20)
  citties$Temperature_Mean[i]=mean(d$Temperature)
  citties$Rain_Mean[i]=mean(d$Rain)
  citties$Size[i]=mean(d$Area)
  citties$ADFC_Mean[i]=mean(d$ADFC_Index)
}

barplot(citties$Value_Mean,citties$Inhabitants_Mean)

plot(citties$Inhabitants_Mean,citties$Value_Mean)
plot(citties$MaleRatio_Mean,citties$Value_Mean)
plot(citties$YouthRatio20_Mean,citties$Value_Mean)
plot(citties$Temperature_Mean,citties$Value_Mean)
plot(citties$Rain_Mean,citties$Value_Mean)
plot(citties$Size,citties$Value_Mean)
plot(citties$ADFC_Mean,citties$Value_Mean)


ggplot() + geom_bar(data=citties, aes(x=Value_Mean, color = Town), size = 2)

ggplot(NULL, aes(v, p)) + 
  geom_bar(data=citties, aes(x = Town, y= Value_Mean, fill = ADFC_Mean),stat="identity", width=.5, position = "dodge")
  
ggplot(NULL, aes(v, p)) + 
  geom_bar(data=citties, aes(x = Town, y= Value_Mean),stat="identity", width=.5, position = "dodge") +
  geom_bar(data=citties, aes(x = Town, y= Value_Median),stat="identity", width=.5, position = "dodge")

