#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Conditional Validation Set Building

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_1.csv",sep=",", encoding="ISO-8859-1")

#Clean Data from unneccesary Variables because of RAM limitations

names(BikeData)
BikeData$Day = NULL
BikeData$Oneway = NULL
BikeData$Weekday = NULL
BikeData$City_Lon = NULL
BikeData$City_Lat = NULL
BikeData$ClosestCinema = NULL
BikeData$Cinemas1kmRadius = NULL
BikeData$InhDestrict = NULL
BikeData$residential = NULL
BikeData$living_street = NULL

BikeData$SuperMarket500mmRadius = NULL
BikeData$ClosestBusStop = NULL
BikeData$Signals1kmRadius = NULL
BikeData$UnmCross1kmRadius = NULL
BikeData$TrainS1kmRadius = NULL
BikeData$ClosestSubway = NULL
BikeData$ClosestBikeShop = NULL
BikeData$BikeShop1kmRadius = NULL
BikeData$ClosestSignals = NULL
BikeData$ClosestUnmCross = NULL
BikeData$ClosestTram = NULL

BikeData$Rain2 = BikeData$Rain^2
BikeData$Temperature2 = BikeData$Temperature^2
BikeData$Inhabitants2 = BikeData$Inhabitants^2
BikeData$ADFC_Index2 = BikeData$ADFC_Index^2
BikeData$UniBuild500mmRadius2 = BikeData$UniBuild500mmRadius^2
BikeData$ClothesShop500mmRadius2 = BikeData$ClothesShop500mmRadius^2
BikeData$ClosestTrainS2 = BikeData$ClosestTrainS^2
BikeData$ClosestBridge2 = BikeData$ClosestBridge^2
BikeData$young302 = BikeData$young30^2
BikeData$PKWs2 = BikeData$PKWs^2

BikeData$Rain3 = BikeData$Rain^3
BikeData$Inhabitants3 = BikeData$Inhabitants^3
BikeData$UniBuild500mmRadius3 = BikeData$UniBuild500mmRadius^3
BikeData$ClothesShop500mmRadius3 = BikeData$ClothesShop500mmRadius^3
BikeData$ClosestTrainS3 = BikeData$ClosestTrainS^3
BikeData$ClosestBridge3 = BikeData$ClosestBridge3

BikeData$SignalsRatio = BikeData$UnmCross250mmRadius/(BikeData$UnmCross250mmRadius + BikeData$Signals250mmRadius + 1)

summary(BikeData)

BikeData = BikeData %>%
  mutate(Value = ifelse(Value == 0,1,Value))

#Get all stations and how many observations each station offers

Obs_perStation = as.data.frame(c(1:nlevels(as.factor(BikeData$Station))))
Obs_perStation$Station = levels(as.factor(BikeData$Station))
names(Obs_perStation)[1] = "Observations"

for(i in 1:nrow(Obs_perStation)){
  d=BikeData[BikeData$Station %in% Obs_perStation$Station[i],]
  Obs_perStation$Observations[i] = nrow(d)
}

sum(Obs_perStation$Observations)

#new we randomly select any station for the validation sets and allways try to get a mostly even number of observation sets

set.seed(2022) 

validation_splits = 5
actual_split = 1
stations_not_chosen = levels(as.factor(BikeData$Station))

stations_splits = as.data.frame(levels(as.factor(BikeData$Station)))
names(stations_splits)[1] = "Station"
stations_splits$Split = 0

observations_per_splits = as.data.frame(c(1:validation_splits))
names(observations_per_splits)[1] = "ValidationSet"
observations_per_splits$Observations = 0

while(length(stations_not_chosen)>0)
{
    x1 <- sample(1:length(stations_not_chosen), 1)
    newStation = stations_not_chosen[x1]
    
    stations_splits[stations_splits$Station == newStation,]$Split = actual_split
    
    observations_per_splits$Observations[actual_split] = observations_per_splits$Observations[actual_split] + Obs_perStation[Obs_perStation$Station == newStation,]$Observations
    
    stations_not_chosen = stations_not_chosen[-x1]
    
    if(median(observations_per_splits$Observations)<=observations_per_splits$Observations[actual_split]){
      
      actual_split = actual_split + 1
      if(actual_split>validation_splits){actual_split=1}
      
    }
}

sum(observations_per_splits$Observations)

#Now create the data set Subsets

validation_set <- list()

for(i in 1:validation_splits){
  
  stations_to_pick = stations_splits[stations_splits$Split %in% i,]
  vs = BikeData[BikeData$Station %in% stations_to_pick$Station,]
  
  validation_set[[i]] <- vs
}

summary(validation_set[1])

rm(list=setdiff(ls(), "validation_set"))

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
save(validation_set,file="ValidationSets.rdata")
