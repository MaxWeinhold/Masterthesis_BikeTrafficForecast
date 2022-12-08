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
