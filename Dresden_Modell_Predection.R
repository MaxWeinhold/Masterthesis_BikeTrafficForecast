#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Dresden Modell Predection

#Clean up memory
rm(list=ls())

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
if(!require("Rcpp")) install.packages("Rcpp")
library(beepr)
library(randomForest)
library(Rcpp)
if(!require("fastDummies")) install.packages("fastDummies")
library(fastDummies)

library(tidyverse)
library(caret)

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Read Bycicle Counting Data----------------------------------------------

DresdenData = read.csv(file = "Dresden_Daten_Komplett.csv",sep=",", encoding="ISO-8859-1")


length(names(DresdenData))
DresdenData = cbind(DresdenData,dummy_cols(DresdenData$stre_type))
DresdenData = cbind(DresdenData,dummy_cols(DresdenData$stre_type_spec))
DresdenData = cbind(DresdenData,dummy_cols(DresdenData$stre_surface))
length(names(DresdenData))


DresdenData$.data_footway = 0
DresdenData$.data_pedestrian = 0
if(is.null(DresdenData$.data_motorway)){DresdenData$.data_motorway = 0}
if(is.null(DresdenData$.data_driveway)){DresdenData$.data_driveway = 0}
if(is.null(DresdenData$.data_sidepath)){DresdenData$.data_sidepath = 0}
if(is.null(DresdenData$.data_sidewalk)){DresdenData$.data_sidewalk = 0}
if(is.null(DresdenData$.data_pebblestone)){DresdenData$.data_pebblestone = 0}
if(is.null(DresdenData$.data_trunk_link)){DresdenData$.data_trunk_link = 0}
if(is.null(DresdenData$.data_living_street)){DresdenData$.data_living_street = 0}
if(is.null(DresdenData$.data_asphalt)){DresdenData$.data_asphalt = 0}
if(is.null(DresdenData$.data_compacted)){DresdenData$.data_compacted = 0}
if(is.null(DresdenData$.data_concrete)){DresdenData$.data_concrete = 0}
if(is.null(DresdenData$.data_fine_gravel)){DresdenData$.data_fine_gravel = 0}
if(is.null(DresdenData$.data_paved)){DresdenData$.data_paved = 0}
if(is.null(DresdenData$.data_paving_stones)){DresdenData$.data_paving_stones = 0}
if(is.null(DresdenData$.data_sett)){DresdenData$.data_sett = 0}
if(is.null(DresdenData$.data_unknown)){DresdenData$.data_unknown = 0}
if(is.null(DresdenData$.data_cycleway)){DresdenData$.data_cycleway = 0}
if(is.null(DresdenData$.data_path)){DresdenData$.data_path = 0}
if(is.null(DresdenData$.data_primary)){DresdenData$.data_primary = 0}
if(is.null(DresdenData$.data_residential)){DresdenData$.data_residential = 0}
if(is.null(DresdenData$.data_secondary)){DresdenData$.data_secondary = 0}
if(is.null(DresdenData$.data_service)){DresdenData$.data_service = 0}
if(is.null(DresdenData$.data_tertiary)){DresdenData$.data_tertiary = 0}
if(is.null(DresdenData$.data_track)){DresdenData$.data_track = 0}
if(is.null(DresdenData$.data_unclassified)){DresdenData$.data_unclassified = 0}
if(is.null(DresdenData$.data_steps)){DresdenData$.data_steps = 0}

DresdenData$Rain2 = DresdenData$Rain^2
DresdenData$Temperature2 = DresdenData$Temperature^2
DresdenData$Inhabitants2 = DresdenData$Inhabitants^2
DresdenData$ADFC_Index2 = DresdenData$ADFC_Index^2

DresdenData$ClosestSchool2 = DresdenData$ClosestSchool^2
DresdenData$Schools500mmRadius2 = DresdenData$Schools500mmRadius^2
DresdenData$UniBuild2kmRadius2 = DresdenData$UniBuild2kmRadius2^2

DresdenData$ClosestUniBuild2 = DresdenData$ClosestUniBuild^2
DresdenData$UniBuild500mmRadius2 = DresdenData$UniBuild500mmRadius^2
DresdenData$ClosestSchool2 = DresdenData$ClosestSchool^2

DresdenData$ClosestClothesShop2 = DresdenData$ClosestClothesShop^2
DresdenData$ClothesShop500mmRadius2 = DresdenData$ClothesShop500mmRadius^2
DresdenData$ClothesShop2kmRadius2 = DresdenData$ClothesShop2kmRadius^2

DresdenData$ClosestTrainS2 = DresdenData$ClosestTrainS^2
DresdenData$ClosestBridge2 = DresdenData$ClosestBridge^2
DresdenData$young302 = DresdenData$young30^2
DresdenData$PKWs2 = DresdenData$PKWs^2

DresdenData$CorInz2 = DresdenData$CorInz^2
DresdenData$stre_dist2 = DresdenData$stre_dist^2
DresdenData$stre_density2 = DresdenData$stre_density^2
DresdenData$stre_lengths2 = DresdenData$stre_lengths^2
DresdenData$stre_lanes2 = DresdenData$stre_lanes^2
DresdenData$stre_maxspeed2 = DresdenData$stre_maxspeed^2
DresdenData$os_way_to_city2 = DresdenData$os_way_to_city^2
DresdenData$cluster_way_to_city2 = DresdenData$cluster_way_to_city^2

DresdenData$Rain3 = DresdenData$Rain^3
DresdenData$Inhabitants3 = DresdenData$Inhabitants^3
DresdenData$UniBuild500mmRadius3 = DresdenData$UniBuild500mmRadius^3
DresdenData$ClothesShop500mmRadius3 = DresdenData$ClothesShop500mmRadius^3
DresdenData$ClosestTrainS3 = DresdenData$ClosestTrainS^3
DresdenData$ClosestBridge3 = DresdenData$ClosestBridge3
DresdenData$stre_lengths3 = DresdenData$stre_lengths^3
DresdenData$stre_lanes3 = DresdenData$stre_lanes^3
DresdenData$stre_maxspeed3 = DresdenData$stre_maxspeed^3
DresdenData$os_way_to_city3 = DresdenData$os_way_to_city^3
DresdenData$cluster_way_to_city3 = DresdenData$cluster_way_to_city^3

DresdenData$SignalsRatio = DresdenData$UnmCross250mmRadius/(DresdenData$UnmCross250mmRadius + DresdenData$Signals250mmRadius + 1)


if(is.null(DresdenData$stre_lengths2)){DresdenData$stre_lengths2 = DresdenData$stre_lengths^2}
if(is.null(DresdenData$ClosestSchool2)){DresdenData$ClosestSchool2 = DresdenData$ClosestSchool^2}
if(is.null(DresdenData$ClosestUniBuild2)){DresdenData$ClosestUniBuild2 = DresdenData$ClosestUniBuild^2}
if(is.null(DresdenData$ClosestClothesShop2)){DresdenData$ClosestClothesShop2 = DresdenData$ClosestClothesShop^2}
if(is.null(DresdenData$stre_lanes2)){DresdenData$stre_lanes2 = DresdenData$stre_lanes^2}
if(is.null(DresdenData$stre_maxspeed2)){DresdenData$stre_maxspeed2 = DresdenData$stre_maxspeed^2}
if(is.null(DresdenData$stre_lengths3)){DresdenData$stre_lengths3 = DresdenData$stre_lengths^3}
if(is.null(DresdenData$stre_lanes3)){DresdenData$stre_lanes3 = DresdenData$stre_lanes^3}
if(is.null(DresdenData$stre_maxspeed3)){DresdenData$stre_maxspeed3 = DresdenData$stre_maxspeed^3}
if(is.null(DresdenData$stre_lanes2)){DresdenData$stre_lanes2 = DresdenData$stre_lanes^2}

#calculated models are her:
setwd("D:/STUDIUM/Münster/7. Semester")

load("Modell3_RF_newDataset.rdata")

projection_pred <- predict(model, newdata = DresdenData, type='response')

summary(as.numeric(projection_pred))
summary(exp(as.numeric(projection_pred)))
summary(DresdenData$Value)

nrow(DresdenData)

nlevels(as.factor(DresdenData$Station))
levels(as.factor(DresdenData$Year))

RMSE = sqrt(mean((DresdenData$Value - exp(projection_pred))^2))

Rvalue = postResample(exp(projection_pred), DresdenData$Value)[2]

