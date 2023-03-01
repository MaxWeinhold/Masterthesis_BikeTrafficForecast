#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Non Spatial Modell Evalaluation

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
if(!require("Rcpp")) install.packages("Rcpp")
library(beepr)
library(randomForest)
library(Rcpp)


library(tidyverse)
library(caret)

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

load("ValidationSets2.rdata")

DS = cbind(validation_set[[1]],validation_set[[2]])
DS = cbind(DS,validation_set[[3]])
DS = cbind(DS,validation_set[[4]])
DS = cbind(DS,validation_set[[5]])

nrow(DS)

samples <- DS$Value %>%
  createDataPartition(p = 0.1, list = FALSE)
DS <- DS[samples, ]

nrow(DS)

training.samples <- DS$Value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- DS[training.samples, ]
test.data <- DS[-training.samples, ]

model <- randomForest(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                        Wind + CloudCover + Humidity + Rain + Temperature +
                        ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center +
                        ClosestCinema + Cinemas1kmRadius + Cinemas3kmRadius +
                        ClosestSchool + Schools500mmRadius + Schools2kmRadius +
                        ClosestUniBuild + UniBuild500mmRadius + UniBuild2kmRadius +
                        ClosestSuperMarket + SuperMarket500mmRadius + SuperMarket1kmRadius +
                        ClosestClothesShop + ClothesShop500mmRadius + ClothesShop2kmRadius +
                        ClosestBusStop + BusStop250mmRadius + BusStop1kmRadius +
                        ClosestSignals + Signals250mmRadius + Signals1kmRadius +
                        ClosestUnmCross + UnmCross250mmRadius + UnmCross1kmRadius +
                        ClosestTrainS + TrainS1kmRadius + TrainS3kmRadius +
                        ClosestBikeShop + BikeShop1kmRadius + BikeShop3kmRadius + ClosestBridge +
                        young18 + young25 + older40 + older60 + Immigrants + PKWs +
                        CorInz + Lockdowns + stre_dist + .data_footway + .data_living_street +
                        .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                        .data_sidewalk + .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                        .data_paved + .data_paving_stones + .data_pebblestone + .data_sett + .data_unknown +
                        stre_lengths + stre_lanes + stre_maxspeed + bridge + .data_cycleway + 
                        secondary + primary + path + living_street + residential + cycleways +
                        Rain2 + Temperature2 + Inhabitants2 + ADFC_Index2 + ClosestSchool2 + 
                        ClosestUniBuild2 + ClosestClothesShop2 + ClosestTrainS2 + young302 + PKWs2 +
                        stre_lengths2 + stre_lanes2 + stre_maxspeed2 + UniBuild500mmRadius3 +
                        ClothesShop500mmRadius3 + ClosestTrainS3 + stre_lengths3 + stre_lanes3 + stre_maxspeed3, 
                      data =  train.data, ntree=400, importance=TRUE)


test_predict <- as.data.frame(predict(model, newdata = test.data, type='response'))
train_predict <- as.data.frame(predict(model, data = train.data, type='response'))

Test_RMSE = sqrt(mean((test.data$Value - exp(test_predict[,1]))^2))
Train_RMSE = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))

Test_R = postResample(exp(test_predict), test.data$Value)[2]
Train_R = postResample(exp(train_predict), train.data$Value)[2]

beep("mario")

