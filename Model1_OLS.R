#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: OLS Regression Modell

library(tidyverse)
library(sandwich)
library(caret)

#Regarding calculation power see following source: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#Historically, R has only utilized one processor, which makes it single-threaded.

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

load("ValidationSets.rdata")

names(validation_set[[1]])

levels(as.factor(validation_set[[1]]$Town))
levels(as.factor(validation_set[[2]]$Town))
levels(as.factor(validation_set[[3]]$Town))
levels(as.factor(validation_set[[4]]$Town))
levels(as.factor(validation_set[[5]]$Town))

length(validation_set)

Evaluation_DF = as.data.frame( matrix(1:length(validation_set)*4, nrow = length(validation_set), ncol = 4) )
names(Evaluation_DF)[1] = "Train_R"
names(Evaluation_DF)[2] = "Train_RMSE"
names(Evaluation_DF)[3] = "Test_R"
names(Evaluation_DF)[4] = "Test_RMSE"

for(i in 1:length(validation_set)){
  
  testSet = validation_set[[i]]
  sets = c(1:length(validation_set))
  sets <- sets[!sets %in% i]
  
  trainSet = rbind(validation_set[[ sets[1] ]],validation_set[[ sets[2] ]])
  trainSet = rbind(trainSet,validation_set[[ sets[3] ]])
  trainSet = rbind(trainSet,validation_set[[ sets[4] ]])
  
  testSet = testSet %>%
    mutate(Value = ifelse(Value == 0,1,Value))
  
  trainSet = trainSet %>%
    mutate(Value = ifelse(Value == 0,1,Value))
  
  trainSet$X = NULL
  testSet$X = NULL
  names(testSet)
  
  #Now do Model calculations
  model <- lm(log(Value) ~ Hour + Months + Weekend + Night + publicHoliday + schoolHoliday + 
                Wind + CloudCover + Humidity + Rain + Temperature + Cinemas3kmRadius +
                ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center + 
                ClosestSchool + Schools500mmRadius + Schools2kmRadius + ClosestUniBuild + UniBuild500mmRadius + UniBuild2kmRadius + 
                ClosestSuperMarket + SuperMarket1kmRadius + ClosestClothesShop + ClothesShop500mmRadius + BusStop250mmRadius + ClothesShop2kmRadius + Signals250mmRadius +
                BusStop250mmRadius + UnmCross250mmRadius + BusStop1kmRadius + Tram250mmRadius + Subway250mmRadius + ClosestTrainS + BikeShop3kmRadius + 
                cycleways + path + secondary + primary + ClosestBridge + young18 + young25 + young30 + 
                older40 + older60 + Immigrants + PKWs +
                Rain2 + Temperature2 + Inhabitants2 + ADFC_Index2 + UniBuild500mmRadius2 + ClothesShop500mmRadius2 +
                ClosestTrainS2 + ClosestBridge2 + young302 + PKWs2 + Rain3 +
                Inhabitants3 + UniBuild500mmRadius3 + ClothesShop500mmRadius3 + ClosestTrainS3 + SignalsRatio, data = trainSet)
  
  summary(model)
  
  test_predict <- model %>% predict(testSet)
  train_predict <- model %>% predict(trainSet)
  
  Evaluation_DF$Test_RMSE[i] = sqrt(mean((testSet$Value - test_predict)^2))
  Evaluation_DF$Train_RMSE[i] = sqrt(mean((trainSet$Value - train_predict)^2))
  
  #cor(test_predict,testSet$Value) ^ 2
  #cor(train_predict,trainSet$Value) ^ 2
  Evaluation_DF$Test_R[i]= postResample(test_predict, testSet$Value)[2]
  Evaluation_DF$Train_R[i]= postResample(train_predict, trainSet$Value)[2]
  
  #vcovHAC(model)
}

mean(Evaluation_DF$Train_R)
mean(Evaluation_DF$Train_RMSE)

mean(Evaluation_DF$Test_R)
mean(Evaluation_DF$Test_RMSE)
