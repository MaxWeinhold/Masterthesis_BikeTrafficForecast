#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Random Forests Modell

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
if(!require("Rcpp")) install.packages("Rcpp")
library(beepr)
library(randomForest)
library(Rcpp)


library(tidyverse)
library(caret)

#Regarding calculation power see following source: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#Historically, R has only utilized one processor, which makes it single-threaded.

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

load("ValidationSets2.rdata")

#Make a simple Test with the support vector regression to show the differences to the OLS regression

names(validation_set[[1]])

svrtest = validation_set[[1]][1:500,]
svrtest$Hour2 = svrtest$Hour^2
svrtest$Hour3 = svrtest$Hour^3


# Create an ols regression model
model1 <- lm(Value ~ Hour + Hour2 + Hour3, data=svrtest)
predict1 <- as.data.frame(predict(model1,svrtest))
predict1$Hour = svrtest$Hour
names(predict1)[1]="Value"

# Create an svm regression model
model2 <- svm(Value ~ Hour, data=svrtest)
predict2 <- as.data.frame(predict(model2, svrtest))
predict2$Hour = svrtest$Hour
names(predict2)[1]="Value"

plot25 = ggplot(svrtest,aes(x = Hour, y = Value)) +
  geom_point(size=1.5)+
  labs(title = "Vergleich zwischen OLS und Support Vector Regression"
       , color = "Methode") +
  ggtitle("Vergleich zwischen OLS und Support Vector Regression") +
  xlab("Uhrzeit") +
  ylab("Fahrradauufkommen") +
  theme_bw() +
  geom_line(data = predict1, aes(x = Hour, y = Value, color='OLS'), size = 1.5) + 
  geom_line(data = predict2, aes(x = Hour, y = Value, color='SVR'), size = 1.5)

plot25

#setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")

#png(file="plot25.png",width=800, height=800)
#plot25
#dev.off()

rm(plot25,model1,model2,predict1,predict2,svrtest)

#Now the real Modell

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

for_start_time <- Sys.time()
for(i in 1:length(validation_set)){
  
  print(i)
  
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
  
  # Split data to reduce duration of computation
  training.samples <- trainSet$Value %>%
    createDataPartition(p = 0.05, list = FALSE)
  train.data  <- trainSet[training.samples, ]
  test.data <- trainSet[-training.samples, ]
  
  #Now do Model calculations
  start_time <- Sys.time()
  print("Starts to train the modell")
  print(start_time)
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
                          .data_motorway + .data_path + .data_pedestrian + .data_primary + .data_residential +
                          .data_secondary + .data_service + .data_steps + .data_tertiary + .data_track +
                          .data_trunk_link + .data_unclassified + .data_driveway + .data_empty + .data_sidepath +
                          .data_sidewalk + .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                          .data_sidewalk + .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                          .data_paved + .data_paving_stones + .data_pebblestone + .data_sett + .data_unknown +
                          stre_lengths + stre_lanes + stre_maxspeed + bridge + os_way_to_city + 
                          cluster_way_to_city + .data_cycleway + 
                          Rain2 + Temperature2 + Inhabitants2 + stre_lengths2 + 
                          os_way_to_city2, data =  train.data, ntree=250, importance=TRUE)
  
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  start_time <- Sys.time()
  print("Starts to calclulate test predictions")
  print(start_time)
  test_predict <- as.data.frame(predict(model, newdata = testSet, type='response'))
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  start_time <- Sys.time()
  print("Starts to calclulate train predictions")
  print(start_time)
  train_predict <- as.data.frame(predict(model, data = train.data, type='response'))
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  Evaluation_DF$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
  Evaluation_DF$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
  
  #cor(test_predict,testSet$Value) ^ 2
  #cor(train_predict,trainSet$Value) ^ 2
  Evaluation_DF$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
  
  print(Evaluation_DF)
  #vcovHAC(model)
}
for_end_time <- Sys.time()
print("The hole process took:")
print(for_end_time - for_start_time)

mean(Evaluation_DF$Train_R)
mean(Evaluation_DF$Train_RMSE)

mean(Evaluation_DF$Test_R)
mean(Evaluation_DF$Test_RMSE)

Evaluation_DF[6,]=c(mean(Evaluation_DF$Train_R),mean(Evaluation_DF$Train_RMSE),mean(Evaluation_DF$Test_R),mean(Evaluation_DF$Test_RMSE))
Evaluation_DF$Sets=c("1","2","3","4","5","Mean")
Evaluation_DF <- Evaluation_DF[, c(5,1,2,3,4)]

beep("mario")

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/ValidationResults")
write.csv(Evaluation_DF,"Modell3_RF.csv")
save(model,file="Modell2_RF_newDataset.rdata")


