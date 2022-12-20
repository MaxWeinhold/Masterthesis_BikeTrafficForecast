#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: OLS Regression Modell_Feature Selection

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)

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

length(validation_set)

Evaluation_DF = as.data.frame( matrix(1:length(validation_set)*4, nrow = length(validation_set), ncol = 4) )
names(Evaluation_DF)[1] = "Train_R"
names(Evaluation_DF)[2] = "Train_RMSE"
names(Evaluation_DF)[3] = "Test_R"
names(Evaluation_DF)[4] = "Test_RMSE"

Evaluation_DF1 = Evaluation_DF
Evaluation_DF2 = Evaluation_DF
Evaluation_DF3 = Evaluation_DF
Evaluation_DF4 = Evaluation_DF
Evaluation_DF5 = Evaluation_DF
Evaluation_DF6 = Evaluation_DF

i=1

for_start_time <- Sys.time()
for(i in 1:length(validation_set)){
  
  for_start_time <- Sys.time()
  
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
  
  #Now do Model calculations
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF1$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF1$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF1$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF1$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  beep("coin")
  
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                 Wind + CloudCover + Humidity + Rain + Temperature, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF2$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF2$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF2$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF2$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  beep("coin")
  
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                 Wind + CloudCover + Humidity + Rain + Temperature +
                 ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF3$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF3$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF3$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF3$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  beep("coin")
  
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                 ClosestBikeShop + BikeShop1kmRadius + BikeShop3kmRadius +
                 cycleways + path + secondary + primary + residential + ClosestBridge +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF4$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF4$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF4$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF4$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  beep("coin")
  
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                 ClosestBikeShop + BikeShop1kmRadius + BikeShop3kmRadius +
                 cycleways + path + secondary + primary + residential + ClosestBridge +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs +
                 Rain2 + Temperature2 + Inhabitants2 + ADFC_Index2 + UniBuild500mmRadius2 +
                 ClothesShop500mmRadius2 + ClosestTrainS2 + ClosestBridge2 + young302 + PKWs2, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF5$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF5$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF5$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF5$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  beep("coin")
  
  model1 <- lm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                 ClosestBikeShop + BikeShop1kmRadius + BikeShop3kmRadius +
                 cycleways + path + secondary + primary + residential + ClosestBridge +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs +
                 Rain2 + Temperature2 + Inhabitants2 + ADFC_Index2 + UniBuild500mmRadius2 +
                 ClothesShop500mmRadius2 + ClosestTrainS2 + ClosestBridge2 + young302 + PKWs2 +
                 Rain3 + Inhabitants3 + UniBuild500mmRadius3 + ClothesShop500mmRadius3 + ClosestTrainS3, data = trainSet)
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(trainSet)
  
  Evaluation_DF6$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF6$Train_RMSE[i] = sqrt(mean((trainSet$Value - exp(train_predict))^2))
  
  Evaluation_DF6$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF6$Train_R[i]= postResample(exp(train_predict), trainSet$Value)[2]
  
  for_end_time <- Sys.time()
  print("This round took took:")
  print(for_end_time - for_start_time)
}


TrainR = as.data.frame(rbind(Evaluation_DF1$Train_R,Evaluation_DF2$Train_R))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF3$Train_R))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF4$Train_R))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF5$Train_R))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF6$Train_R))

names(TrainR)[1]="Split1"
names(TrainR)[2]="Split2"
names(TrainR)[3]="Split3"
names(TrainR)[4]="Split4"
names(TrainR)[5]="Split5"
TrainR$model = c("1","2","3","4","5","6")

TestR = as.data.frame(rbind(Evaluation_DF1$Test_R,Evaluation_DF2$Test_R))
TestR = as.data.frame(rbind(TestR,Evaluation_DF3$Test_R))
TestR = as.data.frame(rbind(TestR,Evaluation_DF4$Test_R))
TestR = as.data.frame(rbind(TestR,Evaluation_DF5$Test_R))
TestR = as.data.frame(rbind(TestR,Evaluation_DF6$Test_R))

names(TestR)[1]="Split1"
names(TestR)[2]="Split2"
names(TestR)[3]="Split3"
names(TestR)[4]="Split4"
names(TestR)[5]="Split5"
TestR$model = c("1","2","3","4","5","6")

R2_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  xlab("Model") +
  labs(y = "Bestimmtheitsmaß") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split1, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split2, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split3, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split4, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split5, group = 1), size = 1 , color = "red") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split1, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split2, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split3, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split4, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split5, group = 1), size = 1 , color = "blue")


TrainR = as.data.frame(rbind(Evaluation_DF1$Train_RMSE,Evaluation_DF2$Train_RMSE))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF3$Train_RMSE))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF4$Train_RMSE))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF5$Train_RMSE))
TrainR = as.data.frame(rbind(TrainR,Evaluation_DF6$Train_RMSE))

names(TrainR)[1]="Split1"
names(TrainR)[2]="Split2"
names(TrainR)[3]="Split3"
names(TrainR)[4]="Split4"
names(TrainR)[5]="Split5"
TrainR$model = c("1","2","3","4","5","6")

TestR = as.data.frame(rbind(Evaluation_DF1$Test_RMSE,Evaluation_DF2$Test_RMSE))
TestR = as.data.frame(rbind(TestR,Evaluation_DF3$Test_RMSE))
TestR = as.data.frame(rbind(TestR,Evaluation_DF4$Test_RMSE))
TestR = as.data.frame(rbind(TestR,Evaluation_DF5$Test_RMSE))
TestR = as.data.frame(rbind(TestR,Evaluation_DF6$Test_RMSE))

names(TestR)[1]="Split1"
names(TestR)[2]="Split2"
names(TestR)[3]="Split3"
names(TestR)[4]="Split4"
names(TestR)[5]="Split5"
TestR$model = c("1","2","3","4","5","6")

MSE_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() + 
  labs(y = "Bestimmtheitsmaß") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  xlab("Model") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split1, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split2, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split3, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split4, group = 1), size = 1 , color = "red") +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split5, group = 1), size = 1 , color = "red") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split1, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split2, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split3, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split4, group = 1), size = 1 , color = "blue") +
  geom_line(data = TestR, mapping = aes(x = model, y = Split5, group = 1), size = 1 , color = "blue")


setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot30.png",width=800, height=800)
R2_plot
dev.off()

png(file="plot31.png",width=800, height=800)
MSE_plot
dev.off()







beep("mario")
