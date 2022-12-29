#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Support Vector Regression Modell_Feature Selection

#In order to create a SVR model with R you will need the package e1071
if(!require("e1071")) install.packages("e1071")
library(e1071)

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)

library(parallel)
citation("parallel")

library(tidyverse)
library(sandwich)
library(caret)

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

load("ValidationSets.rdata")

#Now the real Modell

length(validation_set)

Evaluation_DF = as.data.frame( matrix(1:length(validation_set)*4, nrow = length(validation_set), ncol = 4) )
names(Evaluation_DF)[1] = "Train_R"
names(Evaluation_DF)[2] = "Train_RMSE"
names(Evaluation_DF)[3] = "Test_R"
names(Evaluation_DF)[4] = "Test_RMSE"
Evaluation_DF$Time = 0

Evaluation_DF1 = Evaluation_DF
Evaluation_DF2 = Evaluation_DF
Evaluation_DF3 = Evaluation_DF
Evaluation_DF4 = Evaluation_DF
Evaluation_DF5 = Evaluation_DF
Evaluation_DF6 = Evaluation_DF

i=1

for1_start_time <- Sys.time()
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
  
  # Split data to reduce duration of computation
  training.samples <- trainSet$Value %>%
    createDataPartition(p = 0.0005, list = FALSE)
  train.data  <- trainSet[training.samples, ]
  test.data <- trainSet[-training.samples, ]
  
  rm(trainSet)
  
  loop_start_time1 <- Sys.time()
  #Now do Model calculations
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday, 
                data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time1 <- Sys.time()
  
  Evaluation_DF1$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF1$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF1$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF1$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF1$Time[i] = difftime(loop_end_time1, loop_end_time1, units='mins')
  
  print("Test R")
  print(Evaluation_DF1$Train_R[i])
  print("M0del1 took:")
  print(loop_end_time1 - loop_start_time1)
  
  rm(model1,test_predict,train_predict)
  
  beep("coin")
  
  loop_start_time2 <- Sys.time()
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                 Wind + CloudCover + Humidity + Rain + Temperature, 
                 data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time2 <- Sys.time()
  
  Evaluation_DF2$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF2$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF2$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF2$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF2$Time[i] = difftime(loop_end_time2, loop_start_time2, units='mins')
  
  print("Test R")
  print(Evaluation_DF2$Train_R[i])
  print("M0del2 took:")
  print(loop_end_time2 - loop_start_time2)
  
  rm(model1,test_predict,train_predict)
  
  beep("coin")
  
  loop_start_time3 <- Sys.time()
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                 Wind + CloudCover + Humidity + Rain + Temperature +
                 ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs, 
                 data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time3 <- Sys.time()
  
  Evaluation_DF3$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF3$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF3$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF3$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF3$Time[i] = difftime(loop_end_time3, loop_start_time3, units='mins')
  
  print("Test R")
  print(Evaluation_DF3$Train_R[i])
  print("M0del3 took:")
  print(loop_end_time3 - loop_start_time3)
  
  rm(model1,test_predict,train_predict)
  
  beep("coin")
  
  loop_start_time4 <- Sys.time()
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
                 Wind + CloudCover + Humidity + Rain + Temperature +
                 ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center +
                 ClosestSchool + Schools500mmRadius + Schools2kmRadius +
                 ClosestUniBuild + UniBuild500mmRadius + UniBuild2kmRadius +
                 ClosestSuperMarket + SuperMarket500mmRadius + SuperMarket1kmRadius +
                 ClosestClothesShop + ClothesShop500mmRadius + ClothesShop2kmRadius +
                 ClosestBusStop + BusStop250mmRadius + BusStop1kmRadius +
                 ClosestSignals + Signals250mmRadius + Signals1kmRadius +
                 ClosestTrainS + TrainS1kmRadius + TrainS3kmRadius +
                 young18 + young25 + older40 + older60 + Immigrants + PKWs,
                data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time4 <- Sys.time()
  
  Evaluation_DF4$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF4$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF4$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF4$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF4$Time[i] = difftime(loop_end_time4, loop_start_time4, units='mins')
  
  print("Test R")
  print(Evaluation_DF4$Train_R[i])
  print("M0del4 took:")
  print(loop_end_time4 - loop_start_time4)
  
  rm(model1,test_predict,train_predict)
  
  beep("coin")
  
  loop_start_time5 <- Sys.time()
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                  young18 + young25 + older40 + older60 + Immigrants + PKWs,
                  data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time5 <- Sys.time()
  
  Evaluation_DF5$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF5$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF5$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF5$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF5$Time[i] = difftime(loop_end_time5, loop_start_time5, units='mins')
  
  print("Test R")
  print(Evaluation_DF5$Train_R[i])
  print("M0del5 took:")
  print(loop_end_time5 - loop_start_time5)
  
  rm(model1,test_predict,train_predict)
  
  beep("coin")
  
  loop_start_time6 <- Sys.time()
  model1 <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                  ClothesShop500mmRadius2 + ClosestTrainS2 + ClosestBridge2 + young302 + PKWs2,
                  data =  train.data, type = "eps-regression")
  
  test_predict <- model1 %>% predict(testSet)
  train_predict <- model1 %>% predict(test.data)
  loop_end_time6 <- Sys.time()
  
  Evaluation_DF6$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict))^2))
  Evaluation_DF6$Train_RMSE[i] = sqrt(mean((test.data$Value - exp(train_predict))^2))
  
  Evaluation_DF6$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
  Evaluation_DF6$Train_R[i]= postResample(exp(train_predict), test.data$Value)[2]
  Evaluation_DF6$Time[i] = difftime(loop_end_time6, loop_start_time6, units='mins')
  
  print("Test R")
  print(Evaluation_DF6$Train_R[i])
  print("M0del6 took:")
  print(loop_end_time6 - loop_start_time6)
  
  for_end_time <- Sys.time()
  print("This round took took:")
  print(for_end_time - for_start_time)
  
  rm(model1,test_predict,train_predict)
  
}
for1_end_time <- Sys.time()
print("The hole process took:")
print(for1_end_time - for1_start_time)


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
TrainR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))

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
TestR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))

write.csv(TrainR,"Plot44_45_Train_Data.csv")
write.csv(TestR,"Plot44_45_Test_Data.csv")

R2_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Modellversion") +
  labs(y = "Bestimmtheitsmaß", color = "Set:" ) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split1, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split2, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split3, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split4, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split5, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split1, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split2, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split3, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split4, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split5, group = 2 , color = "Test"), size = 1)

R2_plot_time = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Tainingsdauer in Minuten pro Split") +
  labs(y = "Bestimmtheitsmaß", color = "Set:" ) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split1, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split2, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split3, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split4, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split5, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split1, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split2, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split3, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split4, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split5, group = 2 , color = "Test"), size = 1)

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
TrainR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))

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
TestR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))

MSE_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Modellversion") +
  labs(y = "Mean squared error", color = "Set:" ) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split1, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split2, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split3, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split4, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = model, y = Split5, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split1, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split2, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split3, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split4, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = model, y = Split5, group = 2 , color = "Test"), size = 1)

MSE_plot_time = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Tainingsdauer in Minuten pro Split") +
  labs(y = "Mean squared error", color = "Set:" ) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split1, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split2, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split3, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split4, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TrainR, mapping = aes(x = time, y = Split5, group = 1 , color = "Train"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split1, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split2, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split3, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split4, group = 2 , color = "Test"), size = 1) +
  geom_line(data = TestR, mapping = aes(x = time, y = Split5, group = 2 , color = "Test"), size = 1)


setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot44.png",width=800, height=800)
R2_plot
dev.off()

png(file="plot45.png",width=800, height=800)
R2_plot_time
dev.off()

png(file="plot46.png",width=800, height=800)
MSE_plot
dev.off()

png(file="plot47.png",width=800, height=800)
MSE_plot_time
dev.off()

write.csv(TrainR,"Plot46_47_Train_Data.csv")
write.csv(TestR,"Plot46_47_Test_Data.csv")

beep("mario")
