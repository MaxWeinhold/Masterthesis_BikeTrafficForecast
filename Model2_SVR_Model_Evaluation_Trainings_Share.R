#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Random Forests Modell Selection (training share)

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)
library(e1071)
citation("e1071")

library(tidyverse)
library(caret)

#Regarding calculation power see following source: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#Historically, R has only utilized one processor, which makes it single-threaded.

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/M?nster/7. Semester/Masterarbeit Daten")

load("ValidationSets.rdata")

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
Evaluation_DF$Trainings_Share = 0

Evaluation_DF1 = Evaluation_DF
Evaluation_DF2 = Evaluation_DF
Evaluation_DF3 = Evaluation_DF
Evaluation_DF4 = Evaluation_DF
Evaluation_DF5 = Evaluation_DF
Evaluation_DF6 = Evaluation_DF

for_start_time <- Sys.time()
for(i in 1:length(validation_set)){
  
  start_time <- Sys.time()
  
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
  
  for(j in 1:6){
    
    loop_start_time <- Sys.time()
    
    share = 1
    if(j==1){share = 0.00001}
    if(j==2){share = 0.0001}
    if(j==3){share = 0.001}
    if(j==4){share = 0.0025}
    if(j==5){share = 0.005}
    if(j==6){share = 0.01}
    
    # Split data to reduce duration of computation
    training.samples <- trainSet$Value %>%
      createDataPartition(p = share, list = FALSE)
    train.data  <- trainSet[training.samples, ]
    test.data <- trainSet[-training.samples, ]
    
    #Now do Model calculations
    start_time <- Sys.time()
    print("Starts to train the modell")
    print(start_time)
    model <- svm(log(Value) ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                   Rain3 + Inhabitants3 + UniBuild500mmRadius3 + ClothesShop500mmRadius3 + ClosestTrainS3, 
                   data =  train.data, type = "eps-regression", kernel = "radial", epsilon = 0.1, cachesize = 100)
    
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
    
    loop_end_time <- Sys.time()
    print("One loop took:")
    print(loop_end_time - loop_start_time)
    beep("coin")
    
    
    if(j==1){
      
      Evaluation_DF1$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF1$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF1$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF1$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF1$Trainings_Share[i] = share
      Evaluation_DF1$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==2){
      
      Evaluation_DF2$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF2$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF2$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF2$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF2$Trainings_Share[i] = share
      Evaluation_DF2$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==3){
      
      Evaluation_DF3$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF3$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF3$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF3$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF3$Trainings_Share[i] = share
      Evaluation_DF3$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==4){
      
      Evaluation_DF4$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF4$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF4$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF4$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF4$Trainings_Share[i] = share
      Evaluation_DF4$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==5){
      
      Evaluation_DF5$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF5$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF5$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF5$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF5$Trainings_Share[i] = share
      Evaluation_DF5$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==6){
      
      Evaluation_DF6$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF6$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF6$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF6$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF6$Trainings_Share[i] = share
      Evaluation_DF6$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    
    #vcovHAC(model)
    
  }

}
for_end_time <- Sys.time()
print("The hole process took:")
print(for_end_time - for_start_time)


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
TrainR$model = c(Evaluation_DF1$Trainings_Share[1],Evaluation_DF2$Trainings_Share[1],Evaluation_DF3$Trainings_Share[1],
                 Evaluation_DF4$Trainings_Share[1],Evaluation_DF5$Trainings_Share[1],Evaluation_DF6$Trainings_Share[1])
TrainR$model = TrainR$model*100
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
TestR$model = c(Evaluation_DF1$Trainings_Share[1],Evaluation_DF2$Trainings_Share[1],Evaluation_DF3$Trainings_Share[1],
                Evaluation_DF4$Trainings_Share[1],Evaluation_DF5$Trainings_Share[1],Evaluation_DF6$Trainings_Share[1])
TestR$model = TestR$model*100
TestR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))


R2_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Trainingsanteil in % vom Datensatz") +
  labs(y = "Bestimmtheitsma?", color = "Set:" ) +
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

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot48.png",width=600, height=600)
R2_plot
dev.off()

R2_plot_time = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Tainingsdauer in Minuten pro Split") +
  labs(y = "Bestimmtheitsma?", color = "Set:" ) +
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
png(file="plot49.png",width=600, height=600)
R2_plot_time
dev.off()
write.csv(TrainR,"Plot48_49_Train_Data.csv")
write.csv(TestR,"Plot48_49_Test_Data.csv")

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
TrainR$model = c(Evaluation_DF1$Trainings_Share[1],Evaluation_DF2$Trainings_Share[1],Evaluation_DF3$Trainings_Share[1],
                 Evaluation_DF4$Trainings_Share[1],Evaluation_DF5$Trainings_Share[1],Evaluation_DF6$Trainings_Share[1])
TrainR$model = TrainR$model*100
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
TestR$model = c(Evaluation_DF1$Trainings_Share[1],Evaluation_DF2$Trainings_Share[1],Evaluation_DF3$Trainings_Share[1],
                Evaluation_DF4$Trainings_Share[1],Evaluation_DF5$Trainings_Share[1],Evaluation_DF6$Trainings_Share[1])
TestR$model = TestR$model*100
TestR$time = c(mean(Evaluation_DF1$Time),mean(Evaluation_DF2$Time),mean(Evaluation_DF3$Time),
                mean(Evaluation_DF4$Time),mean(Evaluation_DF5$Time),mean(Evaluation_DF6$Time))

MSE_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Trainingsanteil in % vom Datensatz") +
  labs(y = "RMSE", color = "Set:" ) +
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
  labs(y = "RMSE", color = "Set:" ) +
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

png(file="plot50.png",width=600, height=600)
MSE_plot
dev.off()

png(file="plot51.png",width=600, height=600)
MSE_plot_time
dev.off()

write.csv(TrainR,"Plot50_51_Train_Data.csv")
write.csv(TestR,"Plot50_51_Test_Data.csv")

beep("mario")



