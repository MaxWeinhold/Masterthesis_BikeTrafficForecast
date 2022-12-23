#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Random Forests Modell Selection (tree size)

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)
library(randomForest)

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

load("ValidationSets.rdata")

#Now the real Modell

length(validation_set)

Evaluation_DF = as.data.frame( matrix(1:length(validation_set)*4, nrow = length(validation_set), ncol = 4) )
names(Evaluation_DF)[1] = "Train_R"
names(Evaluation_DF)[2] = "Train_RMSE"
names(Evaluation_DF)[3] = "Test_R"
names(Evaluation_DF)[4] = "Test_RMSE"
Evaluation_DF$Trainings_Share = 0
Evaluation_DF$Time = 0

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
    if(j==1){tree = 10}
    if(j==2){tree = 50}
    if(j==3){tree = 100}
    if(j==4){tree = 500}
    if(j==5){tree = 1000}
    if(j==6){tree = 1500}
    
    # Split data to reduce duration of computation
    training.samples <- trainSet$Value %>%
      createDataPartition(p = 0.0025, list = FALSE)
    train.data  <- trainSet[training.samples, ]
    test.data <- trainSet[-training.samples, ]
    
    #Now do Model calculations
    start_time <- Sys.time()
    print("Starts to train the modell")
    print(start_time)
    model <- randomForest(log(Value) ~ Hour + Months + Weekend + Night + publicHoliday + schoolHoliday + 
                            Wind + CloudCover + Humidity + Rain + Temperature + Cinemas3kmRadius +
                            ADFC_Index + Area + Inhabitants + Male_Ratio + Distance_to_Center + 
                            ClosestSchool + Schools500mmRadius + Schools2kmRadius + ClosestUniBuild + UniBuild500mmRadius + UniBuild2kmRadius+ ClothesShop2kmRadius + Signals250mmRadius +
                            BusStop250mmRadius + UnmCross250mmRadius + BusStop1kmRadius + Tram250mmRadius + Subway250mmRadius + ClosestTrainS + BikeShop3kmRadius + 
                            cycleways + path + secondary + primary + ClosestBridge + young18 + young25 + young30 + 
                            older40 + older60 + Immigrants + PKWs, data =  train.data, ntree=tree, importance=TRUE)
    
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
    
    
    loop_end_time <- Sys.time()
    print("One loop took:")
    print(loop_end_time - loop_start_time)
    
    if(j==1){
      
      Evaluation_DF1$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF1$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF1$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF1$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF1$Trainings_Share[i] = tree
      Evaluation_DF1$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==2){
      
      Evaluation_DF2$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF2$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF2$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF2$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF2$Trainings_Share[i] = tree
      Evaluation_DF2$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==3){
      
      Evaluation_DF3$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF3$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF3$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF3$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF3$Trainings_Share[i] = tree
      Evaluation_DF3$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==4){
      
      Evaluation_DF4$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF4$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF4$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF4$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF4$Trainings_Share[i] = tree
      Evaluation_DF4$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==5){
      
      Evaluation_DF5$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF5$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF5$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF5$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF5$Trainings_Share[i] = tree
      Evaluation_DF5$Time[i] = difftime(loop_end_time, loop_start_time, units='mins')
      
    }
    if(j==6){
      
      Evaluation_DF6$Test_RMSE[i] = sqrt(mean((testSet$Value - exp(test_predict[,1]))^2))
      Evaluation_DF6$Train_RMSE[i] = sqrt(mean((train.data$Value - exp(train_predict[,1]))^2))
      Evaluation_DF6$Test_R[i]= postResample(exp(test_predict), testSet$Value)[2]
      Evaluation_DF6$Train_R[i]= postResample(exp(train_predict), train.data$Value)[2]
      Evaluation_DF6$Trainings_Share[i] = tree
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
TrainR$time = c(7.6/60,18.7/60,0.51,
                2.4,4.67,7.04)

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
TestR$time = c(7.6/60,18.7/60,0.51,
                2.4,4.67,7.04)


R2_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Anzahl der Zufallsbäume") +
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

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot36.png",width=600, height=600)
R2_plot
dev.off()

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

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")
png(file="plot37.png",width=600, height=600)
R2_plot_time
dev.off()
write.csv(TrainR,"Plot36_37_Train_Data.csv")
write.csv(TestR,"Plot36_37_Test_Data.csv")

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
TrainR$time = c(7.6/60,18.7/60,0.51,
                2.4,4.67,7.04)

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
TestR$time = c(7.6/60,18.7/60,0.51,
                2.4,4.67,7.04)

MSE_plot = ggplot(NULL, aes(v, p)) + 
  theme_bw() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")) +
  xlab("Anzahl der Zufallsbäume") +
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

png(file="plot38.png",width=600, height=600)
MSE_plot
dev.off()

png(file="plot39.png",width=600, height=600)
MSE_plot_time
dev.off()

write.csv(TrainR,"Plot38_39_Train_Data.csv")
write.csv(TestR,"Plot38_39_Test_Data.csv")

beep("mario")



