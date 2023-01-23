#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Neural Network Modell

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)

if(!require("neuralnet")) install.packages("neuralnet")
library(neuralnet)

if(!require("MASS")) install.packages("MASS")
library(MASS)

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

svrtest = validation_set[[1]][1:500,]
svrtest = as.data.frame(cbind(svrtest$Value,svrtest$Hour))
names(svrtest)[1]="Value"
names(svrtest)[2]="Hour"
svrtest$Hour2 = svrtest$Hour^2
svrtest$Hour3 = svrtest$Hour^3


# Create an ols regression model
model1 <- lm(Value ~ Hour + Hour2 + Hour3, data=svrtest)
predict1 <- as.data.frame(predict(model1,svrtest, type='response'))
predict1$Hour = svrtest$Hour
names(predict1)[1]="Value"

# Normalize the data
maxs <- apply(svrtest, 2, max) 
mins <- apply(svrtest, 2, min)
scaled <- as.data.frame(scale(svrtest, center = mins, 
                              scale = maxs - mins))

summary(scaled)

# Create an svm regression model
model2 <- neuralnet(Value ~ Hour, data=scaled, hidden = c(5, 3), 
                    linear.output = FALSE)
predict2 <- as.data.frame(predict(model2, scaled, type='response'))

predict2 = predict2 * (max(svrtest$Value) - min(svrtest$Value)) + min(svrtest$Value)

predict2$Hour = svrtest$Hour
names(predict2)[1]="Value"



plot25 = ggplot(svrtest,aes(x = Hour, y = Value)) +
  geom_point(size=1.5)+
  labs(title = "Vergleich zwischen OLS und Neural Network Regression"
       , color = "Methode") +
  ggtitle("Vergleich zwischen OLS und Neural Network Regression") +
  xlab("Uhrzeit") +
  ylab("Fahrradauufkommen") +
  theme_bw() +
  geom_line(data = predict1, aes(x = Hour, y = Value, color='OLS'), size = 1.5) + 
  geom_line(data = predict2, aes(x = Hour, y = Value, color='NN'), size = 1.5)

plot25

setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/thesis_german/Plots")

#png(file="plot27.png",width=800, height=800)
#plot25
#dev.off()

rm(plot25,model1,model2,predict1,predict2,svrtest,scaled)

#Now the real Modell

#levels(as.factor(validation_set[[1]]$Town))
#levels(as.factor(validation_set[[2]]$Town))
#levels(as.factor(validation_set[[3]]$Town))
#levels(as.factor(validation_set[[4]]$Town))
#levels(as.factor(validation_set[[5]]$Town))

#length(validation_set)

Evaluation_DF = as.data.frame( matrix(1:length(validation_set)*4, nrow = length(validation_set), ncol = 4) )
names(Evaluation_DF)[1] = "Train_R"
names(Evaluation_DF)[2] = "Train_RMSE"
names(Evaluation_DF)[3] = "Test_R"
names(Evaluation_DF)[4] = "Test_RMSE"

validation_set[[1]]$Town = NULL
validation_set[[1]]$Station = NULL

validation_set[[2]]$Town = NULL
validation_set[[2]]$Station = NULL

validation_set[[3]]$Town = NULL
validation_set[[3]]$Station = NULL

validation_set[[4]]$Town = NULL
validation_set[[4]]$Station = NULL

validation_set[[5]]$Town = NULL
validation_set[[5]]$Station = NULL

i=2

for_start_time <- Sys.time()
for(i in 1:length(validation_set)){
  
  rm(model,trainSet,testSet,test_predict,train_predict,test.data,end_time,start_time)
  rm(training.samples,train.data,maxs,mins,sets,for_end_time)
  
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
  #names(testSet)
  
  #trainSet <- trainSet %>% mutate_at(names(trainSet), ~(scale(.) %>% as.vector))
  #testSet <- testSet %>% mutate_at(names(testSet), ~(scale(.) %>% as.vector))
  
  # Split data to reduce duration of computation
  training.samples <- trainSet$Value %>%
    createDataPartition(p = 0.025, list = FALSE)
  train.data  <- trainSet[training.samples, ]
  test.data <- trainSet[-training.samples, ]
  
  class(train.data)
  
  maxs <- apply(train.data, 2, max) 
  mins <- apply(train.data, 2, min)
  train.data <- as.data.frame(scale(train.data, center = mins, 
                                  scale = maxs - mins))
  
  summary(train.data)
  
  maxs <- apply(testSet, 2, max) 
  mins <- apply(testSet, 2, min)
  testSet <- as.data.frame(scale(testSet, center = mins, 
                                 scale = maxs - mins))
  
  summary(testSet)
  
  #Now do Model calculations
  start_time <- Sys.time()
  print("Starts to train the modell")
  print(start_time)
  
  #train.data = train.data[is.na(train.data)] <- 0
  #testSet = testSet[is.na(testSet)] <- 0
  testSet = testSet %>% mutate_all(~replace(., is.na(.), 0))
  train.data = train.data %>% mutate_all(~replace(., is.na(.), 0))
  
  #print(summary(train.data))
  #summary(testSet)
  #
  model <- neuralnet(Value ~ Year + Months + Hour + Weekend + Night + publicHoliday + schoolHoliday +
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
                       CorInz + Lockdowns + stre_dist + #path + secondary + primary + residential + 
                       .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                       .data_sidewalk + .data_asphalt + .data_compacted + .data_concrete + .data_fine_gravel +
                       .data_paved + .data_paving_stones + .data_pebblestone + .data_sett + .data_unknown +
                       stre_lengths + stre_lanes + stre_maxspeed + bridge +
                       Rain2 + Temperature2 + Inhabitants2 + stre_lengths2, data =  train.data,
                       hidden = c(48, 26, 16, 8, 4, 2), linear.output = FALSE, lifesign = 'full', rep=3, stepmax = 100000, threshold = 0.025)
  
  #plot(model,col.hidden = 'darkgreen',     
  #     col.hidden.synapse = 'darkgreen',
  #     show.weights = F,
  #     information = F,
  #     fill = 'lightblue')
  
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  start_time <- Sys.time()
  print("Starts to calclulate test predictions")
  print(start_time)
  test_predict <- as.data.frame(predict(model, testSet, type='response'))
  test_predict = test_predict * (max(validation_set[[i]]$Value) - min(validation_set[[i]]$Value)) + min(validation_set[[i]]$Value)
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  start_time <- Sys.time()
  print("Starts to calclulate train predictions")
  print(start_time)
  train_predict <- as.data.frame(predict(model, train.data, type='response'))
  train_predict = train_predict * (max(validation_set[[i]]$Value) - min(validation_set[[i]]$Value)) + min(validation_set[[i]]$Value)
  end_time <- Sys.time()
  print(end_time - start_time)
  beep("coin")
  
  Evaluation_DF$Test_RMSE[i] = sqrt(mean((testSet$Value - test_predict[,1])^2))
  Evaluation_DF$Train_RMSE[i] = sqrt(mean((train.data$Value - train_predict[,1])^2))
  
  #cor(test_predict,testSet$Value) ^ 2
  #cor(train_predict,trainSet$Value) ^ 2
  Evaluation_DF$Test_R[i]= postResample(test_predict, testSet$Value)[2]
  Evaluation_DF$Train_R[i]= postResample(train_predict, train.data$Value)[2]
  
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
write.csv(Evaluation_DF,"Modell4_NN.csv")
save(model,file="Modell4_NN.rdata")

warnings()
