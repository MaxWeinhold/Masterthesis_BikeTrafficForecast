if(!require("tidyverse")) install.packages("tidyverse")
if(!require("caret")) install.packages("caret")
if(!require("MASS")) install.packages("MASS")
if(!require("neuralnet")) install.packages("neuralnet")
library(neuralnet)
library(MASS)
library(tidyverse)
library(caret)
library(randomForest)
library(beepr)

#Clean up memory
rm(list=ls())

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
Test = read.csv("Stadtradeln_Staedte_test4.csv")

Test$Inhabitants_2 = Test$Inhabitants^2
Test$young30_2 = Test$young30^2
Test$PKWs_2 = Test$PKWs^2
Test$Dist2Center_2 = Test$Dist2Center^2
Test$ClosestSuperMarket_2 = Test$ClosestSuperMarket^2
Test$ClothesShop500mmRadius_2 = Test$ClothesShop500mmRadius^2

summary(Test)
names(Test)

Test$X = NULL
Test$town = NULL
Test$Stadt = NULL

Test = Test[ , colSums(is.na(Test))==0]

# Split data to reduce duration of computation
training.samples <- Test$Value %>%
  createDataPartition(p = 1, list = FALSE)
ReducedSet <- Test[training.samples, ]

nrow(Test)
nrow(ReducedSet)

# Split the data into training and test set
set.seed(123)
training.samples <- ReducedSet$Value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- ReducedSet[training.samples, ]
test.data <- ReducedSet[-training.samples, ]

maxs <- apply(train.data, 2, max) 
mins <- apply(train.data, 2, min)
train.data <- as.data.frame(scale(train.data, center = mins, 
                                  scale = maxs - mins))

maxs <- apply(test.data, 2, max) 
mins <- apply(test.data, 2, min)
test.data <- as.data.frame(scale(test.data, center = mins, 
                               scale = maxs - mins))

# Build the model
model <- neuralnet(Value ~ Inhabitants + young25 + older40 +
              PKWs + Dist2Center + UniBuild500mmRadius +
              UniBuild2kmRadius + ClosestSuperMarket + ClothesShop500mmRadius +
              BusStop250mmRadius + Signals250mmRadius + ClosestTrainS + Dist2Center_2 +
              Inhabitants_2 + young30_2 + PKWs_2+ ClosestSuperMarket_2 + ClothesShop500mmRadius_2 +
              ClosestBikeShop, data = train.data, 
              hidden = c(20,2), linear.output = FALSE, lifesign = 'full',
              rep=2, stepmax = 100000, threshold = 0.01)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(train.data)
data.frame( R2 = R2(predictions, train.data$Value),
            RMSE = RMSE(predictions, train.data$Value),
            MAE = MAE(predictions, train.data$Value))

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Value),
            RMSE = RMSE(predictions, test.data$Value),
            MAE = MAE(predictions, test.data$Value))

beep("mario")
