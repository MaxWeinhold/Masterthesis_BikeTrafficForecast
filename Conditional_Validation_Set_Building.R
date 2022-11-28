#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: conditional Validation Set Building

require(caret)

#Regarding calculation power see following source: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#Historically, R has only utilized one processor, which makes it single-threaded.


#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_1.csv",sep=",", encoding="ISO-8859-1")

#Validation_Set <- createFolds(BikeData, k = 5, list = TRUE, returnTrain = FALSE)

summary(BikeData)

levels(as.factor(BikeData$Town))
nlevels(as.factor(BikeData$Station))
levels(as.factor(BikeData$Year))

names(BikeData)

nrow(BikeData)

#names(Validation_Set)[1] <- "train"