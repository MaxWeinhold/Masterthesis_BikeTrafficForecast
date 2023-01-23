#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Dummy Creator

#fastDummies
if(!require("fastDummies")) install.packages("fastDummies")
library(fastDummies)

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_2.csv",sep=",", encoding="ISO-8859-1")

names(BikeData)

class(BikeData$stre_type)
class(BikeData$stre_type_spec)
class(BikeData$stre_surface)

nlevels(as.factor(BikeData$stre_type))
nlevels(as.factor(BikeData$stre_type_spec))
nlevels(as.factor(BikeData$stre_surface))

length(names(BikeData))
BikeData = cbind(BikeData,dummy_cols(BikeData$stre_type))
BikeData = cbind(BikeData,dummy_cols(BikeData$stre_type_spec))
BikeData = cbind(BikeData,dummy_cols(BikeData$stre_surface))
length(names(BikeData))

names(BikeData)
BikeData$X.1 = NULL
BikeData$X = NULL
#BikeData$secondary = NULL
#BikeData$primary = NULL
#BikeData$path = NULL
#BikeData$living_street = NULL
#BikeData$residential = NULL
#BikeData$cycleways = NULL
BikeData$Timestamp = NULL
BikeData$stre_type = NULL
BikeData$stre_type_spec = NULL
BikeData$stre_surface = NULL
BikeData$.data = NULL
BikeData$.data = NULL
BikeData$.data = NULL
BikeData$Weekday = NULL
BikeData$Oneway = NULL
BikeData$Bundesland = NULL
summary(BikeData)

#Save new data set-------------------------------------------------------
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
write.csv(BikeData,"completeDataSet_3.csv")
