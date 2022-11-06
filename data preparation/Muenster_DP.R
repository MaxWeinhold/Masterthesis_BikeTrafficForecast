#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Muenster

library(plyr)

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Münster
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Münster")

#Read Bycicle Counting Data----------------------------------------------
  countingData_ = read.csv(file = "a.csv",sep=";")
