#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Dresden
#
install.packages("readxl") 

library(readxl)
library(lubridate)
library(dplyr)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Dresden
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Dresden/2018")

#Read Bycicle Counting Data----------------------------------------------

files = c("AlbertBruecke_20180101_0000_20180201_Std",
          "AlbertBruecke_20180201_0000_20180301_Std",
          "AlbertBruecke_20180301_0000_20180401_Std",
          "AlbertBruecke_20180401_0000_20180501_Std",
          "AlbertBruecke_20180501_0000_20180601_Std",
          "AlbertBruecke_20180601_0000_20180701_Std",
          "AlbertBruecke_20180701_0000_20180801_Std",
          "AlbertBruecke_20180801_0000_20180901_Std",
          "AlbertBruecke_20180901_0000_20181001_Std",
          "AlbertBruecke_20181001_0000_20181101_Std",
          "AlbertBruecke_20181101_0000_20181201_Std",
          "AlbertBruecke_20181201_0000_20190101_Std",
          "Elberadweg_20180101_0000_20180201_Std",
          "Elberadweg_20180201_0000_20180301_Std",
          "Elberadweg_20180301_0000_20180401_Std",
          "Elberadweg_20180401_0000_20180501_Std",
          "Elberadweg_20180501_0000_20180601_Std"
          )
myList <- list()

for(i in 1:length(files)){
  
  print(paste("Progress:",round(i/length(files)*100,2),"%"))
  
  data1 = read_excel(paste(files[i],".xls",sep=""), sheet = 2)
  data1 = as.data.frame(data1)
  names(data1)=data1[5,]
  data1[5,] = NA
  data1 = na.omit(data1)
  data1[,2:3] = NULL
  data1$Datum=as.numeric(data1$Datum)
  data1$Datum=format(as.POSIXct(data1$Datum * 60 *60 * 24, origin = '1899-12-30', tz = 'CET'),
                     '%d.%m.%Y %I:%M %p')
  data1$Town = "Dresden"
  
  data1$Station = as.data.frame(strsplit(files[i], split = "_"))[1,]
  
  if(as.data.frame(strsplit(files[i], split = "_"))[2,]=="Nord"){
    data1$Station = paste(as.data.frame(strsplit(files[i], split = "_"))[1,],as.data.frame(strsplit(files[i], split = "_"))[2,],sep="_")
  }
  
  if(data1$Station[1]=="AlbertBruecke"){
    data1$Lon = 13.753345646035994
    data1$Lat = 51.058300500187464
  }
  if(data1$Station[1]=="Elberadweg"){
    data1$Lon = 13.77316392373886
    data1$Lat = 51.06259702054758
  }
  if(data1$Station[1]=="Elberadweg_Nord"){
    data1$Lon = 13.778471053476396
    data1$Lat = 51.06577875765613
  }
  if(data1$Station[1]=="StPeter"){
    data1$Lon = 13.74400380101143
    data1$Lat = 51.04774563394987
  }
  if(data1$Station[1]=="Tharandter"){
    data1$Lon = 13.700914794735631
    data1$Lat = 51.02986654078383
  }
  if(data1$Station[1]=="Waldschloss"){
    data1$Lon = 13.777048988826097
    data1$Lat = 51.06058193837442
  }
  if(data1$Station[1]=="Winter"){
    data1$Lon = 13.786633776087726
    data1$Lat = 51.0284279016294
  }
  if(data1$Station[1]=="Kreuz"){
    data1$Lon = 13.743809729934194
    data1$Lat = 51.04760333549463
  }
  data1$Oneway = FALSE
  
  myList[[i]] <- data1
}
