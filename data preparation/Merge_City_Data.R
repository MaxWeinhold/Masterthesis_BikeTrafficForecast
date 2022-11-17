#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Megre city Data

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")


#Read Bycicle Counting Data----------------------------------------------

  Berlin = read.csv(file = "Berlin.csv",sep=",", encoding="ISO-8859-1")
  Bochum = read.csv(file = "Bochum.csv",sep=",", encoding="ISO-8859-1")
  Bonn = read.csv(file = "Bonn.csv",sep=",", encoding="ISO-8859-1")
  Bremen = read.csv(file = "Bremen.csv",sep=",", encoding="ISO-8859-1")
  Darmstadt = read.csv(file = "Darmstadt.csv",sep=",", encoding="ISO-8859-1")
  Düsseldorf = read.csv(file = "Düsseldorf.csv",sep=",", encoding="ISO-8859-1")
  Hamburg = read.csv(file = "Hamburg.csv",sep=",", encoding="ISO-8859-1")
  Mannheim = read.csv(file = "Mannheim.csv",sep=",", encoding="ISO-8859-1")
  München = read.csv(file = "München.csv",sep=",", encoding="ISO-8859-1")
  Münster = read.csv(file = "Münster.csv",sep=",", encoding="ISO-8859-1")
  Oberhausen = read.csv(file = "Oberhausen.csv",sep=",", encoding="ISO-8859-1")
  Rostock = read.csv(file = "Rostock.csv",sep=",", encoding="ISO-8859-1")
  Siegen = read.csv(file = "Siegen.csv",sep=",", encoding="ISO-8859-1")
  
  d1=names(Bremen)
  d2=names(Bochum)
  #names(Münster)
  
  for(i in 1:length(d1)){
    
    for(j in 1:length(d2)){
      
      if(d1[i]==d2[j]){
        
        d1[i]="is there"
        
      }
      
    }
    
  }
  
  d1
  
  
  names(Bremen)
  
  #Merge Data--------------------------------------------------------------
  
  BikeData=rbind(Berlin,Bochum)
  BikeData=rbind(BikeData,Bonn)
  BikeData=rbind(BikeData,Bremen)
  BikeData=rbind(BikeData,Darmstadt)
  BikeData=rbind(BikeData,Düsseldorf)
  BikeData=rbind(BikeData,Hamburg)
  BikeData=rbind(BikeData,Mannheim)
  BikeData=rbind(BikeData,München)
  BikeData=rbind(BikeData,Münster)
  BikeData=rbind(BikeData,Oberhausen)
  BikeData=rbind(BikeData,Rostock)
  BikeData=rbind(BikeData,Siegen)
  
  levels(as.factor(BikeData$Town))
  nlevels(as.factor(BikeData$Station))
  levels(as.factor(BikeData$Year))
  levels(as.factor(BikeData$Density))
  
  BikeData$X <- NULL
  BikeData$Density <- NULL
  BikeData$Timestamp <- NULL
  
  summary(BikeData)
  names(BikeData)
  
  
  write.csv(BikeData,"completeDataSet_1.csv")
  