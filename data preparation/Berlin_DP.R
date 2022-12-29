#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Berlin

library(lubridate)
library(dplyr)
library(geosphere)#package for calculating distance using longitude and latitude

citation("geosphere")

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Berlin
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Berlin")

#Read Bycicle Counting Data----------------------------------------------
  countingData12 = read.csv(file = "Daten2012.csv",sep=";")
  countingData13 = read.csv(file = "Daten2013.csv",sep=";")
  countingData14 = read.csv(file = "Daten2014.csv",sep=";")
  countingData15 = read.csv(file = "Daten2015.csv",sep=";")
  countingData16 = read.csv(file = "Daten2016.csv",sep=";")
  countingData17 = read.csv(file = "Daten2017.csv",sep=";")
  countingData18 = read.csv(file = "Daten2018.csv",sep=";")
  countingData19 = read.csv(file = "Daten2019.csv",sep=";")
  countingData20 = read.csv(file = "Daten2020.csv",sep=";")
  countingData21 = read.csv(file = "Daten2021.csv",sep=";")

  names(countingData21)

#Connect all years----------------------------------------------
  
  countingData=rbind(countingData12,countingData13)
  countingData=rbind(countingData,countingData14)
  countingData=rbind(countingData,countingData15)
  countingData=rbind(countingData,countingData16)
  countingData=rbind(countingData,countingData17)
  countingData=rbind(countingData,countingData18)
  countingData=rbind(countingData,countingData19)
  countingData=rbind(countingData,countingData20)
  countingData=rbind(countingData,countingData21)
  
  
#Divide raw data per stations----------------------------------------------
  
  Data_X02.MI.JAN.N=cbind(countingData$Datum,countingData$X02.MI.JAN.N)
  Data_X02.MI.JAN.S=cbind(countingData$Datum,countingData$X02.MI.JAN.S)
  Data_X03.MI.SAN.O=cbind(countingData$Datum,countingData$X03.MI.SAN.O)
  Data_X03.MI.SAN.W=cbind(countingData$Datum,countingData$X03.MI.SAN.W)
  Data_X05.FK.OBB.O=cbind(countingData$Datum,countingData$X05.FK.OBB.O)  
  Data_X05.FK.OBB.W=cbind(countingData$Datum,countingData$X05.FK.OBB.W)  
  Data_X06.FK.FRA.O=cbind(countingData$Datum,countingData$X06.FK.FRA.O)  
  Data_X06.FK.FRA.W=cbind(countingData$Datum,countingData$X06.FK.FRA.W)  
  Data_X10.PA.BER.N=cbind(countingData$Datum,countingData$X10.PA.BER.N)  
  Data_X10.PA.BER.S=cbind(countingData$Datum,countingData$X10.PA.BER.S)  
  Data_X12.PA.SCH=cbind(countingData$Datum,countingData$X12.PA.SCH)  
  Data_X13.CW.PRI=cbind(countingData$Datum,countingData$X13.CW.PRI)  
  Data_X15.SP.KLO.N=cbind(countingData$Datum,countingData$X15.SP.KLO.N)  
  Data_X15.SP.KLO.S=cbind(countingData$Datum,countingData$X15.SP.KLO.S)  
  Data_X17.SZ.BRE.O=cbind(countingData$Datum,countingData$X17.SZ.BRE.O)  
  Data_X17.SZ.BRE.W=cbind(countingData$Datum,countingData$X17.SZ.BRE.W)  
  Data_X18.TS.YOR.O=cbind(countingData$Datum,countingData$X18.TS.YOR.O)
  Data_X18.TS.YOR.W=cbind(countingData$Datum,countingData$X18.TS.YOR.W)
  Data_X19.TS.MON=cbind(countingData$Datum,countingData$X19.TS.MON)
  Data_X20.TS.MAR.N=cbind(countingData$Datum,countingData$X20.TS.MAR.N)
  Data_X20.TS.MAR.S=cbind(countingData$Datum,countingData$X20.TS.MAR.S)
  Data_X21.NK.MAY=cbind(countingData$Datum,countingData$X21.NK.MAY)
  Data_X21.NK.MAY=cbind(countingData$Datum,countingData$X21.NK.MAY)
  Data_X23.TK.KAI=cbind(countingData$Datum,countingData$X23.TK.KAI)
  Data_X24.MH.ALB=cbind(countingData$Datum,countingData$X24.MH.ALB)
  Data_X26.LI.PUP=cbind(countingData$Datum,countingData$X26.LI.PUP)
  Data_X27.RE.MAR=cbind(countingData$Datum,countingData$X27.RE.MAR)
  
#change data to DataFrame----------------------------------------------
  
  Data_X02.MI.JAN.N=as.data.frame(Data_X02.MI.JAN.N)
  Data_X02.MI.JAN.S=as.data.frame(Data_X02.MI.JAN.S)
  Data_X03.MI.SAN.O=as.data.frame(Data_X03.MI.SAN.O)
  Data_X03.MI.SAN.W=as.data.frame(Data_X03.MI.SAN.W)
  Data_X05.FK.OBB.O=as.data.frame(Data_X05.FK.OBB.O)
  Data_X05.FK.OBB.W=as.data.frame(Data_X05.FK.OBB.W)
  Data_X06.FK.FRA.O=as.data.frame(Data_X06.FK.FRA.O)
  Data_X06.FK.FRA.W=as.data.frame(Data_X06.FK.FRA.W)
  Data_X10.PA.BER.N=as.data.frame(Data_X10.PA.BER.N)
  Data_X10.PA.BER.S=as.data.frame(Data_X10.PA.BER.S)
  Data_X12.PA.SCH=as.data.frame(Data_X12.PA.SCH)
  Data_X13.CW.PRI=as.data.frame(Data_X13.CW.PRI)
  Data_X15.SP.KLO.N=as.data.frame(Data_X15.SP.KLO.N)
  Data_X15.SP.KLO.S=as.data.frame(Data_X15.SP.KLO.S)
  Data_X17.SZ.BRE.O=as.data.frame(Data_X17.SZ.BRE.O)
  Data_X17.SZ.BRE.W=as.data.frame(Data_X17.SZ.BRE.W)
  Data_X18.TS.YOR.O=as.data.frame(Data_X18.TS.YOR.O)
  Data_X18.TS.YOR.W=as.data.frame(Data_X18.TS.YOR.W)
  Data_X19.TS.MON=as.data.frame(Data_X19.TS.MON)
  Data_X20.TS.MAR.N=as.data.frame(Data_X20.TS.MAR.N)
  Data_X20.TS.MAR.S=as.data.frame(Data_X20.TS.MAR.S)
  Data_X21.NK.MAY=as.data.frame(Data_X21.NK.MAY)
  Data_X23.TK.KAI=as.data.frame(Data_X23.TK.KAI)
  Data_X24.MH.ALB=as.data.frame(Data_X24.MH.ALB)
  Data_X26.LI.PUP=as.data.frame(Data_X26.LI.PUP)
  Data_X27.RE.MAR=as.data.frame(Data_X27.RE.MAR)
  
#Rename Columns----------------------------------------------
  
  names(Data_X02.MI.JAN.N)[1]="Timestamp"
  names(Data_X02.MI.JAN.S)[1]="Timestamp"
  names(Data_X03.MI.SAN.O)[1]="Timestamp"
  names(Data_X03.MI.SAN.W)[1]="Timestamp"
  names(Data_X05.FK.OBB.O)[1]="Timestamp"
  names(Data_X05.FK.OBB.W)[1]="Timestamp"
  names(Data_X06.FK.FRA.O)[1]="Timestamp"
  names(Data_X06.FK.FRA.W)[1]="Timestamp"
  names(Data_X10.PA.BER.N)[1]="Timestamp"
  names(Data_X10.PA.BER.S)[1]="Timestamp"
  names(Data_X12.PA.SCH)[1]="Timestamp"
  names(Data_X13.CW.PRI)[1]="Timestamp"
  names(Data_X15.SP.KLO.N)[1]="Timestamp"
  names(Data_X15.SP.KLO.S)[1]="Timestamp"
  names(Data_X17.SZ.BRE.O)[1]="Timestamp"
  names(Data_X17.SZ.BRE.W)[1]="Timestamp"
  names(Data_X18.TS.YOR.O)[1]="Timestamp"
  names(Data_X18.TS.YOR.W)[1]="Timestamp"
  names(Data_X19.TS.MON)[1]="Timestamp"
  names(Data_X20.TS.MAR.N)[1]="Timestamp"
  names(Data_X20.TS.MAR.S)[1]="Timestamp"
  names(Data_X21.NK.MAY)[1]="Timestamp"
  names(Data_X23.TK.KAI)[1]="Timestamp"
  names(Data_X24.MH.ALB)[1]="Timestamp"
  names(Data_X26.LI.PUP)[1]="Timestamp"
  names(Data_X27.RE.MAR)[1]="Timestamp"
  
  names(Data_X02.MI.JAN.N)[2]="Value"
  names(Data_X02.MI.JAN.S)[2]="Value"
  names(Data_X03.MI.SAN.O)[2]="Value"
  names(Data_X03.MI.SAN.W)[2]="Value"
  names(Data_X05.FK.OBB.O)[2]="Value"
  names(Data_X05.FK.OBB.W)[2]="Value"
  names(Data_X06.FK.FRA.O)[2]="Value"
  names(Data_X06.FK.FRA.W)[2]="Value"
  names(Data_X10.PA.BER.N)[2]="Value"
  names(Data_X10.PA.BER.S)[2]="Value"
  names(Data_X12.PA.SCH)[2]="Value"
  names(Data_X13.CW.PRI)[2]="Value"
  names(Data_X15.SP.KLO.N)[2]="Value"
  names(Data_X15.SP.KLO.S)[2]="Value"
  names(Data_X17.SZ.BRE.O)[2]="Value"
  names(Data_X17.SZ.BRE.W)[2]="Value"
  names(Data_X18.TS.YOR.O)[2]="Value"
  names(Data_X18.TS.YOR.W)[2]="Value"
  names(Data_X19.TS.MON)[2]="Value"
  names(Data_X20.TS.MAR.N)[2]="Value"
  names(Data_X20.TS.MAR.S)[2]="Value"
  names(Data_X21.NK.MAY)[2]="Value"
  names(Data_X23.TK.KAI)[2]="Value"
  names(Data_X24.MH.ALB)[2]="Value"
  names(Data_X26.LI.PUP)[2]="Value"
  names(Data_X27.RE.MAR)[2]="Value"

#Add Location Columns----------------------------------------------
  
  Data_X02.MI.JAN.N$Town = "Berlin"
  Data_X02.MI.JAN.S$Town = "Berlin"
  Data_X03.MI.SAN.O$Town = "Berlin"
  Data_X03.MI.SAN.W$Town = "Berlin"
  Data_X05.FK.OBB.O$Town = "Berlin"
  Data_X05.FK.OBB.W$Town = "Berlin"
  Data_X06.FK.FRA.O$Town = "Berlin"
  Data_X06.FK.FRA.W$Town = "Berlin"
  Data_X10.PA.BER.N$Town = "Berlin"
  Data_X10.PA.BER.S$Town = "Berlin"
  Data_X12.PA.SCH$Town = "Berlin"
  Data_X13.CW.PRI$Town = "Berlin"
  Data_X15.SP.KLO.N$Town = "Berlin"
  Data_X15.SP.KLO.S$Town = "Berlin"
  Data_X17.SZ.BRE.O$Town = "Berlin"
  Data_X17.SZ.BRE.W$Town = "Berlin"
  Data_X18.TS.YOR.O$Town = "Berlin"
  Data_X18.TS.YOR.W$Town = "Berlin"
  Data_X19.TS.MON$Town = "Berlin"
  Data_X20.TS.MAR.N$Town = "Berlin"
  Data_X20.TS.MAR.S$Town = "Berlin"
  Data_X21.NK.MAY$Town = "Berlin"
  Data_X23.TK.KAI$Town = "Berlin"
  Data_X24.MH.ALB$Town = "Berlin"
  Data_X26.LI.PUP$Town = "Berlin"
  Data_X27.RE.MAR$Town = "Berlin"
  
  Data_X02.MI.JAN.N$Station = "X02.MI.JAN.N"
  Data_X02.MI.JAN.S$Station = "X02.MI.JAN.S"
  Data_X03.MI.SAN.O$Station = "X03.MI.SAN.O"
  Data_X03.MI.SAN.W$Station = "X03.MI.SAN.W"
  Data_X05.FK.OBB.O$Station = "X05.FK.OBB.O"
  Data_X05.FK.OBB.W$Station = "X05.FK.OBB.W"
  Data_X06.FK.FRA.O$Station = "X06.FK.FRA.O"
  Data_X06.FK.FRA.W$Station = "X06.FK.FRA.W"
  Data_X10.PA.BER.N$Station = "X10.PA.BER.N"
  Data_X10.PA.BER.S$Station = "X10.PA.BER.S"
  Data_X12.PA.SCH$Station = "X12.PA.SCH"
  Data_X13.CW.PRI$Station = "X13.CW.PRI"
  Data_X15.SP.KLO.N$Station = "X15.SP.KLO.N"
  Data_X15.SP.KLO.S$Station = "X15.SP.KLO.S"
  Data_X17.SZ.BRE.O$Station = "X17.SZ.BRE.O"
  Data_X17.SZ.BRE.W$Station = "X17.SZ.BRE.W"
  Data_X18.TS.YOR.O$Station = "X18.TS.YOR.O"
  Data_X18.TS.YOR.W$Station = "X18.TS.YOR.W"
  Data_X19.TS.MON$Station = "X19.TS.MON"
  Data_X20.TS.MAR.N$Station = "X20.TS.MAR.N"
  Data_X20.TS.MAR.S$Station = "X20.TS.MAR.S"
  Data_X21.NK.MAY$Station = "X21.NK.MAY"
  Data_X23.TK.KAI$Station = "X23.TK.KAI"
  Data_X24.MH.ALB$Station = "X24.MH.ALB"
  Data_X26.LI.PUP$Station = "X26.LI.PUP"
  Data_X27.RE.MAR$Station = "X27.RE.MAR"
  
  Data_X02.MI.JAN.N$Lon = 13.41783461
  Data_X02.MI.JAN.S$Lon = 13.41761107
  Data_X03.MI.SAN.O$Lon = 13.37201589
  Data_X03.MI.SAN.W$Lon = 13.37310486
  Data_X05.FK.OBB.O$Lon = 13.44506049
  Data_X05.FK.OBB.W$Lon = 13.44486737
  Data_X06.FK.FRA.O$Lon = 13.474242
  Data_X06.FK.FRA.W$Lon = 13.474414
  Data_X10.PA.BER.N$Lon = 13.41248937
  Data_X10.PA.BER.S$Lon = 13.41216649
  Data_X12.PA.SCH$Lon = 13.40036684
  Data_X13.CW.PRI$Lon = 13.33311991
  Data_X15.SP.KLO.N$Lon = 13.19905274
  Data_X15.SP.KLO.S$Lon = 13.19848797
  Data_X17.SZ.BRE.O$Lon = 13.30916477
  Data_X17.SZ.BRE.W$Lon = 13.30836008
  Data_X18.TS.YOR.O$Lon = 13.37347227
  Data_X18.TS.YOR.W$Lon = 1.37321
  Data_X19.TS.MON$Lon = 13.369785
  Data_X20.TS.MAR.N$Lon = 13.3878453
  Data_X20.TS.MAR.S$Lon = 13.38798692
  Data_X21.NK.MAY$Lon = 13.429
  Data_X23.TK.KAI$Lon = 13.5187
  Data_X24.MH.ALB$Lon = 13.55849
  Data_X26.LI.PUP$Lon = 13.47438
  Data_X27.RE.MAR$Lon = 13.3649435
  
  
  Data_X02.MI.JAN.N$Lat = 52.51393216
  Data_X02.MI.JAN.S$Lat = 52.51394299
  Data_X03.MI.SAN.O$Lat = 52.5271769
  Data_X03.MI.SAN.W$Lat = 52.52768602
  Data_X05.FK.OBB.O$Lat = 52.50119887
  Data_X05.FK.OBB.W$Lat = 52.50127561
  Data_X06.FK.FRA.O$Lat = 52.513584
  Data_X06.FK.FRA.W$Lat = 52.51379
  Data_X10.PA.BER.N$Lat = 52.56692362
  Data_X10.PA.BER.S$Lat = 52.56681286
  Data_X12.PA.SCH$Lat = 52.54883554
  Data_X13.CW.PRI$Lat = 52.48813558
  Data_X15.SP.KLO.N$Lat = 52.53330769
  Data_X15.SP.KLO.S$Lat = 52.53351245
  Data_X17.SZ.BRE.O$Lat = 52.46676019
  Data_X17.SZ.BRE.W$Lat = 52.46721544
  Data_X18.TS.YOR.O$Lat = 52.49193796
  Data_X18.TS.YOR.W$Lat = 52.49228
  Data_X19.TS.MON$Lat = 52.488117
  Data_X20.TS.MAR.N$Lat = 52.43789423
  Data_X20.TS.MAR.S$Lat = 52.43819206
  Data_X21.NK.MAY$Lat = 52.493
  Data_X23.TK.KAI$Lat = 52.45727
  Data_X24.MH.ALB$Lat = 52.4925
  Data_X26.LI.PUP$Lat = 52.50025
  Data_X27.RE.MAR$Lat = 52.55819

  Data_X02.MI.JAN.N$Oneway = FALSE
  Data_X02.MI.JAN.S$Oneway = FALSE
  Data_X03.MI.SAN.O$Oneway = FALSE
  Data_X03.MI.SAN.W$Oneway = FALSE
  Data_X05.FK.OBB.O$Oneway = FALSE
  Data_X05.FK.OBB.W$Oneway = FALSE
  Data_X06.FK.FRA.O$Oneway = FALSE
  Data_X06.FK.FRA.W$Oneway = FALSE
  Data_X10.PA.BER.N$Oneway = FALSE
  Data_X10.PA.BER.S$Oneway = FALSE
  Data_X12.PA.SCH$Oneway = FALSE
  Data_X13.CW.PRI$Oneway = FALSE
  Data_X15.SP.KLO.N$Oneway = FALSE
  Data_X15.SP.KLO.S$Oneway = FALSE
  Data_X17.SZ.BRE.O$Oneway = FALSE
  Data_X17.SZ.BRE.W$Oneway = FALSE
  Data_X18.TS.YOR.O$Oneway = FALSE
  Data_X18.TS.YOR.W$Oneway = FALSE
  Data_X19.TS.MON$Oneway = FALSE
  Data_X20.TS.MAR.N$Oneway = FALSE
  Data_X20.TS.MAR.S$Oneway = FALSE
  Data_X21.NK.MAY$Oneway = FALSE
  Data_X23.TK.KAI$Oneway = FALSE
  Data_X24.MH.ALB$Oneway = FALSE
  Data_X26.LI.PUP$Oneway = FALSE
  Data_X27.RE.MAR$Oneway = FALSE
  
#Summarize Directions----------------------------------------------
  
  Data_X02.MI.JAN=Data_X02.MI.JAN.N
  Data_X02.MI.JAN$Value = as.numeric(Data_X02.MI.JAN.N$Value) + as.numeric(Data_X02.MI.JAN.S$Value)
  Data_X03.MI.SAN=Data_X03.MI.SAN.O
  Data_X03.MI.SAN$Value = as.numeric(Data_X03.MI.SAN.O$Value) + as.numeric(Data_X03.MI.SAN.W$Value)
  Data_X05.FK.OBB=Data_X05.FK.OBB.O
  Data_X05.FK.OBB$Value = as.numeric(Data_X05.FK.OBB.O$Value) + as.numeric(Data_X05.FK.OBB.W$Value)
  Data_X06.FK.FRA=Data_X06.FK.FRA.O
  Data_X06.FK.FRA$Value = as.numeric(Data_X06.FK.FRA.O$Value) + as.numeric(Data_X06.FK.FRA.W$Value)
  Data_X10.PA.BER=Data_X10.PA.BER.N
  Data_X10.PA.BER$Value = as.numeric(Data_X10.PA.BER.N$Value) + as.numeric(Data_X10.PA.BER.S$Value)
  Data_X15.SP.KLO=Data_X15.SP.KLO.N
  Data_X15.SP.KLO$Value = as.numeric(Data_X15.SP.KLO.N$Value) + as.numeric(Data_X15.SP.KLO.S$Value)
  Data_X17.SZ.BRE=Data_X17.SZ.BRE.O
  Data_X17.SZ.BRE$Value = as.numeric(Data_X17.SZ.BRE.O$Value) + as.numeric(Data_X17.SZ.BRE.W$Value)
  Data_X18.TS.YOR=Data_X18.TS.YOR.O
  Data_X18.TS.YOR$Value = as.numeric(Data_X18.TS.YOR.O$Value) + as.numeric(Data_X18.TS.YOR.W$Value)
  Data_X20.TS.MAR=Data_X20.TS.MAR.N
  Data_X20.TS.MAR$Value = as.numeric(Data_X20.TS.MAR.N$Value) + as.numeric(Data_X20.TS.MAR.S$Value)
  
#Connect the Stations----------------------------------------------
  
  rawData=rbind(Data_X02.MI.JAN,Data_X03.MI.SAN)
  rawData=rbind(rawData,Data_X05.FK.OBB)
  rawData=rbind(rawData,Data_X06.FK.FRA)
  rawData=rbind(rawData,Data_X10.PA.BER)
  rawData=rbind(rawData,Data_X12.PA.SCH)
  rawData=rbind(rawData,Data_X13.CW.PRI)
  rawData=rbind(rawData,Data_X15.SP.KLO)
  rawData=rbind(rawData,Data_X17.SZ.BRE)
  rawData=rbind(rawData,Data_X18.TS.YOR)
  rawData=rbind(rawData,Data_X19.TS.MON)
  rawData=rbind(rawData,Data_X20.TS.MAR)
  rawData=rbind(rawData,Data_X21.NK.MAY)
  rawData=rbind(rawData,Data_X23.TK.KAI)
  rawData=rbind(rawData,Data_X24.MH.ALB)
  rawData=rbind(rawData,Data_X26.LI.PUP)
  rawData=rbind(rawData,Data_X27.RE.MAR)
  
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)

#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
  #TimeStamp Configurations
  rawData$Timestamp
  
  rawData$Timestamp = gsub("So, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Mo, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Di, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Mi, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Do, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Fr, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub("Sa, ", "", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jan ", "01.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Feb ", "02.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mrz ", "03.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Apr ", "04.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Mai ", "05.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jun ", "06.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Jul ", "07.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Aug ", "08.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Sep ", "09.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Okt ", "10.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Nov ", "11.", rawData$Timestamp)
  rawData$Timestamp = gsub(" Dez ", "12.", rawData$Timestamp)
  
  rawData$Timestamp=as.POSIXlt(rawData$Timestamp,format="%d.%m.%Y %H:%M")
  
  rawData$Year	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%Y"))
  rawData$Months=month(as.POSIXlt(rawData$Timestamp))
  rawData$Day	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%d"))
  rawData$Summer = ifelse(rawData$Months == "6" | rawData$Months == "7"| rawData$Months == "8", 1, 0)
  rawData$Winter = ifelse(rawData$Months == "12" | rawData$Months == "1"| rawData$Months == "2", 1, 0)
  rawData$Weekday	= format(as.POSIXlt(rawData$Timestamp),"%a")
  rawData$Weekend <- ifelse(rawData$Weekday == "So" | rawData$Weekday == "Sa", 1, 0)
  rawData$Hour	= as.numeric(format(as.POSIXlt(rawData$Timestamp), format = "%H"))
  rawData$Night = ifelse(rawData$Hour<7,1,0)
  
  #Load data for public holidays
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  publicHolidays = read.csv(file = "Feiertage.csv",sep=";")
  
  pH=publicHolidays[publicHolidays$BER %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "BER",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
  #Add Data on the Youth Inhabitant Ratio
  
  youthRatios = read.csv(file = "Altersgruppen.csv",sep=";", encoding="UTF-8")
  names(youthRatios)
  
  youthRatios$Code <- NULL
  youthRatios$Kreis <- NULL
  youthRatios$unter.3.Jahre <- NULL
  youthRatios$X3.bis.unter.6.Jahre <- NULL
  youthRatios$X6.bis.unter.10.Jahre <- NULL
  youthRatios$X10.bis.unter.15.Jahre <- NULL
  youthRatios$X15.bis.unter.18.Jahre <- NULL
  youthRatios$X18.bis.unter.20.Jahre <- NULL
  youthRatios$Insgesamt <- NULL
  youthRatios$Unter.18 <- NULL
  youthRatios$Unter.20 <- NULL
  
  names(youthRatios)[1]="Year"
  names(youthRatios)[2]="Town"
  names(youthRatios)[3]="young18"
  names(youthRatios)[4]="young20"
  
  youthRatios$young18 = gsub(",", ".", youthRatios$young18)
  youthRatios$young20 = gsub(",", ".", youthRatios$young20)
  
  youthRatios$young18 = as.numeric(youthRatios$young18)
  youthRatios$young20 = as.numeric(youthRatios$young20)
  
  rawData = merge(x = rawData,y = youthRatios,
                  by = c("Year","Town"),
                  all = FALSE)
  
  summary(rawData)
  
  #Add Weather Data (Source: Deutscher Wetterdienst)
  
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Berlin")
  Weather_Wind  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_F.csv",sep=",", skip = 1, header = F)
  Weather_CloudCover  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_N.csv",sep=",", skip = 1, header = F)
  Weather_Humidity  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RF.csv",sep=",", skip = 1, header = F)
  Weather_Rain  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_RR.csv",sep=",", skip = 1, header = F)
  Weather_Temperature  = read.csv(file = "Wetterdaten/data_OBS_DEU_PT1H_T2M.csv",sep=",", skip = 1, header = F)
  
  Weather_Wind[1] <- NULL
  Weather_Wind[1] <- NULL
  Weather_Wind[3] <- NULL
  Weather_Wind[3] <- NULL
  Weather_Wind[3] <- NULL
  
  Weather_CloudCover[1] <- NULL
  Weather_CloudCover[1] <- NULL
  Weather_CloudCover[3] <- NULL
  Weather_CloudCover[3] <- NULL
  Weather_CloudCover[3] <- NULL
  
  Weather_Humidity[1] <- NULL
  Weather_Humidity[1] <- NULL
  Weather_Humidity[3] <- NULL
  Weather_Humidity[3] <- NULL
  Weather_Humidity[3] <- NULL
  
  Weather_Rain[1] <- NULL
  Weather_Rain[1] <- NULL
  Weather_Rain[3] <- NULL
  Weather_Rain[3] <- NULL
  Weather_Rain[3] <- NULL
  
  Weather_Temperature[1] <- NULL
  Weather_Temperature[1] <- NULL
  Weather_Temperature[3] <- NULL
  Weather_Temperature[3] <- NULL
  Weather_Temperature[3] <- NULL
  
  names(Weather_Wind)[1]="Timestamp"
  names(Weather_Wind)[2]="Wind"
  
  names(Weather_CloudCover)[1]="Timestamp"
  names(Weather_CloudCover)[2]="CloudCover"
  
  names(Weather_Humidity)[1]="Timestamp"
  names(Weather_Humidity)[2]="Humidity"
  
  names(Weather_Rain)[1]="Timestamp"
  names(Weather_Rain)[2]="Rain"
  
  names(Weather_Temperature)[1]="Timestamp"
  names(Weather_Temperature)[2]="Temperature"
  
  Weather_Wind$Timestamp = gsub("T", " ", Weather_Wind$Timestamp)
  Weather_Wind$Timestamp=as.POSIXlt(Weather_Wind$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Wind$Year	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%Y"))
  Weather_Wind$Months=month(as.POSIXlt(Weather_Wind$Timestamp))
  Weather_Wind$Day	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%d"))
  Weather_Wind$Hour	= as.numeric(format(as.POSIXlt(Weather_Wind$Timestamp), format = "%H"))
  
  Weather_CloudCover$Timestamp = gsub("T", " ", Weather_CloudCover$Timestamp)
  Weather_CloudCover$Timestamp=as.POSIXlt(Weather_CloudCover$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_CloudCover$Year	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%Y"))
  Weather_CloudCover$Months=month(as.POSIXlt(Weather_CloudCover$Timestamp))
  Weather_CloudCover$Day	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%d"))
  Weather_CloudCover$Hour	= as.numeric(format(as.POSIXlt(Weather_CloudCover$Timestamp), format = "%H"))
  
  Weather_Humidity$Timestamp = gsub("T", " ", Weather_Humidity$Timestamp)
  Weather_Humidity$Timestamp=as.POSIXlt(Weather_Humidity$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Humidity$Year	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%Y"))
  Weather_Humidity$Months=month(as.POSIXlt(Weather_Humidity$Timestamp))
  Weather_Humidity$Day	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%d"))
  Weather_Humidity$Hour	= as.numeric(format(as.POSIXlt(Weather_Humidity$Timestamp), format = "%H"))
  
  Weather_Rain$Timestamp = gsub("T", " ", Weather_Rain$Timestamp)
  Weather_Rain$Timestamp=as.POSIXlt(Weather_Rain$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Rain$Year	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%Y"))
  Weather_Rain$Months=month(as.POSIXlt(Weather_Rain$Timestamp))
  Weather_Rain$Day	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%d"))
  Weather_Rain$Hour	= as.numeric(format(as.POSIXlt(Weather_Rain$Timestamp), format = "%H"))
  
  Weather_Temperature$Timestamp = gsub("T", " ", Weather_Temperature$Timestamp)
  Weather_Temperature$Timestamp=as.POSIXlt(Weather_Temperature$Timestamp,format="%Y-%m-%d %H:%M:%S")
  Weather_Temperature$Year	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%Y"))
  Weather_Temperature$Months=month(as.POSIXlt(Weather_Temperature$Timestamp))
  Weather_Temperature$Day	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%d"))
  Weather_Temperature$Hour	= as.numeric(format(as.POSIXlt(Weather_Temperature$Timestamp), format = "%H"))
  
  Weather_Wind$Timestamp <- NULL
  Weather_CloudCover$Timestamp <- NULL
  Weather_Humidity$Timestamp <- NULL
  Weather_Rain$Timestamp <- NULL
  Weather_Temperature$Timestamp <- NULL
  
  rawData = merge(x = rawData,y = Weather_Wind,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Wind)
  
  rawData = merge(x = rawData,y = Weather_CloudCover,
                  by = c("Year","Months","Day","Hour"),
                  all = TRUE)
  
  rawData=na.omit(rawData)
  rm(Weather_CloudCover)
  
  rawData = merge(x = rawData,y = Weather_Humidity,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Humidity)
  
  rawData = merge(x = rawData,y = Weather_Rain,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Rain)
  
  rawData = merge(x = rawData,y = Weather_Temperature,
                  by = c("Year","Months","Day","Hour"),
                  all = FALSE)
  
  rawData=na.omit(rawData)
  rm(Weather_Temperature)
  summary(rawData)
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(rawData,"Berlin.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(4,4,4.1,4.1,4.3,4.3,4.3,4.3,4.1,4.1)
  
  ADFC=as.data.frame(cbind(Year,ADFC_Index))
  
  rawData = merge(x = rawData,y = ADFC,
                   by = c("Year"),
                   all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
#Add Data about number of inhabitants, city size, city center and male and female inhabitant ratio
#Also calculate distance to city ratio
  
  #Load data (source: Destatis)
  
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Einwohner_Destatis")
  Destatis12 = read.csv(file = "31122012_Auszug_GV.csv",sep=";")
  Destatis13 = read.csv(file = "31122013_Auszug_GV.csv",sep=";")
  Destatis14 = read.csv(file = "31122014_Auszug_GV.csv",sep=";")
  Destatis15 = read.csv(file = "31122015_Auszug_GV.csv",sep=";")
  Destatis16 = read.csv(file = "31122016_Auszug_GV.csv",sep=";")
  Destatis17 = read.csv(file = "31122017_Auszug_GV.csv",sep=";")
  Destatis18 = read.csv(file = "31122018_Auszug_GV.csv",sep=";")
  Destatis19 = read.csv(file = "31122019_Auszug_GV.csv",sep=";")
  Destatis20 = read.csv(file = "31122020_Auszug_GV.csv",sep=";")
  Destatis21 = read.csv(file = "31122021_Auszug_GV.csv",sep=";")
  
  title=", Stadt" #This differs, there are cities and also hanseatic cities
  
  test12=as.data.frame(Destatis12[Destatis12$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test12[17] <- NULL
  test12[17] <- NULL
  test12 <- test12 %>% mutate_all(na_if,"")
  names(test12)[1]="number"
  test12=na.omit(test12)
  test12$Year=2012
  
  test13=as.data.frame(Destatis13[Destatis13$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test13[17] <- NULL
  test13[17] <- NULL
  test13 <- test13 %>% mutate_all(na_if,"")
  names(test13)[1]="number"
  test13=na.omit(test13)
  test13$Year=2013
  
  test14=as.data.frame(Destatis14[Destatis14$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test14[17] <- NULL
  test14[17] <- NULL
  test14 <- test14 %>% mutate_all(na_if,"")
  names(test14)[1]="number"
  test14=na.omit(test14)
  test14$Year=2014
  
  test15=as.data.frame(Destatis15[Destatis15$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test15[17] <- NULL
  test15[17] <- NULL
  test15 <- test15 %>% mutate_all(na_if,"")
  names(test15)[1]="number"
  test15=na.omit(test15)
  test15$Year=2015
  
  test16=as.data.frame(Destatis16[Destatis16$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test16[17] <- NULL
  test16[17] <- NULL
  test16 <- test16 %>% mutate_all(na_if,"")
  names(test16)[1]="number"
  test16=na.omit(test16)
  test16$Year=2016
  
  test17=as.data.frame(Destatis17[Destatis17$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test17[17] <- NULL
  test17[17] <- NULL
  test17 <- test17 %>% mutate_all(na_if,"")
  names(test17)[1]="number"
  test17=na.omit(test17)
  test17$Year=2017
  
  test18=as.data.frame(Destatis18[Destatis18$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test18[17] <- NULL
  test18[17] <- NULL
  test18 <- test18 %>% mutate_all(na_if,"")
  names(test18)[1]="number"
  test18=na.omit(test18)
  test18$Year=2018
  
  test19=as.data.frame(Destatis19[Destatis19$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test19[17] <- NULL
  test19[17] <- NULL
  test19 <- test19 %>% mutate_all(na_if,"")
  names(test19)[1]="number"
  test19=na.omit(test19)
  test19$Year=2019
  
  test20=as.data.frame(Destatis20[Destatis20$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test20[17] <- NULL
  test20[17] <- NULL
  test20 <- test20 %>% mutate_all(na_if,"")
  names(test20)[1]="number"
  test20=na.omit(test20)
  test20$Year=2020
  
  test21=as.data.frame(Destatis21[Destatis21$X.6 == paste(toString(rawData$Town[1]),title,sep=""),])
  test21[17] <- NULL
  test21[17] <- NULL
  test21 <- test21 %>% mutate_all(na_if,"")
  names(test21)[1]="number"
  test21=na.omit(test21)
  test21$Year=2021
  
  test=rbind(test12,test13)
  test=rbind(test,test14)
  test=rbind(test,test15)
  test=rbind(test,test16)
  test=rbind(test,test17)
  test=rbind(test,test18)
  test=rbind(test,test19)
  test=rbind(test,test20)
  test=rbind(test,test21)
  
  test$X.7 = gsub(" ", "", test$X.7)
  test$X.7 = gsub(",", ".", test$X.7)
  test$X.7 = as.numeric(test$X.7)
  names(test)[9]="Area"
  
  test$X.8 = gsub(" ", "", test$X.8)
  test$X.8 = as.numeric(test$X.8)
  names(test)[10]="Inhabitants"
  
  test$X.9 = gsub(" ", "", test$X.9)
  test$X.9 = as.numeric(test$X.9) / test$Inhabitants
  names(test)[11]="Male_Ratio"
  
  #City Longitude and Latidtude
  
  test$X.13 = gsub(" ", "", test$X.13)
  test$X.13 = gsub(",", ".", test$X.13)
  test$X.13 = as.numeric(test$X.13)
  names(test)[15]="City_Lon"
  
  test$X.14 = gsub(" ", "", test$X.14)
  test$X.14 = gsub(",", ".", test$X.14)
  test$X.14 = as.numeric(test$X.14)
  names(test)[16]="City_Lat"
  
  names(test)[18]="Density"
  
  test$number <- NULL
  test$X <- NULL
  test$X.1 <- NULL
  test$X.2 <- NULL
  test$X.3 <- NULL
  test$X.4 <- NULL
  test$X.5 <- NULL
  test$X.6 <- NULL
  test$X.10 <- NULL
  test$X.11 <- NULL
  test$X.12 <- NULL
  test$X.17 <- NULL
  
  rawData = merge(x = rawData,y = test,
                  by = c("Year"),
                  all = FALSE)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #calculate distance to city center for every station
  
  #create a matrix, that later will contaion needed information
  distmat=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance and add this in a data frame
  for(i in 1:nlevels(as.factor(rawData$Station))){
    print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    #calculate distance for station
    dist=distm(c(d$Lon[i],d$Lat[i]), c(d$City_Lon[i],d$City_Lat[i]), fun=distGeo)
    print(paste("Distance from",d[1,8]," station to city center is",dist,"meters"))
    
    distmat[i,1]=d[1,8]
    distmat[i,2]=dist
    
    rm(d)
  }
  
  distmat=as.data.frame(distmat)
  names(distmat)[1]="Station"
  names(distmat)[2]="Distance_to_Center"
  distmat$Distance_to_Center=as.numeric(distmat$Distance_to_Center)
  
  rawData = merge(x = rawData,y = distmat,
                  by = c("Station"),
                  all = FALSE)
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  
  #Reading POI from Open Street Map________________________________________________________________________________________________________________________________________-
  #Using the overpass API 
  
  #install the osmdata, sf, tidyverse and ggmap package
  if(!require("osmdata")) install.packages("osmdata")
  if(!require("tidyverse")) install.packages("tidyverse")
  if(!require("sf")) install.packages("sf")
  if(!require("ggmap")) install.packages("ggmap")
  
  citation("sf")
  
  #load packages
  library(tidyverse)
  library(osmdata)
  library(sf)
  library(ggmap)
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("amenity", "cinema")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
    print(cinema$osm_points$name[i])
    print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 1000)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 3000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestCinema"
  distmat_closest$ClosestCinema=as.numeric(distmat_closest$ClosestCinema)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Cinemas1kmRadius"
  distmat_1kmradius$Cinemas1kmRadius=as.numeric(distmat_1kmradius$Cinemas1kmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Cinemas3kmRadius"
  distmat_3kmradius$Cinemas3kmRadius=as.numeric(distmat_3kmradius$Cinemas3kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get all schools________________________________________________________________
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("amenity", "school")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_polygons$osm_id)){
    
    cinmat[i,1]=cinema$osm_polygons$osm_id[i]
    cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
    cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSchool"
  distmat_closest$ClosestSchool=as.numeric(distmat_closest$ClosestSchool)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Schools500mmRadius"
  distmat_1kmradius$Schools500mmRadius=as.numeric(distmat_1kmradius$Schools500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Schools2kmRadius"
  distmat_3kmradius$Schools2kmRadius=as.numeric(distmat_3kmradius$Schools2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get university Buildings_______________________________________________________
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("amenity", "university")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_polygons$osm_id)){
    
    cinmat[i,1]=cinema$osm_polygons$osm_id[i]
    cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
    cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestUniBuild"
  distmat_closest$ClosestUniBuild=as.numeric(distmat_closest$ClosestUniBuild)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="UniBuild500mmRadius"
  distmat_1kmradius$UniBuild500mmRadius=as.numeric(distmat_1kmradius$UniBuild500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="UniBuild2kmRadius"
  distmat_3kmradius$UniBuild2kmRadius=as.numeric(distmat_3kmradius$UniBuild2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  
  #Use Coordinates of Shops and Supermarkets___________________________________________________________________________________
  
  
  #Build a query asking for cinemas
  #building the query
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("shop", "supermarket")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_polygons$osm_id)){
    
    cinmat[i,1]=cinema$osm_polygons$osm_id[i]
    cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
    cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSuperMarket"
  distmat_closest$ClosestSuperMarket=as.numeric(distmat_closest$ClosestSuperMarket)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="SuperMarket500mmRadius"
  distmat_1kmradius$SuperMarket500mmRadius=as.numeric(distmat_1kmradius$SuperMarket500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="SuperMarket1kmRadius"
  distmat_3kmradius$SuperMarket1kmRadius=as.numeric(distmat_3kmradius$SuperMarket1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  available_tags("shop")
  
  #Clothingshops__________________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("shop", "clothes")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #c1lon=cinema$osm_points$geometry[[7]][1]
  #c1lat=cinema$osm_points$geometry[[7]][2]
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_polygons$osm_id), nrow = length(cinema$osm_polygons$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_polygons$osm_id)){
    
    cinmat[i,1]=cinema$osm_polygons$osm_id[i]
    cinmat[i,2]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,1]
    cinmat[i,3]=as.data.frame(cinema$osm_polygons$geometry[[i]][1])[1,2]
    
    #print(cinema$osm_points$name[i])
    #print(cinema$osm_points$geometry[[i]][])
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    #print(levels(as.factor(rawData$Station))[i])
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
      #print(cindist)
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 500)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 2000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestClothesShop"
  distmat_closest$ClosestClothesShop=as.numeric(distmat_closest$ClosestClothesShop)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="ClothesShop500mmRadius"
  distmat_1kmradius$ClothesShop500mmRadius=as.numeric(distmat_1kmradius$ClothesShop500mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="ClothesShop2kmRadius"
  distmat_3kmradius$ClothesShop2kmRadius=as.numeric(distmat_3kmradius$ClothesShop2kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  available_tags("amenity")
  
  #Get Data about public transport by OSM____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("highway", "bus_stop")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$name), nrow = length(cinema$osm_points$name), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$name)){
    
    cinmat[i,1]=cinema$osm_points$name[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestBusStop"
  distmat_closest$ClosestBusStop=as.numeric(distmat_closest$ClosestBusStop)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="BusStop250mmRadius"
  distmat_1kmradius$BusStop250mmRadius=as.numeric(distmat_1kmradius$BusStop250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="BusStop1kmRadius"
  distmat_3kmradius$BusStop1kmRadius=as.numeric(distmat_3kmradius$BusStop1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Crossing Signals_______________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("highway", "traffic_signals")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestSignals"
  distmat_closest$ClosestSignals=as.numeric(distmat_closest$ClosestSignals)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="Signals250mmRadius"
  distmat_1kmradius$Signals250mmRadius=as.numeric(distmat_1kmradius$Signals250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="Signals1kmRadius"
  distmat_3kmradius$Signals1kmRadius=as.numeric(distmat_3kmradius$Signals1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Crossing Unmarked_______________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("crossing", "unmarked")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
  
  #divide in stations in a for loop
  #Each Loop is for one station
  #Than calculate distance to the closest cinema
  for(i in 1:nlevels(as.factor(rawData$Station))){
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    distc=c(1:length(cinmat$name))
    
    #Start loops for each cinemar
    for (j in 1:length(cinmat$name)) {
      cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
      distc[j]=cindist
    }
    
    
    distmat_closest[i,1]=d[1,1]
    distmat_closest[i,2]=min(distc)
    
    distmat_1kmradius[i,1]=d[1,1]
    distmat_1kmradius[i,2]=sum(distc < 250)
    
    distmat_3kmradius[i,1]=d[1,1]
    distmat_3kmradius[i,2]=sum(distc < 1000)
    
  }
  
  distmat_closest=as.data.frame(distmat_closest)
  names(distmat_closest)[1]="Station"
  names(distmat_closest)[2]="ClosestUnmCross"
  distmat_closest$ClosestUnmCross=as.numeric(distmat_closest$ClosestUnmCross)
  
  distmat_1kmradius=as.data.frame(distmat_1kmradius)
  names(distmat_1kmradius)[1]="Station"
  names(distmat_1kmradius)[2]="UnmCross250mmRadius"
  distmat_1kmradius$UnmCross250mmRadius=as.numeric(distmat_1kmradius$UnmCross250mmRadius)
  
  distmat_3kmradius=as.data.frame(distmat_3kmradius)
  names(distmat_3kmradius)[1]="Station"
  names(distmat_3kmradius)[2]="UnmCross1kmRadius"
  distmat_3kmradius$UnmCross1kmRadius=as.numeric(distmat_3kmradius$UnmCross1kmRadius)
  
  rawData = merge(x = rawData,y = distmat_closest,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_1kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  rawData = merge(x = rawData,y = distmat_3kmradius,
                  by = c("Station"),
                  all = FALSE)
  
  
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  #Get Tram Stattions_____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("railway", "tram_stop")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 250)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 1000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestTram"
    distmat_closest$ClosestTram=as.numeric(distmat_closest$ClosestTram)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="Tram250mmRadius"
    distmat_1kmradius$Tram250mmRadius=as.numeric(distmat_1kmradius$Tram250mmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="Tram1kmRadius"
    distmat_3kmradius$Tram1kmRadius=as.numeric(distmat_3kmradius$Tram1kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTram=50000
    rawData$Tram250mmRadius=0
    rawData$Tram1kmRadius=0
  }
  
  summary(rawData)
  
  
  #Get Subway Entrance_____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("railway", "subway_entrance")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 250)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 1000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestSubway"
    distmat_closest$ClosestSubway=as.numeric(distmat_closest$ClosestSubway)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="Subway250mmRadius"
    distmat_1kmradius$Subway250mmRadius=as.numeric(distmat_1kmradius$Subway250mmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="Subway1kmRadius"
    distmat_3kmradius$Subway1kmRadius=as.numeric(distmat_3kmradius$Subway1kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestSubway=50000
    rawData$Subway250mmRadius=0
    rawData$Subway1kmRadius=0
  }
  
  summary(rawData)
  
  #Railway Station operated by the DB Netz AG_____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("railway", "station")%>%
    add_osm_feature("operator", "DB Netz AG")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 1000)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 3000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestTrainS"
    distmat_closest$ClosestTrainS=as.numeric(distmat_closest$ClosestTrainS)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="TrainS1kmRadius"
    distmat_1kmradius$TrainS1kmRadius=as.numeric(distmat_1kmradius$TrainS1kmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="TrainS3kmRadius"
    distmat_3kmradius$TrainS3kmRadius=as.numeric(distmat_3kmradius$TrainS3kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTrainS=50000
    rawData$TrainS1kmRadius=0
    rawData$TrainS3kmRadius=0
  }
  
  summary(rawData)
  
  #Bike Shops_____________________________________________________________
  
  q <- getbb(toString(rawData$Town[1])) %>%
    opq() %>%
    add_osm_feature("shop", "bicycle")
  
  str(q) #query structure
  
  cinema <- osmdata_sf(q)
  
  #cinema$osm_points$osm_id
  
  #create a matrix, that later will contaion needed information about name, longitude and latitude of cinemas
  cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)
  
  for(i in 1:length(cinema$osm_points$osm_id)){
    
    cinmat[i,1]=cinema$osm_points$osm_id[i]
    cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
    cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
    
  }
  
  #cinmat=na.omit(cinmat)
  cinmat=as.data.frame(cinmat)
  names(cinmat)[1]="name"
  names(cinmat)[2]="lon"
  names(cinmat)[3]="lat"
  cinmat$lon=as.numeric(cinmat$lon)
  cinmat$lat=as.numeric(cinmat$lat)
  
  #Not every city has tramstations, so we make a if question for this
  
  if(length(cinmat$name)>0){
    
    distmat_closest=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_1kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    distmat_3kmradius=matrix(1:2*nlevels(as.factor(rawData$Station)), nrow = nlevels(as.factor(rawData$Station)), ncol = 2)
    
    #divide in stations in a for loop
    #Each Loop is for one station
    #Than calculate distance to the closest cinema
    for(i in 1:nlevels(as.factor(rawData$Station))){
      d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
      
      distc=c(1:length(cinmat$name))
      
      #Start loops for each cinemar
      for (j in 1:length(cinmat$name)) {
        cindist=distm(c(d$Lon[i],d$Lat[i]), c(cinmat$lon[j],cinmat$lat[j]), fun=distGeo)
        distc[j]=cindist
      }
      
      
      distmat_closest[i,1]=d[1,1]
      distmat_closest[i,2]=min(distc)
      
      distmat_1kmradius[i,1]=d[1,1]
      distmat_1kmradius[i,2]=sum(distc < 1000)
      
      distmat_3kmradius[i,1]=d[1,1]
      distmat_3kmradius[i,2]=sum(distc < 3000)
      
    }
    
    distmat_closest=as.data.frame(distmat_closest)
    names(distmat_closest)[1]="Station"
    names(distmat_closest)[2]="ClosestBikeShop"
    distmat_closest$ClosestBikeShop=as.numeric(distmat_closest$ClosestBikeShop)
    
    distmat_1kmradius=as.data.frame(distmat_1kmradius)
    names(distmat_1kmradius)[1]="Station"
    names(distmat_1kmradius)[2]="BikeShop1kmRadius"
    distmat_1kmradius$BikeShop1kmRadius=as.numeric(distmat_1kmradius$BikeShop1kmRadius)
    
    distmat_3kmradius=as.data.frame(distmat_3kmradius)
    names(distmat_3kmradius)[1]="Station"
    names(distmat_3kmradius)[2]="BikeShop3kmRadius"
    distmat_3kmradius$BikeShop3kmRadius=as.numeric(distmat_3kmradius$BikeShop3kmRadius)
    
    rawData = merge(x = rawData,y = distmat_closest,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_1kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    rawData = merge(x = rawData,y = distmat_3kmradius,
                    by = c("Station"),
                    all = FALSE)
    
    #rm(list=setdiff(ls(), "rawData"))
    
  }else{
    
    rawData$ClosestTrainS=50000
    rawData$TrainS1kmRadius=0
    rawData$TrainS3kmRadius=0
  }
  
  summary(rawData)
  
  rm(list=setdiff(ls(), "rawData"))
  
  
  
  #RoadNetwork
  
  city=rawData$Town[1]
  
  q1 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "cycleway")
  q2 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "residential")
  q3 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "living_street")
  q4 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "path")
  q5 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "secondary")
  q6 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("highway", "primary")
  q7 <- getbb(city) %>%
    opq() %>%
    add_osm_feature("bridge", "yes")
  
  #str(q1) #query structure
  
  cycleways <- osmdata_sf(q1)
  residential <- osmdata_sf(q2)
  living_street <- osmdata_sf(q3)
  path <- osmdata_sf(q4)
  secondary <- osmdata_sf(q5)
  primary <- osmdata_sf(q6)
  bridge <- osmdata_sf(q7)
  
  dist_mat=as.data.frame(levels(as.factor(rawData$Station)))
  dist_mat$cycleways = 9999
  dist_mat$residential = 9999
  dist_mat$living_street = 9999
  dist_mat$path = 9999
  dist_mat$secondary = 9999
  dist_mat$primary = 9999
  
  bool_mat=as.data.frame(levels(as.factor(rawData$Station)))
  bool_mat$cycleways = 0
  bool_mat$residential = 0
  bool_mat$living_street = 0
  bool_mat$path = 0
  bool_mat$secondary = 0
  bool_mat$primary = 0
  
  bridge_mat=levels(as.factor(rawData$Station))
  bridge_mat=as.data.frame(bridge_mat)
  bridge_mat$ClosestBridge = 9999
  bridge_mat$isBridge = 0
  
  i=1
  
  for(i in 1:nlevels(as.factor(rawData$Station))){
    
    d=rawData[rawData$Station %in% toString(levels(as.factor(rawData$Station))[i]),]
    
    DT = as.data.frame(cbind(d$Lon[i],d$Lat[i]))
    names(DT)[1]="long1"
    names(DT)[2]="lat1"
    DT2 = st_as_sf(DT, coords = c("long1","lat1"))
    DT2 <- st_set_crs(DT2, 4269)
    st_crs(DT2) <- 4269 
    DT3cycleways = st_transform(cycleways$osm_lines$geometry,4269)
    DT3residential = st_transform(residential$osm_lines$geometry,4269)
    DT3living_street = st_transform(living_street$osm_lines$geometry,4269)
    DT3path = st_transform(path$osm_lines$geometry,4269)
    DT3secondary = st_transform(secondary$osm_lines$geometry,4269)
    DT3primary = st_transform(primary$osm_lines$geometry,4269)
    DT3bridge = st_transform(bridge$osm_lines$geometry,4269)
    
    dist_mat$cycleways[i]=min(st_distance(DT2$geometry, DT3cycleways))
    dist_mat$residential[i]=min(st_distance(DT2$geometry, DT3residential))
    dist_mat$living_street[i]=min(st_distance(DT2$geometry, DT3living_street))
    dist_mat$path[i]=min(st_distance(DT2$geometry, DT3path))
    dist_mat$secondary[i]=min(st_distance(DT2$geometry, DT3secondary))
    dist_mat$primary[i]=min(st_distance(DT2$geometry, DT3primary))
    
    bridge_mat$ClosestBridge[i]=min(st_distance(DT2$geometry, DT3primary))
    if(bridge_mat$ClosestBridge[i]<5){bridge_mat$isBridge[i]=1}
    
    if(dist_mat$cycleways[i]<5){bool_mat$cycleways[i]=1}
    if(dist_mat$residential[i]<5){bool_mat$residential[i]=1}
    if(dist_mat$living_street[i]<5){bool_mat$living_street[i]=1}
    if(dist_mat$path[i]<5){bool_mat$path[i]=1}
    if(dist_mat$secondary[i]<5){bool_mat$secondary[i]=1}
    if(dist_mat$primary[i]<5){bool_mat$primary[i]=1}
  }
  
  dist_mat
  bool_mat
  bridge_mat
  
  names(bool_mat)[1]="Station"
  names(bridge_mat)[1]="Station"
  
  rawData = merge(x = rawData,y = bool_mat,
                  by = c("Station"),
                  all = FALSE)
  
  
  rawData = merge(x = rawData,y = bridge_mat,
                  by = c("Station"),
                  all = FALSE)
  
  summary(rawData)
  
  citation ("osmdata")
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(rawData,paste(toString(rawData$Town[1]),".csv",sep=""))
  