#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Berlin

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
  