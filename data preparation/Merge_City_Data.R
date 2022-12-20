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
  Leipzig = read.csv(file = "Leipzig.csv",sep=",", encoding="ISO-8859-1")
  Erfurt = read.csv(file = "Erfurt.csv",sep=",", encoding="ISO-8859-1")
  Tübingen = read.csv(file = "Tübingen.csv",sep=",", encoding="ISO-8859-1")
  Konstanz = read.csv(file = "Konstanz.csv",sep=",", encoding="ISO-8859-1")
  Heidelberg = read.csv(file = "Heidelberg.csv",sep=",", encoding="ISO-8859-1")
  Ulm = read.csv(file = "Ulm.csv",sep=",", encoding="ISO-8859-1")
  #Offenburg = read.csv(file = "Offenburg.csv",sep=",", encoding="ISO-8859-1")
  Freiburg = read.csv(file = "Freiburg.csv",sep=",", encoding="ISO-8859-1")
  Lörrach = read.csv(file = "Lörrach.csv",sep=",", encoding="ISO-8859-1")
  Ludwigsburg = read.csv(file = "Ludwigsburg.csv",sep=",", encoding="ISO-8859-1")
  
#Small corrections-------------------------------------------------------
  
  Heidelberg$Oneway = FALSE
  Ulm$Oneway = FALSE
  Offenburg$Oneway = FALSE
  Freiburg$Oneway = FALSE
  Lörrach$Oneway = FALSE
  Ludwigsburg$Oneway = FALSE
  
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
  BikeData=rbind(BikeData,Leipzig)
  BikeData=rbind(BikeData,Erfurt)
  BikeData=rbind(BikeData,Tübingen)
  BikeData=rbind(BikeData,Konstanz)
  BikeData=rbind(BikeData,Heidelberg)
  BikeData=rbind(BikeData,Ulm)
  #BikeData=rbind(BikeData,Offenburg)
  BikeData=rbind(BikeData,Freiburg)
  BikeData=rbind(BikeData,Lörrach)
  BikeData=rbind(BikeData,Ludwigsburg)

#print some informations-------------------------------------------------
  
  names(BikeData)
  levels(as.factor(BikeData$Town))
  nlevels(as.factor(BikeData$Station))
  levels(as.factor(BikeData$Station))
  levels(as.factor(BikeData$Year))
  levels(as.factor(BikeData$Density))
  
  BikeData$X <- NULL
  BikeData$Density <- NULL
  BikeData$Timestamp <- NULL
  
  summary(BikeData)
  names(BikeData)
  
  nrow(BikeData)
  
  rm(list=setdiff(ls(), "BikeData"))
  
#Make minor changes with new data sources--------------------------------
  
  #Add Data about number of inhabitants depending on age
  #Also calculate distance to city ratio
  
  #Load data (source: Destatis)
  
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Destatis")
  Destatis12 = read.csv(file = "Altersgruppen 2012.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis13 = read.csv(file = "Altersgruppen 2013.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis14 = read.csv(file = "Altersgruppen 2014.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis15 = read.csv(file = "Altersgruppen 2015.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis16 = read.csv(file = "Altersgruppen 2016.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis17 = read.csv(file = "Altersgruppen 2017.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis18 = read.csv(file = "Altersgruppen 2018.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis19 = read.csv(file = "Altersgruppen 2019.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis20 = read.csv(file = "Altersgruppen 2020.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis21 = read.csv(file = "Altersgruppen 2021.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  Destatis22 = read.csv(file = "Altersgruppen 2021.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  
  Altersgruppen = rbind(Destatis12,Destatis13)
  Altersgruppen = rbind(Altersgruppen,Destatis14)
  Altersgruppen = rbind(Altersgruppen,Destatis15)
  Altersgruppen = rbind(Altersgruppen,Destatis16)
  Altersgruppen = rbind(Altersgruppen,Destatis17)
  Altersgruppen = rbind(Altersgruppen,Destatis18)
  Altersgruppen = rbind(Altersgruppen,Destatis19)
  Altersgruppen = rbind(Altersgruppen,Destatis20)
  Altersgruppen = rbind(Altersgruppen,Destatis21)
  names(Altersgruppen)
  Altersgruppen$X.1 = NULL
  Altersgruppen$Timestamp=as.POSIXlt(Altersgruppen$X,format="%d.%m.%Y")
  Altersgruppen$Year	= as.numeric(format(as.POSIXlt(Altersgruppen$Timestamp), format = "%Y"))
  Altersgruppen$Timestamp = NULL
  names(Altersgruppen)[2]="Town"
  Altersgruppen$X = NULL
  for(i in 1: nrow(Altersgruppen)){
    if(Altersgruppen$Town[i] == "Berlin, kreisfreie Stadt"){Altersgruppen$Town[i] = "Berlin"}
    else if(Altersgruppen$Town[i] == "Bochum, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bochum"}
    else if(Altersgruppen$Town[i] == "Bonn, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bonn"}
    else if(Altersgruppen$Town[i] == "Bremen, kreisfreie Stadt"){Altersgruppen$Town[i] = "Bremen"}
    else if(Altersgruppen$Town[i] == "Darmstadt, kreisfreie Stadt"){Altersgruppen$Town[i] = "Darmstadt"}
    else if(Altersgruppen$Town[i] == "Düsseldorf, kreisfreie Stadt"){Altersgruppen$Town[i] = "Düsseldorf"}
    else if(Altersgruppen$Town[i] == "Hamburg, kreisfreie Stadt"){Altersgruppen$Town[i] = "Hamburg"}
    else if(Altersgruppen$Town[i] == "Leipzig, kreisfreie Stadt"){Altersgruppen$Town[i] = "Leipzig"}
    else if(Altersgruppen$Town[i] == "Mannheim, kreisfreie Stadt"){Altersgruppen$Town[i] = "Mannheim"}
    else if(Altersgruppen$Town[i] == "München, kreisfreie Stadt"){Altersgruppen$Town[i] = "München"}
    else if(Altersgruppen$Town[i] == "Münster, kreisfreie Stadt"){Altersgruppen$Town[i] = "Münster"}
    else if(Altersgruppen$Town[i] == "Oberhausen, kreisfreie Stadt"){Altersgruppen$Town[i] = "Oberhausen"}
    else if(Altersgruppen$Town[i] == "Rostock, kreisfreie Stadt"){Altersgruppen$Town[i] = "Rostock"}
    else if(Altersgruppen$Town[i] == "Siegen-Wittgenstein, Landkreis"){Altersgruppen$Town[i] = "Siegen"}
    else if(Altersgruppen$Town[i] == "Erfurt, kreisfreie Stadt"){Altersgruppen$Town[i] = "Erfurt"}
    else if(Altersgruppen$Town[i] == "Tübingen, Landkreis"){Altersgruppen$Town[i] = "Tübingen"}
    else if(Altersgruppen$Town[i] == "Konstanz, Landkreis"){Altersgruppen$Town[i] = "Konstanz"}
    else if(Altersgruppen$Town[i] == "Heidelberg, kreisfreie Stadt"){Altersgruppen$Town[i] = "Heidelberg"}
    else if(Altersgruppen$Town[i] == "Ulm, kreisfreie Stadt"){Altersgruppen$Town[i] = "Ulm"}
    else if(Altersgruppen$Town[i] == "Ortenaukreis"){Altersgruppen$Town[i] = "Offenburg"}
    else if(Altersgruppen$Town[i] == "Freiburg im Breisgau, kreisfreie Stadt"){Altersgruppen$Town[i] = "Freiburg"}
    else if(Altersgruppen$Town[i] == "Lörrach, Landkreis"){Altersgruppen$Town[i] = "Lörrach"}
    else if(Altersgruppen$Town[i] == "Ludwigsburg, Landkreis"){Altersgruppen$Town[i] = "Ludwigsburg"}
    else{Altersgruppen$Town[i] = NA}
  }
  Altersgruppen = na.omit(Altersgruppen)
  
  levels(as.factor(Altersgruppen$Town))
  
  names(Altersgruppen)
  Altersgruppen$young18 = (as.numeric(Altersgruppen$unter.3.Jahre) +
    as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
    as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
    as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
    as.numeric(Altersgruppen$X15.bis.unter.18.Jahre)
    )/as.numeric(Altersgruppen$Insgesamt) * 100
  Altersgruppen$young25 = (as.numeric(Altersgruppen$unter.3.Jahre) +
    as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
    as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
    as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
    as.numeric(Altersgruppen$X15.bis.unter.18.Jahre) +
    as.numeric(Altersgruppen$X18.bis.unter.20.Jahre) + 
    as.numeric(Altersgruppen$X20.bis.unter.25.Jahre)
  )/as.numeric(Altersgruppen$Insgesamt) * 100
  Altersgruppen$young30 = (as.numeric(Altersgruppen$unter.3.Jahre) +
    as.numeric(Altersgruppen$X3.bis.unter.6.Jahre) +
    as.numeric(Altersgruppen$X6.bis.unter.10.Jahre) +
    as.numeric(Altersgruppen$X10.bis.unter.15.Jahre) + 
    as.numeric(Altersgruppen$X15.bis.unter.18.Jahre) +
    as.numeric(Altersgruppen$X18.bis.unter.20.Jahre) + 
    as.numeric(Altersgruppen$X20.bis.unter.25.Jahre) +
    as.numeric(Altersgruppen$X25.bis.unter.30.Jahre)
  )/as.numeric(Altersgruppen$Insgesamt) * 100
  Altersgruppen$older40 = (as.numeric(Altersgruppen$X40.bis.unter.45.Jahre) +
    as.numeric(Altersgruppen$X45.bis.unter.50.Jahre) +
    as.numeric(Altersgruppen$X50.bis.unter.55.Jahre) +
    as.numeric(Altersgruppen$X55.bis.unter.60.Jahre) + 
    as.numeric(Altersgruppen$X60.bis.unter.65.Jahre) +
    as.numeric(Altersgruppen$X65.bis.unter.75.Jahre) + 
    as.numeric(Altersgruppen$X75.Jahre.und.mehr)
  )/as.numeric(Altersgruppen$Insgesamt) * 100
  Altersgruppen$older60 = (as.numeric(Altersgruppen$X60.bis.unter.65.Jahre) +
    as.numeric(Altersgruppen$X65.bis.unter.75.Jahre) + 
    as.numeric(Altersgruppen$X75.Jahre.und.mehr)
  )/as.numeric(Altersgruppen$Insgesamt) * 100
  Altersgruppen$Insgesamt = as.numeric(Altersgruppen$Insgesamt)
  
  Altersgruppen$unter.3.Jahre = NULL
  Altersgruppen$X3.bis.unter.6.Jahre = NULL
  Altersgruppen$X6.bis.unter.10.Jahre = NULL
  Altersgruppen$X10.bis.unter.15.Jahre = NULL
  Altersgruppen$X15.bis.unter.18.Jahre = NULL
  Altersgruppen$X18.bis.unter.20.Jahre = NULL
  Altersgruppen$X20.bis.unter.25.Jahre = NULL
  Altersgruppen$X25.bis.unter.30.Jahre = NULL
  Altersgruppen$X25.bis.unter.30.Jahre = NULL
  Altersgruppen$X30.bis.unter.35.Jahre = NULL
  Altersgruppen$X35.bis.unter.40.Jahre = NULL
  Altersgruppen$X40.bis.unter.45.Jahre = NULL
  Altersgruppen$X45.bis.unter.50.Jahre = NULL
  Altersgruppen$X50.bis.unter.55.Jahre = NULL
  Altersgruppen$X55.bis.unter.60.Jahre = NULL
  Altersgruppen$X60.bis.unter.65.Jahre = NULL
  Altersgruppen$X65.bis.unter.75.Jahre = NULL
  Altersgruppen$X75.Jahre.und.mehr = NULL
  
  names(Altersgruppen)[2]="InhDestrict"
  
  BikeData$young18 = NULL
  BikeData$young20 = NULL
  
  d = Altersgruppen[which(Altersgruppen$Year==2021),]
  d$Year = 2022
  Altersgruppen = rbind(Altersgruppen,d)
  
  BikeData = merge(x = BikeData,y = Altersgruppen,
                   by = c("Year","Town"),
                   all = TRUE)
  
  Immigrants = read.csv(file = "Auslaenderstatistik 12 bis 22.csv",sep=";", encoding="ISO-8859-1", skip = 7)
  names(Immigrants)
  Immigrants$Timestamp=as.POSIXlt(Immigrants$X31.12.2012,format="%d.%m.%Y")
  Immigrants$Year	= as.numeric(format(as.POSIXlt(Immigrants$Timestamp), format = "%Y"))
  Immigrants$X31.12.2012 = NULL
  Immigrants$X01003 = NULL
  Immigrants$X8510 = NULL
  Immigrants$X8195 = NULL
  Immigrants$Timestamp = NULL
  names(Immigrants)[1]="Town"
  names(Immigrants)[2]="Immigrants"
  for(i in 1: nrow(Immigrants)){
    if(Immigrants$Town[i] == "Berlin, kreisfreie Stadt"){Immigrants$Town[i] = "Berlin"}
    else if(Immigrants$Town[i] == "Bochum, kreisfreie Stadt"){Immigrants$Town[i] = "Bochum"}
    else if(Immigrants$Town[i] == "Bonn, kreisfreie Stadt"){Immigrants$Town[i] = "Bonn"}
    else if(Immigrants$Town[i] == "Bremen, kreisfreie Stadt"){Immigrants$Town[i] = "Bremen"}
    else if(Immigrants$Town[i] == "Darmstadt, kreisfreie Stadt"){Immigrants$Town[i] = "Darmstadt"}
    else if(Immigrants$Town[i] == "Düsseldorf, kreisfreie Stadt"){Immigrants$Town[i] = "Düsseldorf"}
    else if(Immigrants$Town[i] == "Hamburg, kreisfreie Stadt"){Immigrants$Town[i] = "Hamburg"}
    else if(Immigrants$Town[i] == "Leipzig, kreisfreie Stadt"){Immigrants$Town[i] = "Leipzig"}
    else if(Immigrants$Town[i] == "Mannheim, kreisfreie Stadt"){Immigrants$Town[i] = "Mannheim"}
    else if(Immigrants$Town[i] == "München, kreisfreie Stadt"){Immigrants$Town[i] = "München"}
    else if(Immigrants$Town[i] == "Münster, kreisfreie Stadt"){Immigrants$Town[i] = "Münster"}
    else if(Immigrants$Town[i] == "Oberhausen, kreisfreie Stadt"){Immigrants$Town[i] = "Oberhausen"}
    else if(Immigrants$Town[i] == "Rostock, kreisfreie Stadt"){Immigrants$Town[i] = "Rostock"}
    else if(Immigrants$Town[i] == "Siegen-Wittgenstein, Landkreis"){Immigrants$Town[i] = "Siegen"}
    else if(Immigrants$Town[i] == "Erfurt, kreisfreie Stadt"){Immigrants$Town[i] = "Erfurt"}
    else if(Immigrants$Town[i] == "Tübingen, Landkreis"){Immigrants$Town[i] = "Tübingen"}
    else if(Immigrants$Town[i] == "Konstanz, Landkreis"){Immigrants$Town[i] = "Konstanz"}
    else if(Immigrants$Town[i] == "Heidelberg, kreisfreie Stadt"){Immigrants$Town[i] = "Heidelberg"}
    else if(Immigrants$Town[i] == "Ulm, kreisfreie Stadt"){Immigrants$Town[i] = "Ulm"}
    else if(Immigrants$Town[i] == "Ortenaukreis"){Immigrants$Town[i] = "Offenburg"}
    else if(Immigrants$Town[i] == "Freiburg im Breisgau, kreisfreie Stadt"){Immigrants$Town[i] = "Freiburg"}
    else if(Immigrants$Town[i] == "Lörrach, Landkreis"){Immigrants$Town[i] = "Lörrach"}
    else if(Immigrants$Town[i] == "Ludwigsburg, Landkreis"){Immigrants$Town[i] = "Ludwigsburg"}
    else{Immigrants$Town[i] = NA}
  }
  Immigrants = na.omit(Immigrants)

  d = Immigrants[which(Immigrants$Year==2021),]
  d$Year = 2022
  Immigrants = rbind(Immigrants,d)
  
  Immigrants$Immigrants = as.numeric(Immigrants$Immigrants)
  
  BikeData = merge(x = BikeData,y = Immigrants,
                   by = c("Year","Town"),
                   all = TRUE)
  
  BikeData$Immigrants = BikeData$Immigrants/BikeData$InhDestrict*100
  
  PKW = read.csv(file = "PKWs12 bis 22.csv",sep=";", encoding="ISO-8859-1", skip = 5)
  names(PKW)
  PKW$Timestamp=as.POSIXlt(PKW$X,format="%d.%m.%Y")
  PKW$Year	= as.numeric(format(as.POSIXlt(PKW$Timestamp), format = "%Y"))
  PKW$X = NULL
  PKW$Timestamp = NULL
  PKW$X.1 = NULL
  names(PKW)[1]="Town"
  names(PKW)[2]="PKWs"
  for(i in 1: nrow(PKW)){
    if(PKW$Town[i] == "Berlin, kreisfreie Stadt"){PKW$Town[i] = "Berlin"}
    else if(PKW$Town[i] == "Bochum, kreisfreie Stadt"){PKW$Town[i] = "Bochum"}
    else if(PKW$Town[i] == "Bonn, kreisfreie Stadt"){PKW$Town[i] = "Bonn"}
    else if(PKW$Town[i] == "Bremen, kreisfreie Stadt"){PKW$Town[i] = "Bremen"}
    else if(PKW$Town[i] == "Darmstadt, kreisfreie Stadt"){PKW$Town[i] = "Darmstadt"}
    else if(PKW$Town[i] == "Düsseldorf, kreisfreie Stadt"){PKW$Town[i] = "Düsseldorf"}
    else if(PKW$Town[i] == "Hamburg, kreisfreie Stadt"){PKW$Town[i] = "Hamburg"}
    else if(PKW$Town[i] == "Leipzig, kreisfreie Stadt"){PKW$Town[i] = "Leipzig"}
    else if(PKW$Town[i] == "Mannheim, kreisfreie Stadt"){PKW$Town[i] = "Mannheim"}
    else if(PKW$Town[i] == "München, kreisfreie Stadt"){PKW$Town[i] = "München"}
    else if(PKW$Town[i] == "Münster, kreisfreie Stadt"){PKW$Town[i] = "Münster"}
    else if(PKW$Town[i] == "Oberhausen, kreisfreie Stadt"){PKW$Town[i] = "Oberhausen"}
    else if(PKW$Town[i] == "Rostock, kreisfreie Stadt"){PKW$Town[i] = "Rostock"}
    else if(PKW$Town[i] == "Siegen-Wittgenstein, Landkreis"){PKW$Town[i] = "Siegen"}
    else if(PKW$Town[i] == "Erfurt, kreisfreie Stadt"){PKW$Town[i] = "Erfurt"}
    else if(PKW$Town[i] == "Tübingen, Landkreis"){PKW$Town[i] = "Tübingen"}
    else if(PKW$Town[i] == "Konstanz, Landkreis"){PKW$Town[i] = "Konstanz"}
    else if(PKW$Town[i] == "Heidelberg, kreisfreie Stadt"){PKW$Town[i] = "Heidelberg"}
    else if(PKW$Town[i] == "Ulm, kreisfreie Stadt"){PKW$Town[i] = "Ulm"}
    else if(PKW$Town[i] == "Ortenaukreis"){PKW$Town[i] = "Offenburg"}
    else if(PKW$Town[i] == "Freiburg im Breisgau, kreisfreie Stadt"){PKW$Town[i] = "Freiburg"}
    else if(PKW$Town[i] == "Lörrach, Landkreis"){PKW$Town[i] = "Lörrach"}
    else if(PKW$Town[i] == "Ludwigsburg, Landkreis"){PKW$Town[i] = "Ludwigsburg"}
    else{PKW$Town[i] = NA}
  }
  PKW = na.omit(PKW)
  PKW$PKWs=as.numeric(PKW$PKWs)
  
  BikeData = merge(x = BikeData,y = PKW,
                  by = c("Year","Town"),
                  all = TRUE)
  
  BikeData$PKWs = BikeData$PKWs/BikeData$InhDestrict
  
  summary(BikeData)
  
#Check Data set ---------------------------------------------------------
  
  BikeData = na.omit(BikeData)
  nlevels(as.factor(BikeData$Station))
  b = nlevels(as.factor(BikeData$Station))
  a -b
  levels(as.factor(BikeData$Town))
  names(BikeData)
  summary(BikeData)
  
#Last corrections
  
  summary(BikeData$CloudCover)
  levels(as.factor(BikeData$CloudCover))
  
  
  BikeData = BikeData[!(BikeData$CloudCover==-1),]
  
  
  summary(BikeData$CloudCover)
  levels(as.factor(BikeData$CloudCover))
  
  BikeData = na.omit(BikeData)
  
#Save new data set-------------------------------------------------------
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
  write.csv(BikeData,"completeDataSet_1.csv")
  
  