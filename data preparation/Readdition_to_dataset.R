#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation: Readditon to Dataset

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

#Load Data Set
BikeData = read.csv(file = "completeDataSet_1.csv",sep=",", encoding="ISO-8859-1")
names(BikeData)
levels(as.factor(BikeData$Town))

#Add Corona Variables

#Using the Corona Incidence

#BikeData$CorInc = -1
#BikeData$CorInc <- ifelse(BikeData$Year < 2020 , 0, -1)
#BikeData$CorInc <- ifelse(BikeData$Year == 2020 &&  BikeData$Months < 3, 0, -1)
#BikeData$CorInc <- ifelse(BikeData$Year == 2020 &&  BikeData$Months == 3 && BikeData$Day < 10, 0, -1)

BikeData$CorNull = 0

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/RKI")
laenderData = read.csv(file = "Inzidenz_Laender_2020_Mae_bis_Nov.csv",sep=";", encoding="ISO-8859-1", header = FALSE)

laenderData2 <- data.frame(t(laenderData[-1]))
colnames(laenderData2) <- laenderData[, 1]
rm(laenderData)

laenderData2$Timestamp = as.POSIXlt(laenderData2$Datum,format="%d.%m.%Y")
laenderData2$Months = as.numeric(format(as.POSIXlt(laenderData2$Timestamp), format = "%m"))
laenderData2$Day = as.numeric(format(as.POSIXlt(laenderData2$Timestamp), format = "%d"))
laenderData2$Year = 2020

names(laenderData2)
names(laenderData2)[2]="BadenWürttemberg"
names(laenderData2)[9]="MecklenburgVorpommern"
names(laenderData2)[11]="NordrheinWestfalen"
names(laenderData2)[12]="RheinlandPfalz"
names(laenderData2)[15]="SachsenAnhalt"
names(laenderData2)[16]="SchleswigHolstein"

ber = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Berlin, fixed = TRUE)),laenderData2$Day))
ber = as.data.frame(cbind(ber,laenderData2$Months))
ber = as.data.frame(cbind(ber,laenderData2$Year))
names(ber)[1]="inzidenz_Bundesland"
names(ber)[2]="Day"
names(ber)[3]="Months"
names(ber)[4]="Year"
ber$Bundesland="Berlin"

nrw = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$NordrheinWestfalen, fixed = TRUE)),laenderData2$Day))
nrw = as.data.frame(cbind(nrw,laenderData2$Months))
nrw = as.data.frame(cbind(nrw,laenderData2$Year))
names(nrw)[1]="inzidenz_Bundesland"
names(nrw)[2]="Day"
names(nrw)[3]="Months"
names(nrw)[4]="Year"
nrw$Bundesland="NordrheinWestfalen"

bre = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Bremen, fixed = TRUE)),laenderData2$Day))
bre = as.data.frame(cbind(bre,laenderData2$Months))
bre = as.data.frame(cbind(bre,laenderData2$Year))
names(bre)[1]="inzidenz_Bundesland"
names(bre)[2]="Day"
names(bre)[3]="Months"
names(bre)[4]="Year"
bre$Bundesland="Bremen"

hes = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Hessen, fixed = TRUE)),laenderData2$Day))
hes = as.data.frame(cbind(hes,laenderData2$Months))
hes = as.data.frame(cbind(hes,laenderData2$Year))
names(hes)[1]="inzidenz_Bundesland"
names(hes)[2]="Day"
names(hes)[3]="Months"
names(hes)[4]="Year"
hes$Bundesland="Hessen"

thu = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Thüringen, fixed = TRUE)),laenderData2$Day))
thu = as.data.frame(cbind(thu,laenderData2$Months))
thu = as.data.frame(cbind(thu,laenderData2$Year))
names(thu)[1]="inzidenz_Bundesland"
names(thu)[2]="Day"
names(thu)[3]="Months"
names(thu)[4]="Year"
thu$Bundesland="Thüringen"

bwb = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$BadenWürttemberg, fixed = TRUE)),laenderData2$Day))
bwb = as.data.frame(cbind(bwb,laenderData2$Months))
bwb = as.data.frame(cbind(bwb,laenderData2$Year))
names(bwb)[1]="inzidenz_Bundesland"
names(bwb)[2]="Day"
names(bwb)[3]="Months"
names(bwb)[4]="Year"
bwb$Bundesland="BadenWürttemberg"

ham = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Hamburg, fixed = TRUE)),laenderData2$Day))
ham = as.data.frame(cbind(ham,laenderData2$Months))
ham = as.data.frame(cbind(ham,laenderData2$Year))
names(ham)[1]="inzidenz_Bundesland"
names(ham)[2]="Day"
names(ham)[3]="Months"
names(ham)[4]="Year"
ham$Bundesland="Hamburg"

sac = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Sachsen, fixed = TRUE)),laenderData2$Day))
sac = as.data.frame(cbind(sac,laenderData2$Months))
sac = as.data.frame(cbind(sac,laenderData2$Year))
names(sac)[1]="inzidenz_Bundesland"
names(sac)[2]="Day"
names(sac)[3]="Months"
names(sac)[4]="Year"
sac$Bundesland="Sachsen"

bay = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Bayern, fixed = TRUE)),laenderData2$Day))
bay = as.data.frame(cbind(bay,laenderData2$Months))
bay = as.data.frame(cbind(bay,laenderData2$Year))
names(bay)[1]="inzidenz_Bundesland"
names(bay)[2]="Day"
names(bay)[3]="Months"
names(bay)[4]="Year"
bay$Bundesland="Bayern"

mvp = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$MecklenburgVorpommern, fixed = TRUE)),laenderData2$Day))
mvp = as.data.frame(cbind(mvp,laenderData2$Months))
mvp = as.data.frame(cbind(mvp,laenderData2$Year))
names(mvp)[1]="inzidenz_Bundesland"
names(mvp)[2]="Day"
names(mvp)[3]="Months"
names(mvp)[4]="Year"
mvp$Bundesland="MecklenburgVorpommern"

corBL = as.data.frame(rbind(ber,nrw))
corBL = as.data.frame(rbind(corBL,nrw))
corBL = as.data.frame(rbind(corBL,bre))
corBL = as.data.frame(rbind(corBL,hes))
corBL = as.data.frame(rbind(corBL,thu))
corBL = as.data.frame(rbind(corBL,bwb))
corBL = as.data.frame(rbind(corBL,ham))
corBL = as.data.frame(rbind(corBL,sac))
corBL = as.data.frame(rbind(corBL,bay))
corBL = as.data.frame(rbind(corBL,mvp))

BikeData$Bundesland = "empty"

BikeData[BikeData$Town == "Berlin", ]$Bundesland = "Berlin"
BikeData[BikeData$Town == "Bochum", ]$Bundesland = "NordrheinWestfalen"
BikeData[BikeData$Town == "Bonn", ]$Bundesland = "NordrheinWestfalen"
BikeData[BikeData$Town == "Bremen", ]$Bundesland = "Bremen"
BikeData[BikeData$Town == "Darmstadt", ]$Bundesland = "Hessen"
BikeData[BikeData$Town == "Düsseldorf", ]$Bundesland = "NordrheinWestfalen"
BikeData[BikeData$Town == "Erfurt", ]$Bundesland = "Thüringen"
BikeData[BikeData$Town == "Freiburg", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Hamburg", ]$Bundesland = "Hamburg"
BikeData[BikeData$Town == "Heidelberg", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Konstanz", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Leipzig", ]$Bundesland = "Sachsen"
BikeData[BikeData$Town == "Lörrach", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Ludwigsburg", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Mannheim", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "München", ]$Bundesland = "Bayern"
BikeData[BikeData$Town == "Münster", ]$Bundesland = "NordrheinWestfalen"
BikeData[BikeData$Town == "Rostock", ]$Bundesland = "MecklenburgVorpommern"
BikeData[BikeData$Town == "Siegen", ]$Bundesland = "NordrheinWestfalen"
BikeData[BikeData$Town == "Tübingen", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Ulm", ]$Bundesland = "BadenWürttemberg"
BikeData[BikeData$Town == "Oberhausen", ]$Bundesland = "NordrheinWestfalen"

levels(as.factor(BikeData$Bundesland))

BikeData = merge(x = BikeData,y = corBL,
  by = c("Year","Months","Day","Bundesland"),
  all = TRUE)

BikeData$inzidenz_Bundesland[is.na(BikeData$inzidenz_Bundesland)] <- 0
BikeData = na.omit(BikeData)

summary(BikeData$inzidenz_Bundesland)

rm(list=setdiff(ls(), c("BikeData")))

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/RKI")
landkreisData = read.csv(file = "Inzidenz_Kreise_ab_2020_Nov.csv",sep=";", encoding="ISO-8859-1", header = FALSE)
landkreisData[,1]=NULL
landkreisData[,2]=NULL

names(landkreisData)[1]="Stadt"

#Berlin

#Sonderfall
#SK Berlin Charlottenburg-Wilmersdorf
#SK Berlin Friedrichshain-Kreuzberg
#SK Berlin Lichtenberg
#SK Berlin Marzahn-Hellersdorf
#SK Berlin Mitte
#SK Berlin Neuk^lln
#SK Berlin Pankow
#SK Berlin Reinickendorf
#SK Berlin Spandau
#SK Berlin Steglitz-Zehlendorf
#SK Berlin Tempelhof-Sch^neberg
#SK Berlin Treptow-K^penick
s1 =landkreisData[landkreisData$Stadt == "SK Berlin Charlottenburg-Wilmersdorf", ]
s2 =landkreisData[landkreisData$Stadt == "SK Berlin Friedrichshain-Kreuzberg", ]
s3 =landkreisData[landkreisData$Stadt == "SK Berlin Lichtenberg", ]
s4 =landkreisData[landkreisData$Stadt == "SK Berlin Marzahn-Hellersdorf", ]
s5 =landkreisData[landkreisData$Stadt == "SK Berlin Mitte", ]
s6 =landkreisData[landkreisData$Stadt == "SK Berlin Neukölln", ]
s7 =landkreisData[landkreisData$Stadt == "SK Berlin Pankow", ]
s8 =landkreisData[landkreisData$Stadt == "SK Berlin Reinickendorf", ]
s9 =landkreisData[landkreisData$Stadt == "SK Berlin Spandau", ]
s10 =landkreisData[landkreisData$Stadt == "SK Berlin Steglitz-Zehlendorf", ]
s11 =landkreisData[landkreisData$Stadt == "SK Berlin Tempelhof-Schöneberg", ]
s12 =landkreisData[landkreisData$Stadt == "SK Berlin Treptow-Köpenick", ]

inzidenz_kommunal1 = as.numeric(sub(",", ".", s1, fixed = TRUE))
inzidenz_kommunal2 = as.numeric(sub(",", ".", s2, fixed = TRUE))
inzidenz_kommunal3 = as.numeric(sub(",", ".", s3, fixed = TRUE))
inzidenz_kommunal4 = as.numeric(sub(",", ".", s4, fixed = TRUE))
inzidenz_kommunal5 = as.numeric(sub(",", ".", s5, fixed = TRUE))
inzidenz_kommunal6 = as.numeric(sub(",", ".", s6, fixed = TRUE))
inzidenz_kommunal7 = as.numeric(sub(",", ".", s7, fixed = TRUE))
inzidenz_kommunal8 = as.numeric(sub(",", ".", s8, fixed = TRUE))
inzidenz_kommunal9 = as.numeric(sub(",", ".", s9, fixed = TRUE))
inzidenz_kommunal10 = as.numeric(sub(",", ".", s10, fixed = TRUE))
inzidenz_kommunal11 = as.numeric(sub(",", ".", s11, fixed = TRUE))
inzidenz_kommunal12 = as.numeric(sub(",", ".", s12, fixed = TRUE))

inzidenz_kommunal = c(1:length(inzidenz_kommunal1))
for(i in 1:length(inzidenz_kommunal1)){
  
  inzidenz_kommunal[i] = round(mean (c(inzidenz_kommunal1[i],inzidenz_kommunal2[i],inzidenz_kommunal3[i],inzidenz_kommunal4[i],
                                 inzidenz_kommunal5[i],inzidenz_kommunal6[i],inzidenz_kommunal7[i],inzidenz_kommunal8[i],
                                 inzidenz_kommunal9[i],inzidenz_kommunal10[i],inzidenz_kommunal11[i],inzidenz_kommunal12[i])))
  
}
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

berlin = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
berlin <- data.frame(t(berlin[-1]))
names(berlin)[1] = "Timestamp"
names(berlin)[2] = "Inzidenz_Stadt"
berlin$Town = "Berlin"

#Bochum

s =landkreisData[landkreisData$Stadt == "SK Bochum", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bochum = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bochum <- data.frame(t(bochum[-1]))
names(bochum)[1] = "Timestamp"
names(bochum)[2] = "Inzidenz_Stadt"
bochum$Town = "Bochum"

#Bonn

s =landkreisData[landkreisData$Stadt == "SK Bonn", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bonn = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bonn <- data.frame(t(bonn[-1]))
names(bonn)[1] = "Timestamp"
names(bonn)[2] = "Inzidenz_Stadt"
bonn$Town = "Bonn"

#Bremen

s =landkreisData[landkreisData$Stadt == "SK Bremen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bremen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bremen <- data.frame(t(bremen[-1]))
names(bremen)[1] = "Timestamp"
names(bremen)[2] = "Inzidenz_Stadt"
bremen$Town = "Bremen"

#Darmstadt

s =landkreisData[landkreisData$Stadt == "SK Darmstadt", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

darmstadt = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
darmstadt <- data.frame(t(darmstadt[-1]))
names(darmstadt)[1] = "Timestamp"
names(darmstadt)[2] = "Inzidenz_Stadt"
darmstadt$Town = "Darmstadt"

#Düsseldorf

s =landkreisData[landkreisData$Stadt == "SK Düsseldorf", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

dusseldorf = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
dusseldorf <- data.frame(t(dusseldorf[-1]))
names(dusseldorf)[1] = "Timestamp"
names(dusseldorf)[2] = "Inzidenz_Stadt"
dusseldorf$Town = "Düsseldorf"

#Erfurt

s =landkreisData[landkreisData$Stadt == "SK Erfurt", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

erfurt = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
erfurt <- data.frame(t(erfurt[-1]))
names(erfurt)[1] = "Timestamp"
names(erfurt)[2] = "Inzidenz_Stadt"
erfurt$Town = "Erfurt"

#Freiburg

s =landkreisData[landkreisData$Stadt == "SK Freiburg i.Breisgau", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

freiburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
freiburg <- data.frame(t(freiburg[-1]))
names(freiburg)[1] = "Timestamp"
names(freiburg)[2] = "Inzidenz_Stadt"
freiburg$Town = "Freiburg"

#Hamburg

s =landkreisData[landkreisData$Stadt == "SK Hamburg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

hamburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
hamburg <- data.frame(t(hamburg[-1]))
names(hamburg)[1] = "Timestamp"
names(hamburg)[2] = "Inzidenz_Stadt"
hamburg$Town = "Hamburg"

#Heidelberg

s =landkreisData[landkreisData$Stadt == "SK Heidelberg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

heidelberg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
heidelberg <- data.frame(t(heidelberg[-1]))
names(heidelberg)[1] = "Timestamp"
names(heidelberg)[2] = "Inzidenz_Stadt"
heidelberg$Town = "Heidelberg"

#Konstanz

s =landkreisData[landkreisData$Stadt == "LK Konstanz", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

konstanz = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
konstanz <- data.frame(t(konstanz[-1]))
names(konstanz)[1] = "Timestamp"
names(konstanz)[2] = "Inzidenz_Stadt"
konstanz$Town = "Konstanz"

#Leipzig

s =landkreisData[landkreisData$Stadt == "SK Leipzig", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

leipzig = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
leipzig <- data.frame(t(leipzig[-1]))
names(leipzig)[1] = "Timestamp"
names(leipzig)[2] = "Inzidenz_Stadt"
leipzig$Town = "Leipzig"

#Lörrach

s =landkreisData[landkreisData$Stadt == "LK Lörrach", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

lorrach = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
lorrach <- data.frame(t(lorrach[-1]))
names(lorrach)[1] = "Timestamp"
names(lorrach)[2] = "Inzidenz_Stadt"
lorrach$Town = "Lörrach"

#Ludwigsburg

s =landkreisData[landkreisData$Stadt == "LK Ludwigsburg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

ludwigsburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
ludwigsburg <- data.frame(t(ludwigsburg[-1]))
names(ludwigsburg)[1] = "Timestamp"
names(ludwigsburg)[2] = "Inzidenz_Stadt"
ludwigsburg$Town = "Ludwigsburg"

#Mannheim

s =landkreisData[landkreisData$Stadt == "SK Mannheim", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

mannheim = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
mannheim <- data.frame(t(mannheim[-1]))
names(mannheim)[1] = "Timestamp"
names(mannheim)[2] = "Inzidenz_Stadt"
mannheim$Town = "Mannheim"

#München

s =landkreisData[landkreisData$Stadt == "SK München", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

munchen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
munchen <- data.frame(t(munchen[-1]))
names(munchen)[1] = "Timestamp"
names(munchen)[2] = "Inzidenz_Stadt"
munchen$Town = "München"

#Münster

s =landkreisData[landkreisData$Stadt == "SK Münster", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

munster = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
munster <- data.frame(t(munster[-1]))
names(munster)[1] = "Timestamp"
names(munster)[2] = "Inzidenz_Stadt"
munster$Town = "Münster"

#Rostock

s =landkreisData[landkreisData$Stadt == "SK Rostock", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

rostock = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
rostock <- data.frame(t(rostock[-1]))
names(rostock)[1] = "Timestamp"
names(rostock)[2] = "Inzidenz_Stadt"
rostock$Town = "Rostock"

#Siegen

s =landkreisData[landkreisData$Stadt == "LK Siegen-Wittgenstein", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

siegen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
siegen <- data.frame(t(siegen[-1]))
names(siegen)[1] = "Timestamp"
names(siegen)[2] = "Inzidenz_Stadt"
siegen$Town = "Siegen"

#Tübingen

s =landkreisData[landkreisData$Stadt == "LK Tübingen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

tubingen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
tubingen <- data.frame(t(tubingen[-1]))
names(tubingen)[1] = "Timestamp"
names(tubingen)[2] = "Inzidenz_Stadt"
tubingen$Town = "Tübingen"

#Ulm

s =landkreisData[landkreisData$Stadt == "SK Ulm", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

ulm = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
ulm <- data.frame(t(ulm[-1]))
names(ulm)[1] = "Timestamp"
names(ulm)[2] = "Inzidenz_Stadt"
ulm$Town = "Ulm"

#Oberhausen

s =landkreisData[landkreisData$Stadt == "SK Oberhausen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

oberhausen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
oberhausen <- data.frame(t(oberhausen[-1]))
names(oberhausen)[1] = "Timestamp"
names(oberhausen)[2] = "Inzidenz_Stadt"
oberhausen$Town = "Oberhausen"

landkreisData2 = as.data.frame(rbind(berlin,bochum))
landkreisData2 = as.data.frame(rbind(landkreisData2,bonn))
landkreisData2 = as.data.frame(rbind(landkreisData2,bremen))
landkreisData2 = as.data.frame(rbind(landkreisData2,darmstadt))
landkreisData2 = as.data.frame(rbind(landkreisData2,dusseldorf))
landkreisData2 = as.data.frame(rbind(landkreisData2,erfurt))
landkreisData2 = as.data.frame(rbind(landkreisData2,freiburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,hamburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,heidelberg))
landkreisData2 = as.data.frame(rbind(landkreisData2,konstanz))
landkreisData2 = as.data.frame(rbind(landkreisData2,leipzig))
landkreisData2 = as.data.frame(rbind(landkreisData2,lorrach))
landkreisData2 = as.data.frame(rbind(landkreisData2,ludwigsburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,mannheim))
landkreisData2 = as.data.frame(rbind(landkreisData2,munchen))
landkreisData2 = as.data.frame(rbind(landkreisData2,munster))
landkreisData2 = as.data.frame(rbind(landkreisData2,rostock))
landkreisData2 = as.data.frame(rbind(landkreisData2,siegen))
landkreisData2 = as.data.frame(rbind(landkreisData2,tubingen))
landkreisData2 = as.data.frame(rbind(landkreisData2,ulm))
landkreisData2 = as.data.frame(rbind(landkreisData2,oberhausen))

landkreisData2$Timestamp = as.POSIXlt(landkreisData2$Timestamp,format="%d.%m.%Y")
landkreisData2$Year = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%Y"))
landkreisData2$Months = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%m"))
landkreisData2$Day = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%d"))
landkreisData2$Timestamp = NULL

names(landkreisData2)

BikeData = merge(x = BikeData,y = landkreisData2,
                 by = c("Year","Months","Day","Town"),
                 all = TRUE)

BikeData$Inzidenz_Stadt[is.na(BikeData$Inzidenz_Stadt)] <- 0
BikeData = na.omit(BikeData)

BikeData$Inzidenz_Stadt = as.numeric(BikeData$Inzidenz_Stadt)
summary(BikeData$Inzidenz_Stadt)

rm(list=setdiff(ls(), c("BikeData")))

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/RKI")
landkreisData = read.csv(file = "Inzidenz_Kreise_ab_2021_Sep.csv",sep=";", encoding="ISO-8859-1", header = FALSE)
landkreisData[,2]=NULL

names(landkreisData)[1]="Stadt"

#Berlin

#Sonderfall
#SK Berlin Charlottenburg-Wilmersdorf
#SK Berlin Friedrichshain-Kreuzberg
#SK Berlin Lichtenberg
#SK Berlin Marzahn-Hellersdorf
#SK Berlin Mitte
#SK Berlin Neuk^lln
#SK Berlin Pankow
#SK Berlin Reinickendorf
#SK Berlin Spandau
#SK Berlin Steglitz-Zehlendorf
#SK Berlin Tempelhof-Sch^neberg
#SK Berlin Treptow-K^penick
s1 =landkreisData[landkreisData$Stadt == "SK Berlin Charlottenburg-Wilmersdorf", ]
s2 =landkreisData[landkreisData$Stadt == "SK Berlin Friedrichshain-Kreuzberg", ]
s3 =landkreisData[landkreisData$Stadt == "SK Berlin Lichtenberg", ]
s4 =landkreisData[landkreisData$Stadt == "SK Berlin Marzahn-Hellersdorf", ]
s5 =landkreisData[landkreisData$Stadt == "SK Berlin Mitte", ]
s6 =landkreisData[landkreisData$Stadt == "SK Berlin Neukölln", ]
s7 =landkreisData[landkreisData$Stadt == "SK Berlin Pankow", ]
s8 =landkreisData[landkreisData$Stadt == "SK Berlin Reinickendorf", ]
s9 =landkreisData[landkreisData$Stadt == "SK Berlin Spandau", ]
s10 =landkreisData[landkreisData$Stadt == "SK Berlin Steglitz-Zehlendorf", ]
s11 =landkreisData[landkreisData$Stadt == "SK Berlin Tempelhof-Schöneberg", ]
s12 =landkreisData[landkreisData$Stadt == "SK Berlin Treptow-Köpenick", ]

inzidenz_kommunal1 = as.numeric(sub(",", ".", s1, fixed = TRUE))
inzidenz_kommunal2 = as.numeric(sub(",", ".", s2, fixed = TRUE))
inzidenz_kommunal3 = as.numeric(sub(",", ".", s3, fixed = TRUE))
inzidenz_kommunal4 = as.numeric(sub(",", ".", s4, fixed = TRUE))
inzidenz_kommunal5 = as.numeric(sub(",", ".", s5, fixed = TRUE))
inzidenz_kommunal6 = as.numeric(sub(",", ".", s6, fixed = TRUE))
inzidenz_kommunal7 = as.numeric(sub(",", ".", s7, fixed = TRUE))
inzidenz_kommunal8 = as.numeric(sub(",", ".", s8, fixed = TRUE))
inzidenz_kommunal9 = as.numeric(sub(",", ".", s9, fixed = TRUE))
inzidenz_kommunal10 = as.numeric(sub(",", ".", s10, fixed = TRUE))
inzidenz_kommunal11 = as.numeric(sub(",", ".", s11, fixed = TRUE))
inzidenz_kommunal12 = as.numeric(sub(",", ".", s12, fixed = TRUE))

inzidenz_kommunal = c(1:length(inzidenz_kommunal1))
for(i in 1:length(inzidenz_kommunal1)){
  
  inzidenz_kommunal[i] = round(mean (c(inzidenz_kommunal1[i],inzidenz_kommunal2[i],inzidenz_kommunal3[i],inzidenz_kommunal4[i],
                                       inzidenz_kommunal5[i],inzidenz_kommunal6[i],inzidenz_kommunal7[i],inzidenz_kommunal8[i],
                                       inzidenz_kommunal9[i],inzidenz_kommunal10[i],inzidenz_kommunal11[i],inzidenz_kommunal12[i])))
  
}
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

berlin = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
berlin <- data.frame(t(berlin[-1]))
names(berlin)[1] = "Timestamp"
names(berlin)[2] = "Inzidenz_Stadt2"
berlin$Town = "Berlin"

#Bochum

s =landkreisData[landkreisData$Stadt == "SK Bochum", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bochum = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bochum <- data.frame(t(bochum[-1]))
names(bochum)[1] = "Timestamp"
names(bochum)[2] = "Inzidenz_Stadt2"
bochum$Town = "Bochum"

#Bonn

s =landkreisData[landkreisData$Stadt == "SK Bonn", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bonn = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bonn <- data.frame(t(bonn[-1]))
names(bonn)[1] = "Timestamp"
names(bonn)[2] = "Inzidenz_Stadt2"
bonn$Town = "Bonn"

#Bremen

s =landkreisData[landkreisData$Stadt == "SK Bremen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

bremen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
bremen <- data.frame(t(bremen[-1]))
names(bremen)[1] = "Timestamp"
names(bremen)[2] = "Inzidenz_Stadt2"
bremen$Town = "Bremen"

#Darmstadt

s =landkreisData[landkreisData$Stadt == "SK Darmstadt", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

darmstadt = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
darmstadt <- data.frame(t(darmstadt[-1]))
names(darmstadt)[1] = "Timestamp"
names(darmstadt)[2] = "Inzidenz_Stadt2"
darmstadt$Town = "Darmstadt"

#Düsseldorf

s =landkreisData[landkreisData$Stadt == "SK Düsseldorf", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

dusseldorf = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
dusseldorf <- data.frame(t(dusseldorf[-1]))
names(dusseldorf)[1] = "Timestamp"
names(dusseldorf)[2] = "Inzidenz_Stadt2"
dusseldorf$Town = "Düsseldorf"

#Erfurt

s =landkreisData[landkreisData$Stadt == "SK Erfurt", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

erfurt = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
erfurt <- data.frame(t(erfurt[-1]))
names(erfurt)[1] = "Timestamp"
names(erfurt)[2] = "Inzidenz_Stadt2"
erfurt$Town = "Erfurt"

#Freiburg

s =landkreisData[landkreisData$Stadt == "SK Freiburg i.Breisgau", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

freiburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
freiburg <- data.frame(t(freiburg[-1]))
names(freiburg)[1] = "Timestamp"
names(freiburg)[2] = "Inzidenz_Stadt2"
freiburg$Town = "Freiburg"

#Hamburg

s =landkreisData[landkreisData$Stadt == "SK Hamburg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

hamburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
hamburg <- data.frame(t(hamburg[-1]))
names(hamburg)[1] = "Timestamp"
names(hamburg)[2] = "Inzidenz_Stadt2"
hamburg$Town = "Hamburg"

#Heidelberg

s =landkreisData[landkreisData$Stadt == "SK Heidelberg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

heidelberg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
heidelberg <- data.frame(t(heidelberg[-1]))
names(heidelberg)[1] = "Timestamp"
names(heidelberg)[2] = "Inzidenz_Stadt2"
heidelberg$Town = "Heidelberg"

#Konstanz

s =landkreisData[landkreisData$Stadt == "LK Konstanz", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

konstanz = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
konstanz <- data.frame(t(konstanz[-1]))
names(konstanz)[1] = "Timestamp"
names(konstanz)[2] = "Inzidenz_Stadt2"
konstanz$Town = "Konstanz"

#Leipzig

s =landkreisData[landkreisData$Stadt == "SK Leipzig", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

leipzig = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
leipzig <- data.frame(t(leipzig[-1]))
names(leipzig)[1] = "Timestamp"
names(leipzig)[2] = "Inzidenz_Stadt2"
leipzig$Town = "Leipzig"

#Lörrach

s =landkreisData[landkreisData$Stadt == "LK Lörrach", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

lorrach = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
lorrach <- data.frame(t(lorrach[-1]))
names(lorrach)[1] = "Timestamp"
names(lorrach)[2] = "Inzidenz_Stadt2"
lorrach$Town = "Lörrach"

#Ludwigsburg

s =landkreisData[landkreisData$Stadt == "LK Ludwigsburg", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

ludwigsburg = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
ludwigsburg <- data.frame(t(ludwigsburg[-1]))
names(ludwigsburg)[1] = "Timestamp"
names(ludwigsburg)[2] = "Inzidenz_Stadt2"
ludwigsburg$Town = "Ludwigsburg"

#Mannheim

s =landkreisData[landkreisData$Stadt == "SK Mannheim", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

mannheim = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
mannheim <- data.frame(t(mannheim[-1]))
names(mannheim)[1] = "Timestamp"
names(mannheim)[2] = "Inzidenz_Stadt2"
mannheim$Town = "Mannheim"

#München

s =landkreisData[landkreisData$Stadt == "SK München", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

munchen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
munchen <- data.frame(t(munchen[-1]))
names(munchen)[1] = "Timestamp"
names(munchen)[2] = "Inzidenz_Stadt2"
munchen$Town = "München"

#Münster

s =landkreisData[landkreisData$Stadt == "SK Münster", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

munster = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
munster <- data.frame(t(munster[-1]))
names(munster)[1] = "Timestamp"
names(munster)[2] = "Inzidenz_Stadt2"
munster$Town = "Münster"

#Rostock

s =landkreisData[landkreisData$Stadt == "SK Rostock", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

rostock = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
rostock <- data.frame(t(rostock[-1]))
names(rostock)[1] = "Timestamp"
names(rostock)[2] = "Inzidenz_Stadt2"
rostock$Town = "Rostock"

#Siegen

s =landkreisData[landkreisData$Stadt == "LK Siegen-Wittgenstein", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

siegen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
siegen <- data.frame(t(siegen[-1]))
names(siegen)[1] = "Timestamp"
names(siegen)[2] = "Inzidenz_Stadt2"
siegen$Town = "Siegen"

#Tübingen

s =landkreisData[landkreisData$Stadt == "LK Tübingen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

tubingen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
tubingen <- data.frame(t(tubingen[-1]))
names(tubingen)[1] = "Timestamp"
names(tubingen)[2] = "Inzidenz_Stadt2"
tubingen$Town = "Tübingen"

#Ulm

s =landkreisData[landkreisData$Stadt == "SK Ulm", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

ulm = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
ulm <- data.frame(t(ulm[-1]))
names(ulm)[1] = "Timestamp"
names(ulm)[2] = "Inzidenz_Stadt2"
ulm$Town = "Ulm"

#Oberhausen

s =landkreisData[landkreisData$Stadt == "SK Oberhausen", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

oberhausen = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
oberhausen <- data.frame(t(oberhausen[-1]))
names(oberhausen)[1] = "Timestamp"
names(oberhausen)[2] = "Inzidenz_Stadt2"
oberhausen$Town = "Oberhausen"

landkreisData2 = as.data.frame(rbind(berlin,bochum))
landkreisData2 = as.data.frame(rbind(landkreisData2,bonn))
landkreisData2 = as.data.frame(rbind(landkreisData2,bremen))
landkreisData2 = as.data.frame(rbind(landkreisData2,darmstadt))
landkreisData2 = as.data.frame(rbind(landkreisData2,dusseldorf))
landkreisData2 = as.data.frame(rbind(landkreisData2,erfurt))
landkreisData2 = as.data.frame(rbind(landkreisData2,freiburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,hamburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,heidelberg))
landkreisData2 = as.data.frame(rbind(landkreisData2,konstanz))
landkreisData2 = as.data.frame(rbind(landkreisData2,leipzig))
landkreisData2 = as.data.frame(rbind(landkreisData2,lorrach))
landkreisData2 = as.data.frame(rbind(landkreisData2,ludwigsburg))
landkreisData2 = as.data.frame(rbind(landkreisData2,mannheim))
landkreisData2 = as.data.frame(rbind(landkreisData2,munchen))
landkreisData2 = as.data.frame(rbind(landkreisData2,munster))
landkreisData2 = as.data.frame(rbind(landkreisData2,rostock))
landkreisData2 = as.data.frame(rbind(landkreisData2,siegen))
landkreisData2 = as.data.frame(rbind(landkreisData2,tubingen))
landkreisData2 = as.data.frame(rbind(landkreisData2,ulm))
landkreisData2 = as.data.frame(rbind(landkreisData2,oberhausen))

landkreisData2$Timestamp = as.POSIXlt(landkreisData2$Timestamp,format="%d.%m.%Y")
landkreisData2$Year = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%Y"))
landkreisData2$Months = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%m"))
landkreisData2$Day = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%d"))
landkreisData2$Timestamp = NULL

names(landkreisData2)

BikeData = merge(x = BikeData,y = landkreisData2,
                 by = c("Year","Months","Day","Town"),
                 all = TRUE)

BikeData$Inzidenz_Stadt2[is.na(BikeData$Inzidenz_Stadt2)] <- 0
BikeData = na.omit(BikeData)

BikeData$Inzidenz_Stadt2 = as.numeric(BikeData$Inzidenz_Stadt2)
summary(BikeData$Inzidenz_Stadt2)
summary(BikeData$Inzidenz_Stadt)
summary(BikeData$inzidenz_Bundesland)

rm(list=setdiff(ls(), c("BikeData")))

BikeData$CorInz = BikeData$inzidenz_Bundesland + BikeData$Inzidenz_Stadt + BikeData$Inzidenz_Stadt2
BikeData$inzidenz_Bundesland = NULL
BikeData$Inzidenz_Stadt = NULL
BikeData$Inzidenz_Stadt2 = NULL
BikeData$CorNull = NULL

summary(BikeData$CorInz)

summary(BikeData)








