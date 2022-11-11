#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Bonn

library(lubridate)
library(dplyr)
library(plyr)
library(geosphere)#package for calculating distance using longitude and latitude

#Clean up memory
rm(list=ls())

#Target storage location (inside the GitHub Repository)
#C:\Users\MaxWe\Documents\GitHub\Masterthesis_BikeTrafficForecast\data preparation

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
#D:\STUDIUM\Münster\7. Semester\Masterarbeit Daten\Bochum
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Bonn")

#Read Bycicle Counting Data----------------------------------------------
  countingData_KennedybrueckeNordseite_2018 = read.csv(file = "501_2018BNKennedybrückeNordseite.csv",sep=";")
  countingData_KennedybrückeSüdseite_2018 = read.csv(file = "502_2018BNKennedybrückeSüdseite.csv",sep=";")
  countingData_NordbrückeSüdseite_2018 = read.csv(file = "503_2018BNNordbrückeSüdseite.csv",sep=";")
  countingData_NordbrückeNordseite_2018 = read.csv(file = "504_2018BNNordbrückeNordseite.csv",sep=";")
  countingData_SüdbrückeSüdseite_2018 = read.csv(file = "505_2018BNSüdbrückeSüdseite.csv",sep=";")
  countingData_SüdbrückeNordseite_2018 = read.csv(file = "506_2018BNSüdbrückeNordseite.csv",sep=";")
  countingData_Estermannufer_2018 = read.csv(file = "507_2018BNEstermannufer.csv",sep=";")
  countingData_VonSandtufer_2018 = read.csv(file = "508_2018BNVonSandtufer.csv",sep=";")
  countingData_Rhenusallee_2018 = read.csv(file = "509_2018BNRhenusallee.csv",sep=";")
  countingData_Bröhltalweg_2018 = read.csv(file = "510_2018BNBröhltalweg.csv",sep=";")
  countingData_BrühlerStraße_2018 = read.csv(file = "511_2018BNBrühlerStraße.csv",sep=";")
  countingData_StraßburgerWeg_2018 = read.csv(file = "512_2018BNStraßburgerWeg.csv",sep=";")
  countingData_WilhelmSpiritusUfer_2018 = read.csv(file = "513_2018BNWilhelmSpiritusUfer.csv",sep=";")
  countingData_McCloyWeg_2018 = read.csv(file = "514_2018BNMcCloyWeg.csv",sep=";")
  countingData_WegDammBonnBeuel_2018 = read.csv(file = "515_2018BNWegDammBonnBeuel.csv",sep=";")
  
  countingData_Bonn_2019 = read.csv(file = "ErgebnisseFahrradmessstellenBonn2019.csv",sep=";")
  countingData_Bonn_2020_1 = read.csv(file = "fahrradzaehlungenbonnjanuarbismai2020.csv",sep=";")
  countingData_Bonn_2020_2 = read.csv(file = "fahrradzaehlungenbonnjulibisdezember2020_1.csv",sep=";")

  #Change count frequency to hourly data----------------------------------------------
  
  names(countingData_KennedybrueckeNordseite_2018)
  countingData_KennedybrueckeNordseite_2018$Uhrzeit=paste(countingData_KennedybrueckeNordseite_2018$Datum,countingData_KennedybrueckeNordseite_2018$X, sep=" ")
  countingData_KennedybrueckeNordseite_2018$Uhrzeit=cut(strptime(countingData_KennedybrueckeNordseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S01KennedyBR_N_18=ddply(countingData_KennedybrueckeNordseite_2018,.(Uhrzeit),summarize,RichtungA=sum(X5.01.BN...KennedybrÃ.cke..Nordseite..Radfahrer.Ri..Innenstadt),
                         RichtungB=sum(X5.01.BN...KennedybrÃ.cke..Nordseite..Radfahrer.Ri..Beuel))
  
  #This table didnt include a column for the summed up directions, so i have to calculate this
  
  S01KennedyBR_N_18=as.data.frame(cbind(S01KennedyBR_N_18[,'Uhrzeit',drop = FALSE],as.numeric(S01KennedyBR_N_18$RichtungA)+as.numeric(S01KennedyBR_N_18$RichtungB)))
  names(S01KennedyBR_N_18)[2]="Value"
  
  names(countingData_KennedybrückeSüdseite_2018)
  countingData_KennedybrückeSüdseite_2018$Uhrzeit=paste(countingData_KennedybrückeSüdseite_2018$Datum,countingData_KennedybrückeSüdseite_2018$X, sep=" ")
  countingData_KennedybrückeSüdseite_2018$Uhrzeit=cut(strptime(countingData_KennedybrückeSüdseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S02KennedyBR_S_18=ddply(countingData_KennedybrückeSüdseite_2018,.(Uhrzeit),summarize,Value=sum(X5.02.BN...KennedybrÃ.cke..SÃ.dseite..Barometer))
  
  names(countingData_NordbrückeSüdseite_2018)
  countingData_NordbrückeSüdseite_2018$Uhrzeit=paste(countingData_NordbrückeSüdseite_2018$Datum,countingData_NordbrückeSüdseite_2018$X, sep=" ")
  countingData_NordbrückeSüdseite_2018$Uhrzeit=cut(strptime(countingData_NordbrückeSüdseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S03NordBR_S_18=ddply(countingData_NordbrückeSüdseite_2018,.(Uhrzeit),summarize,Value=sum(X5.03.BN...NordbrÃ.cke..SÃ.dseite.))
  
  names(countingData_NordbrückeNordseite_2018)
  countingData_NordbrückeNordseite_2018$Uhrzeit=paste(countingData_NordbrückeNordseite_2018$Datum,countingData_NordbrückeNordseite_2018$X, sep=" ")
  countingData_NordbrückeNordseite_2018$Uhrzeit=cut(strptime(countingData_NordbrückeNordseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S04NordBR_N_18=ddply(countingData_NordbrückeNordseite_2018,.(Uhrzeit),summarize,Value=sum(X5.04.BN...NordbrÃ.cke..Nordseite.))
  
  names(countingData_SüdbrückeSüdseite_2018)
  countingData_SüdbrückeSüdseite_2018$Uhrzeit=paste(countingData_SüdbrückeSüdseite_2018$Datum,countingData_SüdbrückeSüdseite_2018$X, sep=" ")
  countingData_SüdbrückeSüdseite_2018$Uhrzeit=cut(strptime(countingData_SüdbrückeSüdseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S05SuedBR_S_18=ddply(countingData_SüdbrückeSüdseite_2018,.(Uhrzeit),summarize,Value=sum(X5.05.BN...SÃ.dbrÃ.cke..SÃ.dseite.))
  
  names(countingData_SüdbrückeNordseite_2018)
  countingData_SüdbrückeNordseite_2018$Uhrzeit=paste(countingData_SüdbrückeNordseite_2018$Datum,countingData_SüdbrückeNordseite_2018$X, sep=" ")
  countingData_SüdbrückeNordseite_2018$Uhrzeit=cut(strptime(countingData_SüdbrückeNordseite_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S06SuedBR_N_18=ddply(countingData_SüdbrückeNordseite_2018,.(Uhrzeit),summarize,Value=sum(X5.06.BN...SÃ.dbrÃ.cke..Nordseite.))
  
  names(countingData_Estermannufer_2018)
  countingData_Estermannufer_2018$Uhrzeit=paste(countingData_Estermannufer_2018$Datum,countingData_Estermannufer_2018$X, sep=" ")
  countingData_Estermannufer_2018$Uhrzeit=cut(strptime(countingData_Estermannufer_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S07Estermannufer_18=ddply(countingData_Estermannufer_2018,.(Uhrzeit),summarize,Value=sum(X5.07.BN...Estermannufer))
  
  names(countingData_VonSandtufer_2018)
  countingData_VonSandtufer_2018$Uhrzeit=paste(countingData_VonSandtufer_2018$Datum,countingData_VonSandtufer_2018$X, sep=" ")
  countingData_VonSandtufer_2018$Uhrzeit=cut(strptime(countingData_VonSandtufer_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S08VonSandtufer_18=ddply(countingData_VonSandtufer_2018,.(Uhrzeit),summarize,Value=sum(X5.08.BN...Von.Sandt.Ufer))
  
  names(countingData_Rhenusallee_2018)
  countingData_Rhenusallee_2018$Uhrzeit=paste(countingData_Rhenusallee_2018$Datum,countingData_Rhenusallee_2018$X, sep=" ")
  countingData_Rhenusallee_2018$Uhrzeit=cut(strptime(countingData_Rhenusallee_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S09Rhenusallee_18=ddply(countingData_Rhenusallee_2018,.(Uhrzeit),summarize,Value=sum(X5.09.BN...Rhenusallee))
  
  names(countingData_Bröhltalweg_2018)
  countingData_Bröhltalweg_2018$Uhrzeit=paste(countingData_Bröhltalweg_2018$Datum,countingData_Bröhltalweg_2018$X, sep=" ")
  countingData_Bröhltalweg_2018$Uhrzeit=cut(strptime(countingData_Bröhltalweg_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S10Broehltalweg_18=ddply(countingData_Bröhltalweg_2018,.(Uhrzeit),summarize,Value=sum(X5.10.BN...BrÃ.hltalweg))
  
  names(countingData_BrühlerStraße_2018)
  countingData_BrühlerStraße_2018$Uhrzeit=paste(countingData_BrühlerStraße_2018$Datum,countingData_BrühlerStraße_2018$X, sep=" ")
  countingData_BrühlerStraße_2018$Uhrzeit=cut(strptime(countingData_BrühlerStraße_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S11BruehlerStr_18=ddply(countingData_BrühlerStraße_2018,.(Uhrzeit),summarize,Value=sum(X5.11.BN...BrÃ.hler.StraÃYe))
  
  names(countingData_StraßburgerWeg_2018)
  countingData_StraßburgerWeg_2018$Uhrzeit=paste(countingData_StraßburgerWeg_2018$Datum,countingData_StraßburgerWeg_2018$X, sep=" ")
  countingData_StraßburgerWeg_2018$Uhrzeit=cut(strptime(countingData_StraßburgerWeg_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S12StrassburgerWeg_18=ddply(countingData_StraßburgerWeg_2018,.(Uhrzeit),summarize,Value=sum(X5.12.BN...StraÃYburger.Weg))
  
  names(countingData_WilhelmSpiritusUfer_2018)
  countingData_WilhelmSpiritusUfer_2018$Uhrzeit=paste(countingData_WilhelmSpiritusUfer_2018$Datum,countingData_WilhelmSpiritusUfer_2018$X, sep=" ")
  countingData_WilhelmSpiritusUfer_2018$Uhrzeit=cut(strptime(countingData_WilhelmSpiritusUfer_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S13WilhelmSpiritusUfer_18=ddply(countingData_WilhelmSpiritusUfer_2018,.(Uhrzeit),summarize,Value=sum(X5.13.BN...Wilhelm.Spiritus.Ufer))
  
  names(countingData_McCloyWeg_2018)
  countingData_McCloyWeg_2018$Uhrzeit=paste(countingData_McCloyWeg_2018$Datum,countingData_McCloyWeg_2018$X, sep=" ")
  countingData_McCloyWeg_2018$Uhrzeit=cut(strptime(countingData_McCloyWeg_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S14McCloyWeg_18=ddply(countingData_McCloyWeg_2018,.(Uhrzeit),summarize,Value=sum(X5.14.BN...Mc.Cloy.Weg))
  
  names(countingData_WegDammBonnBeuel_2018)
  countingData_WegDammBonnBeuel_2018$Uhrzeit=paste(countingData_WegDammBonnBeuel_2018$Datum,countingData_WegDammBonnBeuel_2018$X, sep=" ")
  countingData_WegDammBonnBeuel_2018$Uhrzeit=cut(strptime(countingData_WegDammBonnBeuel_2018$Uhrzeit,"%d/%m/%Y %H:%M"),"hour")
  S15WegDammBonnBeuel_18=ddply(countingData_WegDammBonnBeuel_2018,.(Uhrzeit),summarize,Value=sum(X5.15.BN...Weg.auf.Damm.Neil))
  
  names(countingData_Bonn_2019)
  countingData_Bonn_2019$Uhrzeit=cut(strptime(countingData_Bonn_2019$Time,"%d.%m.%Y %H:%M"),"hour")
  countingData_Bonn_2019=ddply(countingData_Bonn_2019,.(Uhrzeit),summarize,
                        S01KennedyBR_N=sum(X5.01.BN...KennedybrYcke..Nordseite.),
                        S02KennedyBR_S=sum(X5.02.BN...KennedybrYcke..SYdseite..Barometer),
                        S03NordBR_S=sum(X5.03.BN...NordbrYcke..SYdseite.),
                        S04NordBR_N=sum(X5.04.BN...NordbrYcke..Nordseite.),
                        S05SuedBR_S=sum(X5.05.BN...SYdbrYcke..SYdseite.),
                        S06SuedBR_N=sum(X5.06.BN...SYdbrYcke..Nordseite.),
                        S07Estermannufer=sum(X5.07.BN...Estermannufer),
                        S08VonSandtufer=sum(X5.08.BN...Von.Sandt.Ufer),
                        S09Rhenusallee=sum(X5.09.BN...Rhenusallee),
                        S10Broehltalweg=sum(X5.10.BN...Brshltalweg),
                        S11BruehlerStr=sum(X5.11.BN...BrYhler.Stra.e),
                        S13WilhelmSpiritusUfer=sum(X5.13.BN...Wilhelm.Spiritus.Ufer),
                        S14McCloyWeg=sum(X5.14.BN...Mc.Cloy.Weg),
                        S15WegDammBonnBeuel=sum(X5.15.BN...Weg.auf.Damm.Neil))  
  
  names(countingData_Bonn_2020_1)
  countingData_Bonn_2020_1$Uhrzeit=cut(strptime(countingData_Bonn_2020_1$Time,"%d.%m.%Y %H:%M"),"hour")
  countingData_Bonn_2020_1=ddply(countingData_Bonn_2020_1,.(Uhrzeit),summarize,
                               S01KennedyBR_N=sum(X5.01.BN...KennedybrÃ.cke..Nordseite.),
                               S02KennedyBR_S=sum(X5.02.BN...KennedybrÃ.cke..SÃ.dseite..Barometer),
                               S03NordBR_S=sum(X5.03.BN...NordbrÃ.cke..SÃ.dseite.),
                               S04NordBR_N=sum(X5.04.BN...NordbrÃ.cke..Nordseite.),
                               S05SuedBR_S=sum(X5.05.BN...SÃ.dbrÃ.cke..SÃ.dseite.),
                               S06SuedBR_N=sum(X5.06.BN...SÃ.dbrÃ.cke..Nordseite.),
                               S07Estermannufer=sum(X5.07.BN...Estermannufer),
                               S08VonSandtufer=sum(X5.08.BN...Von.Sandt.Ufer),
                               S09Rhenusallee=sum(X5.09.BN...Rhenusallee),
                               S10Broehltalweg=sum(X5.10.BN...BrÃ.hltalweg),
                               S11BruehlerStr=sum(X5.11.BN...BrÃ.hler.StraÃYe),
                               S13WilhelmSpiritusUfer=sum(X5.13.BN...Wilhelm.Spiritus.Ufer),
                               S14McCloyWeg=sum(X5.14.BN...Mc.Cloy.Weg),
                               S15WegDammBonnBeuel=sum(X5.15.BN...Weg.auf.Damm.Neil))  
  
  names(countingData_Bonn_2020_2)
  countingData_Bonn_2020_2$Uhrzeit=cut(strptime(countingData_Bonn_2020_2$Time,"%d.%m.%Y %H:%M"),"hour")
  countingData_Bonn_2020_2=ddply(countingData_Bonn_2020_2,.(Uhrzeit),summarize,
                                 S01KennedyBR_N=sum(X5.01.BN...KennedybrÃ.cke..Nordseite.),
                                 S02KennedyBR_S=sum(X5.02.BN...KennedybrÃ.cke..SÃ.dseite..Barometer),
                                 S03NordBR_S=sum(X5.03.BN...NordbrÃ.cke..SÃ.dseite.),
                                 S04NordBR_N=sum(X5.04.BN...NordbrÃ.cke..Nordseite.),
                                 S07Estermannufer=sum(X5.07.BN...Estermannufer),
                                 S08VonSandtufer=sum(X5.08.BN...Von.Sandt.Ufer),
                                 S09Rhenusallee=sum(X5.09.BN...Rhenusallee),
                                 S10Broehltalweg=sum(X5.10.BN...BrÃ.hltalweg),
                                 S11BruehlerStr=sum(X5.11.BN...BrÃ.hler.StraÃYe),
                                 S13WilhelmSpiritusUfer=sum(X5.13.BN...Wilhelm.Spiritus.Ufer),
                                 S14McCloyWeg=sum(X5.14.BN...Mc.Cloy.Weg),
                                 S15WegDammBonnBeuel=sum(X5.15.BN...Weg.auf.Damm.Neil))
  
#Divide raw data per stations----------------------------------------------

  S01KennedyBR_N_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S01KennedyBR_N))
  S01KennedyBR_N_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S01KennedyBR_N)) 
  S01KennedyBR_N_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S01KennedyBR_N)) 
  
  S02KennedyBR_S_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S02KennedyBR_S))
  S02KennedyBR_S_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S02KennedyBR_S)) 
  S02KennedyBR_S_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S02KennedyBR_S)) 
  
  S03NordBR_S_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S03NordBR_S))
  S03NordBR_S_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S03NordBR_S)) 
  S03NordBR_S_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S03NordBR_S)) 
  
  S04NordBR_N_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S04NordBR_N))
  S04NordBR_N_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S04NordBR_N)) 
  S04NordBR_N_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S04NordBR_N)) 
  
  S05SuedBR_S_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S05SuedBR_S))
  S05SuedBR_S_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S05SuedBR_S))
  
  S06SuedBR_N_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S06SuedBR_N))
  S06SuedBR_N_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S06SuedBR_N))
  
  S07Estermannufer_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S07Estermannufer))
  S07Estermannufer_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S07Estermannufer)) 
  S07Estermannufer_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S07Estermannufer)) 
  
  S08VonSandtufer_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S08VonSandtufer))
  S08VonSandtufer_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S08VonSandtufer)) 
  S08VonSandtufer_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S08VonSandtufer)) 
  
  S09Rhenusallee_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S09Rhenusallee))
  S09Rhenusallee_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S09Rhenusallee)) 
  S09Rhenusallee_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S09Rhenusallee)) 
  
  S10Broehltalweg_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S10Broehltalweg))
  S10Broehltalweg_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S10Broehltalweg)) 
  S10Broehltalweg_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S10Broehltalweg)) 
  
  S11BruehlerStr_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S11BruehlerStr))
  S11BruehlerStr_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S11BruehlerStr)) 
  S11BruehlerStr_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S11BruehlerStr)) 
  
  S13WilhelmSpiritusUfer_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S13WilhelmSpiritusUfer))
  S13WilhelmSpiritusUfer_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S13WilhelmSpiritusUfer)) 
  S13WilhelmSpiritusUfer_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S13WilhelmSpiritusUfer)) 
  
  S14McCloyWeg_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S14McCloyWeg))
  S14McCloyWeg_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S14McCloyWeg)) 
  S14McCloyWeg_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S14McCloyWeg)) 
  
  S15WegDammBonnBeuel_19=as.data.frame(cbind(countingData_Bonn_2019[,'Uhrzeit',drop = FALSE],countingData_Bonn_2019$S15WegDammBonnBeuel))
  S15WegDammBonnBeuel_20_1=as.data.frame(cbind(countingData_Bonn_2020_1[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_1$S15WegDammBonnBeuel)) 
  S15WegDammBonnBeuel_20_2=as.data.frame(cbind(countingData_Bonn_2020_2[,'Uhrzeit',drop = FALSE],countingData_Bonn_2020_2$S15WegDammBonnBeuel)) 
  
#Rename Columns----------------------------------------------
  
  names(S01KennedyBR_N_18)[2]="Value"
  names(S01KennedyBR_N_19)[2]="Value"
  names(S01KennedyBR_N_20_1)[2]="Value"
  names(S01KennedyBR_N_20_2)[2]="Value"
  
  names(S02KennedyBR_S_18)[2]="Value"
  names(S02KennedyBR_S_19)[2]="Value"
  names(S02KennedyBR_S_20_1)[2]="Value"
  names(S02KennedyBR_S_20_2)[2]="Value"
  
  names(S03NordBR_S_18)[2]="Value"
  names(S03NordBR_S_19)[2]="Value"
  names(S03NordBR_S_20_1)[2]="Value"
  names(S03NordBR_S_20_2)[2]="Value"
  
  names(S04NordBR_N_18)[2]="Value"
  names(S04NordBR_N_19)[2]="Value"
  names(S04NordBR_N_20_1)[2]="Value"
  names(S04NordBR_N_20_2)[2]="Value"
  
  names(S05SuedBR_S_18)[2]="Value"
  names(S05SuedBR_S_19)[2]="Value"
  names(S05SuedBR_S_20_1)[2]="Value"
  
  names(S06SuedBR_N_18)[2]="Value"
  names(S06SuedBR_N_19)[2]="Value"
  names(S06SuedBR_N_20_1)[2]="Value"
  
  names(S07Estermannufer_18)[2]="Value"
  names(S07Estermannufer_19)[2]="Value"
  names(S07Estermannufer_20_1)[2]="Value"
  names(S07Estermannufer_20_2)[2]="Value"
  
  names(S08VonSandtufer_18)[2]="Value"
  names(S08VonSandtufer_19)[2]="Value"
  names(S08VonSandtufer_20_1)[2]="Value"
  names(S08VonSandtufer_20_2)[2]="Value"
  
  names(S09Rhenusallee_18)[2]="Value"
  names(S09Rhenusallee_19)[2]="Value"
  names(S09Rhenusallee_20_1)[2]="Value"
  names(S09Rhenusallee_20_2)[2]="Value"
  
  names(S10Broehltalweg_18)[2]="Value"
  names(S10Broehltalweg_19)[2]="Value"
  names(S10Broehltalweg_20_1)[2]="Value"
  names(S10Broehltalweg_20_2)[2]="Value"
  
  names(S11BruehlerStr_18)[2]="Value"
  names(S11BruehlerStr_19)[2]="Value"
  names(S11BruehlerStr_20_1)[2]="Value"
  names(S11BruehlerStr_20_2)[2]="Value"
  
  names(S12StrassburgerWeg_18)[2]="Value"
  
  names(S13WilhelmSpiritusUfer_18)[2]="Value"
  names(S13WilhelmSpiritusUfer_19)[2]="Value"
  names(S13WilhelmSpiritusUfer_20_1)[2]="Value"
  names(S13WilhelmSpiritusUfer_20_2)[2]="Value"
  
  names(S14McCloyWeg_18)[2]="Value"
  names(S14McCloyWeg_19)[2]="Value"
  names(S14McCloyWeg_20_1)[2]="Value"
  names(S14McCloyWeg_20_2)[2]="Value"  
  
  names(S15WegDammBonnBeuel_18)[2]="Value"
  names(S15WegDammBonnBeuel_19)[2]="Value"
  names(S15WegDammBonnBeuel_20_1)[2]="Value"
  names(S15WegDammBonnBeuel_20_2)[2]="Value"  
  
#Connect Years together----------------------------------------------
  
  S01KennedyBR_N=rbind(S01KennedyBR_N_18,S01KennedyBR_N_19)
  S01KennedyBR_N=rbind(S01KennedyBR_N,S01KennedyBR_N_20_1)
  S01KennedyBR_N=rbind(S01KennedyBR_N,S01KennedyBR_N_20_2)
  
  S02KennedyBR_S=rbind(S02KennedyBR_S_18,S02KennedyBR_S_19)
  S02KennedyBR_S=rbind(S02KennedyBR_S,S02KennedyBR_S_20_1)
  S02KennedyBR_S=rbind(S02KennedyBR_S,S02KennedyBR_S_20_2)
  
  S03NordBR_S=rbind(S03NordBR_S_18,S03NordBR_S_19)
  S03NordBR_S=rbind(S03NordBR_S,S03NordBR_S_20_1)
  S03NordBR_S=rbind(S03NordBR_S,S03NordBR_S_20_2)
  
  S04NordBR_N=rbind(S04NordBR_N_18,S04NordBR_N_19)
  S04NordBR_N=rbind(S04NordBR_N,S04NordBR_N_20_1)
  S04NordBR_N=rbind(S04NordBR_N,S04NordBR_N_20_2)
  
  S05SuedBR_S=rbind(S05SuedBR_S_18,S05SuedBR_S_19)
  S05SuedBR_S=rbind(S05SuedBR_S,S05SuedBR_S_20_1)
  
  S06SuedBR_N=rbind(S06SuedBR_N_18,S06SuedBR_N_19)
  S06SuedBR_N=rbind(S06SuedBR_N,S06SuedBR_N_20_1)
  
  S07Estermannufer=rbind(S07Estermannufer_18,S07Estermannufer_19)
  S07Estermannufer=rbind(S07Estermannufer,S07Estermannufer_20_1)
  S07Estermannufer=rbind(S07Estermannufer,S07Estermannufer_20_2)
  
  S08VonSandtufer=rbind(S08VonSandtufer_18,S08VonSandtufer_19)
  S08VonSandtufer=rbind(S08VonSandtufer,S08VonSandtufer_20_1)
  S08VonSandtufer=rbind(S08VonSandtufer,S08VonSandtufer_20_2)
  
  S09Rhenusallee=rbind(S09Rhenusallee_18,S09Rhenusallee_19)
  S09Rhenusallee=rbind(S09Rhenusallee,S09Rhenusallee_20_1)
  S09Rhenusallee=rbind(S09Rhenusallee,S09Rhenusallee_20_2)
  
  S10Broehltalweg=rbind(S10Broehltalweg_18,S10Broehltalweg_19)
  S10Broehltalweg=rbind(S10Broehltalweg,S10Broehltalweg_20_1)
  S10Broehltalweg=rbind(S10Broehltalweg,S10Broehltalweg_20_2)
  
  S11BruehlerStr=rbind(S11BruehlerStr_18,S11BruehlerStr_19)
  S11BruehlerStr=rbind(S11BruehlerStr,S11BruehlerStr_20_1)
  S11BruehlerStr=rbind(S11BruehlerStr,S11BruehlerStr_20_2)
  
  S12StrassburgerWeg=S12StrassburgerWeg_18
  
  S13WilhelmSpiritusUfer=rbind(S13WilhelmSpiritusUfer_18,S13WilhelmSpiritusUfer_19)
  S13WilhelmSpiritusUfer=rbind(S13WilhelmSpiritusUfer,S13WilhelmSpiritusUfer_20_1)
  S13WilhelmSpiritusUfer=rbind(S13WilhelmSpiritusUfer,S13WilhelmSpiritusUfer_20_2)
  
  S14McCloyWeg=rbind(S14McCloyWeg_18,S14McCloyWeg_19)
  S14McCloyWeg=rbind(S14McCloyWeg,S14McCloyWeg_20_1)
  S14McCloyWeg=rbind(S14McCloyWeg,S14McCloyWeg_20_2)
  
  S15WegDammBonnBeuel=rbind(S15WegDammBonnBeuel_18,S15WegDammBonnBeuel_19)
  S15WegDammBonnBeuel=rbind(S15WegDammBonnBeuel,S15WegDammBonnBeuel_20_1)
  S15WegDammBonnBeuel=rbind(S15WegDammBonnBeuel,S15WegDammBonnBeuel_20_2)
  
#Add Location Columns----------------------------------------------
  
  S01KennedyBR_N$Town = "Bonn"
  S02KennedyBR_S$Town = "Bonn"
  S03NordBR_S$Town = "Bonn"
  S04NordBR_N$Town = "Bonn"
  S05SuedBR_S$Town = "Bonn"
  S06SuedBR_N$Town = "Bonn"
  S07Estermannufer$Town = "Bonn"
  S08VonSandtufer$Town = "Bonn"
  S09Rhenusallee$Town = "Bonn"
  S10Broehltalweg$Town = "Bonn"
  S11BruehlerStr$Town = "Bonn"
  S12StrassburgerWeg$Town = "Bonn"
  S13WilhelmSpiritusUfer$Town = "Bonn"
  S14McCloyWeg$Town = "Bonn"
  S15WegDammBonnBeuel$Town = "Bonn"
  
  
  S01KennedyBR_N$Station = "KennedyBR_N"
  S02KennedyBR_S$Station = "KennedyBR_S"
  S03NordBR_S$Station = "NordBR_S"
  S04NordBR_N$Station = "NordBR_N"
  S05SuedBR_S$Station = "SuedBR_S"
  S06SuedBR_N$Station = "SuedBR_N"
  S07Estermannufer$Station = "Estermannufer"
  S08VonSandtufer$Station = "VonSandtufer"
  S09Rhenusallee$Station = "Rhenusallee"
  S10Broehltalweg$Station = "Broehltalweg"
  S11BruehlerStr$Station = "BruehlerStr"
  S12StrassburgerWeg$Station = "StrassburgerWeg"
  S13WilhelmSpiritusUfer$Station = "WilhelmSpiritusUfer"
  S14McCloyWeg$Station = "McCloyWeg"
  S15WegDammBonnBeuel$Station = "WegDammBonnBeuel"
  
  S01KennedyBR_N$Lon = 7.113214410281800
  S02KennedyBR_S$Lon = 7.10662675446168
  S03NordBR_S$Lon = 7.0967883292648300
  S04NordBR_N$Lon = 7.101340951428840
  S05SuedBR_S$Lon = 7.142160722735420
  S06SuedBR_N$Lon = 7.144403347868700
  S07Estermannufer$Lon = 7.068537905249
  S08VonSandtufer$Lon = 7.166909
  S09Rhenusallee$Lon = 7.134297
  S10Broehltalweg$Lon = 7.113626
  S11BruehlerStr$Lon = 7.069422
  S12StrassburgerWeg$Lon = 7.1155871
  S13WilhelmSpiritusUfer$Lon = 7.1132844
  S14McCloyWeg$Lon = 7.1880155
  S15WegDammBonnBeuel$Lon = 7.11554
  
  S01KennedyBR_N$Lat = 50.73871331747280	
  S02KennedyBR_S$Lat = 50.7376321974817	
  S03NordBR_S$Lat = 50.75490457745970	
  S04NordBR_N$Lat = 50.75769827163260	
  S05SuedBR_S$Lat = 50.71546283304070	
  S06SuedBR_N$Lat = 50.719435998458900	
  S07Estermannufer$Lat = 50.7680717984213	
  S08VonSandtufer$Lat = 50.700893	
  S09Rhenusallee$Lat = 50.72213	
  S10Broehltalweg$Lat = 50.742508	
  S11BruehlerStr$Lat = 50.740383	
  S12StrassburgerWeg$Lat = 50.7171927	
  S13WilhelmSpiritusUfer$Lat = 50.7269498	
  S14McCloyWeg$Lat = 50.6709115	
  S15WegDammBonnBeuel$Lat = 50.73356	
  
  S01KennedyBR_N$Oneway = FALSE
  S02KennedyBR_S$Oneway = FALSE
  S03NordBR_S$Oneway = FALSE
  S04NordBR_N$Oneway = FALSE
  S05SuedBR_S$Oneway = FALSE
  S06SuedBR_N$Oneway = FALSE
  S07Estermannufer$Oneway = FALSE
  S08VonSandtufer$Oneway = FALSE
  S09Rhenusallee$Oneway = FALSE
  S10Broehltalweg$Oneway = FALSE
  S11BruehlerStr$Oneway = FALSE
  S12StrassburgerWeg$Oneway = FALSE
  S13WilhelmSpiritusUfer$Oneway = FALSE
  S14McCloyWeg$Oneway = FALSE
  S15WegDammBonnBeuel$Oneway = FALSE
  
  #S01KennedyBR_N$Road_type = "Bridge"
  #S02KennedyBR_S$Road_type = "Bridge"
  #S03NordBR_S$Road_type = "Bridge"
  #S04NordBR_N$Road_type = "Bridge"
  #S05SuedBR_S$Road_type = "Bridge"
  #S06SuedBR_N$Road_type = "Bridge"
  #S07Estermannufer$Road_type = "Street"
  #S08VonSandtufer$Road_type = "Pathway"
  #S09Rhenusallee$Road_type = "Pathway"
  #S10Broehltalweg$Road_type = "Pathway"
  #S11BruehlerStr$Road_type = "Street"
  #S12StrassburgerWeg$Road_type = "Sreet"
  #S13WilhelmSpiritusUfer$Road_type = "Pathway"
  #S14McCloyWeg$Road_type = "Pathway"
  #S15WegDammBonnBeuel$Road_type = "Street"
  
#Summarize Directions----------------------------------------------
  
  S01KennedyBR=S01KennedyBR_N
  S01KennedyBR$Value = as.numeric(S01KennedyBR_N$Value) + as.numeric(S02KennedyBR_S$Value)
  S03NordBR=S03NordBR_S
  S03NordBR$Value = as.numeric(S03NordBR_S$Value) + as.numeric(S04NordBR_N$Value)
  S05SuedBR=S05SuedBR_S
  S05SuedBR$Value = as.numeric(S05SuedBR_S$Value) + as.numeric(S06SuedBR_N$Value)

#Connect the Stations----------------------------------------------
  
  rawData=rbind(S01KennedyBR,S03NordBR)
  rawData=rbind(rawData,S05SuedBR)
  rawData=rbind(rawData,S07Estermannufer)
  rawData=rbind(rawData,S08VonSandtufer)
  rawData=rbind(rawData,S09Rhenusallee)
  rawData=rbind(rawData,S10Broehltalweg)
  rawData=rbind(rawData,S11BruehlerStr)
  rawData=rbind(rawData,S12StrassburgerWeg)
  rawData=rbind(rawData,S13WilhelmSpiritusUfer)
  rawData=rbind(rawData,S14McCloyWeg)
  rawData=rbind(rawData,S15WegDammBonnBeuel)
  
  
  names(rawData)[1]="Timestamp"
  rawData=na.omit(rawData)
  rawData$Value=as.numeric(rawData$Value)
  summary(rawData)  
  
#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays
  
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
  
  pH=publicHolidays[publicHolidays$NRW %in% TRUE,]
  rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)
  
  #Load data for school holidays
  schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")
  
  sH=schoolHolidays[schoolHolidays$Bundesland %in% "NRW",]
  x <- vector()
  for(i in 1:length(sH$Startdatum)){
    x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
  }
  rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)
  
  summary(rawData)
  
#Add Weather Data (Source: Deutscher Wetterdienst)
  
  rm(publicHolidays)
  rm(schoolHolidays)
  rm(sH)
  rm(pH)
  rm(x)
  rm(i)
  rm(countingData_Bonn_2019)
  rm(countingData_Bonn_2020_1)
  rm(countingData_Bonn_2020_2)
  rm(countingData_Bröhltalweg_2018,countingData_BrühlerStraße_2018,countingData_Estermannufer_2018)
  rm(countingData_KennedybrückeSüdseite_2018,countingData_KennedybrueckeNordseite_2018,countingData_McCloyWeg_2018)
  rm(countingData_NordbrückeNordseite_2018,countingData_NordbrückeSüdseite_2018,countingData_Rhenusallee_2018)
  rm(countingData_StraßburgerWeg_2018,countingData_SüdbrückeNordseite_2018,countingData_SüdbrückeSüdseite_2018)
  rm(countingData_VonSandtufer_2018,countingData_WegDammBonnBeuel_2018,countingData_WilhelmSpiritusUfer_2018)
  rm(S01KennedyBR,S01KennedyBR_N,S01KennedyBR_N_18,S01KennedyBR_N_19,S01KennedyBR_N_20_1,S01KennedyBR_N_20_2)
  rm(S02KennedyBR_S,S02KennedyBR_S_18,S02KennedyBR_S_19,S02KennedyBR_S_20_1,S02KennedyBR_S_20_2)
  rm(S03NordBR,S03NordBR_S_18,S03NordBR_S_19,S03NordBR_S_20_1,S03NordBR_S_20_2)
  rm(S04NordBR_N,S04NordBR_N_18,S04NordBR_N_19,S04NordBR_N_20_1,S04NordBR_N_20_2)
  rm(S05SuedBR,S05SuedBR_S,S05SuedBR_S_18,S05SuedBR_S_19,S05SuedBR_S_20_1)
  rm(S06SuedBR_N,S06SuedBR_N_18,S06SuedBR_N_19,S06SuedBR_N_20_1)
  rm(S07Estermannufer,S07Estermannufer_18,S07Estermannufer_19
     ,S07Estermannufer_20_1,S07Estermannufer_20_2)
  rm(S08VonSandtufer,S08VonSandtufer_1,S08VonSandtufer_18,S08VonSandtufer_19,S08VonSandtufer_20_2)
  rm(S10Broehltalweg,S10Broehltalweg_18,S10Broehltalweg_19,S10Broehltalweg_20_1,S10Broehltalweg_20_2)
  rm(S11BruehlerStr,S11BruehlerStr_18,S11BruehlerStr_19,S11BruehlerStr_20_1,S11BruehlerStr_20_2)
  rm(S12StrassburgerWeg,S12StrassburgerWeg_18)
  rm(S13WilhelmSpiritusUfer,S13WilhelmSpiritusUfer_18,S13WilhelmSpiritusUfer_19,S13WilhelmSpiritusUfer_20_1,S13WilhelmSpiritusUfer_20_2)
  rm(S14McCloyWeg,S14McCloyWeg_18,S14McCloyWeg_19,S14McCloyWeg_20_1,S14McCloyWeg_20_2)
  rm(S15WegDammBonnBeuel,S15WegDammBonnBeuel_18,S15WegDammBonnBeuel_19,S15WegDammBonnBeuel_20_1,S15WegDammBonnBeuel_20_2)
  rm(S03NordBR_S,S09Rhenusallee,S09Rhenusallee_18,S09Rhenusallee_19,S09Rhenusallee_20_1,S09Rhenusallee_20_2)
  rm(countingData_Bröhltalweg_2018,countingData_BrühlerStraße_2018)
  rm(countingData_KennedybrückeSüdseite_2018,countingData_NordbrückeNordseite_2018,countingData_NordbrückeSüdseite_2018)
  rm(countingData_StraßburgerWeg_2018,countingData_SüdbrückeNordseite_2018,countingData_SüdbrückeSüdseite_2018)
  
  #Import Weather Data
  setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Bonn")
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
                  all = FALSE)
  
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
  write.csv(rawData,"Bonn.csv")
  
# Adding ADFC-Fahrradklima Values
  
  Year=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
  ADFC_Index=c(3.8,3.8,3.9,3.9,4.1,4.1,4.2,4.2,4.2,4.2)
  
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
  
  