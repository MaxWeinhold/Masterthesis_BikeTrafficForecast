#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Data preperation Bonn

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
  countingData_Bonn_2021 = read.csv(file = ".ergebnissefahrradzaehlungen2021csv",sep=";")

  #Change count frequency to hourly data----------------------------------------------