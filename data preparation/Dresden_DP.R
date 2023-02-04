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
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/Dresden")

#Read Bycicle Counting Data----------------------------------------------

files = c("2018/AlbertBruecke_20180101_0000_20180201_Std",
          "2018/AlbertBruecke_20180201_0000_20180301_Std",
          "2018/AlbertBruecke_20180301_0000_20180401_Std",
          "2018/AlbertBruecke_20180401_0000_20180501_Std",
          "2018/AlbertBruecke_20180501_0000_20180601_Std",
          "2018/AlbertBruecke_20180601_0000_20180701_Std",
          "2018/AlbertBruecke_20180701_0000_20180801_Std",
          "2018/AlbertBruecke_20180801_0000_20180901_Std",
          "2018/AlbertBruecke_20180901_0000_20181001_Std",
          "2018/AlbertBruecke_20181001_0000_20181101_Std",
          "2018/AlbertBruecke_20181101_0000_20181201_Std",
          "2018/AlbertBruecke_20181201_0000_20190101_Std",
          "2018/Elberadweg_20180101_0000_20180201_Std",
          "2018/Elberadweg_20180201_0000_20180301_Std",
          "2018/Elberadweg_20180301_0000_20180401_Std",
          "2018/Elberadweg_20180401_0000_20180501_Std",
          "2018/Elberadweg_20180501_0000_20180601_Std",
          "2018/Elberadweg_20180601_0000_20180701_Std",
          "2018/Elberadweg_20180701_0000_20180801_Std",
          "2018/Elberadweg_20180801_0000_20180901_Std",
          "2018/Elberadweg_20180901_0000_20181001_Std",
          "2018/Elberadweg_20181001_0000_20181101_Std",
          "2018/Elberadweg_20181101_0000_20181201_Std",
          "2018/Elberadweg_20181201_0000_20190101_Std",
          "2018/Elberadweg_Nord_20180101_0000_20180201_Std",
          "2018/Elberadweg_Nord_20180201_0000_20180301_Std",
          "2018/Elberadweg_Nord_20180301_0000_20180401_Std",
          "2018/Elberadweg_Nord_20180401_0000_20180501_Std",
          "2018/Elberadweg_Nord_20180501_0000_20180601_Std",
          "2018/Elberadweg_Nord_20180601_0000_20180701_Std",
          "2018/Elberadweg_Nord_20180701_0000_20180801_Std",
          "2018/Elberadweg_Nord_20180801_0000_20180901_Std",
          "2018/Elberadweg_Nord_20180901_0000_20181001_Std",
          "2018/Elberadweg_Nord_20181001_0000_20181101_Std",
          "2018/Elberadweg_Nord_20181101_0000_20181201_Std",
          "2018/Elberadweg_Nord_20181201_0000_20190101_Std",
          "2018/Kreuz_20180101_0000_20180201_Std",
          "2018/Kreuz_20180201_0000_20180301_Std",
          "2018/Kreuz_20180301_0000_20180401_Std",
          "2018/Kreuz_20180401_0000_20180501_Std",
          "2018/Kreuz_20180501_0000_20180601_Std",
          "2018/Kreuz_20180601_0000_20180701_Std",
          "2018/Kreuz_20180701_0000_20180801_Std",
          "2018/Kreuz_20180801_0000_20180901_Std",
          "2018/Kreuz_20180901_0000_20181001_Std",
          "2018/Kreuz_20181001_0000_20181101_Std",
          "2018/Kreuz_20181101_0000_20181201_Std",
          "2018/Kreuz_20181201_0000_20190101_Std",
          "2018/StPeter_20180101_0000_20180201_Std",
          "2018/StPeter_20180201_0000_20180301_Std",
          "2018/StPeter_20180301_0000_20180401_Std",
          "2018/StPeter_20180401_0000_20180501_Std",
          "2018/StPeter_20180501_0000_20180601_Std",
          "2018/StPeter_20180601_0000_20180701_Std",
          "2018/StPeter_20180701_0000_20180801_Std",
          "2018/StPeter_20180801_0000_20180901_Std",
          "2018/StPeter_20180901_0000_20181001_Std",
          "2018/StPeter_20181001_0000_20181101_Std",
          "2018/StPeter_20181101_0000_20181201_Std",
          "2018/StPeter_20181201_0000_20190101_Std",
          "2018/Tharandter_20180101_0000_20180201_Std",
          "2018/Tharandter_20180201_0000_20180301_Std",
          "2018/Tharandter_20180301_0000_20180401_Std",
          "2018/Tharandter_20180401_0000_20180501_Std",
          "2018/Tharandter_20180501_0000_20180601_Std",
          "2018/Tharandter_20180601_0000_20180701_Std",
          "2018/Tharandter_20180701_0000_20180801_Std",
          "2018/Tharandter_20180801_0000_20180901_Std",
          "2018/Tharandter_20180901_0000_20181001_Std",
          "2018/Tharandter_20181001_0000_20181101_Std",
          "2018/Tharandter_20181101_0000_20181201_Std",
          "2018/Tharandter_20181201_0000_20190101_Std",
          "2018/Waldschloss_20180101_0000_20180201_Std",
          "2018/Waldschloss_20180201_0000_20180301_Std",
          "2018/Waldschloss_20180301_0000_20180401_Std",
          "2018/Waldschloss_20180401_0000_20180501_Std",
          "2018/Waldschloss_20180501_0000_20180601_Std",
          "2018/Waldschloss_20180601_0000_20180701_Std",
          "2018/Waldschloss_20180701_0000_20180801_Std",
          "2018/Waldschloss_20180801_0000_20180901_Std",
          "2018/Waldschloss_20180901_0000_20181001_Std",
          "2018/Waldschloss_20181001_0000_20181101_Std",
          "2018/Waldschloss_20181101_0000_20181201_Std",
          "2018/Waldschloss_20181201_0000_20190101_Std",
          "2018/Winter_20180101_0000_20180201_Std",
          "2018/Winter_20180201_0000_20180301_Std",
          "2018/Winter_20180301_0000_20180401_Std",
          "2018/Winter_20180401_0000_20180501_Std",
          "2018/Winter_20180501_0000_20180601_Std",
          "2018/Winter_20180601_0000_20180701_Std",
          "2018/Winter_20180701_0000_20180801_Std",
          "2018/Winter_20180801_0000_20180901_Std",
          "2018/Winter_20180901_0000_20181001_std",
          "2018/Winter_20181001_0000_20181101_Std",
          "2018/Winter_20181101_0000_20181201_Std",
          "2018/Winter_20181201_0000_20190101_Std",
          "2019/AlbertBruecke_20190101_0000_20190201_Std",
          "2019/AlbertBruecke_20190201_0000_20190301_Std",
          "2019/AlbertBruecke_20190301_0000_20190401_Std.",
          "2019/AlbertBruecke_20190401_0000_20190501_Std",
          "2019/AlbertBruecke_20190501_0000_20190601_Std",
          "2019/AlbertBruecke_20190601_0000_20190701_Std",
          "2019/AlbertBruecke_20190701_0000_20190801_Std",
          "2019/AlbertBruecke_20190801_0000_20190901_Std",
          "2019/AlbertBruecke_20191001_0000_20191101_Std",
          "2019/AlbertBruecke_20191101_0000_20191201_Std",
          "2019/AlbertBruecke_20191201_0000_20200101_Std",
          "2019/Elberadweg_20190101_0000_20190201_Std",
          "2019/Elberadweg_20190201_0000_20190301_Std",
          "2019/Elberadweg_20190301_0000_20190401_Std",
          "2019/Elberadweg_20190401_0000_20190501_Std",
          "2019/Elberadweg_20190501_0000_20190601_Std",
          "2019/Elberadweg_20190601_0000_20190701_Std",
          "2019/Elberadweg_20190701_0000_20190801_Std",
          "2019/Elberadweg_20190801_0000_20190901_Std",
          #"2019/Elberadweg_20190901_0000_20191001_Std",
          "2019/Elberadweg_20191001_0000_20191101_Std",
          "2019/Elberadweg_20191101_0000_20191201_Std",
          "2019/Elberadweg_20191201_0000_20200101_Std",
          "2019/Elberadweg_Nord_20190101_0000_20190201_Std",
          "2019/Elberadweg_Nord_20190201_0000_20190301_Std",
          "2019/Elberadweg_Nord_20190301_0000_20190401_Std",
          "2019/Elberadweg_Nord_20190401_0000_20190501_Std",
          "2019/Elberadweg_Nord_20190501_0000_20190601_Std",
          "2019/Elberadweg_Nord_20190601_0000_20190701_Std",
          "2019/Elberadweg_Nord_20190701_0000_20190801_Std",
          "2019/Elberadweg_Nord_20190801_0000_20190901_Std",
          #"2019/Elberadweg_Nord_20190901_0000_20191001_Std",
          "2019/Elberadweg_Nord_20191001_0000_20191101_Std",
          "2019/Elberadweg_Nord_20191101_0000_20191201_Std",
          "2019/Elberadweg_Nord_20191201_0000_20200101_Std",
          "2019/Kreuz_20190101_0000_20190201_Std",
          "2019/Kreuz_20190201_0000_20190301_Std",
          "2019/Kreuz_20190301_0000_20190401_Std",
          "2019/Kreuz_20190401_0000_20190501_Std",
          "2019/Kreuz_20190501_0000_20190601_Std",
          "2019/Kreuz_20190601_0000_20190701_Std",
          "2019/Kreuz_20190701_0000_20190801_Std",
          "2019/Kreuz_20190801_0000_20190901_Std",
          #"2019/Kreuz_20190901_0000_20191001_Std",
          "2019/Kreuz_20191001_0000_20191101_Std",
          "2019/Kreuz_20191101_0000_20191201_Std",
          "2019/Kreuz_20191201_0000_20200101_Std",
          "2019/StPeter_20190101_0000_20190201_Std",
          "2019/StPeter_20190201_0000_20190301_Std",
          "2019/StPeter_20190301_0000_20190401_Std",
          "2019/StPeter_20190401_0000_20190501_Std",
          "2019/StPeter_20190501_0000_20190531_Std",
          "2019/StPeter_20190601_0000_20190701_Std",
          "2019/StPeter_20190701_0000_20190801_Std",
          "2019/StPeter_20190801_0000_20190901_Std.",
          #"2019/StPeter_20190901_0000_20191001_Std",
          "2019/StPeter_20191001_0000_20191101_Std",
          "2019/StPeter_20191101_0000_20191201_Std",
          "2019/StPeter_20191201_0000_20200101_Std",
          "2019/Tharandter_20190101_0000_20190201_Std",
          "2019/Tharandter_20190201_0000_20190301_Std",
          "2019/Tharandter_20190301_0000_20190401_Std",
          "2019/Tharandter_20190401_0000_20190501_Std",
          "2019/Tharandter_20190501_0000_20190601_Std",
          "2019/Tharandter_20190601_0000_20190701_Std",
          "2019/Tharandter_20190701_0000_20190801_Std",
          "2019/Tharandter_20190801_0000_20190901_Std",
          #"2019/Tharandter_20190901_0000_20191001_Std",
          "2019/Tharandter_20191001_0000_20191101_Std",
          "2019/Tharandter_20191101_0000_20191201_Std",
          "2019/Tharandter_20191201_0000_20200101_Std",
          "2019/Waldschloss_20190101_0000_20190201_Std",
          "2019/Waldschloss_20190201_0000_20190301_Std",
          "2019/Waldschloss_20190301_0000_20190401_Std",
          "2019/Waldschloss_20190401_0000_20190501_Std",
          "2019/Waldschloss_20190501_0000_20190601_Std",
          "2019/Waldschloss_20190601_0000_20190701_Std",
          "2019/Waldschloss_20190701_0000_20190801_Std",
          "2019/Waldschloss_20190801_0000_20190901_Std",
          #"2019/Waldschloss_20190901_0000_20191001_Std",
          "2019/Waldschloss_20191001_0000_20191101_Std",
          "2019/Waldschloss_20191101_0000_20191201_Std",
          "2019/Waldschloss_20191201_0000_20200101_Std",
          "2019/Winter_20190101_0000_20190201_Std",
          "2019/Winter_20190201_0000_20190301_Std",
          "2019/Winter_20190301_0000_20190401_Std",
          "2019/Winter_20190401_0000_20190501_Std",
          "2019/Winter_20190601_0000_20190701_Std",
          "2019/Winter_20190701_0000_20190801_Std",
          "2019/Winter_20190801_0000_20190901_Std",
          #"2019/Winter_20190901_0000_20191001_Std",
          "2019/Winter_20191001_0000_20191101_Std",
          "2019/Winter_20191101_0000_20191201_Std",
          "2019/Winter_20191201_0000_20200101_Std",
          "2020/AlbertBruecke_20200101_0000_20200201_Std",
          "2020/AlbertBruecke_20200201_0000_20200301_Std.",
          "2020/AlbertBruecke_20200301_0000_20200401_Std",
          "2020/AlbertBruecke_20200401_0000_20200501_0000_Std",
          "2020/AlbertBruecke_20200501_0000_20200601_0000_Std",
          "2020/AlbertBruecke_20200601_0000_20200701_Std.",
          "2020/AlbertBruecke_20200701_0000_20200801_0000_Std",
          "2020/AlbertBruecke_20200801_0000_20200901_Std",
          "2020/AlbertBruecke_20200901_0000_20201001_0000_Std",
          "2020/AlbertBruecke_20201001_0000_20201101_Std.",
          "2020/AlbertBruecke_20201101_0000_20201201_Std.",
          "2020/AlbertBruecke_20201201_0000_20210101_Std.",
          "2020/Elberadweg_20200101_0000_20200201_Std",
          "2020/Elberadweg_20200201_0000_20200301_Std.",
          "2020/Elberadweg_20200301_0000_20200401_Std",
          "2020/Elberadweg_20200401_0000_20200501_0000_Std",
          "2020/Elberadweg_20200501_0000_20200601_0000_Std",
          "2020/Elberadweg_20200601_0000_20200701_Std.",
          "2020/Elberadweg_20200701_0000_20200801_Std.",
          "2020/Elberadweg_20200801_0000_20200901_Std.",
          "2020/Elberadweg_20200901_0000_20201001_0000_Std",
          "2020/Elberadweg_20201001_0000_20201101_Std.",
          "2020/Elberadweg_20201101_0000_20201201_Std",
          "2020/Elberadweg_20201201_0000_20210101_Std.",
          "2020/Elberadweg_Nord_20200101_0000_20200201_Std",
          "2020/Elberadweg_Nord_20200201_0000_20200301_Std.",
          "2020/Elberadweg_Nord_20200301_0000_20200401_Std",
          "2020/Elberadweg_Nord_20200401_0000_20200501_0000_Std",
          "2020/Elberadweg_Nord_20200501_0000_20200601_0000_Std",
          "2020/Elberadweg_Nord_20200601_0000_20200701_Std.",
          "2020/Elberadweg_Nord_20200701_0000_20200801_Std.",
          "2020/Elberadweg_Nord_20200801_0000_20200901_Std.",
          "2020/Elberadweg_Nord_20200901_0000_20201001_0000_Std",
          "2020/Elberadweg_Nord_20201001_0000_20201101_Std.",
          "2020/Elberadweg_Nord_20201101_0000_20201201_Std.",
          "2020/Elberadweg_Nord_20201201_0000_20210101_Std.",
          "2020/Kreuz_20200101_0000_20200201_Std",
          "2020/Kreuz_20200201_0000_20200301_Std",
          "2020/Kreuz_20200301_0000_20200401_Std",
          "2020/Kreuz_20200401_0000_20200501_0000_Std",
          "2020/Kreuz_20200501_0000_20200601_0000_Std",
          "2020/Kreuz_20200601_0000_20200701_Std.",
          "2020/Kreuz_20200701_0000_20200801_Std.",
          "2020/Kreuz_20200801_0000_20200901_Std",
          "2020/Kreuz_20200901_0000_20201001_0000_Std",
          "2020/Kreuz_20201001_0000_20201101_Std.",
          "2020/Kreuz_20201101_0000_20201201_Std.",
          "2020/Kreuz_20201201_0000_20210101_Std.",
          "2020/StPeter_20200101_0000_20200201_Std.",
          "2020/StPeter_20200201_0000_20200301_Std",
          "2020/StPeter_20200301_0000_20200401_Std",
          "2020/StPeter_20200401_0000_20200501_0000_Std",
          "2020/StPeter_20200501_0000_20200601_0000_Std",
          "2020/StPeter_20200601_0000_20200701_Std",
          "2020/StPeter_20200701_0000_20200801_Std",
          "2020/StPeter_20200801_0000_20200901_Std",
          "2020/StPeter_20200901_0000_20201001_0000_Std",
          "2020/StPeter_20201001_0000_20201101_Std.",
          "2020/StPeter_20201101_0000_20201201_Std.",
          "2020/StPeter_20201201_0000_20210101_Std",
          "2020/Tharandter_20200101_0000_20200201_Std.",
          "2020/Tharandter_20200201_0000_20200301_Std",
          "2020/Tharandter_20200301_0000_20200401_Std",
          "2020/Tharandter_20200401_0000_20200501_0000_Std",
          "2020/Tharandter_20200501_0000_20200601_0000_Std",
          "2020/Tharandter_20200601_0000_20200701_Std.",
          "2020/Tharandter_20200701_0000_20200801_Std",
          "2020/Tharandter_20200801_0000_20200901_Std.",
          "2020/Tharandter_20200901_0000_20201001_0000_Std",
          "2020/Tharandter_20201001_0000_20201101_Std",
          "2020/Tharandter_20201101_0000_20201201_Std.",
          "2020/Tharandter_20201201_0000_20210101_Std",
          "2020/Waldschloss_20200101_0000_20200201_Std",
          "2020/Waldschloss_20200201_0000_20200301_Std",
          "2020/Waldschloss_20200301_0000_20200401_Std",
          "2020/Waldschloss_20200401_0000_20200501_0000_Std",
          "2020/Waldschloss_20200501_0000_20200601_0000_Std",
          "2020/Waldschloss_20200601_0000_20200701_Std.",
          #"2020/Waldschloss_20200701_0000_20200801_0000_Std",
          "2020/Waldschloss_20200801_0000_20200901_Std.",
          "2020/Waldschloss_20200901_0000_20201001_0000_Std",
          "2020/Waldschloss_20201001_0000_20201101_Std",
          "2020/Waldschloss_20201101_0000_20201201_Std.",
          "2020/Waldschloss_20201201_0000_20210101_Std",
          "2020/Winter_20200101_0000_20200201_Std.",
          "2020/Winter_20200201_0000_20200301_Std",
          "2020/Winter_20200301_0000_20200401_Std",
          "2020/Winter_20200401_0000_20200501_0000_Std",
          "2020/Winter_20200501_0000_20200601_0000_Std",
          "2020/Winter_20200601_0000_20200701_Std.",
          "2020/Winter_20200701_0000_20200801_Std",
          "2020/Winter_20200801_0000_20200901_Std.",
          "2020/Winter_20200901_0000_20201001_0000._Std",
          "2020/Winter_20201001_0000_20201101_Std",
          "2020/Winter_20201101_0000_20201201_Std.",
          "2020/Winter_20201201_0000_20210101_Std",
          "2021/AlbertBruecke_20210101_0000_20210201_Std",
          #"2021/AlbertBruecke_20210201_0000_20210301_Std",
          "2021/AlbertBruecke_20210301_0000_20210401_0000_Std",
          "2021/AlbertBruecke_20210401_0000_20210501_0000_Std",
          "2021/AlbertBruecke_20210501_0000_20210601_0000_Std",
          "2021/AlbertBruecke_20210601_0000_20210701_0000_Std",
          "2021/AlbertBruecke_20210701_0000_20210801_0000_Std",
          "2021/AlbertBruecke_20210801_0000_20210901_0000_Std",
          "2021/AlbertBruecke_20210901_0000_20211001_0000_Std",
          "2021/AlbertBruecke_20211001_0000_20211101_0000_Std",
          "2021/AlbertBruecke_20211101_0000_20211201_0000_Std",
          "2021/AlbertBruecke_20211201_0000_20220101_0000_Std",
          "2021/Elberadweg_20210101_0000_20210201_Std",
          #"2021/Elberadweg_20210201_0000_20210301_Std",
          #"2021/Elberadweg_20210301_0000_20210401_0000_Std",
          "2021/Elberadweg_20210401_0000_20210501_0000_Std",
          "2021/Elberadweg_20210501_0000_20210601_0000_Std",
          "2021/Elberadweg_20210601_0000_20210701_0000_Std",
          "2021/Elberadweg_20210701_0000_20210801_0000_Std",
          "2021/Elberadweg_20210801_0000_20210901_0000_Std",
          "2021/Elberadweg_20210901_0000_20211001_0000_Std",
          "2021/Elberadweg_20211001_0000_20211101_0000_Std",
          "2021/Elberadweg_20211101_0000_20211201_0000_Std",
          "2021/Elberadweg_20211201_0000_20220101_0000_Std",
          "2021/Elberadweg_Nord_20210101_0000_20210201_Std",
          #"2021/Elberadweg_Nord_20210201_0000_20210301_Std",
          "2021/Elberadweg_Nord_20210301_0000_20210401_0000_Std",
          "2021/Elberadweg_Nord_20210401_0000_20210501_0000_Std",
          "2021/Elberadweg_Nord_20210501_0000_20210601_0000_Std",
          "2021/Elberadweg_Nord_20210601_0000_20210701_0000_Std",
          "2021/Elberadweg_Nord_20210701_0000_20210801_0000_Std",
          "2021/Elberadweg_Nord_20210801_0000_20210901_0000_Std",
          "2021/Elberadweg_Nord_20210901_0000_20211001_0000_Std",
          "2021/Elberadweg_Nord_20211001_0000_20211101_0000_Std",
          "2021/Elberadweg_Nord_20211101_0000_20211201_0000_Std",
          "2021/Elberadweg_Nord_20211201_0000_20220101_0000_Std",
          "2021/Kreuz_20210101_0000_20210201_Std",
          "2021/Kreuz_20210201_0000_20210301_Std",
          "2021/Kreuz_20210201_0000_20210301_Std",
          "2021/Kreuz_20210401_0000_20210501_0000_Std",
          "2021/Kreuz_20210501_0000_20210601_0000_Std",
          "2021/Kreuz_20210601_0000_20210701_0000_Std",
          "2021/Kreuz_20210701_0000_20210801_0000_Std",
          "2021/Kreuz_20210801_0000_20210901_0000_Std",
          "2021/Kreuz_20210901_0000_20211001_0000_Std",
          "2021/Kreuz_20211001_0000_20211101_0000_Std",
          "2021/Kreuz_20211101_0000_20211201_0000_Std",
          "2021/Kreuz_20211201_0000_20220101_0000_Std",
          "2021/StPeter_20210101_0000_20210201_Std",
          "2021/StPeter_20210201_0000_20210301_Std",
          "2021/StPeter_20210301_0000_20210401_0000_Std",
          "2021/StPeter_20210401_0000_20210501_0000_Std",
          "2021/StPeter_20210501_0000_20210601_000_Std",
          "2021/StPeter_20210601_0000_20210701_0000_Std",
          "2021/StPeter_20210701_0000_20210801_0000_Std",
          "2021/StPeter_20210801_0000_20210901_0000_Std",
          "2021/StPeter_20210901_0000_20211001_0000_Std",
          "2021/StPeter_20211001_0000_20211101_0000_Std",
          "2021/StPeter_20211101_0000_20211201_0000_Std",
          "2021/StPeter_20211201_0000_20220101_0000_Std",
          "2021/Tharandter_20210101_0000_20210201_Std",
          #"2021/Tharandter_20210201_0000_20210301_Std",
          "2021/Tharandter_20210301_0000_20210401_0000_Std",
          "2021/Tharandter_20210401_0000_20210501_0000_Std",
          "2021/Tharandter_20210501_0000_20210601_0000_Std",
          "2021/Tharandter_20210601_0000_20210701_0000_Std",
          "2021/Tharandter_20210701_0000_20210801_0000_Std",
          "2021/Tharandter_20210701_0000_20210801_0000_Std",
          "2021/Tharandter_20210901_0000_20211001_0000_Std",
          "2021/Tharandter_20211001_0000_20211101_0000_Std",
          "2021/Tharandter_20211101_0000_20211201_0000_Std",
          "2021/Tharandter_20211201_0000_20220101_0000_Std",
          "2021/Waldschloss_20210101_0000_20210201_Std.",
          #"2021/Waldschloss_20210201_0000_20210301_Std",
          "2021/Waldschloss_20210301_0000_20210401_0000_Std",
          "2021/Waldschloss_20210401_0000_20210501_0000_Std",
          "2021/Waldschloss_20210501_0000_20210601_0000_Std",
          "2021/Waldschloss_20210601_0000_20210701_0000_Std",
          "2021/Waldschloss_20210701_0000_20210801_0000_Std",
          "2021/Waldschloss_20210901_0000_20211001_0000_Std",
          "2021/Waldschloss_20211001_0000_20211101_0000_Strd",
          "2021/Waldschloss_20211101_0000_20211201_0000_Std",
          "2021/Waldschloss_20211201_0000_20220101_0000_Std",
          "2021/Winter_20210101_0000_20210201_Std",
          #"2021/Winter_20210201_0000_20210301_Std",
          "2021/Winter_20210301_0000_20210401_0000_Std",
          "2021/Winter_20210401_0000_20210501_0000_Std",
          "2021/Winter_20210501_0000_20210601_0000_Std",
          "2021/Winter_20210601_0000_20210701_0000_Std",
          "2021/Winter_20210701_0000_20210801_0000_Std",
          "2021/Winter_20210801_0000_20210901_0000_Std",
          "2021/Winter_20210901_0000_20211001_0000_Std",
          "2021/Winter_20211001_0000_20211101_0000_Std",
          "2021/Winter_20211101_0000_20211201_0000_Std",
          "2021/Winter_20211201_0000_20220101_0000_Std"
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
  
  data1$Station = gsub("2018/", "", data1$Station)
  data1$Station = gsub("2019/", "", data1$Station)
  data1$Station = gsub("2020/", "", data1$Station)
  data1$Station = gsub("2021/", "", data1$Station)
  
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

for(i in 2:length(files)){
  
  print(paste("Progress:",round(i/length(files)*100,2),"%"))
  
  if(i==2){
    rawData = rbind(myList[[1]],myList[[2]])
    }
  else{
    rawData = rbind(rawData,myList[[i]])
    }
  
}

names(rawData)[1] = "Timestamp"
names(rawData)[2] = "Value"

summary(rawData)
nrow(rawData)

#Time related Data including Year, Months, Summer, Winter, Weekday, Weekends, Hour and Night, Public and School Holidays

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

pH=publicHolidays[publicHolidays$SAC %in% TRUE,]
rawData$publicHoliday = ifelse(as.Date(rawData$Timestamp) %in% as.Date(pH$Datum,format="%d.%m.%y"),1,0)

#Load data for school holidays
schoolHolidays = read.csv(file = "Schulferien.csv",sep=",")

sH=schoolHolidays[schoolHolidays$Bundesland %in% "SAC",]
x <- vector()
for(i in 1:length(sH$Startdatum)){
  x = append(x, as.Date(sH$Startdatum,format="%d.%m.%y")[i]:as.Date(sH$Enddatum,format="%d.%m.%y")[i])
}
rawData$schoolHoliday = ifelse(as.numeric(as.Date(rawData$Timestamp)) %in% x,1,0)

summary(rawData)


#Add Weather Data (Source: Deutscher Wetterdienst)

rm(list=setdiff(ls(), "rawData"))

#Import Weather Data
setwd(paste("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten/",rawData$Town[1],sep=""))
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
#setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
#write.csv(rawData,"Tübingen.csv")



rawData$young18 = 16
rawData$young20 = 18  

summary(rawData)

# Adding ADFC-Fahrradklima Values

Year=c(2018,2019,2020,2021)
ADFC_Index=c(4,4,4,4)

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
  
  distmat[i,1]=d$Station[1]
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
cinmat=matrix(1:3*length(cinema$osm_points$osm_id), nrow = length(cinema$osm_points$osm_id), ncol = 3)

for(i in 1:length(cinema$osm_points$osm_id)){
  
  cinmat[i,1]=cinema$osm_points$osm_id[i]
  cinmat[i,2]=cinema$osm_points$geometry[[i]][1]
  cinmat[i,3]=cinema$osm_points$geometry[[i]][2]
  
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
  add_osm_feature("operator", "DB Netz AG;DB Station&Service AG")

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

#citation ("osmdata")
#setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
#write.csv(rawData,paste(toString(rawData$Town[1]),".csv",sep=""))

#Make minor changes with new data sources--------------------------------

BikeData = rawData

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
  else if(Altersgruppen$Town[i] == "Dresden, kreisfreie Stadt"){Altersgruppen$Town[i] = "Dresden"}
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
  else if(Immigrants$Town[i] == "Dresden, kreisfreie Stadt"){Immigrants$Town[i] = "Dresden"}
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
  else if(PKW$Town[i] == "Dresden, kreisfreie Stadt"){PKW$Town[i] = "Dresden"}
  else{PKW$Town[i] = NA}
}
PKW = na.omit(PKW)
PKW$PKWs=as.numeric(PKW$PKWs)

BikeData = merge(x = BikeData,y = PKW,
                 by = c("Year","Town"),
                 all = TRUE)

BikeData$PKWs = BikeData$PKWs/BikeData$InhDestrict

summary(BikeData)
rm(list=setdiff(ls(), c("BikeData")))




#Hier noch Probleme




#Using the Corona Incidence

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


sac = as.data.frame(cbind(as.numeric(sub(",", ".", laenderData2$Sachsen, fixed = TRUE)),laenderData2$Day))
sac = as.data.frame(cbind(sac,laenderData2$Months))
sac = as.data.frame(cbind(sac,laenderData2$Year))
names(sac)[1]="inzidenz_Bundesland"
names(sac)[2]="Day"
names(sac)[3]="Months"
names(sac)[4]="Year"
sac$Bundesland="Sachsen"

corBL = sac

BikeData$Bundesland = "empty"

BikeData$Bundesland = "Sachsen"

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

#Leipzig

s =landkreisData[landkreisData$Stadt == "SK Dresden", ]
inzidenz_kommunal = as.numeric(sub(",", ".", s, fixed = TRUE))
inzidenz_kommunal = na.omit(inzidenz_kommunal)

Timestamp = landkreisData[1,]
Timestamp[1] = NULL

leipzig = as.data.frame(rbind(Timestamp,inzidenz_kommunal))
leipzig <- data.frame(t(leipzig[-1]))
names(leipzig)[1] = "Timestamp"
names(leipzig)[2] = "Inzidenz_Stadt"
leipzig$Town = "Dresden"


landkreisData2 = leipzig

landkreisData2$Timestamp = as.POSIXlt(landkreisData2$Timestamp,format="%d.%m.%Y")
landkreisData2$Year = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%Y"))
landkreisData2$Months = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%m"))
landkreisData2$Day = as.numeric(format(as.POSIXlt(landkreisData2$Timestamp), format = "%d"))
landkreisData2$Timestamp = NULL

names(landkreisData2)

BikeData$Year

BikeData = merge(x = BikeData,y = landkreisData2,
                 by = c("Year","Months","Day","Town"),
                 all = FALSE)

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

kontaktbeschr=c(20200322:20200331,20200401:20200431,20200501:20200506,
                20201102:20201131,20201201:20201231,20210101:20210131,20210201:20210214)
kontaktbeschr = na.omit(kontaktbeschr)

BikeData$Timestamp = as.POSIXct(as.character(paste(BikeData$Year,BikeData$Months,BikeData$Day,sep=".")), format="%Y.%m.%d")

summary(BikeData$Timestamp)

Kontaktbeschr = as.POSIXct(as.character(kontaktbeschr), format="%Y%m%d")
BikeData$Lockdowns	= ifelse(BikeData$Timestamp %in% Kontaktbeschr,1,0)
summary(BikeData$Lockdowns)

