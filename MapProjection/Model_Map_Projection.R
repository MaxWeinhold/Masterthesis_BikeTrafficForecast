#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model Map Projection

if(!require("beepr")) install.packages("beepr")
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(beepr)
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)


#Clean up memory
rm(list=ls())

#calculated models are her:
setwd("C:/Users/MaxWe/Documents/GitHub/Masterthesis_BikeTrafficForecast/ValidationResults")

load("Modell1_OLS.rdata")
summary(model)

city = "Münster"

#building the query
q <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway")

str(q) #query structure

streets <- osmdata_sf(q)

streets

#our background map
myLocation<-c(7.597514856738869,51.94573812395569,   7.652382675482133,51.9756143280805)

#get_stamenmap(bbox=myLocation, maptype="terrain-background", zoom=7)
mad_map <- get_stamenmap(bbox=myLocation, maptype="terrain-background", zoom=14)
#mad_map <- get_map(getbb(city), maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 1
          )+
  labs(x = "", y = "")
