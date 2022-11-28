#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#OSM Data Testing

#For that purpose we will use the osmdata package.
if(!require("osmdata")) install.packages("osmdata")
library(osmdata)

#Do not forget to give credit to the creators.
citation ("osmdata")

#The sf we will need to make geometrical calculations.
if(!require("sf")) install.packages("sf")
library(sf)

#Further we need to access tidyverse.
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("geosphere")) install.packages("geosphere")
library(geosphere)#package for calculating distance using longitude and latitude

#First we determine which city we want to study.
city="Berlin"

Brandenburg_Gate=c(13.377336846520663,52.516264818429924)

#As second we build a query asking for traffic signals in Berlin.
q <- getbb(city) %>%
  opq() %>%
  add_osm_feature("amenity", "school")

#Read the osm data format as a list in R.
schools <- osmdata_sf(q)

length(schools$osm_points$osm_id)

length(schools$osm_lines$osm_id)

length(schools$osm_polygons$osm_id)
as.data.frame(schools$osm_polygons$geometry[[1]][1])[1,]



