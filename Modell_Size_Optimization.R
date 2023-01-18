#Spatial prediction of urban bicycle traffic volume with machine learning
#Maximilian Weinhold
#------------------------------------------------------------------------
#Model calculations: Modell Size Optimization

#In order to make a notification sound to inform the user that calculations are finished
if(!require("beepr")) install.packages("beepr")
library(beepr)

#Regarding calculation power see following source: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#Historically, R has only utilized one processor, which makes it single-threaded.

#Clean up memory
rm(list=ls())

#Source storage location (outside the GitHub Repository)
#Because of file size limitation
#files about 100 MB have to be excluded
setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")

load("ValidationSets.rdata")

names(validation_set[[1]])

n = 200000

for_start_time <- Sys.time()
for(i in 1:length(validation_set)){
  
  print(i)
  n_stations = nlevels(as.factor(validation_set[[i]]$Station))
  validation_set[[i]]$chosen = 0
  
  #validation_set[[i]] = na.omit(validation_set[[i]])
  
  size_before = as.numeric(object.size(validation_set))
  
  for(j in 1:(nlevels(as.factor(validation_set[[i]]$Station)))){
    
    print(levels(as.factor(validation_set[[i]]$Station))[j])
    print(paste("Station",j))
    
    n_all = which(validation_set[[i]]$Station == as.character(levels(as.factor(validation_set[[i]]$Station))[j]))
    
    n_sel = sample(n_all,as.integer(n/n_stations),replace=TRUE)
    
    length(n_sel)*n_stations
    
    validation_set[[i]]$chosen[n_sel] = 1
    
  }
  
  n_more = sample(which(validation_set[[i]]$chosen==0),n-sum(validation_set[[i]]$chosen==1),replace=FALSE)
  validation_set[[i]]$chosen[n_more] = 1
  
  validation_set[[i]]<-validation_set[[i]][(validation_set[[i]]$chosen==1),]
  size_after = as.numeric(object.size(validation_set))
  print(size_after-size_before)
  
}
for_end_time <- Sys.time()
print("The hole process took:")
print(for_end_time - for_start_time)

as.numeric(object.size(validation_set))/1000000000

nrow(validation_set[[1]])
nrow(validation_set[[2]])
nrow(validation_set[[3]])
nrow(validation_set[[4]])
nrow(validation_set[[5]])

summary(validation_set[[1]]$chosen)
summary(validation_set[[2]]$chosen)
summary(validation_set[[3]]$chosen)
summary(validation_set[[4]]$chosen)
summary(validation_set[[5]]$chosen)

rm(list=setdiff(ls(), "validation_set"))

setwd("D:/STUDIUM/Münster/7. Semester/Masterarbeit Daten")
save(validation_set,file="ValidationSets2.rdata")

beep("mario")
