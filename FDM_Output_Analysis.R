#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script imports and analyzes FDM v2.0 outputs for the Eglin Air Force Base
#50-year prescribed fire simulations. Analysis will consist of different ways to quantify fuel
#and fire behavior hazard at the base under different prescribed burning scenarios including
#1) 125% of current -- 125k acres prescribed burned/year
#2) 100% of current -- 100k acres prescribed burned/year
#3) 75% of current -- 75k acres prescribed burned/year
#4) 50% of current -- 50k acres prescribed burned/year

#Author: Jim Cronan
#Organization: US Forest Service
#Address: 
#Pacific Wildland Fire Sciences Laboratory
#400 North 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: May 2, 2018

#Libraries
library(raster)

#Set working directory
setwd("C:/Users/jcronan/Documents/GitHub/FDM-Eglin-Analysis/inputs")

#Import input parameters
params <- read.csv("input_params.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

fft <- read.csv("fft_outputs.csv", header=TRUE, 
                sep=",", na.strings="NA", dec=".", strip.white=TRUE)


setwd("C:/usfs_sef_outputs_FDM/results_r888")

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster("f088810005.asc")

#Import .asc files
f05 <- matrix(scan("f088810005.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f10 <- matrix(scan("f088810010.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f15 <- matrix(scan("f088810015.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f20 <- matrix(scan("f088810020.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f25 <- matrix(scan("f088810025.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f30 <- matrix(scan("f088810030.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f35 <- matrix(scan("f088810035.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f40 <- matrix(scan("f088810040.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f45 <- matrix(scan("f088810045.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
f50 <- matrix(scan("f088810050.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)

#Make a map of litter loading
f05_litter <- lapply(f05, function(x) fft$LLM_litter_load[match(x, fft$Fuelbed_number)])
f05_benchmark_ros <- lapply(f05, function(x) fft$Benchmark_ROS[match(x, fft$Fuelbed_number)])

f05_lm <- matrix(f05_litter, f.head@nrows, f.head@ncols)
str(f05_lm)
head(f05_lm)
head(f05_litter)
length(f05_litter)
length(f05)

test <- vector()
for(i in 1:length(f05))
{
  test[i] <- length(f05_litter[[i]])
  }


names(fft)

str(f05)

aa <- matrix(c(1,9,1,2,3,4,4,4,1,3,2,1,3,4,4,4),4,4)
bb <- data.frame(a = c(1,2,3,4), b = c(11, 22, 33, 44))

cc <-lapply(aa, function(x) bb$b[match(x, bb$a)])
matrix(cc, 4,4)


aa[aa == bb$a] <- bb$b


bb$a[1]


cc <- aa == bb$a]
cc



