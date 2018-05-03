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

setwd("C:/usfs_cronan_gis/SEF/FDM_outputs_vFP/run_102")

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster("r1021101.asc")

#Import .asc files
fm.1 <- matrix(scan("r1021101.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)



