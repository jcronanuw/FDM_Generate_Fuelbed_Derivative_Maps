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

#Variable of interest
#List column number in fft output table for this variable:
# = Benchmark ROS
fccsVar <- 19
#19 -- Custom Flame Length prediction

#Files
type_in <- "f"
type_out <- "l"
run <- "0888"
rx_fire <- "100"
intervals <- c("00", "05", as.character(seq(10,50,5)))

filenames_in <- vector()
filenames_out <- vector()
for(i in 1:length(intervals))
{
  filenames_in[i] <- paste(type_in, run, rx_fire, intervals[i], ".asc", sep = "")
  filenames_out[i] <- paste(type_out, run, rx_fire, intervals[i], ".asc", sep = "")
}

#Set up a list to hold input and output maps
maps_in <- list()
maps_out <- list()

#Set working directory
setwd("C:/Users/jcronan/Documents/GitHub/FDM-Eglin-Analysis/inputs")

#Import input parameters
params <- read.csv("input_params.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

fft <- read.csv("fft_outputs.csv", header=TRUE, 
                sep=",", na.strings="NA", dec=".", strip.white=TRUE)

predicted_pigs <- read.table("predicted_pigs_altered_DELETE.csv", header=TRUE, 
                             sep=",", na.strings="NA", dec=".", strip.white=TRUE)

setwd("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_0888_in/fuelbed_no")

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster(filenames_in[1])

setwd("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_0888_in/fuelbed_no")

#Import .asc files
for(i in 1:length(intervals))
{
  maps_in[[i]] <- matrix(scan(filenames_in[i],skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
}

#Swicth Working Directories
setwd("C:/Users/jcronan/Documents/GitHub/EglinAirForceBase/inputs")
#Import master fuelbed lookup table
lut <- read.csv("sef_lut_all.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


#Subset fuelbeds that were deleted, but remain on FCCS basemap
deleted <- lut$fuelbed[lut[[12]] == 0]

#Identify their replacements
replacements <- lut$post_1[lut[[12]] == 0]

#Replace deleted fuelbeds with replacements for each map.
for(i in 1:length(intervals))
  {
  if(is.list(mapply(function(x) replacements[deleted %in% x], 
                    maps_in[[i]][maps_in[[i]] %in% deleted])) == T)
  {
    maps_in[[i]] <- maps_in[[i]]
    } else
      {
        #Replace deleted fuelbeds
        maps_in[[i]][maps_in[[i]] %in% deleted] <- mapply(function(x) replacements[deleted %in% x], 
                                                          maps_in[[i]][maps_in[[i]] %in% deleted])
      }
}

#Change number of digits in fft object from 5 to 7 by adding zeros. This will allign fuelbed naming
#convention with that in output fuelbed maps.

#Split fuelbed numbers into character digits
fb_digits <- strsplit(as.character(fft$Fuelbed_number), split = "")

#Add in two zeroes so structure matches fuelbed output maps
new_fb_digits <- list()
for(i in 1:length(fb_digits))
{
  new_fb_digits[[i]] <- c(fb_digits[[i]][1], 
                     "0", 
                     fb_digits[[i]][2], 
                     fb_digits[[i]][3], 
                     fb_digits[[i]][4], 
                     "0", 
                     fb_digits[[i]][5])
}

#Merge character digit elements together and convert to numeric string
new_fb_numeric <- list()
for(i in 1:length(fb_digits))
{
  new_fb_numeric[[i]] <- as.numeric(
    paste(new_fb_digits[[i]][1], 
          new_fb_digits[[i]][2], 
          new_fb_digits[[i]][3], 
          new_fb_digits[[i]][4], 
          new_fb_digits[[i]][5], 
          new_fb_digits[[i]][6], 
          new_fb_digits[[i]][7], 
          sep = ""))
}

#Convert from list to vector
new_fb <- unlist(new_fb_numeric)

#Create a new FFT output lookup table that crosswalks complete fuelbed list with Anne Andreu's FCCS fuelbeds.
fft_complete <- fft
fft_complete <- fft_complete[NULL,]

for(i in 1:length(predicted_pigs$fuelbed))
{
    fft_complete[i,] <- fft[new_fb == predicted_pigs$andreu_fuelbed_no[i],]
}

#Convert maps to vectors (will result in quicker crosswalks with FFT FCCS fuelbed properties).
base_vector <- list()
for(i in 1:length(intervals))
{
  base_vector[[i]] <- as.vector(maps_in[[i]])
}

#Crosswalk fuelbed maps (as vectors) with desired fuels and predicted fire behavior variables.
#Desired variables
#Litter and herb loading
#Total surface fuel load
#Aboveground Tree Biomass
#Shrub Loading
#DWD
#Total aboveground loading
#Flame Length
#Carbon Mass

#Create vector for desired crosswalk variables
var_vector <- list()

#Crosswalk fuelbed numbers with crosswalk variables
#f05_vector_fccsVar <- fft_complete$Benchmark_ROS[match(f05_vector, predicted_pigs$fuelbed)]
for(i in 1:length(intervals))
{
  var_vector[[i]] <- as.vector(fft_complete[[fccsVar]])[match(base_vector[[i]], predicted_pigs$fuelbed)]
}

#Convert vectors into matrices
#f05_fccsVar <- matrix(f05_vector_fccsVar, f.head@nrows, f.head@ncols)
for(i in 1:length(intervals))
{
  maps_out[[i]] <- matrix(var_vector[[i]], f.head@nrows, f.head@ncols)
}

#Zero value fuelbeds
zeros <- c(5099000, 6000000)

#Set values from NA to zero for developed land and water fuelbeds (not procssed through FFT)
#f05_fccsVar[f05 %in% zeros] <- 0
for(i in 1:length(intervals))
{
  maps_out[[i]][maps_in[[i]] %in% zeros] <- 0
}

#Assign negative number to development and water fuelbeds
for(i in 1:length(intervals))
{
  assignments <- seq((-1*length(zeros)),-1,1)
  for (a in 1:length(zeros))
  {
    maps_out[[i]][maps_in[[i]] == zeros[a]] <- assignments[a]
  }
}

#Testing
#Make sure crosswalk was successful
test_results <- data.frame(map = intervals, 
                           multiples = rep(0,length(intervals)), 
                           fuelbeds_no_fccsVar = rep(0,length(intervals)), 
                           fuelbeds_na__fccsVar = rep(0,length(intervals)))
na_fuelbeds <- list()

for(a in 1:length(intervals))
{
  test_base <- maps_in[[a]]
  test_var <- maps_out[[a]]
  crosswalk_values <- vector()
  for(i in 1:length(new_fb))
    {
    crosswalk_values[i] <- length(unique(test_var[test_base == new_fb[i]])) 
    }
  #Results should be 0, 1
  test_results[a,2] <- ifelse(any(unique(crosswalk_values) %in% c(0,1)),"OK", "ERROR")
  
  #Results should be zero
  test_results[a,3] <- ifelse(length(test_base[test_base %in% new_fb[crosswalk_values == 0]]) == 0, "OK", "ERROR")
  
  test_results[a,4] <- ifelse(length(test_var[is.na(test_var) == T]) == length(test_base[test_base == -9999]), "OK", "ERROR")
  na_fuelbeds[[a]] <- sort(unique(test_base[is.na(test_var) == T]))
  }

#Show test results
test_results
na_fuelbeds

#Replace NA values with -9999. Ascii to raster batch conversion script cannot handle NA values.
for(i in 1:length(intervals))
{
  maps_out[[i]][is.na(maps_out[[i]]) == T] <- -9999
}

##Open metadata for spatial datasets
setwd("C:/Users/jcronan/Documents/GitHub/FDM-Eglin-Analysis/inputs")
metadata <- read.table(paste("eglin_raster_metadata.txt", sep = ""), 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
options(digits = 15)

#Create vectors from metadata list
md.desc <- as.vector(unlist(metadata[,1]))
md.valu <- as.vector(unlist(metadata[,2]))
#Create a name tag for simulation year to add to output file name.
simYear_name_tag <- ifelse(a < 10, paste("0", as.character(a), sep = ""), as.character(a))

#Seprate header metadata into seperate lines.
line1 <- paste(paste(md.desc[1]), paste("         ", md.valu[1]))
line2 <- paste(paste(md.desc[2]), paste("         ", md.valu[2]))
line3 <- paste(paste(md.desc[3]), paste("     ", md.valu[3]))
line4 <- paste(paste(md.desc[4]), paste("     ", md.valu[4]))
line5 <- paste(paste(md.desc[5]), paste("      ", md.valu[5]))
line6 <- paste(paste(md.desc[6]), paste("  ", md.valu[6]))

setwd("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_0888_out/r_0888_flameLength/ascii")
for(i in 1:length(intervals))
{
  #Print header information to map
  cat(line1, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  cat(line2, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  cat(line3, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  cat(line4, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  cat(line5, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  cat(line6, file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  
  #Save stand map.
  cat(c(t(maps_out[[i]])), file = paste(filenames_out[i], sep = ""), fill = T, append = T)#
  }

#CREATE CHANGE MAPS AND ASSOCIATED METRICS
maps_out_zero <- maps_out
for(i in 1:length(maps_out_zero))
{
  maps_out_zero[[i]][maps_out_zero[[i]] < 0] <- 0
}

flameLength_change <- list()
change_out <- c("change_10_0.asc","change_20_0.asc","change_30_0.asc", 
                "change_50_0.asc", "change_50_0.asc")
b <- c(3,5,7,9,11)

for(i in 1:length(change_out))
{
  flameLength_change[[i]] <- maps_out[[b[i]]] - maps_out[[1]]
}

for(i in 1:length(change_out))
{
  flameLength_change[[i]][maps_in[[1]] %in% zeros] <- -8888
  flameLength_change[[i]][maps_in[[1]] == -9999] <- -9999
}

##Open metadata for spatial datasets
setwd("C:/Users/jcronan/Documents/GitHub/FDM-Eglin-Analysis/inputs")
metadata <- read.table(paste("eglin_raster_metadata.txt", sep = ""), 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
options(digits = 15)

#Create vectors from metadata list
md.desc <- as.vector(unlist(metadata[,1]))
md.valu <- as.vector(unlist(metadata[,2]))
#Create a name tag for simulation year to add to output file name.
simYear_name_tag <- ifelse(a < 10, paste("0", as.character(a), sep = ""), as.character(a))

#Seprate header metadata into seperate lines.
line1 <- paste(paste(md.desc[1]), paste("         ", md.valu[1]))
line2 <- paste(paste(md.desc[2]), paste("         ", md.valu[2]))
line3 <- paste(paste(md.desc[3]), paste("     ", md.valu[3]))
line4 <- paste(paste(md.desc[4]), paste("     ", md.valu[4]))
line5 <- paste(paste(md.desc[5]), paste("      ", md.valu[5]))
line6 <- paste(paste(md.desc[6]), paste("  ", md.valu[6]))

setwd("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_0888_out/r_0888_flameLength/ascii_change")
for(i in 1:length(change_out))
{
  #Print header information to map
  cat(line1, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  cat(line2, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  cat(line3, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  cat(line4, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  cat(line5, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  cat(line6, file = paste(change_out[i], sep = ""), fill = T, append = T)#
  
  #Save stand map.
  cat(c(t(flameLength_change[[i]])), file = paste(change_out[i], sep = ""), fill = T, append = T)#
}










