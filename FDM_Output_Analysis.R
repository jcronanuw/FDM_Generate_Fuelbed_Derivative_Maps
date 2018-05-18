#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script imports FDM v2.0 FUELBED MAP outputs for the Eglin Air Force Base
#50-year prescribed fire simulations and maps simulated and changes in FFT outputs that serve 
#as indicators of fuel and fire hazard. 

#The script is meant to be run on outputs from an individual simulation.
#Please specify FFT outputs you want to crosswalk with fuelbed numbers.

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

#################################################################################################
#################################################################################################
#RUN INPUTS

#Libraries
library(raster)

#FFT OUPUT
#Variable of interest
#List column number in fft output table for this variable:
#List name of variable as it will appear in outgoing file structure.
#fccsVar_col <- c(3,4,5,6,7,8,9)
#fccsVar_name <- c("fineFuelLoad", "forestFloorLoad", "totalFuelLoad","flameLength", "rateOfSpread", 
#                  "availableFuels", "crownFirePotential")

fccsVar_col <- c(7,8,9)
#fccsVar_name <- c("rateOfSpread", "availableFuels", "crownFirePotential")

#FUELBED CONDITIONS
#Variable of interest
#List name of variable as it will appear in outgoing file structure.
fuelbedVar_name <- c("cover", "mfri")
fuelbedVar_out <- c("r", "s")

#Run Parameters
type_in <- "f"
#type_out <- c("t", "u", "v", "w", "x", "y", "z")
type_out <- c("x", "y", "z")

run <- "0888"
rx_fire <- "100"
intervals <- c("00", "05", as.character(seq(10,50,5)))

#Create a lookup table to be printed to the directory as a reference.
file_out_lookup <- data.frame(file_start_letter = type_out, hazard_measure = fccsVar_name)

#Save file
setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_out", sep = ""))
cat(c(t(file_out_lookup)), file = "file_out_lookup.txt", fill = T, append = T)#

#Set up filenames for incoming FDM maps
filenames_in <- vector()
for(i in 1:length(intervals))
{
  filenames_in[i] <- paste(type_in, run, rx_fire, intervals[i], ".asc", sep = "")
}

#################################################################################################
#Set working directory
setwd("C:/Users/jcronan/Documents/GitHub/FDM-Eglin-Analysis/inputs")

#Import input parameters
params <- read.csv("input_params.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

fft <- read.csv("fft_outputs.csv", header=TRUE, 
                sep=",", na.strings="NA", dec=".", strip.white=TRUE)

predicted_pigs <- read.table("predicted_pigs_altered_DELETE.csv", header=TRUE, 
                             sep=",", na.strings="NA", dec=".", strip.white=TRUE)

##Open metadata for spatial datasets
metadata <- read.table(paste("eglin_raster_metadata.txt", sep = ""), 
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
options(digits = 15)

#################################################################################################
setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_in/fuelbed_no", sep = ""))

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster(filenames_in[1])

#Set up a list to hold input and output maps
maps_in <- list()

#Import .asc files
for(i in 1:length(intervals))
{
  maps_in[[i]] <- matrix(scan(filenames_in[i],skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
}

#################################################################################################
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

#################################################################################################
#################################################################################################

#Change number of digits in fft object from 5 to 7 by adding zeros. This will allign fuelbed 
#naming convention with that in output fuelbed maps.

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

#################################################################################################
#################################################################################################
#Create a new FFT output lookup table that crosswalks complete fuelbed list with Anne Andreu's 
#FCCS fuelbeds.
fft_add <- fft
fft_add <- fft_add[NULL,]

for(i in 1:length(predicted_pigs$fuelbed))
{
  fft_add[i,] <- fft[new_fb == predicted_pigs$andreu_fuelbed_no[i],]
}

#Replace NA values with zero
fft_add[is.na(fft_add) == T] <- 0


#Measures I need to create from fft outputs
fine_surface_fuel_load <- fft_add$LLM_litter_load + fft_add$Herb_primary_load + 
  fft_add$Shrub_primary_crown_load + fft_add$Woody_sound_1hr_load
forest_floor_load <- fft_add$LLM_litter_load + fft_add$Ground_upperduff_load


#Create a data.frame that has variables you want to analyze.
fft_complete <- data.frame("Fuelbed_number" = fft_add$Fuelbed_number, 
                           "Fuelbed_name" = fft_add$Fuelbed_name, 
                           "Fine_fuel_load" = fine_surface_fuel_load, 
                           "Forest_floor_load" = forest_floor_load, 
                           "Total_fuel_load" = fft_add$Total_aboveground_biomass, 
                           "Custom_FL" = fft_add$Custom_FL, 
                           "Custom_ROS" = fft_add$Custom_ROS, 
                           "Available_fuel_potential" = fft_add$Available_fuel_potential,
                           "Crown_fire_potential" = fft_add$Crown_fire_potential)

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#BEGIN LOOPING THROUGH FFT OUTPUT VARIABLES YOU WANT TO MAP

for(z in 1:length(fccsVar_col))
{
  #################################################################################################
  #################################################################################################
  #Convert maps to vectors (will result in quicker crosswalks with FFT FCCS fuelbed properties).
  base_vector <- list()
  for(i in 1:length(intervals))
    {
    base_vector[[i]] <- as.vector(maps_in[[i]])
    }
  
  #Set up filenames for outgoing maps
  filenames_out <- vector()
  for(i in 1:length(intervals))
    {
    filenames_out[i] <- paste(type_out[z], run, rx_fire, intervals[i], ".asc", sep = "")
    }
  
  #################################################################################################
  #################################################################################################
  #Create vector for desired crosswalk variables
  var_vector <- list()
  
  #Crosswalk fuelbed numbers with crosswalk variables
  for(i in 1:length(intervals))
    {
    var_vector[[i]] <- as.vector(fft_complete[[fccsVar_col[z]]])[match(base_vector[[i]], 
                                                                   predicted_pigs$fuelbed)]
    }
  #################################################################################################
  #################################################################################################
  #Convert vectors into matrices
  #Set up a list to hold input and output maps
  maps_out <- list()
  
  #Convert list of output vectors into a matrix.
  for(i in 1:length(intervals))
    {
    maps_out[[i]] <- matrix(var_vector[[i]], f.head@nrows, f.head@ncols)
    }
  
  #################################################################################################
  #################################################################################################
  #Zero value fuelbeds
  zeros <- c(5099000, 6000000)
  
  #Set values from NA to zero for developed land and water fuelbeds (not procssed through FFT)
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
  
  #################################################################################################
  #################################################################################################
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
    test_results[a,3] <- ifelse(length(test_base[test_base %in% new_fb[crosswalk_values == 0]]) == 0, 
                                "OK", "ERROR")
    
    test_results[a,4] <- ifelse(length(test_var[is.na(test_var) == T]) == length(test_base[test_base == -9999]), 
                                "OK", "ERROR")
    na_fuelbeds[[a]] <- sort(unique(test_base[is.na(test_var) == T]))
    }
  
  #Show test results
  print(fccsVar_name[z])
  print(test_results)
  print(na_fuelbeds)
  
  #################################################################################################
  #################################################################################################
  #Replace NA values with -9999. Ascii to raster batch conversion script cannot handle NA values.
  for(i in 1:length(intervals))
    {
    maps_out[[i]][is.na(maps_out[[i]]) == T] <- -9999
    }
  
  #################################################################################################
  #################################################################################################
  #Create vectors from metadata list
  md.desc <- as.vector(unlist(metadata[,1]))
  md.valu <- as.vector(unlist(metadata[,2]))
  
  #Seprate header metadata into seperate lines.
  line1 <- paste(paste(md.desc[1]), paste("         ", md.valu[1]))
  line2 <- paste(paste(md.desc[2]), paste("         ", md.valu[2]))
  line3 <- paste(paste(md.desc[3]), paste("     ", md.valu[3]))
  line4 <- paste(paste(md.desc[4]), paste("     ", md.valu[4]))
  line5 <- paste(paste(md.desc[5]), paste("      ", md.valu[5]))
  line6 <- paste(paste(md.desc[6]), paste("  ", md.valu[6]))
  
  #Set working directory for output maps
  setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_out/r_", run, "_", fccsVar_name[z], 
              "/ascii", sep = ""))
  
  #Save output maps
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
  
  #################################################################################################
  #################################################################################################
  #CREATE CHANGE MAPS AND ASSOCIATED METRICS
  maps_out_change <- list()
  change_out <- c("10_00.asc","20_00.asc","30_00.asc", "40_00.asc", "50_00.asc")
  map_numbers <- c(3,5,7,9,11)
  
  for(i in 1:length(change_out))
    {
    maps_out_change[[i]] <- maps_out[[map_numbers[i]]] - maps_out[[1]]
    }
  
  for(i in 1:length(change_out))
    {
    maps_out_change[[i]][maps_in[[1]] %in% zeros] <- -8888
    maps_out_change[[i]][maps_in[[1]] == -9999] <- -9999
    }
  
  #################################################################################################
  #################################################################################################
  #Create vectors from metadata list
  md.desc <- as.vector(unlist(metadata[,1]))
  md.valu <- as.vector(unlist(metadata[,2]))
  
  #Seprate header metadata into seperate lines.
  line1 <- paste(paste(md.desc[1]), paste("         ", md.valu[1]))
  line2 <- paste(paste(md.desc[2]), paste("         ", md.valu[2]))
  line3 <- paste(paste(md.desc[3]), paste("     ", md.valu[3]))
  line4 <- paste(paste(md.desc[4]), paste("     ", md.valu[4]))
  line5 <- paste(paste(md.desc[5]), paste("      ", md.valu[5]))
  line6 <- paste(paste(md.desc[6]), paste("  ", md.valu[6]))
  
  #Set working directory for output maps
  setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_out/r_", run, "_", fccsVar_name[z], 
              "/ascii_change", sep = ""))
  
  #Save output maps
  for(i in 1:length(change_out))
    {
    #Print header information to map
    cat(line1, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    cat(line2, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    cat(line3, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    cat(line4, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    cat(line5, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    cat(line6, file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
    
    #Save stand map.
    cat(c(t(maps_out_change[[i]])), file = paste(type_out[z], "_", change_out[i], sep = ""), fill = T, append = T)#
  }
}

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#GENERATE MAPS THAT SHOW FUELBED CONDITIONS

#Remove maps out objects to free up RAM space
rm(maps_out, maps_out_change, maps_out_zero)

#################################################################################################
#################################################################################################
#Remove the -9999 as No Value and replace with 1000000 so I can disect fuelbed numbers.
for(i in 1:length(maps_in))
{
  maps_in[[i]][maps_in[[i]] == -9999] <- 1000000
}

#################################################################################################
#################################################################################################
#Create vectors and remove scientific notation
maps_in_character <- list()
for(i in 1:length(maps_in))
{
  maps_in_character[[i]] <- as.character(maps_in[[i]])
  options(scipen = 7)
}  

#Change to vector
vector_in_character <- list()
for(i in 1:length(maps_in))
{
  vector_in_character[[i]] <- as.vector(maps_in_character[[i]])
}  

#Create lists to hold ouputs
maps_cover <- list()
maps_mfri <- list()

#Split out digits from fuelbed numbers
for(i in 1:length(maps_in))
{
  split_0 <- unlist(strsplit(vector_in_character[[i]], vector()))
  split_cover <- as.numeric(split_0[seq(3,length(split_0),7)])
  split_mfri <- as.numeric(split_0[seq(5,length(split_0),7)])
  maps_cover[[i]] <- matrix(split_cover, f.head@nrows, f.head@ncols)
  maps_mfri[[i]] <-  matrix(split_mfri, f.head@nrows, f.head@ncols)
  print(i)
  }

#################################################################################################
#################################################################################################
#Create vectors from metadata list
md.desc <- as.vector(unlist(metadata[,1]))
md.valu <- as.vector(unlist(metadata[,2]))

#Seprate header metadata into seperate lines.
line1 <- paste(paste(md.desc[1]), paste("         ", md.valu[1]))
line2 <- paste(paste(md.desc[2]), paste("         ", md.valu[2]))
line3 <- paste(paste(md.desc[3]), paste("     ", md.valu[3]))
line4 <- paste(paste(md.desc[4]), paste("     ", md.valu[4]))
line5 <- paste(paste(md.desc[5]), paste("      ", md.valu[5]))
line6 <- paste(paste(md.desc[6]), paste("  ", md.valu[6]))

for(y in 1:length(fuelbedVar_name))
{
  if(y == 1)
  {
    condition_out <- maps_cover
    } else
    {
      condition_out <- maps_mfri
    }

  #Set working directory for output map.
  setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_out/r_", run, "_", fuelbedVar_name[y], 
              "/ascii", sep = ""))
  
  #Set up vector to hold file out names
  fuelbed_condition_filenames_out <- vector()
  
  #Save output maps
  for(i in 1:length(intervals))
    {
    #Set up filenames for outgoing maps
    fuelbed_condition_filenames_out[i] <- paste(fuelbedVar_out[y], run, rx_fire, intervals[i], ".asc", sep = "")
    
    #Print header information to map
    cat(line1, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    cat(line2, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    cat(line3, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    cat(line4, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    cat(line5, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    cat(line6, file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    
    #Save stand map.
    cat(c(t(condition_out[[i]])), file = paste(fuelbed_condition_filenames_out[i], sep = ""), fill = T, append = T)#
    print(c(y, i))
  }
}

#################################################################################################
#################################################################################################
#  END





