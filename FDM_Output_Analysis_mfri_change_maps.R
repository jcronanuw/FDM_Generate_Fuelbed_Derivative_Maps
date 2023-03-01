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

#Computer
desk <- "C:/Users/jcron/"
comp <- desk

#External drive location
ed <- "E"

#Set this to one if you want to export a key that explains what variable each file
#prefix denotes
prefixKey <- 0

#FFT OUPUT
#Variable of interest
#List column number in fft output table for this variable:
#List name of variable as it will appear in outgoing file structure.
fccsVar_col <- c(3,4,5,6,7,8,9)
fccsVar_name <- c("fineFuelLoad", "forestFloorLoad", "totalFuelLoad","flameLength", "rateOfSpread", 
                  "availableFuels", "crownFirePotential")

#Create definitions for change maps
fccsVar_changeName <- vector()
for(i in 1:length(fccsVar_name))
  {
  fccsVar_changeName[i] <- paste("change_in_", fccsVar_name[i], sep = "")
  }

#FUELBED CONDITIONS
#Variable of interest
#List name of variable as it will appear in outgoing file structure.
fuelbedVar_name <- c("cover", "mfri")
fuelbedVar_out <- c("r", "s")

#Run Parameters
type_in <- "fab_"
type_out <- c("t", "u", "v", "w", "x", "y", "z")
type_out_change <- c("c", "d", "e", "f", "g", "h", "i")

#Create a table that provides definitions for file prefix letters
file_prefix_key <- data.frame(prefix = c(fuelbedVar_out, type_out,
                                         type_out_change),
                              definition = c(fuelbedVar_name, fccsVar_name,
                                             fccsVar_changeName))

#Save table
if(prefixKey == 1)
  {
  setwd(paste(ed, ":/FDM_Simulations_Post_Processing_Step_01", sep = ""))
  write.csv(file_prefix_key, file = paste("file_prefix_key.csv", sep = ""))
  } else {}


run_in <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010")
rx_fire <- "075"
rx2 <- "337"
intervals <- c("05", "10", "15", "20", "25", "30", "35", "40", "45", "50")
#intervals <- c("00", "05", as.character(seq(10,50,5)))

#Zero value fuelbeds
zeros <- c(5099000, 6000000)

#Create a lookup table to be printed to the directory as a reference.
file_out_lookup <- data.frame(file_start_letter = type_out, hazard_measure = fccsVar_name)

#Save file
# --- Jan 18, 2023 ---- DO YOU NEED THIS?
#setwd(paste("C:/usfs_cronan_gis/SEF/FDM_IAWF_runs/run_", run, "_out", sep = ""))
#cat(c(t(file_out_lookup)), file = "file_out_lookup.txt", fill = T, append = T)#

#################################################################################################
#Set working directory
setwd(paste(comp, "Documents/GitHub/FDM_Generate_Fuelbed_Derivative_Maps/inputs", sep = ""))


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
setwd(paste(comp, "Documents/FDM_2023_Simulation_Data/Step_01_FDM_Outputs/results_rab_050k_001", 
            sep = ""))

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster("fab_050k_00122505.asc")

#################################################################################################
setwd(paste(comp, "Documents/GitHub/EglinAirForceBase/inputs", 
            sep = ""))

#Open year 0 fuelbed map
f.map <- matrix(scan("sef_fmap_v2_1771x3491.txt", skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)

for(aa in 1:length(run_in))
  {
  
  #Create shortened run identifer (2 instead of 3 integers) for outgoing file names
  run_out <- substring(run_in[aa], 2, 3)
  
  #Set up filenames for incoming FDM maps
  filenames_in <- vector()
  for(i in 1:length(intervals))
  {
    filenames_in[i] <- paste(type_in, rx_fire, "k_", run_in[aa], rx2, intervals[i], ".asc", sep = "")
  }
  
  #################################################################################################
  setwd(paste(comp, "Documents/FDM_2023_Simulation_Data/Step_01_FDM_Outputs/results_rab_", 
            rx_fire, "k_", run_in[aa], 
            sep = ""))

  #Set up a list to hold input and output maps
  maps_in <- list()

  #Import .asc files
  for(i in 1:length(intervals))
    {
  maps_in[[i]] <- matrix(scan(filenames_in[i],skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
  }

  #Add year 0 maps to catalog of output maps
  intervals2 <- c("00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50")
  maps_in2 <- list()
  for(i in 1:length(intervals2))
  {
    if(i == 1)
    {
      maps_in2[[i]] <- f.map
    } else
    {
      maps_in2[[i]] <- maps_in[[i - 1]]
    }
  }
  
  #Remove maps_in (replaced with maps_in2)
  rm(maps_in)
  
  #################################################################################################
  #Swicth Working Directories
  setwd(paste(comp, "Documents/GitHub/EglinAirForceBase/inputs", sep = ""))
  #Import master fuelbed lookup table
  lut <- read.csv("sef_lut_all.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  
  #Subset fuelbeds that were deleted, but remain on FCCS basemap
  deleted <- lut$fuelbed[lut[[12]] == 0]

  #Identify their replacements
  replacements <- lut$post_1[lut[[12]] == 0]

  #Replace deleted fuelbeds with replacements for each map.
  for(i in 1:length(intervals2))
    {

    if(is.list(mapply(function(x) replacements[deleted %in% x], 
                    maps_in2[[i]][maps_in2[[i]] %in% deleted])) == T)
      {
      maps_in2[[i]] <- maps_in2[[i]]
      } else
        {
          #Replace deleted fuelbeds
          maps_in2[[i]][maps_in2[[i]] %in% deleted] <- mapply(function(x) replacements[deleted %in% x], 
                                                      maps_in2[[i]][maps_in2[[i]] %in% deleted])
        }
    }

  #################################################################################################
  #################################################################################################
  #################################################################################################
  #################################################################################################
  #GENERATE MAPS THAT SHOW FUELBED CONDITIONS
  
  #################################################################################################
  #################################################################################################
  #Remove the -9999 as No Value and replace with 1000000 so I can disect fuelbed numbers.
  for(i in 1:length(maps_in2))
  {
    maps_in2[[i]][maps_in2[[i]] == -9999] <- 1000000
  }
  
  #################################################################################################
  #################################################################################################
  #Create vectors and remove scientific notation
  maps_in_character <- list()
  for(i in 1:length(maps_in2))
  {
    options(scipen = 7)
    maps_in_character[[i]] <- as.character(maps_in2[[i]])
  }  
  
  #Change to vector
  vector_in_character <- list()
  for(i in 1:length(maps_in2))
  {
    vector_in_character[[i]] <- as.vector(maps_in_character[[i]])
  }  
  
  #Create lists to hold ouputs
  maps_mfri <- list()
  
  #Split out digits from fuelbed numbers
  for(i in 1:length(maps_in2))
  {
    split_0 <- unlist(strsplit(vector_in_character[[i]], vector()))
    split_mfri <- as.numeric(split_0[seq(5,length(split_0),7)])
    maps_mfri[[i]] <-  matrix(split_mfri, f.head@nrows, f.head@ncols)
    rm(split_0, split_mfri)
    }

#################################################################################################
#################################################################################################
#CREATE CHANGE MAPS FOR MFRI MAP

#Set up filenames for outgoing maps
filenames_out_change <- vector()
for(i in 1:length(intervals2))
{
  filenames_out_change[i] <- paste("b", rx_fire, "_", run_out, "_", intervals2[i], ".asc", sep = "")
}

mfri_out_change <- list()
map_numbers <- 1:length(intervals2)


for(i in 1:length(intervals2))
{
  mfri_out_change[[i]] <- maps_mfri[[map_numbers[i]]] - maps_mfri[[1]]
}

for(i in 1:length(filenames_out_change))
{
  mfri_out_change[[i]][maps_in2[[1]] %in% zeros] <- -8888
  mfri_out_change[[i]][maps_in2[[1]] == -9999] <- -9999
}

#################################################################################################
#################################################################################################
#Reset working directory
setwd(paste(ed, ":/FDM_2023_Simulation_Data/Step_02_Fuelbed_Derivative_Maps/maps", sep = ""))

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

#Save output maps
for(i in 1:length(intervals2))
{
  #Save stand map.
  #Combine metadata and pixel attribute into a single vector.
  a <- c(line1, line2, line3, line4, line5, line6, t(mfri_out_change[[i]]))
  
  #Name file
  file <- filenames_out_change[i]
  write.table(a, file=file, row.names=FALSE, col.names=FALSE, sep=", ",
              append=TRUE, quote=FALSE)
}

print(aa)
}
  #################################################################################################
  #################################################################################################
  #  END
  
  
  
  
  
  