#Variograms

#source data
source(file= "Andrew_Data_Source.R")
pacman::p_load(gstat, tidyverse, stringr, tidyr)

#locations of each buoy

Buoy <- c("41016", "42002", "42003", "42019", "BUSL1", "GBCL1", "MLRF1", "SMKF1")
N <- c(24.600, 26.055, 25.925, 27.910, 27.883, 27.800, 25.012, 24.628)
W <- c(76.500, 93.646, 85.615, 95.345, 90.900, 93.100, 80.376, 81.109)
location_data <- data.frame(Buoy, N, W)

#combine YY MM DD
st41016$Date <- str_c(st41016$YY, st41016$MM, st41016$DD)

#format as a date (NOT WORKING)
st41016$Date <- as.numeric(as.Date(st41016$Date))



#take each buoy and find the average over each day for each variable

#make that a new data frame 

#then add on location of each buoy

#then I think I can make the variogram for each day

#in for loop add column for latitude and longitude that goes to the buoy 