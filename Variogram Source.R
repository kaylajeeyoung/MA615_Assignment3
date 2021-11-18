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
st42002$Date <- str_c(st42002$YY, st42002$MM, st42002$DD)
st42003$Date <- str_c(st42003$YY, st42003$MM, st42003$DD)
st42019$Date <- str_c(st42019$YY, st42019$MM, st42019$DD)
stbusl1$Date <- str_c(stbusl1$YY, stbusl1$MM, stbusl1$DD)
stgbcl1$Date <- str_c(stgbcl1$YY, stgbcl1$MM, stgbcl1$DD)
stmlrf1$Date <- str_c(stmlrf1$YY, stmlrf1$MM, stmlrf1$DD)
stsmkf1$Date <- str_c(stsmkf1$YY, stsmkf1$MM, stsmkf1$DD)
st
#lubridate package

#filter out uneeded columns
#probably a way to write a function for this?
b41016 <- st41016[-c(1:5, 8:11, 13:16)]
b42002 <- st42002[-c(1:5, 8:11, 13:16)]
b42003 <- st42003[-c(1:5, 8:11, 13:16)]
b42019 <- st42019[-c(1:5, 8:11, 13:16)]
bBUSL1 <- stbusl1[-c(1:5, 8:11, 13:16)]
bGBCL1 <- stgbcl1[-c(1:5, 8:11, 13:16)]
bMLRF1 <- stmlrf1[-c(1:5, 8:11, 13:16)]
bSMKF1 <- stsmkf1[-c(1:5, 8:11, 13:16)]

#add buoy name
b41016$buoy <- "41016"
b42002$buoy <- "42002"
b42003$buoy <- "42003"
b42019$buoy <- "42019"
bBUSL1$buoy <- "BUSL1"
bGBCL1$buoy <- "GBCL1"
bMLRF1$buoy <- "MLRF1"
bSMKF1$buoy <- "SMKF1"

#turn this into a function that will run for each buoy
meantables<- function(i){
mean_WSPD <- i %>%
  group_by(Date) %>%
  summarize(mean_WSPD = mean(WSPD, na.rm = TRUE)) 
mean_GST <- i %>%
  group_by(Date) %>% 
  summarize(mean_GST = mean(GST, na.rm = TRUE)) 
mean_BAR <- i %>%
  group_by(Date) %>%
  summarize(mean_BAR = mean(BAR, na.rm = TRUE))
mean_data <- left_join(mean_WSPD, mean_GST, by="Date")
mean_data <- left_join(mean_data, mean_GST, by="Date")
return(mean_data)
}
meantables(bSMKF1)

#make a table of all the buoys
#something along the lines of this
vario_data <- left_join(mean_WSPD, mean_GST, by="Date")
vario_data <- left_join(vario_data, mean_BAR, by="Date")

#then add on location of each buoy

#then I think I can make the variogram for each day

#in for loop add column for latitude and longitude that goes to the buoy 