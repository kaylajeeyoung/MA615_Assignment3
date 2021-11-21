#Variograms

#source data
source(file= "Andrew_Data_Source.R")
pacman::p_load(gstat, tidyverse, stringr, tidyr)

#locations of each buoy

Buoy <- c("41016", "42002", "42003", "42019", "BUSL1", "GBCL1", "MLRF1", "SMKF1")
latitude <- c(24.600, 26.055, 25.925, 27.910, 27.883, 27.800, 25.012, 24.628)
longitude <- c(-76.500, -93.646, -85.615, -95.345, -90.900, -93.100, -80.376, -81.109)
location_data <- data.frame(Buoy, latitude, longitude)

#combine YY MM DD
st41016$Date <- format(as.Date(with(st41016, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
st42002$Date <- format(as.Date(with(st42002, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
st42003$Date <- format(as.Date(with(st42003, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
st42019$Date <- format(as.Date(with(st42019, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
stbusl1$Date <- format(as.Date(with(stbusl1, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
stgbcl1$Date <- format(as.Date(with(stgbcl1, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
stmlrf1$Date <- format(as.Date(with(stmlrf1, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")
stsmkf1$Date <- format(as.Date(with(stsmkf1, paste(YY, MM, DD,sep="-")), "%Y-%m-%d"), "19%y-%m-%d")

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
ignore <- left_join(mean_WSPD, mean_GST, by="Date")
mean_data <- left_join(ignore, mean_BAR, by="Date")
return(mean_data)
}
#that works just have to do for each buoy
b41016 <- meantables(b41016)
b42002 <- meantables(b42002)
b42003 <- meantables(b42003)
b42019 <- meantables(b42019)
bBUSL1 <- meantables(bBUSL1)
bGBCL1 <- meantables(bGBCL1)
bMLRF1 <- meantables(bMLRF1)
bSMKF1 <- meantables(bSMKF1)

#add buoy name to the mean tables
b41016$Buoy <- "41016"
b42002$Buoy <- "42002"
b42003$Buoy <- "42003"
b42019$Buoy <- "42019"
bBUSL1$Buoy <- "BUSL1"
bGBCL1$Buoy <- "GBCL1"
bMLRF1$Buoy <- "MLRF1"
bSMKF1$Buoy <- "SMKF1"

#then join them together
all_buoy <- rbind(b41016, b42002, b42003, b42019, bBUSL1, 
                  bGBCL1, bMLRF1, bSMKF1)
all_buoy <- left_join(all_buoy, location_data, by= "Buoy")



#make a variogram
library(sp)

#make co just to keep all_buoy as a data frame as well
co <- all_buoy
coordinates(co) <- ~latitude+longitude

#variogram of windspeed
vario_wspd <- variogram(mean_WSPD~1, co)
fit1 <- fit.variogram(vario_wspd, model=vgm("Gau", psill = 14, range = 6), fit.method= 6)
plot(vario_wspd, fit1)

#variogram of windgust
vario_gst <- variogram(mean_GST~1, co)
fit2 <- fit.variogram(vario_gst, model=vgm("Gau", psill = 200, range = 2), fit.method= 6)
plot(vario_gst, fit2)

#variogram of barometric pressure
vario_bar <- variogram(mean_BAR~1, co)
fit3 <- fit.variogram(vario_bar, model=vgm("Gau", psill = 40000000, range = 3.2), fit.method= 6)
plot(vario_bar, fit3)


#function to separate the dates into useful information
separate_dates <- function(buoy){
  date_matrix <- buoy$Date %>% str_split_fixed("-", 3)
  buoy$Year <- date_matrix[,1]
  buoy$Month <- date_matrix[,2]
  buoy$Day <- date_matrix[,3]
  invisible(buoy)
}
all_buoy <- separate_dates(all_buoy)

#combine andrew and buoy
andrew_buoy <- inner_join(all_buoy, andrew, "Day")
andrew_buoy <- andrew_buoy[-c(1,13)]

#obtain base map
map_base <- default_map()
map_base <- map_base$data
base_map <- ggplot(map_base, aes(long, lat)) + 
  geom_polygon(aes(group = group), fill = "white", color = "black")

#add andrew
andrew <- andrew %>% filter(Day > 19)
all_buoy <- all_buoy %>% filter(Day > 19)
base_map + 
  geom_path(data = andrew, aes(x = longitude, y = latitude), color = "blue", size = 1) + 
  geom_point(data = location_data, aes(x = longitude, y = latitude, color = Buoy)) + 
  ggtitle("Hurricane Andrew's path and nearby buoys") + 
  theme_bw()

  
