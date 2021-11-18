#Source file of Andrew-Data
pacman::p_load(tidyverse,
               drat,
               hurricaneexposuredata,
               hurricaneexposure,
               weathermetrics)

addRepo("geanders")

data("hurr_tracks")
data("rain")

head(hurr_tracks,15)
head(rain, 15)

###########################

hmapper <- function(hurr){
  
  rmap = map_counties(storm = hurr, metric = "rainfall") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap = map_counties(storm = hurr, metric = "wind") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  expos = map_rain_exposure(storm =hurr, 
                            rain_limit = 175, 
                            dist_limit = 500, 
                            days_included =-5:3) +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ml <-  list(rmap, wmap, expos)
  names(ml) <- c("rmap", "wmap", "expos")
  
  return(ml)
}

mapps <- hmapper("Andrew-1992") 


mapps$rmap
mapps$wmap
mapps$expos

###################################

map_counties(storm = "Andrew-1992", metric= "wind", days_included = -5:5) +
  ggtitle("Wind Andrew")

map_counties(storm = "Andrew-1992", metric = "rainfall", days_included = -5:5) +
  ggtitle("rain Andrew")

map_counties(storm = "Andrew-1992", metric= "rainfall", days_included = -1:0) +
  ggtitle("Rain Andrew") #rainfall

########################################

clean_dates <- function(station){
  station <- station %>% filter(MM == 8)
  station <- station %>% filter(DD >= 16 & DD <= 28)
}

########Read the buoy data  

#southern florida 
stmlrf1 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=MLRF1&year=1992&f1=wdir&t1a=gt&v1a=1&t1b=&v1b=&c1=and&f2=wtmp&t2a=gt&v2a=1&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

#florida keys
stsmkf1 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=SMKF1&year=1992&f1=wdir&t1a=gt&v1a=1&t1b=&v1b=&c1=and&f2=wtmp&t2a=gt&v2a=1&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

#bahamas
st41016 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=41016&year=1992&f1=wspd&t1a=ge&v1a=1&t1b=&v1b=&c1=and&f2=wtmp&t2a=ge&v2a=1&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

#east gulf
st42003 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=42003&year=1992&f1=wspd&t1a=ge&v1a=1&t1b=&v1b=&c1=and&f2=wtmp&t2a=ge&v2a=1&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

#west gulf
stbusl1 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=BUSL1&year=1992&f1=wspd&t1a=gt&v1a=1&t1b=&v1b=&c1=&f2=&t2a=&v2a=&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

stgbcl1 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=GBCL1&year=1992&f1=wspd&t1a=gt&v1a=1&t1b=&v1b=&c1=&f2=&t2a=&v2a=&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

##near texas
st42002 <- clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=42002&year=1992&f1=wspd&t1a=gt&v1a=1&t1b=&v1b=&c1=&f2=&t2a=&v2a=&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))

st42019 <-  clean_dates(read.table(url("https://www.ndbc.noaa.gov/histsearch.php?station=42019&year=1992&f1=wspd&t1a=gt&v1a=1&t1b=&v1b=&c1=&f2=&t2a=&v2a=&t2b=&v2b=&c2=&f3=&t3a=&v3a=&t3b=&v3b=&mode=data"), skip = 2, header = TRUE))