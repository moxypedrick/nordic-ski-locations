
library(tidyverse)
library(stringr)     

library(sf)     
library(raster)
library(sp)


library(mapview)
library(tmap)

library(tidycensus)
library(tigris)
library(lehdr)

library(httr) 
library(jsonlite)
library(viridis)
library(ggpubr)
library(rgdal)



nordic_df <- read_csv("C:/Users/dpedrick/desktop/nordic_ski_area.csv")
Nordic_Centers <- st_as_sf(nordic_df, coords = c("X", "Y"), crs = 4326) 
nordic <- nordicBEFORE

# use first variablet to make isochrone
x = nordic_df[1,"X"]
y = nordic_df[1,"Y"]
url2 = paste0(x,",",y)


time <- "45"
nordic_centers_driving_times <- get_isochrone(url2, st_crs(nordicBEFORE), time)


for(i in seq(2,nrow(nordic_df),by=1)){
  x = nordic_df[i,"X"]
  y = nordic_df[i,"Y"]
  url2 = paste0(x,",",y)
  isochrone2 <- get_isochrone(url2, st_crs(nordicBEFORE), time)
  nordic_centers_driving_times <- rbind(nordic_centers_driving_times, isochrone2)
  
}


nordic_centers_driving_time_combine <- st_combine(nordic_centers_driving_times) %>%
  st_transform(3857)

nordic_centers_driving_time_union <- st_union(nordic_centers_driving_times) %>%
  st_transform(3857)




nordic_centers <- st_buffer(nordic, dist = 160000)

nordic_centers_union <- st_union(nordic_centers)

st_write(nordic_centers_driving_times, "nordic_centers_driving_times.shp")

#Fix overlappying polygon lines in QGIS
#
#

dir = 'C:/Users/dpedrick/Desktop/OneDrive-2019-09-30'
# Read in data:

## Read in City of Atlanta property data

nordic_centers_45_min_driveshed <- st_read(paste(dir,"/nordic_centers_driving_isochrone.shp",sep = '')) %>%
  st_transform(3857)


#
#
#Try to get college information
#Add airport/amtrak data to show time back to home
#


ua <- st_as_sf(
  tigris::urban_areas(cb = TRUE, year = 2017)
) %>%
  st_transform(3857)


pop2010 <- get_acs(geography = "urban area",  
                              variables = c("B01001_001", "B01001H_001", 
                                            "B15002_001", "B15002_015", 
                                            "B15002_032" , "B19013_001"),  
                              year = 2010, output = "wide")

nordic_ua_joined <- get_urban_areas_within_buffer(ua, nordic_centers_union, pop2010)
Urban_Areas_within_drive_time <- get_urban_areas_within_buffer(ua, nordic_centers_driving_time_union, pop2010)


nordic_ua_mapbox_buffer <- nordic_ua_mapbox_buffer %>%
  filter(totalPop < 5000000) %>%
  mutate(
    density = (as.integer(ALAND10)/2589988.1103)/as.integer(totalPop),
    landArea = as.integer(ALAND10)/2589988,
    waterArea = as.integer(AWATER10)/2589988
  )

nordic_ua_mapbox_buffer <- nordic_ua_mapbox_buffer %>%
  filter(totalPop <1000000, landArea < 100, totalPop>5000) %>%
  dplyr::select(
    Name = NAME10, 
    GEOID = GEOID10,
    totalPop = totalPop,
    landArea = landArea,
    density = density,
    collegeRate = college_rate
  )


#The U.S. Census Bureau defines an urban area as 
#"core census block groups or blocks that have a 
#population density of at least 1,000 people per 
#square mile (386 per square kilometer) and 
#surrounding census blocks that have an overall 
#density of at least 500 people per square mile 
#(193 per square kilometer)".


tmap_mode("view")
tm_shape(nordic_centers_45_min_driveshed) +
  tm_polygons(col="yellow", alpha = 0.4, title="Urban Areas near Nordic Centers ")  +
  tm_shape(Urban_Areas_within_drive_time) +
  tm_polygons(col="green", alpha = 0.7, 
              id= "Name", title="45 minute drive-shed") +
  tm_shape(Nordic_Centers) +
    tm_dots(id = "Name", col = "red", alpha = 1)
  

get_urban_areas_within_buffer <- function(ua, buffer, censusData){
  nordic_ua <- ua[buffer,]
 
  nordic_ua <- nordic_ua %>% 
    mutate(
      GEOID = GEOID10
    )
  
  
  pop2010 <- censusData 
  
  data2010 <- pop2010 %>% 
    mutate(
      totalPop = B01001_001E,
      total_grt_25 = B15002_001E,
      men_college =  B15002_015E, 
      women_college = B15002_032E,
      college_rate = (men_college+women_college)/total_grt_25, 
      median =  as.numeric(B19013_001E),
      log_transformation = log(median)
    ) %>%
    dplyr::select(GEOID, totalPop, total_grt_25, men_college, 
                  women_college, college_rate, median, log_transformation)
  
  nordic_ua_joined <- inner_join(nordic_ua, data2010, by = "GEOID")
  
  
  return(nordic_ua_joined)
}

get_isochrone <- function(nordic_UA, nordic_crs, bufferTime){
  #nordic_df <- nordic_UAs
  accessToken = "pk.eyJ1IjoibW94eXBlZCIsImEiOiJjaWgydGpwdmYweHJydnFtMzZzOXpmNjg3In0.5TXWYv0Z7nsOZHneIQOhxg"
  bufferTime = bufferTime
  url_1 <- "https://api.mapbox.com/isochrone/v1/mapbox/driving/"
  url_3 <- paste0("?contours_minutes=",bufferTime,"&polygons=true&access_token=pk.eyJ1IjoibW94eXBlZCIsImEiOiJjaWgydGpwdmYweHJydnFtMzZzOXpmNjg3In0.5TXWYv0Z7nsOZHneIQOhxg") 
  #url_2 <- "-84.380683,33.767111"
  url_2 <- nordic_UA
  print(url_2)
  
  nordicBEFORE <- nordic_crs

  result1_raw <<-
    paste0(url_1, url_2, url_3) %>% 
    GET() %>% 
    content(as = "text") %>%
    fromJSON()
  
  listLength <- length(result1_raw$features$geometry$coordinates[[1]])
  # read in output from Google 
  
  
  firstSegment = listLength/2
  start = (firstSegment) + 1
  end = listLength
  
  b <- cbind(result1_raw$features$geometry$coordinates[[1]][1:firstSegment], result1_raw$features$geometry$coordinates[[1]][start:end])
  
  py1 <- coords2Polygons(b, ID = "A") 
  isochrone <- st_as_sf(py1) 
  st_crs(isochrone) <- nordicBEFORE
  #isochrone <- isochrone %>%
  #  st_transform(26967)

  
  return(isochrone)
  
}


###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
