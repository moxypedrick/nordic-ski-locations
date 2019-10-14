#' 10/12/19
#' @author David Pedrick
#' @description   Practicing location and market search techniques to determine
#' the best city to move to that meets the following criteria
#' 1. Near nordic skiing
#' 2. Access to FS/NPS trailheads
#' 3. City of limited sprawl
#' 4. City of min and max population
#' 5. City with educated population
#' 
#' @note The U.S. Census Bereau defines an urban area as a "core census
#' block groups or blocks that have a population density of at least
#' 1,000 people per square mile and surrounding census blocks that
#' have an overall density of at least 500 people per square mile.
#' 


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
library(xml2)
library(curl)
library(maptools)
library(GISTools)


get_urban_areas_within_buffer <- function(ua, buffer, censusData){
  # ua are the urban area polygons
  # buffer is the 2hr buffer from the nordic centers
  # is it possible to say which buffer was applied to which ua?
  
  buffer <- st_as_sf(buffer) %>%
    st_transform(3857)
  
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
  #print(url_2)
  
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

get_isoline <- function(origin, departure, range_type, range, mode, app_id = '', app_code = '', name = ''){
  #' isoline
  #' 
  #' Sends an isoline request to the HERE Isolines API.
  #' See https://developer.here.com/documentation/routing/topics/resource-calculate-isoline.html. The response is 
  #' post-processed with a smoothing algorithm.
  #' 
  #' @param origin origin coordinates
  #' @param departure departure time
  #' @param range_type range type, either time or distance
  #' @param range range, for `range_type` distance provide units in meters, for `range_type` time provide units in seconds
  #' @param mode transportation mode, either car or pedestrian
  #' 
  #' @param app_id app_id provided by HERE ---- Mtp0R3CkrTuuXGy0HvrX
  #' @param app_code app_code provided by HERE ---- MmRLt66MX33Jl1c6RF3F0Q
  #' 
  #' @return SpatialPolygonsDataFrame
  #format_time = function(x, origin) {
  #  tz = tz_lookup_coords(str_split(origin, ',', simplify = TRUE)[1] %>% as.numeric(),
  #                        str_split(origin, ',', simplify = TRUE)[2] %>% as.numeric(), warn = FALSE)
  #  
  #  ymd_hms(x, tz = tz) %>% format('%Y-%m-%dT%H:%M:%S', tz = tz)
  #}
  formatted_mode = paste0('fastest;', mode, ';traffic:enabled')
  #formatted_departure = format_time(departure, origin)
  #,  '&departure=', formatted_departure
  url = paste0('https://isoline.route.api.here.com/routing/7.2/calculateisoline.xml?',
               '&app_id=', app_id,
               '&app_code=', app_code,
               '&start=geo!', origin,
               '&rangeType=', range_type,
               '&range=', range,
               '&mode=', formatted_mode,
               '&resolution=', 1)
  #print(url)
  text_response <- url %>%
    GET() %>% 
    content(as = "text") %>%
    read_xml()
  #print(text_response)
  #view(text_response)
  #print(url)
  xml_response <- text_response
  #xml_response = read_xml(getURL(url, .mapUnicode = F))
  if('subtype' %in% names(xml_attrs(xml_response))) {
    error_node = xml_attrs(xml_response)[['subtype']]
  } else {
    error_node = NULL
  }
  if (!is.null(error_node)) {
    data = error_node
  } else {
    component = xml_response %>% xml_find_all("//Component//Shape") %>% xml_text()
    if(length(component) == 0) {
      data = 'Unknown error occured. Please try again.'
    } else {
      poly = sapply(1:length(component), function(x) {
        coords = str_split(component[x], ' ') %>% unlist()
        lat = sapply(coords, function(x) { str_split(x, ',')[[1]][1] }) %>% as.numeric()
        lng = sapply(coords, function(x) { str_split(x, ',')[[1]][2] }) %>% as.numeric()
        list(Polygon(as.matrix(data.frame(lng, lat)))) %>% Polygons(ID = x)
      })
      df = data.frame('origin' = rep(origin, length(data)), 'departure' = rep(departure, length(data)),
                      'range_type' = rep(range_type, length(data)), 'range' = rep(range, length(data)),
                      'Name' = rep(name, length(data)))
      #print(length(poly))
      data = SpatialPolygons(poly, proj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) 
      #%>%
      # smooth(method = 'ksmooth', smoothness = 3)
      #view(data)
      #print(length(data))
      if (length(data)>1){
        #print(data)
        data_final <- SpatialPolygonsDataFrame(data[1], data = df, match.ID = FALSE)
        for(i in seq(2,length(data),by=1)){
          #print('loop')
          #print(data[i])
          data_1 <- SpatialPolygonsDataFrame(data[i], data = df, match.ID = FALSE) 
          data_final <- rbind(data_final, data_1)
        }
        data <- data_final
      } else{
        data <- SpatialPolygonsDataFrame(data, data = df, match.ID = FALSE) 
      }
    }
  }
  return(data)
}

get_distance <- function(origin, destination, app_id = '', app_code = ''){
  #' distance
  
  url = paste0('https://route.api.here.com/routing/7.2/calculateroute.json?',
               'app_id=', app_id,
               '&app_code=', app_code,
               '&waypoint0=geo!', origin,
               '&waypoint1=geo!', destination,
               '&mode=fastest;car;traffic:disabled')
  
  text_response <- url %>%
    GET() %>% 
    content(as = "text") %>%
    fromJSON()
  
  return(text_response)
}

qgisWork <- function(){
  # Use st_combine to add them to one feature
  # Then export to shapefile and edit all overlapping lines
  # and lines the loop over themselves. 
  #
  
  st_write(nordic_centers_driving_times_120, "nordic_centers_driving_time_isochrone.shp")
  # Open file in QGIS and eliminate any line that loops over itself
  # Iteratively use the function that determines which features have this problem
  # Read data back into R:
  
  dir = 'C:/Users/dpedrick/Desktop/OneDrive-2019-09-30'
  nordic_centers_45_min_driveshed <- st_read(paste(dir,"/nordic_centers_driving_isochrone.shp",sep = '')) %>%
    st_transform(3857)
  
}

isochrone_from_points_df <- function(points_layer, time){
  # Repeat process to make 60 minute isochrone 
  # this isochrone will be used to cast a wider net over city search
  # using the larger list of cities I'll check to see which of these are
  # near qualifying running, biking, backcountry hiking to establish a score/tradeoff with nordic
  
  # use first variablet to initialize the isochrone layer
  nordic_df <- points_layer
  x = nordic_df[1,"X"]
  y = nordic_df[1,"Y"]
  url2 = paste0(y,",",x)
  name = nordic_df[1,"Name"]
  
  # Initialize the nordic_centers_driving_times_120 SpatialPolygonsDataFrame
  nordic_centers_driving_times_30 <- get_isoline(url2,'10/1/19 14:00:00','time',time,'car','Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q', name)
  
  # Iterate over the nordic areas and get the isochrone for each
  # Add each isochrone to the nordic_centers_driving_times_60 layer
  
  for(i in seq(2,nrow(nordic_df),by=1)){
    x = nordic_df[i,"X"]
    y = nordic_df[i,"Y"]
    url2 = paste0(y,",",x)
    name = nordic_df[i,"Name"]
    isochrone2 <- get_isoline(url2, '10/1/19 14:00:00',  'time',time,'car','Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q', name)
    nordic_centers_driving_times_30 <- rbind(nordic_centers_driving_times_30, isochrone2)
  }
  return (nordic_centers_driving_times_30)
}

isochrone_from_polygons <- function(polygon_layer, time, cityLimit, gridResolution){
  
  # iterate over the urban areas
  # for big urban areas make multiple grid points to generate 
  # isochrone from
  # generate isochrone for each point and then count how many 
  # trailheads are accessible
  urban_areas_within_2hr_nordic_buffer <- polygon_layer
  urban_areas_within_2hr_nordic_buffer <- urban_areas_within_2hr_nordic_buffer[order(urban_areas_within_2hr_nordic_buffer$landArea),]
  centroid <- st_centroid(urban_areas_within_2hr_nordic_buffer[1,]$geometry)
  
  ## Change the coordinates to be Lat Lon
  centroid <- centroid %>%
    st_transform(4326)
  
  #query to make isochrone
  #x = nordic_df[1,"X"]
  y = st_coordinates(centroid)
  url2 = paste0(y[,2],",",y[,1])
  name <- urban_areas_within_2hr_nordic_buffer[1,]$Name
  geoid <- urban_areas_within_2hr_nordic_buffer[1,]$GEOID
  # Initialize the nordic_centers_driving_times_120 SpatialPolygonsDataFrame
  ua_driving_isochrones_120 <- get_isoline(url2,'10/1/19 14:00:00','time',time,'car','Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q', name)
  
  ua_driving_isochrones_120$Name <- name
  ua_driving_isochrones_120$GEOID <- geoid
  print(nrow(urban_areas_within_2hr_nordic_buffer))
  #View(ua_driving_isochrones_120)
  
  # use first variablet to initialize the isochrone layer
  for(i in seq(2, nrow(urban_areas_within_2hr_nordic_buffer),by=1)){
    print(i)
    
    urban_area <- urban_areas_within_2hr_nordic_buffer[i,]$landArea
    name <- urban_areas_within_2hr_nordic_buffer[i,]$Name
    geoid <- urban_areas_within_2hr_nordic_buffer[i,]$GEOID
    
    print(name)
    ua <- urban_areas_within_2hr_nordic_buffer %>% filter(Name == name)
    if(urban_area > cityLimit){
      #rasterize and then make grid of multi-points for isochrone query
      raster_template  <- raster(extent(ua), resolution = gridResolution, 
                                 crs = st_crs(urban_areas_within_2hr_nordic_buffer)$proj4string)
      
      city_r <- rasterize(ua, raster_template , field = 1)
      # convert raster to polygons (i.e., fishnet)
      city_fishnet <- rasterToPolygons(city_r) %>%
        st_as_sf()
      # convert polygons to their centroids 
      city_fishnet_cent <- st_centroid(city_fishnet) %>%
        st_transform(4326)
      
      #plot(city_fishnet_cent)
     
      #print(nrow(city_fishnet_cent))
      for(i in seq(1,nrow(city_fishnet_cent),by=1)){
        x = city_fishnet_cent[i,]$geometry
        y = st_coordinates(x)
        url2 = paste0(y[,2],",",y[,1])
        isochrone2 <- get_isoline(url2, '10/1/19 14:00:00',  'time',time,'car','Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q', name)
        isochrone2$Name <- name
        isochrone2$GEOID <- geoid
        ua_driving_isochrones_120 <- rbind(ua_driving_isochrones_120, isochrone2)
      }
    }
    else {
      centroid <- st_centroid(urban_areas_within_2hr_nordic_buffer[i,]$geometry)
      
      ## Change the coordinates to be Lat Lon
      centroid <- centroid %>%
        st_transform(4326)
      
      #query to make isochrone
      #x = nordic_df[1,"X"]
      y = st_coordinates(centroid)
      url2 = paste0(y[,2],",",y[,1])
      print(url2)
      # Initialize the nordic_centers_driving_times_120 SpatialPolygonsDataFrame
      isochrone2 <- get_isoline(url2,'10/1/19 14:00:00','time',time,'car','Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q', name)
      isochrone2$Name <- name
      isochrone2$GEOID <- geoid
      # Iterate over the nordic areas and get the isochrone for each
      # Add each isochrone to the nordic_centers_driving_times_60 layer
      ua_driving_isochrones_120 <- rbind(ua_driving_isochrones_120, isochrone2)
    }
  }
  #plot(ua_driving_isochrones_120)
  return (ua_driving_isochrones_120)
}

get_nearest_nordic <- function(UA_polygon_layer, nordic_point_layer){
  # Need to list which Nordic Area is closest for each UA and the distance/time to it
  
  #   use api to determine which is closest
  #   add that to the UA data and the time
  
  UA_polygon_layer <- UA_polygon_layer %>%
    st_transform(4326)
  nordic_point_layer <- nordic_point_layer  %>%
    st_transform(4326)
  
  #   iterate over the UA polygons
  
  urban_area_nearest_nordic <- data.frame('na','na','na','na','na','na','na','na')
  names(urban_area_nearest_nordic) <- c("GEOID", "Start", "Nordic1", "Coords1", "Nordic2", "Coords2", "Nordic3", "Coords3")
  
  for(i in seq(1, nrow(UA_polygon_layer), by = 1)){
    df <- data.frame('na',0.0, "na","na")
    names(df) <- c("Name", "distance", "start", "end")
    
    geoid <- UA_polygon_layer[i,]$GEOID
    centroid <- UA_polygon_layer[i,]$geometry 
    #print(centroid)
    centroid <- st_centroid(centroid) 
    y = st_coordinates(centroid)
    
    
    #   iterate over the returned nordic areas
    for(p in seq(1, nrow(nordic_point_layer), by=1)){
      name <- nordic_point_layer[p,]$Name
      z = st_coordinates(nordic_point_layer[p,])
      
      
      oldLat <- y[,1] * 3.14159 / 180 #lat
      oldLon <- y[,2] * 3.14159 / 180 #lon
      newLat <- z[,1] * 3.14159 / 180 #lat
      newLon <- z[,2] * 3.14159 / 180 #lon
      
      x <- sin((oldLat-newLat)/2)**2
      v <- cos(oldLat)
      b <- cos(newLat)
      c <- sin((oldLon - newLon)/2)**2
      sqrt <- sqrt(x+v*b*c)
      asin <- asin(sqrt)*2
      distanceRadians <- asin*180*60
      distance <- distanceRadians / 3.1415926
      
      # calculate distance using lat/lon
      # add to list, get nearest 3 , add to a dataframe
      de <- data.frame(name, distance, toString(y), toString(z))
      names(de) <- c("Name", "distance", "start", "end")
      df <- rbind(df, de)
      
      #get_distance <- get_distance(origin_1, destination_1, app_id = 'Mtp0R3CkrTuuXGy0HvrX', app_code = 'MmRLt66MX33Jl1c6RF3F0Q')
    }
    df <- df[order(df$distance),]
    nearest_nordic <- df[2:4,]
    
    de <- data.frame(geoid, df[2,"start"], df[2,"Name"], df[2,"end"], df[3,"Name"], df[3,"end"], df[4,"Name"], df[4,"end"])
    names(de) <- c("GEOID", "Start", "Nordic1", "Coords1", "Nordic2", "Coords2", "Nordic3", "Coords3")
    
    urban_area_nearest_nordic <- rbind(urban_area_nearest_nordic, de)
    
    #' order dataframe, df
    #' get first three values
    #' add those to ad dataframe that is GOEID, GEOID, GEOID, GEOID
  }
  return(urban_area_nearest_nordic)
}


## Read in Nordic Ski Areas that I have mapped on Google Maps. 
## Use Googles crs for this
## Create 2 hour isochrone for each nordi areas
nordic_df <- read_csv("C:/Users/dpedrick/desktop/OneDrive-2019-09-30/nordic-ski-locations/nordic_ski_area.csv")
Nordic_Centers <- st_as_sf(nordic_df, coords = c("X", "Y"), crs = 4326) 
nordic <- nordicBEFORE

fs_trailheads <- st_read("C:/Users/dpedrick/desktop/OneDrive-2019-09-30/fs_trailheads/fs_trailhead.shp") %>%
  st_transform(26967) %>%
  as_Spatial(.)

nps_trailheads <- st_read("C:/Users/dpedrick/desktop/OneDrive-2019-09-30/np_trailheads/nps_trailheads.shp") %>%
  st_transform(3857) %>%
  as_Spatial(.)

ua <- st_as_sf(
  tigris::urban_areas(cb = TRUE, year = 2017)
) %>%
  st_transform(3857)

pop2010 <- get_acs(geography = "urban area",  
                   variables = c("B01001_001", "B01001H_001", 
                                 "B15002_001", "B15002_015", 
                                 "B15002_032" , "B19013_001"),  
                   year = 2010, output = "wide")

# Download urban areas and transform to common crs
# Download population data and join with urban areas

two_hour_nordic_buffers <- isochrone_from_points_df(nordic_df, 7200)

urban_areas_within_2hr_nordic_buffer <- get_urban_areas_within_buffer(ua, two_hour_nordic_buffers, pop2010)

# Filter by urban areas with less than 5 million people and mutate the 
# units of density, land area, and water area
# This is all urban areas that meet the most important metric - within driving distance of nordic
# now determine which urban areas have best access to tralheads
urban_areas_within_2hr_nordic_buffer <- urban_areas_within_2hr_nordic_buffer %>%
  filter(totalPop < 5000000) %>%
  mutate(
    density = as.integer(totalPop)/(as.numeric(ALAND10)/2589988.1103),
    landArea = as.numeric(ALAND10)/2589988,
    waterArea = as.numeric(AWATER10)/2589988
  ) %>%
  dplyr::select(
    Name = NAME10, 
    GEOID = GEOID10,
    totalPop = totalPop,
    landArea = landArea,
    density = density,
    collegeRate = college_rate,
    uaType = UATYP10,
    lsadType = LSAD10
  )


# Determine area able to access from the UAs that are within 2hrs of nordic skiing
# aggregate the UA buffers by GEOID so there is only one buffer per UA
# Write to file to save progress

two_hr_isochrone_from_UAs_within_Nordic_isochrone <- isochrone_from_polygons(urban_areas_within_2hr_nordic_buffer,7200,300,10000)
two_hr_trail_area_union <- raster::aggregate(two_hr_isochrone_from_UAs_within_Nordic_isochrone, by="GEOID", dissolve=TRUE)

nps_trailheads <- spTransform(nps_trailheads, proj4string(two_hr_trail_area_union))
fs_trailheads <- spTransform(fs_trailheads, proj4string(two_hr_trail_area_union))

two_hr_trail_area_union$nps_trail_count = poly.counts(nps_trailheads, two_hr_trail_area_union)
two_hr_trail_area_union$fs_trail_count = poly.counts(fs_trailheads, two_hr_trail_area_union)


two_hr_trail_area <- st_drop_geometry(st_as_sf(two_hr_trail_area_union))

urban_areas_with_score <- inner_join(urban_areas_within_2hr_nordic_buffer, two_hr_trail_area, by = "GEOID")

urban_areas_with_score <- urban_areas_with_score %>%
  mutate(
    total_trailheads = nps_trail_count + fs_trail_count
  ) 


urban_area_nearest_nordic <- get_nearest_nordic(urban_areas_with_score, Nordic_Centers)

urban_areas_with_score_and_nearest_nordic <- inner_join(urban_areas_with_score, urban_area_nearest_nordic, by = "GEOID")

#writeOGR(urban_areas_with_score_and_nearest_nordic, layer = 'urban_areas_with_score_and_nearest_nordic', 'C:/Users/dpedrick/desktop/OneDrive-2019-09-30/nordic-ski-locations/urban_area_ranking', driver="ESRI Shapefile")
#st_write(urban_areas_with_score_and_nearest_nordic, "urban_areas_with_score_and_nearest_nordic.shp")


#iterate over the urban areas and get time estimates


ua_length <- nrow(urban_areas_with_score_and_nearest_nordic)
urban_areas_with_score_and_nearest_nordic["nordic1_time"] <- 0.0
urban_areas_with_score_and_nearest_nordic["nordic2_time"] <- 0.0
urban_areas_with_score_and_nearest_nordic["nordic3_time"] <- 0.0


for(i in seq(1, ua_length, by=1)){
  start <- strsplit(as.character(urban_areas_with_score_and_nearest_nordic[i,]$Start),",")
  start_b <-paste(gsub(" ","", start[[1]][2],fixed=TRUE),gsub(" ","", start[[1]][1],fixed=TRUE), sep = ",", collapse = NULL)
  
  end1 <- strsplit(as.character(urban_areas_with_score_and_nearest_nordic[i,]$Coords1), ",")
  end1_b <- paste(gsub(" ","", end1[[1]][2],fixed=TRUE),gsub(" ","", end1[[1]][1],fixed=TRUE), sep=",",collapse=NULL)
  end2 <- strsplit(as.character(urban_areas_with_score_and_nearest_nordic[i,]$Coords2), ",")
  end2_b <- paste(gsub(" ","", end2[[1]][2],fixed=TRUE),gsub(" ","", end2[[1]][1],fixed=TRUE), sep=",",collapse=NULL)
  end3 <- strsplit(as.character(urban_areas_with_score_and_nearest_nordic[i,]$Coords3),",")
  end3_b <- paste(gsub(" ","", end3[[1]][2],fixed=TRUE),gsub(" ","", end3[[1]][1],fixed=TRUE), sep=",",collapse=NULL)
  
  #get_distance <- function(origin, destination, app_id = '', app_code = '')
  time1 <- get_distance(start_b, end1_b,'Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q')
  time2 <- get_distance(start_b, end2_b,'Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q')
  time3 <- get_distance(start_b, end3_b,'Mtp0R3CkrTuuXGy0HvrX','MmRLt66MX33Jl1c6RF3F0Q')
  
  urban_areas_with_score_and_nearest_nordic[i,]$nordic1_time <- as.double(time1$response$route$summary$trafficTime)/60
  urban_areas_with_score_and_nearest_nordic[i,]$nordic2_time <- as.double(time2$response$route$summary$trafficTime)/60
  urban_areas_with_score_and_nearest_nordic[i,]$nordic3_time <- as.double(time2$response$route$summary$trafficTime)/60
  
}



#Join urban_areas_with_score_and_nearest_nordic by GEOID with the urban area isochrone layer

two_hr_trail_area_union2 <- st_as_sf(two_hr_trail_area_union)
urban_areas_with_score_and_nearest_nordic2 <- st_as_sf(urban_areas_with_score_and_nearest_nordic)
two_hr_trail_area_union2 <- two_hr_trail_area_union2 %>%
  dplyr::select(
    GEOID = GEOID
  )
two_hr_trail_area_union2 <-  inner_join(two_hr_trail_area_union2, st_drop_geometry(urban_areas_with_score_and_nearest_nordic2), by = "GEOID")
two_hr_trail_area_union3 <- st_union(st_as_sf(two_hr_trail_area_union))


two_hour_nordic_buffers <- st_as_sf(two_hour_nordic_buffers) %>%
  dplyr::select(
    Name = Name, 
    Location = origin,
    Depart_Time = departure,
    Units = range_type,
    Time = range
  )

#' @note Write out finished files for use later and analysis in QGIS
#' 1. urban_areas_with_score_and_nearest_nordic -- the urban area data that has trailhead counts and nordic access information
#' 2. two_hr_trail_area_union -- the 2hr buffer of all urban areas with trailhead counts
#' 3. two_hour_nordic_buffers -- the 2hr buffer of all nordic areas


st_write(urban_areas_with_score_and_nearest_nordic, "urban_areas_with_score_and_nearest_nordic.shp")
st_write(two_hr_trail_area_union2, "two_hr_trail_area_union2.shp")
st_write(two_hour_nordic_buffers, "two_hour_nordic_buffers.shp")



fs_trailheads <- fs_trailheads[two_hr_trail_area_union,]
nps_trailheads <- nps_trailheads[two_hr_trail_area_union,]


#' Read in necessary layers for online map
#'@author David Pedrick
#' urban_areas_with_score_and_nearest_nordic
#' nordic_centers
#' nordic_centers_isochrone
#' urban_areas_isochrone
#' forest_service_trailheads
#' national_parks_trailheads





tmap_mode("view")
tm_shape(two_hour_nordic_buffers) +
  tm_polygons(col="blue", alpha = 0.1) +
  tm_shape(two_hr_trail_area_union3) +
  tm_polygons(col="yellow", alpha=0.1) +
  tm_shape(Nordic_Centers) +
  tm_dots(id = "Name", col = "red", alpha = 1)+
  tm_shape(fs_trailheads) +
  tm_dots(id = "Name", col = "blue", alpha = 1)+
  tm_shape(nps_trailheads)+
  tm_dots(id = "Name", col = "yellow", alpha = 1)+
  tm_shape(urban_areas_with_score_and_nearest_nordic) +
  tm_polygons(col="green", alpha = 0.7, 
              id= "Name", title="45 minute drive-shed") 


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



# use first variablet to initialize the isochrone layer
#x = nordic_df[1,"X"]
#y = nordic_df[1,"Y"]
#url2 = paste0(x,",",y)



## Mapbox
#time <- "60"
#nordic_centers_driving_times <- get_isochrone(url2, st_crs(nordicBEFORE), time)

# Iterate over the nordic areas and get the isochrone for each
# Add each isochrone to the nordic_centers_driving_times layer

#for(i in seq(2,nrow(nordic_df),by=1)){
#  x = nordic_df[i,"X"]
#  y = nordic_df[i,"Y"]
#  url2 = paste0(x,",",y)
#  isochrone2 <- get_isochrone(url2, st_crs(nordicBEFORE), time)
#  nordic_centers_driving_times <- rbind(nordic_centers_driving_times, isochrone2)
#}

# Use st_combine to add them to one feature
# Then export to shapefile and edit all overlapping lines
# and lines the loop over themselves. 

#nordic_centers_driving_time_combine_mapbox <- st_combine(nordic_centers_driving_times) %>%
#  st_transform(3857)


#dir = 'C:/Users/dpedrick/Desktop/OneDrive-2019-09-30'
#nordic_centers_60_min_driveshed <- st_read(paste(dir,"/nordic_centers_driving_60_time_isochrone.shp",sep = '')) %>%
#  st_transform(3857)

# Add a 50 mile buffer to the isochrone to mimic a bigger isochrone
# Mapbox only allows making an 60 minute isochrone


#nordic_centers_60_min_driveshed <- nordic_centers_60_min_driveshed%>%
#  st_transform(3857)


#nordic_centers_big_buffer <- st_buffer(nordic_centers_60_min_driveshed, dist = 160000)

#(nordic_centers_big_buffer, "nordic_centers_60_min_driveshed.shp")


# Load in NP/FS trailhead 
# Subset trailheads by the nordic_centers_big_buffer
# Subset UA by nordic_centers_big_buffer
# Make driveshed for each UA
# Subset trailheads by UA driveshed



#
#Try to get college information
#Add airport/amtrak data to show time back to home
#



