library(tidyverse)
library(rworldmap)
library(sp)
library(feather)
library(leaflet)
library(maptools)
library(rgeos)
library(rgdal)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
options(scipen = '999')

# Read in data
df <- read_feather('data/data.feather')

# Remove any NAs
df <- df %>%
  filter(!is.na(lon),
         !is.na(lat))

# Define function for calculating distance
get_distance <- function(lon1, 
                         lat1, 
                         lon2, 
                         lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# Remove those activities which are suspiciously large
# (ie, the start point is very far from the end point)
too_big = df %>% 
  group_by(activity_id) %>% 
  summarise(start_lat = dplyr::first(lat),
            start_lon = dplyr::first(lon),
            end_lat = dplyr::last(lat),
            end_lon = dplyr::last(lon)) %>%
  ungroup %>%
  mutate(km = get_distance(lon1 = start_lon,
                           lat1 = start_lat,
                           lon2 = end_lon,
                           lat2 = end_lat)) %>%
  mutate(too_big = km >= 100) %>%
  filter(too_big) %>%
  .$activity_id
df <- df %>%
  filter(!activity_id %in% too_big)

# Get a world map, downloaded from http://thematicmapping.org/downloads/world_borders.php
world_sp <- rgdal::readOGR("spatial", "TM_WORLD_BORDERS-0.3")

# Get major populated places from http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-populated-places/
cities <- rgdal::readOGR('spatial', 'ne_10m_populated_places')

# Get which country each activity started in
start_country <-
  df %>% 
  group_by(activity_id) %>% 
  summarise(lat = dplyr::first(lat),
            lon = dplyr::first(lon)) %>%
  ungroup
start_country <- data.frame(start_country)
coordinates(start_country) <- ~lon+lat
proj4string(start_country) <- proj4string(world_sp)
start_country$location <- world_sp@data$NAME[over(start_country, polygons(world_sp))]
start_country <- start_country@data
start_country$country <- as.character(start_country$location)
start_country <- start_country %>%
  dplyr::select(activity_id, country)
df <- df %>%
  left_join(start_country,
            by = 'activity_id')

# Get the nearest city to each activities start point
start_city <-
  df %>% 
  group_by(activity_id) %>% 
  summarise(lat = dplyr::first(lat),
            lon = dplyr::first(lon)) %>%
  ungroup
start_city <- data.frame(start_city)
coordinates(start_city) <- ~lon+lat
proj4string(start_city) <- proj4string(world_sp)
# Get distances (takes a little while)
distances <- gDistance(spgeom1 = start_city,
                       spgeom2 = cities,
                       byid = TRUE)
closest <- apply(distances, 2, function(x){
  which.min(x)
})
start_city$city <- paste0(cities$NAME[closest], ', ',
                          cities$ADM0NAME[closest])
start_city <- start_city@data
df <- left_join(df,
                start_city)

# Define function for leaflet plot of only a country
leaf_country <- function(location,
                         limit = NULL,
                         city = FALSE){ # or NULL
  if(city){
    sub_data <- df %>%
      filter(city == location)
  } else {
    sub_data <- df %>%
      filter(country == location)
  }
  
  
  l <- leaflet() %>%
    addProviderTiles(provider = 'CartoDB.DarkMatter',
                     options = providerTileOptions(opacity = 0.95))
  captured_activities <- sub_data %>%
    group_by(activity_id) %>%
    tally %>%
    .$activity_id
  if(!is.null(limit)){
    if(limit < length(captured_activities)){
      captured_activities <- sample(captured_activities,
                                    limit)
    }
  }
  the_alpha <- ifelse(length(captured_activities) > 50,
                      0.4,
                      ifelse(length(captured_activities) > 100,
                             0.3,
                             0.5))
  for (i in 1:length(captured_activities)){
    message(i)
    this_activity_id <- captured_activities[i]
    sub_sub_data <- sub_data %>%
      filter(activity_id == this_activity_id)
    l <- l %>%
      addPolylines(data = sub_sub_data, lng = ~lon, lat = ~lat,
                   color = 'red',
                   opacity = 1 - the_alpha,
                   weight = 0.3)
  }
  return(l)
}

# uk <- leaf_country(location = 'United Kingdom')
# saveWidget(uk, paste0(getwd(), '/widgets/uk.html'),
#            selfcontained = TRUE)
# 
# spain <- leaf_country(location = 'Spain')
# saveWidget(spain, paste0(getwd(), '/widgets/spain.html'),
#            selfcontained = TRUE)
# 
# holland <- leaf_country(location = 'Netherlands')
# saveWidget(holland, paste0(getwd(), '/widgets/holland.html'),
#            selfcontained = TRUE)
# 
# italy <- leaf_country(location = 'Italy')
# saveWidget(italy, paste0(getwd(), '/widgets/italy'),
#            selfcontained = TRUE)

all_countries <- sort(unique(df$country))
for(i in 1:length(all_countries)){
  this_country <- all_countries[i]
  if(grepl(' ', this_country)){
    file_name <- paste0(unlist(strsplit(this_country, ' ')), collapse = '_')
  } else {
    file_name <- this_country
  }
  file_name <- gsub('(', '', file_name, fixed = TRUE)
  file_name <- gsub(')', '', file_name, fixed = TRUE)
  message(this_country)
  assign(file_name,
         leaf_country(location = this_country))
  saveWidget(get(file_name), paste0(getwd(), '/widgets/',
                                       file_name, '.html'),
             selfcontained = TRUE)
}


all_cities <- sort(unique(df$city))
for(i in 1:length(all_cities)){
  this_city <- all_cities[i]
  message(this_city)
  if(grepl(' ', this_city)){
    file_name <- paste0(unlist(strsplit(this_city, ' ')), collapse = '_')
  } else {
    file_name <- this_city
  }
  file_name <- gsub('(', '', file_name, fixed = TRUE)
  file_name <- gsub(')', '', file_name, fixed = TRUE)
  file_name <- gsub(',', '_', file_name, fixed = TRUE)
  assign(file_name,
         leaf_country(location = this_city,
                      city = TRUE))
  saveWidget(get(file_name), paste0(getwd(), '/widgets/',
                                    file_name, '.html'),
             selfcontained = TRUE)
}

pdf('~/Desktop/world.pdf',
    height = 20,
    width = 35)
map('world',
    fill = TRUE,
    border = NA,
    col = adjustcolor('black', alpha.f = 0))
maps::map('world')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "black")
plot(world_sp, add = TRUE)
all_activities <- unique(df$activity_id)
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(length(unique(df$country)))
colors <- cols[as.numeric(factor(df$country))]
df$color <- colors
cols <- sample(cols, length(cols))
laa <- length(all_activities)
for(i in 1:laa){
  message(paste0(i, ' of ', laa))
  this_activity <- all_activities[i]
  sub_data <- df %>% filter(activity_id == this_activity)
  lines(sub_data$lon,
        sub_data$lat,
        col = adjustcolor(sub_data$color,
                          alpha.f = 0.1),
        cex = 0.01)
}
dev.off()

by_country <- df %>%
  group_by(country) %>%
  summarise(n_activities = length(unique(activity_id))) %>%
  left_join(world_sp@data %>%
            rename(country = NAME) %>%
              dplyr::select(country, AREA, POP2005, LON, LAT)) %>%
  mutate(p_area = n_activities / AREA,
         p_pop = n_activities / POP2005)
