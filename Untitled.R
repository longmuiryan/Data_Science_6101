# Load package function

loadPkg = function(pkg, character.only = FALSE) { 
  if (!character.only) { pkg <- as.character(substitute(pkg)) }
  if (!require(pkg,character.only=T, quietly =T)) {  install.packages(pkg,dep=T,repos="http://cran.us.r-project.org"); if(!require(pkg,character.only=T)) stop("Package not found") } 
}
loadPkg(knitr)

# unload/detact package when done using it
unloadPkg = function(pkg, character.only = FALSE) { 
  if(!character.only) { pkg <- as.character(substitute(pkg)) } 
  search_item <- paste("package", pkg,sep = ":") 
  while(search_item %in% search()) { detach(search_item, unload = TRUE, character.only = TRUE) } 
}

# Set wd
setwd(getwd())

# Packages
library(leaflet)
library(maps)
library(rgdal)
library(geojsonio)
library(rjson)
library(jsonlite)
library(tibble)
library(dplyr)
library(magrittr)
library(tidyverse)

# ==========================================================================================
# ==========================================================================================



# Test basic coordinates

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  
m  # Print the map

# ==========================================================================================
# ==========================================================================================

# Test map

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

                           
flyTo(mapStates, lng=38.50, lat=122.32, zoom = 18)
# ==========================================================================================
# ==========================================================================================

# Napa Valley Test
    
california <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(lng=-122.2654, lat=38.5025, popup="Napa County") %>%
      # flyTo(lng=-122.2654, lat=38.5025, zoom = 50)
      # Napa Valley lat=38.5025 N, lng=122.2654 W
california

# A smol joke

zzyzx <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lng=-116.1039, lat=35.1428, popup="Zzyzx, Wine Capital of the World") %>%
  flyTo(lng=-116.1039, lat=35.1428, zoom = 30)
  # Zzyzx lat=35.1428 N, llng=116.1039 W
zzyzx

#California Counties

# california_counties <- rgdal::readOGR("/Users/perry/Documents/Git/Intro To Data Science/edwinbet/california-counties-2012.json")
california_counties2 <- geojsonio::geojson_read("json/california-counties-2012.geojson", what = "sp")

california_counties2_tbl <- as_tibble(california_counties2)
california_counties2_tbl

county_list <- california_counties2_tbl["name"]
write_csv(county_list, 'california_county_list.csv')
x <- toJSON(res1)
cat(x)

# Map, no fill
leaflet(california_counties2) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1) 
            
# Map, with fill
leaflet(california_counties2) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, 
              fillColor = ~pal(log10(CENSUSAREA)),
              label = ~paste0(COUNTY, ": ", formatC(CENSUSAREA, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(CENSUSAREA), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

# Trying to readin GeoJSON

us_states <- rgdal::readOGR("json/us_states.json")
us_counties <- rgdal::readOGR("json/us_counties3.json")

pal <- colorNumeric("viridis", NULL)

leaflet(us_counties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, 
      fillColor = ~pal(log10(CENSUSAREA)),
      label = ~paste0(COUNTY, ": ", formatC(CENSUSAREA, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(CENSUSAREA), opacity = 1.0,
      labFormat = labelFormat(transform = function(x) round(10^x)))

# ========================================================================
# ========================================================================

# Attempting to conver json to df

result <- fromJSON(file = "json/us_counties2.json")
json_df <- as.data.frame(result)
summary(json_df)

california_counties <- subset(json_df, COUNTY == 6)

setwd(getwd())

# Read in data set
counties <- fromJSON(readLines("json/us_counties3.json"))
df <- data.frame(counties)

# Check structure

str(df)

# Flatten?

counties_flat <- flatten(df) %>%
str(counties_flat)
names(counties_flat)

# Table

counties_tbl <- as_data_frame(counties_flat)
counties_tbl

counties_tbl %>% mutate(features.geometry.coordinates = as.character(features.geometry.coordinates)) %>% select(features.geometry.coordinates)


## Scratch

# Test counties              

us_counties <- geojsonio::geojson_read("json/us_counties3.json", what = "sp")
names(us_counties)

pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(COUNTY))n 
labFormat = labelFormat(transform = function(x) round(10^x))


# ==========================================================================================
# ==========================================================================================



