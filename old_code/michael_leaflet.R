# Load package function

loadPkg = function(pkg, character.only = FALSE) { 
  if (!character.only) { pkg <- as.character(substitute(pkg)) }
  if (!require(pkg,character.only=T, quietly =T)) {  install.packages(pkg,dep=T,repos="http://cran.us.r-project.org"); if(!require(pkg,character.only=T)) stop("Package not found") } 
}

# unload/detact package when done using it
unloadPkg = function(pkg, character.only = FALSE) { 
  if(!character.only) { pkg <- as.character(substitute(pkg)) } 
  search_item <- paste("package", pkg,sep = ":") 
  while(search_item %in% search()) { detach(search_item, unload = TRUE, character.only = TRUE) } 
}

# Set wd
setwd(getwd())

# Packages
library(knitr)
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
# Try zooming in on different places of interest
# ==========================================================================================

# Napa Valley Test

california <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lng=-122.2654, lat=38.5025, popup="Napa County") %>%
  flyTo(lng=-122.2654, lat=38.5025, zoom = 50)
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

# ==========================================================================================
# Let's try making a map of the counties in the United States on a color gradient based on
# the area of the county.
# County file from:  https://eric.clst.org/tech/usgeojson/
# ==========================================================================================

us_counties <- rgdal::readOGR("json/us_counties3.json")

pal <- colorNumeric("viridis", NULL)

leaflet(us_counties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, 
              fillColor = ~pal(log10(CENSUSAREA)),
              label = ~paste0(COUNTY, ": ", formatC(CENSUSAREA, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(CENSUSAREA), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

# ==========================================================================================
# Now that we know we can make such a map, let's try to do the same for our POI from the
# wine data set. We'll start with a map of California on a color gradient for quantity of 
# wine in a county.
# California county file from http://boundaries.latimes.com/sets/
# Topographic data from:
# https://raw.githubusercontent.com/deldersveld/topojson/master/countries/us-states/CA-06-california-counties.json
# ==========================================================================================

# We need to relate the counties to quantity of wine from the wine dataset. To do so, we'll have to
# get the quantity of wine produced in each county from our wine dataset and pass it to the map.

# First, we'll need to read in the .geojson file
california_counties2 <- geojsonio::geojson_read("json/california-counties-2012.geojson", what = "sp")

# Inspect the data
california_counties2_tbl <- as_tibble(california_counties2)
california_counties2_tbl

# Grab list of california counties and export to .csv and give to Ryan to add quantity of wine in each county.
california_county_list <- california_counties2_tbl["name"]
write_csv(county_list, 'california_county_list.csv')

# Import .csv of quantity of wine/california county
county_wine <- data.frame(read.csv("production_by_county.csv"))
county_wine_tbl <- as_tibble(county_wine)
county_wine_tbl

# Styling
colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e','#7a0177', '#49006a')
pal <- colorNumeric(colors, NULL, reverse=T)
california_topoData <- readLines("json/california_topography.json") %>% 
  paste(collapse = "\n")

# Basic California map
# California coordinates: 36.7783° N, 119.4179° W

california_basic <- leaflet(california_counties2) %>%
  setView(lng=-119.4179, lat=36.783, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addTopoJSON(california_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .8) 

california_basic

# Map, with wine quantity
california_fill <- leaflet(california_counties2) %>%
  setView(lng=-119.4179, lat=36.783, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, 
              fillColor = ~pal(county_wine$count),
              label = ~paste0(county_wine$county, ": ", formatC(county_wine$count, big.mark = ",")))  %>%
  addTopoJSON(california_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend(pal = pal, values = county_wine$count, opacity = 1.0, title = "Quantity")

california_fill

# Idea: Zoom in on each county and inspect the number of vineyards/wineries in each county

# First, we'll need to read in the .geojson file
ca_avas <- geojsonio::geojson_read("json/ca_avas.geojson", what = "sp")

# Inspect the data
ca_avas_tbl <- as_tibble(ca_avas)
ca_avas_tbl

# Grab list of california counties and export to .csv and give to Ryan to add quantity of wine in each county.
ca_avas_list <- ca_avas_tbl["name"]
write_csv(ca_avas_list, 'data/ca_avas_list.csv')

ca_wine_reviews <- raw_wine_reviews %>% 
  filter(province == "California")

ca_region1 <- ca_wine_reviews$region_1
df_region1 <- data.frame(ca_region1)
write_csv(df_region1, 'data/ca_region1.csv')

ca_region2 <- ca_wine_reviews$region_2
df_region2 <- data.frame(ca_region2)
write_csv(df_region2, 'data/ca_region2.csv')

# Import .csv of quantity of wine/california county
county_wine <- data.frame(read.csv("production_by_county.csv"))
county_wine_tbl <- as_tibble(county_wine)
county_wine_tbl

# Styling
colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e','#7a0177', '#49006a')
pal <- colorNumeric(colors, NULL, reverse=T)
california_topoData <- readLines("json/california_topography.json") %>% 
  paste(collapse = "\n")

# Basic California map
# California coordinates: 36.7783° N, 119.4179° W

california_avas_basic <- leaflet(ca_ava_geojson) %>%
  setView(lng=-119.4179, lat=36.783, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addTopoJSON(california_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .8) 

california_avas_basic

# Map, with wine quantity
california_fill <- leaflet(california_counties2) %>%
  setView(lng=-119.4179, lat=36.783, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, 
              fillColor = ~pal(county_wine$count),
              label = ~paste0(county_wine$county, ": ", formatC(county_wine$count, big.mark = ",")))  %>%
  addTopoJSON(california_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend(pal = pal, values = county_wine$count, opacity = 1.0, title = "Quantity")

california_fill
# ==========================================================================================
# Now let's do the same thing for France
# Provinces geoJSON from:
# https://github.com/gregoiredavid/france-geojson/blob/master/regions.geojson
# Topography JSON from: https://github.com/deldersveld/topojson
# ==========================================================================================

# We need to relate the provinces to quantity of wine from the wine dataset. To do so, we'll have to
# get the quantity of wine produced in each county from our wine dataset and pass it to the map.

# First, we'll need to read in the .geojson file
france_provinces <- geojsonio::geojson_read("json/france_provinces.geojson", what = "sp")

# Inspect the data
france_provinces_tbl <- as_tibble(france_provinces)
france_provinces_tbl

# Grab list of france provinces and export to .csv and give to Ryan to add quantity of wine in each county.
france_province_list <- france_provinces_tbl["nom"]
write_csv(county_list, 'france_province_list.csv')

# Import .csv of quantity of wine/france province
france_province_wine <- data.frame(read.csv(""))
france_province_wine_tbl <- as_tibble(france_province_wine)
france_province_wine_tbl

# Styling
# pal <- colorNumeric(c('#fde0dd', '#fa9fb5', '#c51b8a'), NULL, reverse=T)
colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e','#7a0177', '#49006a')
pal <- colorNumeric(colors, NULL, reverse=T)
france_topoData <- readLines("json/france_topography.json") %>% 
  paste(collapse = "\n")

# Basic France map
# France coordinates: 46.2276° N, 2.2137° E

france_basic <- leaflet(france_provinces) %>%
  setView(lng=2.2137, lat=46.2276, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addTopoJSON(france_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .8) 

france_basic

# Map, with wine quantity
france_fill<- leaflet(france_provinces) %>%
  setView(lng=2.2137, lat=46.2276, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, 
              fillColor = ~pal(france_province_wine$),
              label = ~paste0(france_province_wine$, ": ", formatC(france_province_wine$, big.mark = ",")))  %>%
  addTopoJSON(france_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend(pal = pal, values = france_province_wine$, opacity = 1.0, title = "Quantity")

france_fill

# ==========================================================================================
# Now let's do the same thing for Italy
# Provinces geoJSON from:
# https://github.com/openpolis/geojson-italy/blob/master/geojson/limits_IT_provinces.geojson
# Topography JSON from: 
 # https://github.com/deldersveld/topojson/blob/master/countries/italy/italy-provinces.json?short_path=7b13096
# ==========================================================================================

# We need to relate the provinces to quantity of wine from the wine dataset. To do so, we'll have to
# get the quantity of wine produced in each province from our wine dataset and pass it to the map.

# First, we'll need to read in the .geojson file
italy_provinces <- geojsonio::geojson_read("json/italy_provinces.geojson", what = "sp")

# Inspect the data
italy_provinces_tbl <- as_tibble(italy_provinces)
italy_provinces_tbl

# Grab list of france provinces and export to .csv and give to Ryan to add quantity of wine in each county.
italy_province_list <- italy_provinces_tbl["prov_name"]
write_csv(italy_province_list, 'data/italy_province_list.csv')

# Import .csv of quantity of wine/italy province
italy_province_wine <- data.frame(read.csv(""))
italy_province_wine_tbl <- as_tibble(italy_province_wine)
italy_province_wine_tbl

# Styling
# pal <- colorNumeric(c('#fde0dd', '#fa9fb5', '#c51b8a'), NULL, reverse=T)
colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e','#7a0177', '#49006a')
pal <- colorNumeric(colors, NULL, reverse=T)
italy_topoData <- readLines("json/italy_topography.json") %>% 
  paste(collapse = "\n")

# Basic Italy map
# Italy coordinates: 41.8719° N, 12.5674° E

italy_basic <- leaflet(italy_provinces) %>%
  setView(lng=12.5674, lat=41.8719, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addTopoJSON(italy_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .8) 

italy_basic

# Map, with wine quantity
italy_fill<- leaflet(italy_provinces) %>%
  setView(lng=2.2137, lat=46.2276, zoom = 5) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, 
              fillColor = ~pal(italy_province_wine$),
              label = ~paste0(italy_province_wine$, ": ", formatC(italy_province_wine$, big.mark = ",")))  %>%
  addTopoJSON(italy_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend(pal = pal, values = italy_province_wine$, opacity = 1.0, title = "Quantity")

italy_fill

# ==========================================================================================
# Now let's do the same thing for Spain
# Provinces geoJSON from:
# https://github.com/codeforamerica/click_that_hood/blob/master/public/data/spain-provinces.geojson
# Topography JSON from: 
# https://github.com/deldersveld/topojson/blob/master/countries/spain/spain-province-with-canary-islands.json
# ==========================================================================================

# We need to relate the provinces to quantity of wine from the wine dataset. To do so, we'll have to
# get the quantity of wine produced in each province from our wine dataset and pass it to the map.

# First, we'll need to read in the .geojson file
spain_provinces <- geojsonio::geojson_read("json/spain_provinces.geojson", what = "sp")

# Inspect the data
spain_provinces_tbl <- as_tibble(spain_provinces)
spain_provinces_tbl

# Grab list of france provinces and export to .csv and give to Ryan to add quantity of wine in each county.
spain_province_list <- spain_provinces_tbl["name"]
write_csv(county_list, 'spain_province_list.csv')

# Import .csv of quantity of wine/spain province
spain_province_wine <- data.frame(read.csv(""))
spain_province_wine_tbl <- as_tibble(spain_province_wine)
spain_province_wine_tbl

# Styling
# pal <- colorNumeric(c('#fde0dd', '#fa9fb5', '#c51b8a'), NULL, reverse=T)
colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e','#7a0177', '#49006a')
pal <- colorNumeric(colors, NULL, reverse=T)
spain_topoData <- readLines("json/spain_topography.json") %>% 
  paste(collapse = "\n")

# Basic spain map
# Spain coordinates: 40.4637° N, 3.7492° W

spain_basic <- leaflet(spain_provinces) %>%
  setView(lng=-3.7492, lat=40.4637, zoom = 4) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addTopoJSON(spain_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .8) 

spain_basic

# Map, with wine quantity
spain_fill<- leaflet(spain_provinces) %>%
  setView(lng=3.7492, lat=40.4637, zoom = 4) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, 
              fillColor = ~pal(spain_province_wine$),
              label = ~paste0(spain_province_wine$, ": ", formatC(spain_province_wine$, big.mark = ",")))  %>%
  addTopoJSON(spain_topoData, weight = 1, color = "#444444", fill = FALSE) %>%
  addLegend(pal = pal, values = spain_province_wine$, opacity = 1.0, title = "Quantity")

spain_fill

