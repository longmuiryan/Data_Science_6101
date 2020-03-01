
setwd("~/Desktop/Git/edwinbet")

# =============================================================================
# Maps  
# =============================================================================

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(geojsonio)


# =============================================================================
# Data Cleaning 
# =============================================================================

# Raw data 
raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

# -----------------------------------------------------------------------------
# Classify region 
# -----------------------------------------------------------------------------

# Regions 
france.v <- c("Bordeaux", "Burgundy", "Alsace", "Loire Valley", "Champagne",
              "Southwest France", "Provence", "Rhône Valley", "Beaujolais", "France Other")
italy.v <- c("Tuscany", "Veneto", "Northeastern Italy", "Sicily & Sardinia", 
             "Southern Italy", "Central Italy", "Catalonia", "Italy Other")
spain.v <- c("Northern Spain", "Douro", "Spain Other")
states.v <- c("California", "Washington", "Oregon", "Piedmont", "New York",
              "Virginia") 

wine_reviews <- raw_wine_reviews %>% 
  mutate(country = case_when(
    province %in% states.v ~ "United States", 
    province %in% france.v ~ "France",
    province %in% italy.v ~ "Italy",
    province %in% spain.v ~ "Spain" )) %>%
  filter(!is.na(country))

# -----------------------------------------------------------------------------
# Classify California county 
# -----------------------------------------------------------------------------

# Counties 
napa <- c("Napa Valley")
sonoma <- c("Sonoma", "Russian River Valley", "Sonoma Coast", "Sonoma County", 
  "Carneros", "Dry Creek Valley")
santa_barbara <- c("Santa Barbara County", "Sta. Rita Hills")
san_luis_obispo <- c("San Luis Obispo", "Paso Robles")

ca_wine_reviews <- raw_wine_reviews %>% 
  filter(province == "California") %>% 
  mutate(county = case_when(
    region_1 %in% napa ~ "Napa",
    region_1 %in% sonoma ~ "Sonoma",
    region_1 %in% santa_barbara ~ "Santa Barbara",
    region_1 %in% san_luis_obispo ~ "San Luis Obispo" )) %>%
  filter(!is.na(county))

# -----------------------------------------------------------------------------
# Prepare map data 
# -----------------------------------------------------------------------------

# Grab the geojson & json 
ca_geojson <- geojsonio::geojson_read("california-counties-2012.geojson", what = "sp")
ca_topo <- readLines("json/california_topography.json") %>% paste(collapse = "\n")

# Grab the counties from the geojson
ca_counties <- tibble(california_counties$name) %>% setNames("county")

# Merge in w/ wine reviews 
ca_county_count <- ca_wine_reviews %>% 
  group_by(county) %>% 
  summarise(count = n()) %>%
  right_join(ca_counties, by = "county") %>% 
  mutate(count = ifelse(is.na(count), 0, count))

# -----------------------------------------------------------------------------
# Mapping function 
# -----------------------------------------------------------------------------

wine_heat <- function(location, count, topo, geojson){
  
  colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497',
     '#ae017e','#7a0177', '#49006a')
  pal <- colorNumeric(colors, NULL, reverse = F)
  
  heat_map <- leaflet(geojson) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
        fillColor = ~pal(count), label = ~paste0(location))  %>%
    addTopoJSON(topo, weight = 1, color = "#444444", fill = FALSE) %>%
    addLegend(pal = pal, values = count, opacity = 1.0, title = "Quantity")
  return(heat_map)
}

# -----------------------------------------------------------------------------
# Call function 
# -----------------------------------------------------------------------------

wine_heat(ca_county_count$county, ca_county_count$count, ca_topo, ca_geojson)
