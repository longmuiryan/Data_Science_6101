
setwd("~/Desktop/Git/edwinbet")

# =============================================================================
# Maps  
# =============================================================================

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(geojsonio)
library(leaflet)
library(ggplot2)

# =============================================================================
# Data Cleaning 
# =============================================================================

# Raw data 
raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

# -----------------------------------------------------------------------------
# Select Top 10 Countries
# -----------------------------------------------------------------------------

# Get top 10 wine producers
top_10 <- raw_wine_reviews %>%
  mutate(country = as.character(country)) %>%
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

top_10 <- top_10 %>% 
  mutate(country = ifelse(country == "US", "United States of America", country))

# -----------------------------------------------------------------------------
# Prepare map data 
# -----------------------------------------------------------------------------

# Grab the geojson & json 
world_geojson <- geojsonio::geojson_read("json/countries.geojson", what = "sp")

# Grab the countries from the geojson
countries <- tibble(world_geojson$ADMIN) %>% 
  setNames("country")

# Merge in w/ wine reviews 
country_count <- top_10 %>% 
  right_join(countries, by = "country")
# mutate(count = ifelse(is.na(count), 0, count))

# -----------------------------------------------------------------------------
# Histogram
# -----------------------------------------------------------------------------

world_bar <- ggplot(top_10, aes(country, count)) +
  geom_bar(stat="identity") + 
  labs(title="Top 10 Wine Producers", x="Country", y="Quantity of Wine") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

world_bar

# -----------------------------------------------------------------------------
# Mapping function 
# -----------------------------------------------------------------------------

wine_heat <- function(location, count, geojson){
  
  colors <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497',
              '#ae017e','#7a0177', '#49006a')
  pal <- colorNumeric(colors, NULL, reverse = F)
  
  heat_map <- leaflet(geojson) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                fillColor = ~pal(count),
                label = ~paste0(location, ": ", formatC(count, big.mark = ",")))  %>%
    # addTopoJSON(topo, weight = 1, color = "#444444", fill = FALSE) %>%
    addLegend(pal = pal, values = count, opacity = 1.0, title = "Quantity")
  return(heat_map)
}

# -----------------------------------------------------------------------------
# Call function 
# -----------------------------------------------------------------------------

wine_heat(country_count$country, country_count$count, world_geojson)



