
# =============================================================================
# Categorization Problems 
#   The raw data from Kaggle is missing key variables neccesary for analysis
#   (e.g., country, county, grape, style, and year). This script aims to create 
#   some of these variables 
# 
# Note:
#   All the steps are performed seperately and the data isn't exported to a 
#   dataset. Haven't figured out how to fit them together. 
# =============================================================================

# Set working directory to the top of the repo 
setwd("~/Desktop/Git/edwinbet")

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(tidytext) # used to scape most common words 
library(ggmap) # used to gather lat and lon from google maps 
library(googleway) # used to convert lat and long to elavation


# ----------------------------------------------------------------------------
# Data 
# ----------------------------------------------------------------------------

raw_wine_reviews.df <- read.csv("data/winemag-data-130k-v2.csv") 

# =============================================================================
# Red or White 
#   Create variable that categorizes wine variety into one of two
#   categories, red or white 
# =============================================================================

# Look at the 50 most common wines reviewed by WineEnthusiast 
# wine_varities.df <- raw_wine_reviews.df %>%
#   group_by(variety) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count)) %>%
#   select(variety) %>%
#   slice(1:50)

# Add each of the varities to their associated vector 
red.v <- c(
  "Pinot Noir", "Cabernet Sauvignon", "Red Blend", "Bordeaux-style Red Blend", "Syrah",
  "Merlot", "Nebbiolo", "Zinfandel", "Sangiovese", "Malbec", "Portuguese Red", "Tempranillo",
  "Rhône-style Red Blend", "Cabernet Franc", "Shiraz", "Petite Sirah", "Sangiovese Grosso",
  "Barbera", "Grenache", "Corvina, Rondinella, Molinara", "Tempranillo Blend", "Carmenère", 
  "Nero d'Avola", "Aglianico", "Moscato", "Garnacha"
)

white.v <- c(
 "Chardonnay", "Riesling", "Sauvignon Blanc", "White Blend", "Sparkling Blend", "Pinot Gris",
 "Champagne Blend", "Grüner Veltliner", "Portuguese White", "Bordeaux-style White Blend",
 "Pinot Grigio", "Gamay", "Gewürztraminer", "Viognier", "Glera", "Chenin Blanc", "Albariño", 
 "Pinot Blanc", "Rhône-style White Blend", "Sauvignon", "Verdejo", "Melon"
)

# create color variable 
wine_reviews.df <- raw_wine_reviews.df %>%
  mutate(color = case_when(
    variety %in% red.v ~ "red",
    variety %in% white.v ~ "white"
  ))
  
# How well did we do? What proportion of observations were we able to 
# identify color
wine_reviews.df %>% summarise(populated_percent = 1 - sum(is.na(color))/nrow(.))
       
# =================================================================================
# Popular words in reviews 
# =================================================================================

# what are the most popular words in WineEnthusiast reviews
# wine_words.df <- raw_wine_reviews.df %>%
#   mutate_if(is.factor, as.character) %>% 
#   unnest_tokens(word, description) %>%
#   anti_join(stop_words) %>% group_by(word) %>%
#   tally() %>% arrange(desc(n)) 

# top_words <- c(
#   "acidity", "tannins", "cherry", "black", "ripe", "red", "spice", "oak", "fresh",
#   "rich", "dry", "berry", "nose", "plum", "soft", "apple", "fruits",
#   "sweet", "white", "crisp", "blackberry", "light", "dark", "citrus",
#   "bodied", "vanilla", "bright", "pepper", "green", "lemon",
#   "raspberry", "peach", "chocolate", "dried", "pear"
# )

# for(word in top_words){
#  x <- str_glue("{word} = ifelse(str_detect(description, \"{word}\"), 1, 0),")
#  print(x)
# }

wine_reviews.df <- wine_reviews.df %>% 
  mutate( 
    acidity = ifelse(str_detect(description, "acidity"), 1, 0),
    tannins = ifelse(str_detect(description, "tannins"), 1, 0),
    cherry = ifelse(str_detect(description, "cherry"), 1, 0),
    ripe = ifelse(str_detect(description, "ripe"), 1, 0),
    spice = ifelse(str_detect(description, "spice"), 1, 0),
    oak = ifelse(str_detect(description, "oak"), 1, 0),
    fresh = ifelse(str_detect(description, "fresh"), 1, 0),
    rich = ifelse(str_detect(description, "rich"), 1, 0),
    dry = ifelse(str_detect(description, "dry"), 1, 0),
    berry = ifelse(str_detect(description, "berry"), 1, 0),
    plum = ifelse(str_detect(description, "plum"), 1, 0),
    soft = ifelse(str_detect(description, "soft"), 1, 0),
    apple = ifelse(str_detect(description, "apple"), 1, 0),
    fruits = ifelse(str_detect(description, "fruits"), 1, 0),
    sweet = ifelse(str_detect(description, "sweet"), 1, 0),
    crisp = ifelse(str_detect(description, "crisp"), 1, 0),
    blackberry = ifelse(str_detect(description, "blackberry"), 1, 0),
    light = ifelse(str_detect(description, "light"), 1, 0),
    dark = ifelse(str_detect(description, "dark"), 1, 0),
    citrus = ifelse(str_detect(description, "citrus"), 1, 0),
    bodied = ifelse(str_detect(description, "bodied"), 1, 0),
    vanilla = ifelse(str_detect(description, "vanilla"), 1, 0),
    bright = ifelse(str_detect(description, "bright"), 1, 0),
    pepper = ifelse(str_detect(description, "pepper"), 1, 0),
    green = ifelse(str_detect(description, "green"), 1, 0),
    lemon = ifelse(str_detect(description, "lemon"), 1, 0),
    raspberry = ifelse(str_detect(description, "raspberry"), 1, 0),
    peach = ifelse(str_detect(description, "peach"), 1, 0),
    chocolate = ifelse(str_detect(description, "chocolate"), 1, 0),
    dried = ifelse(str_detect(description, "dried"), 1, 0),
    pear = ifelse(str_detect(description, "pear"), 1, 0)
  )


# =============================================================================
# Popular Wine Critics 
# =============================================================================

# # who has reviewed the most wine? 
# raw_wine_reviews.df %>% 
#   group_by(taster_twitter_handle) %>% 
#   tally() %>% arrange(desc(n))
#
# # create variables based on reviewer following 
# top_tasters <- c(
#   "Alexander Peartree", "Anna Lee C. Iijima", "Anne Krebiehl MW",
#   "Carrie Dykes", "Christina Pickard", "Fiona Adams", "Jeff Jenssen",
#   "Jim Gordon", "Joe Czerwinski", "Kerin O’Keefe", "Lauren Buzzeo",
#   "Matt Kettmann", "Michael Schachner", "Mike DeSimone", "Paul Gregutt",
#   "Roger Voss", "Sean P. Sullivan", "Susan Kostrzewa", "Virginie Boone"
# )
# 
# # store each of tasters number of followers as fo April 17th 
# taster_following <- c(
#   0, 0, 7246, 111, 3710, 267, 2622, 1009, 4732, 6283, 1956, 2565,
#   1451, 2622, 3202, 1047, 10200, 1958, 2313
# )

# for (i in 1:19) {
#   x <- str_glue("taster_name == \"{top_tasters[i]}\" ~ {taster_following[i]},")
#   print(x)
# }

# create variable meassuring the number of followers of the reviewer 
wine_reviews.df <- wine_reviews.df %>%
  mutate(taster_following = case_when(
    taster_name == "Alexander Peartree" ~ 0,
    taster_name == "Anna Lee C. Iijima" ~ 0,
    taster_name == "Anne Krebiehl MW" ~ 7246,
    taster_name == "Carrie Dykes" ~ 111,
    taster_name == "Christina Pickard" ~ 3710,
    taster_name == "Fiona Adams" ~ 267,
    taster_name == "Jeff Jenssen" ~ 2622,
    taster_name == "Jim Gordon" ~ 1009,
    taster_name == "Joe Czerwinski" ~ 4732,
    taster_name == "Kerin O’Keefe" ~ 6283,
    taster_name == "Lauren Buzzeo" ~ 1956,
    taster_name == "Matt Kettmann" ~ 2565,
    taster_name == "Michael Schachner" ~ 1451,
    taster_name == "Mike DeSimone" ~ 2622,
    taster_name == "Paul Gregutt" ~ 3202,
    taster_name == "Roger Voss" ~ 1047,
    taster_name == "Sean P. Sullivan" ~ 10200,
    taster_name == "Susan Kostrzewa" ~ 1958,
    taster_name == "Virginie Boone" ~ 2313,
    taster_name == "" ~ 0
  ))

# create dummy variabe for  if wine reviewed by reivew 
wine_reviews.df <- wine_reviews.df %>% 
  mutate( 
    "Alexander Peartree" = ifelse(taster_name == "Alexander Peartree", 1, 0),
    "Anna Lee C. Iijima" = ifelse(taster_name == "Anna Lee C. Iijima", 1, 0),
    "Anne Krebiehl MW" = ifelse(taster_name == "Anne Krebiehl MW", 1, 0),
    "Carrie Dykes" = ifelse(taster_name == "Carrie Dykes", 1, 0),
    "Christina Pickard" = ifelse(taster_name == "Christina Pickard", 1, 0),
    "Fiona Adams" = ifelse(taster_name == "Fiona Adams", 1, 0),
    "Jeff Jenssen" = ifelse(taster_name == "Jeff Jenssen", 1, 0),
    "Jim Gordon" = ifelse(taster_name == "Jim Gordon", 1, 0),
    "Joe Czerwinski" = ifelse(taster_name == "Joe Czerwinski", 1, 0),
    "Kerin O’Keefe" = ifelse(taster_name == "Kerin O’Keefe", 1, 0),
    "Lauren Buzzeo" = ifelse(taster_name == "Lauren Buzzeo", 1, 0),
    "Matt Kettmann" = ifelse(taster_name == "Matt Kettmann", 1, 0),
    "Michael Schachner" = ifelse(taster_name == "Michael Schachner", 1, 0),
    "Mike DeSimone" = ifelse(taster_name == "Mike DeSimone", 1, 0),
    "Paul Gregutt" = ifelse(taster_name == "Paul Gregutt", 1, 0),
    "Roger Voss" = ifelse(taster_name == "Roger Voss", 1, 0),
    "Sean P. Sullivan" = ifelse(taster_name == "Sean P. Sullivan", 1, 0),
    "Susan Kostrzewa" = ifelse(taster_name == "Susan Kostrzewa", 1, 0),
    "Virginie Boone" = ifelse(taster_name == "Virginie Boone", 1, 0)
  )


# -----------------------------------------------------------------------------
# Obtain Coordinates 
# You're going to need to register an API key before you run the first part of the 
# code, however, I've already stored the data 
# -----------------------------------------------------------------------------

# set_key("[your key]")
# register_google("[your key]")

# -----------------------------------------------------------------------------
# functions 
# -----------------------------------------------------------------------------

try_elevation <- function(x) {
  out <- tryCatch(
    expr = elevation(google_elevation(x)),
    error = function(e){return(NA)}
  )
  return(out)
}

# -----------------------------------------------------------------------------
# query lon and lat from google maps 
# ----------------------------------------------------------------------------

# # countries
# countries <- raw_wine_reviews.df %>%
#   group_by(country) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   filter(n > 50) %>%
#   mutate(type = "country") %>%
#   select(country, type) %>%
#   rename(location = country) %>%
#   mutate_all(as.character)
# 
# # region
# regions <- raw_wine_reviews.df %>%
#   group_by(region_1) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   filter(n > 50) %>%
#   mutate(type = "region_1") %>%
#   select(region_1, type) %>%
#   rename(location = region_1) %>%
#   mutate_all(as.character)
# 
# # province
# provinces <- raw_wine_reviews.df %>%
#   group_by(province) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   filter(n > 50) %>%
#   mutate(type = "province") %>%
#   select(province, type) %>%
#   rename(location = province) %>%
#   mutate_all(as.character)
# 
# locations <- countries %>%
#   bind_rows(provinces) %>%
#   bind_rows(regions) %>%
#   mutate(
#     lon = geocode(location)$lon,
#     lat = geocode(location)$lat
#   )

# ---------------------------------------------------------------------
# query elevation using lon and lat 
# ---------------------------------------------------------------------

# locations <- locations %>%
#   select(-c(X, X.1)) %>%
#   mutate(el = NA)
# 
# for (i in 1:nrow(locations)) {
#   print(i)
#   locations[i, 5] <- try_elevation(locations[i, ])
# }

# write.csv(locations, "data/region_locations.csv")

# --------------------------------------------------------------------
# 
# --------------------------------------------------------------------

locations.df <- read.csv("data/region_locations.csv")

country.df <- locations.df %>%
  filter(type == "country") %>%
  select(location, lon, lat, el) %>% 
  mutate_at(vars(location), as.character) %>% 
  rename(
    country_lon = lon,
    country_lat = lat,
    country_el = el,
    country = location
  )

region_1.df <- locations.df %>%
  filter(type == "region_1") %>%
  select(location, lon, lat, el) %>% 
  mutate_at(vars(location), as.character) %>% 
  rename(
    region_1_lon = lon,
    region_1_lat = lat,
    region_1_el = el, 
    region_1 = location
  )

province.df <- locations.df %>%
  filter(type == "province") %>%
  select(location, lon, lat, el) %>% 
  mutate_at(vars(location), as.character) %>% 
  rename(
    province_lon = lon,
    province_lat = lat,
    province_el = el,
    province = location
  )

wine_reviews.df <- wine_reviews.df %>% 
  left_join(country.df, by = "country") %>% 
  left_join(region_1.df, by = "region_1") %>% 
  left_join(province.df, by = "province") 


wine_reviews.df <- wine_reviews.df %>%
  mutate(
    composite_lon =
      ifelse(!is.na(region_1_lon), region_1_lon,
        ifelse(!is.na(province_lon), province_lon,
          ifelse(!is.na(country_lon), country_lon, NA))),
    composite_lat =
      ifelse(!is.na(region_1_lat), region_1_lat,
        ifelse(!is.na(province_lat), province_lat,
          ifelse(!is.na(country_lat), country_lat, NA))),
    composite_el =
      ifelse(!is.na(region_1_el), region_1_el,
        ifelse(!is.na(province_el), province_el,
          ifelse(!is.na(country_el), country_el, NA)))
  )


wine_reviews.df <- wine_reviews.df %>% 
  select(-c(region_1_lon, region_1_lat, province_lon, province_lat, 
    country_lon, country_lat, region_1_el, province_el, country_el))

# How well did we do? What proportion of observations were we able to 
# identify elevation
wine_reviews.df %>% summarise(populated_percent = 1 - sum(is.na(composite_el))/nrow(.))

# clean up 
rm(region_1.df, country.df, province.df, locations.df)

# ------------------------------------------------------------------------
# Write data set 
# ------------------------------------------------------------------------

write.csv(wine_reviews.df, "data/wine_reviews.csv")

