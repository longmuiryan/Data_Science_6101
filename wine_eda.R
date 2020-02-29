
# =============================================================================
# Wine EDA 
# =============================================================================

setwd("~/Desktop/Data Science/Data/Wine")

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(kableExtra)

# =============================================================================
# Data Section  
# =============================================================================

raw_wine_reviews <- read.csv("winemag-data-130k-v2.csv")

# =============================================================================
# EDA Section 
#   We're going to start wide and narrow towards California 
# =============================================================================

# -----------------------------------------------------------------------------
# Lets 'glimpse' the data and see what we're working with 
# -----------------------------------------------------------------------------

glimpse(raw_wine_reviews)

# =============================================================================
# Who makes the most >80pt wine? 
# =============================================================================

wine_production<- raw_wine_reviews %>%
  group_by(province) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# -----------------------------------------------------------------------------
# Issue: We're interested studying wine markets on a broad scale, however, the
# province variable is more specific than we'd like it to be. 
# Objectice: Italy, Spain, France and the United States are among the top wine 
# producing countries in the world. Properly classify wines produces in these 
# countries. 
# Source: https://en.wikipedia.org/wiki/List_of_wine-producing_regions
# -----------------------------------------------------------------------------

# Regions 
france.v <- c("Bordeaux", "Burgundy", "Alsace", "Loire Valley", "Champagne",
  "Southwest France", "Provence", "Rhône Valley", "Beaujolais", "France Other")
italy.v <- c("Tuscany", "Veneto", "Northeastern Italy", "Sicily & Sardinia", 
  "Southern Italy", "Central Italy", "Catalonia", "Italy Other")
spain.v <- c("Northern Spain", "Douro", "Spain Other")
states.v <- c("California", "Washington", "Oregon", "Piedmont", "New York",
  "Virginia") 

# Classificaiton 
wine_reviews <- raw_wine_reviews %>% 
  mutate(country = case_when(
    province %in% states.v ~ "United States", 
    province %in% france.v ~ "France",
    province %in% italy.v ~ "Italy",
    province %in% spain.v ~ "Spain" )) %>%
  filter(!is.na(country))

# -----------------------------------------------------------------------------
# We fucking did it! By categorizing the top 30 out of the total 426 wine producing
# regions reviewed in Wine Enthusiast we we're able to properaly classify roughly 
# 80% of all the wines reviewed. 
# 
# Lets go back and try and answer our previous question 
# -----------------------------------------------------------------------------

wine_production <- wine_reviews %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# -----------------------------------------------------------------------------
# Pretty nuts. Althought Wikipedia says Italy produces more than 25% more wine 
# than the U.S., The U.S. has more than 5 times as many wines reviewed than 
# Italy in our sample. Could this a point of bias? 
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 
# 
# -----------------------------------------------------------------------------
















