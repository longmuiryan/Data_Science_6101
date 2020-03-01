
# =============================================================================
# Categorization Problems 
#   The raw data from Kaggle is missing key variables neccesary for analysis
#   (e.g., country, county, grape, style, and year). This script aims to create 
#   some of these variables 
# 
# Sections:
#   1.Year
#   2.California styles & grapes 
#   3.California counties
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

# ----------------------------------------------------------------------------
# Data 
# ----------------------------------------------------------------------------

raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv") 

# =============================================================================
# 1. Time Classification 
#   Objectives: Create year variable using the title of the wine 
# =============================================================================

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

wine_reviews <- raw_wine_reviews %>% 
  mutate(year = numextract(title))

# -----------------------------------------------------------------------------
# Export data 
# -----------------------------------------------------------------------------

# setwd("~/Desktop/Git/edwinbet")
# write_csv()

# =============================================================================
# Style & Grape
#   Objectives: Create grape and style variables using the variety variable. 
# =============================================================================

# ----------------------------------------------------------------------------
# We're going to identify the predominant styles and grapes. Look at the 50 
# most common wines produced in California and sort them into the grapes.v and 
# styles.v vectors. 
# ----------------------------------------------------------------------------

ca_top_wines <- raw_wine_reviews %>% 
  filter(province == "California") %>% 
  group_by(variety) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:50)

# ----------------------------------------------------------------------------
# Sort the varieties into styles and grapes, starting with the most common 
# varieties. 
# ----------------------------------------------------------------------------

styles.v <- c("Bordeaux", "Rhône")

grapes.v <- c("Cabernet Sauvignon", "Chardonnay", "Merlot", "Pinot noir",
  "Sauvignon blanc", "Syrah","Zinfandel")

# ----------------------------------------------------------------------------
# Create new grape and style variables 
# For thos who aren't familiar with loops I've included a base case. 
# The base case serves illustrate what's happening for each element 
# in the vector. 
# ----------------------------------------------------------------------------

# base case 
ca_wine_reviews <- ca_wine_reviews %>% 
  mutate(style = ifelse(str_detect(variety, "Bordeaux"), "Bordeaux", NA))

# iterate over styles 
for(i in styles.v){
ca_wine_reviews <- ca_wine_reviews %>% 
  mutate(style = ifelse(str_detect(variety, i), i, NA))
}

# iterate over grapes 
for(i in grapes.v){
  ca_wine_reviews <- ca_wine_reviews %>% 
    mutate(grape = ifelse(str_detect(variety, i), i, NA))
}

# -----------------------------------------------------------------------------
# How well did we do? What proportion of observations were we able to 
# identify style and grape? 
# -----------------------------------------------------------------------------
  
# grapes 
nrow(ca_wine_reviews %>% filter(!is.na(grape))) / nrow(ca_wine_reviews)

# styles 
nrow(ca_wine_reviews %>% filter(!is.na(style))) / nrow(ca_wine_reviews)

# -----------------------------------------------------------------------------
# Export data 
# -----------------------------------------------------------------------------

# write.csv(ca_wine_reviews, "ca_wine_reviews.csv")

# =============================================================================
# California County
#   Objectives: Create count variables using the region variables. 
# =============================================================================

# -----------------------------------------------------------------------------
# What counties produce the most wine? 
# -----------------------------------------------------------------------------

top_county <- raw_wine_reviews %>% 
  filter(province == "California") %>% 
  group_by(region_1) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:50)

# -----------------------------------------------------------------------------
# Sort the regions into counties, starting with those that make this most 
# wine.
# -----------------------------------------------------------------------------

# Counties 
napa <- c("Napa Valley")
sonoma <- c("Sonoma", "Russian River Valley", "Sonoma Coast", "Sonoma County", 
  "Carneros", "Dry Creek Valley")
santa_barbara <- c("Santa Barbara County", "Sta. Rita Hills")
san_luis_obispo <- c("San Luis Obispo", "Paso Robles")

ca_wine_reviews <- ca_wine_reviews %>% 
  filter(province == "California") %>% 
  mutate(county = case_when(
    region_1 %in% napa ~ "Napa",
    region_1 %in% sonoma ~ "Sonoma",
    region_1 %in% santa_barbara ~ "Santa Barbara",
    region_1 %in% san_luis_obispo ~ "San Luis Obispo" ))



 




