

# =============================================================================
# Style & Group Categorization 
# =============================================================================

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(dplyr)
library(stringr)

setwd("~/Desktop/Git/edwinbet")

# ----------------------------------------------------------------------------
# Data 
# ----------------------------------------------------------------------------

raw_wine_reviews <- read.csv("winemag-data-130k-v2.csv") 

ca_wine_reviews <- raw_wine_reviews %>%
  filter(province == "California") %>% 
  mutate_if(is.factor, as.character)

# ----------------------------------------------------------------------------
# Hardcodes 
# ----------------------------------------------------------------------------

styles <- c("Bordeaux", "Rhône")

grapes <- c("Cabernet Sauvignon", "Chardonnay", "Merlot", "Pinot noir",
  "Sauvignon blanc", "Syrah","Zinfandel")

# ----------------------------------------------------------------------------
# Create new grape and style variables 
#   For thos who aren't familiar with loops I've included a base case. 
#   The base case serves illustrate what's happening for each element 
#   in the vector. 
# ----------------------------------------------------------------------------

# base case 
ca_wine_reviews <- ca_wine_reviews %>% 
  mutate(variety = as.character(variety)) %>% 
  mutate(style = ifelse(str_detect(variety, "Bordeaux"), "Bordeaux", NA))

# iterate over styles 
for(i in styles){
ca_wine_reviews <- ca_wine_reviews %>% 
  mutate(variety = as.character(variety)) %>% 
  mutate(style = ifelse(str_detect(variety, i), i, NA))
}

# iterate over grapes 
for(i in grapes){
  ca_wine_reviews <- ca_wine_reviews %>% 
    mutate(variety = as.character(variety)) %>% 
    mutate(grape = ifelse(str_detect(variety, i), i, NA))
}







