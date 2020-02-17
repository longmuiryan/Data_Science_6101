

# =============================================================================
# Style & Group Categorization 
#   Objectives: Create grape and style variables using the variety variable. 
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
# If we're going to identify the prodinent styles and grapes we need
# to take another glance of the data. Look at the 50 most common wines produced
# in California and add the grape and style to the hard codes section. 
# ----------------------------------------------------------------------------

ca_top_wines <- ca_wine_reviews %>% 
  group_by(variety) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:50)

# lets get rid of it when we're done 
rm("ca_top_wines")

# ----------------------------------------------------------------------------
# Hardcodes 
# ----------------------------------------------------------------------------

styles <- c("Bordeaux", "Rhône")

grapes <- c("Cabernet Sauvignon", "Chardonnay", "Merlot", "Pinot noir",
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
for(i in styles){
ca_wine_reviews <- ca_wine_reviews %>% 
  mutate(style = ifelse(str_detect(variety, i), i, NA))
}

# iterate over grapes 
for(i in grapes){
  ca_wine_reviews <- ca_wine_reviews %>% 
    mutate(grape = ifelse(str_detect(variety, i), i, NA))
}

# -----------------------------------------------------------------------------
# Export Data 
# -----------------------------------------------------------------------------

write.csv(ca_wine_reviews, "ca_wine_reviews.csv")







