
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
library(tidytext)

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
wine_varities.df <- raw_wine_reviews.df %>% 
  group_by(variety) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% select(variety) %>% 
  slice(1:50)

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

# Not included: "Rosé" and "Port" 


# ----------------------------------------------------------------------------
# Create color variable 
# ----------------------------------------------------------------------------

wine_reviews.df <- raw_wine_reviews.df %>%
  mutate(color = case_when(
    variety %in% red.v ~ "red", 
    variety %in% white.v ~ "white"
  ))
  
# -----------------------------------------------------------------------------
# How well did we do? What proportion of observations were we able to 
# identify color
# -----------------------------------------------------------------------------
  
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

top_words <- c("acidity", "tannins", "cherry", "black", "ripe", "red", "spice", "oak", "fresh", 
  "rich", "dry", "berry", "nose", "plum", "soft", "apple", "fruits", 
  "sweet", "white", "crisp", "blackberry", "light", "dark", "citrus", 
  "bodied", "vanilla", "bright", "pepper", "green", "lemon",
  "raspberry", "peach", "chocolate", "dried", "pear")

# for(word in top_words){
#  x <- str_glue("{word} = ifelse(str_detect(description, \"{word}\"), 1, 0),")
#  print(x)
# }

wine_reviews.df <- wine_reviews.df %>% 
  mutate( 
    acidity = ifelse(str_detect(description, "acidity"), 1, 0),
    tannins = ifelse(str_detect(description, "tannins"), 1, 0),
    cherry = ifelse(str_detect(description, "cherry"), 1, 0),
    black = ifelse(str_detect(description, "black"), 1, 0),
    ripe = ifelse(str_detect(description, "ripe"), 1, 0),
    red = ifelse(str_detect(description, "red"), 1, 0),
    spice = ifelse(str_detect(description, "spice"), 1, 0),
    oak = ifelse(str_detect(description, "oak"), 1, 0),
    fresh = ifelse(str_detect(description, "fresh"), 1, 0),
    rich = ifelse(str_detect(description, "rich"), 1, 0),
    dry = ifelse(str_detect(description, "dry"), 1, 0),
    berry = ifelse(str_detect(description, "berry"), 1, 0),
    nose = ifelse(str_detect(description, "nose"), 1, 0),
    plum = ifelse(str_detect(description, "plum"), 1, 0),
    soft = ifelse(str_detect(description, "soft"), 1, 0),
    apple = ifelse(str_detect(description, "apple"), 1, 0),
    fruits = ifelse(str_detect(description, "fruits"), 1, 0),
    sweet = ifelse(str_detect(description, "sweet"), 1, 0),
    white = ifelse(str_detect(description, "white"), 1, 0),
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




