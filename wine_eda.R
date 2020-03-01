# =============================================================================
# Wine EDA 
# =============================================================================

setwd("~/Desktop/Git/edwinbet")

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(gridExtra)
source("plotting_functions.R")

# =============================================================================
# Data Section  
# =============================================================================

raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

# =============================================================================
# 1. Summary of the dataset  
# =============================================================================

# -----------------------------------------------------------------------------
# Lets 'glimpse' the data and see what we're working with 
# -----------------------------------------------------------------------------

glimpse(raw_wine_reviews)

# =============================================================================
# 2. Descriptive Statistic: Global Perspective 
# =============================================================================

# Creat year variable 
wine_reviews <- raw_wine_reviews %>% 
  mutate(year = as.numeric(str_extract(title, "\\-*\\d+\\.*\\d*"))) %>% 
  mutate(year = ifelse(year > 1975 & year < 2017, year, NA))

# Summary statistics of  year, points, and price 
wine_reviews %>% 
  select(year, points, price) %>% 
  summary()

# =============================================================================
# 2. Graphical Representation of Data & Normality Tests 
# =============================================================================

# Summary plots of year, pionts and price 
year_box <-  wine_reviews %>% wine_box(year, "Year")
price_box <- wine_reviews %>% wine_box(price, "Price")
points_box <- wine_reviews %>% wine_box(points, "Points")

year_qq <-  wine_reviews %>% wine_qq(year, "Year")
price_qq <- wine_reviews %>% wine_qq(price, "Points")
points_qq <- wine_reviews %>% wine_qq(points, "Points")

year_hist <-  wine_reviews %>% wine_hist(year, "Year", bins = 40)
price_hist <- wine_reviews %>% wine_hist(price, "Price", bins = 40)
points_hist <- wine_reviews %>% wine_hist(points, "Points", bins = 20)

grid.arrange(year_box, year_qq, year_hist, ncol=3 )
grid.arrange(points_box, points_qq, points_hist, ncol=3 )
grid.arrange(price_box, price_qq, price, ncol=3 )

# -----------------------------------------------------------------------------
# Who makes the most >80pt wine? You'd think it might be Italy but the answer 
# might suprise you
# Source: https://en.wikipedia.org/wiki/List_of_wine-producing_regions
# -----------------------------------------------------------------------------

raw_wine_reviews %>%
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

# -----------------------------------------------------------------------------
# Note: Do we think Italy makes less >80pt wine or could it be that 
# Wine Enthusiast is more likely to review a bottle of wine from the United 
# States? This could potentially be a point of bias in our sample. 
# -----------------------------------------------------------------------------


# ============================================================================
# Calfornia wine production 
# ============================================================================

# ----------------------------------------------------------------------------
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



update the master 







