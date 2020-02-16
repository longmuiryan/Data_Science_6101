
# =============================================================================
# Wine EDA 
# =============================================================================

setwd("~/Desktop/Data Science/Data/Wine")

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(kableExtra)

# =============================================================================
# Data Section  
#  Objectives: load in the data 
# =============================================================================

raw_wine_reviews <- read.csv("winemag-data-130k-v2.csv")

# =============================================================================
# EDA Section 
#  Objectives: Learn about California wine country 
# =============================================================================

# -----------------------------------------------------------------------------
# Lets start off easy. Click on the dataframe in the global working 
# enviroment and get an idea of what you're working with. Alternatively 
# you can use functions like head(), summary(), and glipse() to do the 
# same thing
# -----------------------------------------------------------------------------

glimpse(raw_wine_reviews)

# -----------------------------------------------------------------------------
# Use the filter() function to select wines grown in California. Assign the 
# output to the data frame ca_wine_reviews 
# -----------------------------------------------------------------------------

ca_wine_reviews <- raw_wine_reviews %>%
  filter(province == "California")

# -----------------------------------------------------------------------------
# Now you know what wines are grown in California, but what are the most
# popular varities? Use the group_by() and summarize() functions to collapse 
# the ca_wine_reviews data frame and count the total number of wines grown 
# for each variety. Assign the output to the data frame ca_popular_wines 
# -----------------------------------------------------------------------------

ca_popular_wines <- ca_wine_reviews %>% 
  group_by(variety) %>% 
  summarize(count = n())

# -----------------------------------------------------------------------------
# We nearly have the answer to the question we're interested in 
# answering, what are the most popular varities in Califorrnia? Use the 
# arrange() and slice() functions to return a data frame containing the 
# 10 most popular wines grown in Califonia. Asign the output to 
# ca_top_10. 
# -----------------------------------------------------------------------------

ca_top_10 <- ca_popular_wines %>% 
  arrange(desc(count)) %>%
  slice(c(1:10)) 

# Note: You might notice that the wines are not properly categorized. 
# We should consider a better procedure for grouping wine vareties in 
# the future. 

# Note: Notice this group of functions could easily answer many of 
# the questions we're interested in (e.g., what regions produce the best 
# wine? What wines is California best known for?) 

# -----------------------------------------------------------------------------
# Lets group vines by variety see if there's a relationship between 
# points and price 
# -----------------------------------------------------------------------------



  
  
  

