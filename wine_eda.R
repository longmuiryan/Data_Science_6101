# =============================================================================
# Wine EDA 
# =============================================================================

setwd("~/Desktop/Git/edwinbet")

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(gridExtra)
source("eda_functions.R")

# =============================================================================
# 0.0 Data Section  
# =============================================================================

raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

# -----------------------------------------------------------------------------
# 0.1 Parse data variable 
# -----------------------------------------------------------------------------

wine_reviews <- raw_wine_reviews %>% 
  mutate(year = as.numeric(str_extract(title, "\\-*\\d+\\.*\\d*"))) %>% 
  mutate(year = ifelse(year > 1950 & year < 2017, year, NA))

# -----------------------------------------------------------------------------
# 0.2 Subset into relavent groups 
# -----------------------------------------------------------------------------

ca_wine_reviews <- wine_reviews %>% filter(province == "California")
italy_wine_reviews <- wine_reviews %>% filter(country == "Italy")
france_wine_reviews <- wine_reviews %>% filter(province == "France")
  
# =============================================================================
# 1.0 Summary of the dataset  
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Lets 'glimpse' the data and see what we're working with 
# -----------------------------------------------------------------------------

tibble(wine_reviews)

# -----------------------------------------------------------------------------
# 1.2 Summary statistics 
# -----------------------------------------------------------------------------

# Summary statistics of year, points, and price 
wine_reviews %>% 
  select(year, points, price) %>% 
  summary()

# =============================================================================
# 3. Graphical Representation of Data 
# =============================================================================

# Summary plots of year, points and price 
year_box <-  wine_reviews %>% wine_box(year, "Year")
price_box <- wine_reviews %>% wine_box(price, "Price")
points_box <- wine_reviews %>% wine_box(points, "Points")

year_qq <-  wine_reviews %>% wine_qq(year, "Year")
price_qq <- wine_reviews %>% wine_qq(price, "Points")
points_qq <- wine_reviews %>% wine_qq(points, "Points")

year_hist <-  wine_reviews %>% wine_hist(year, "Year", bins = 40)
price_hist <- wine_reviews %>% wine_hist(price, "Price", bins = 40)
points_hist <- wine_reviews %>% wine_hist(points, "Points", bins = 20)

lay <- rbind(c(1,2),
             c(1,3))
grid.arrange(grobs = list(price_box, price_qq, price_hist), layout_matrix = lay)

# =============================================================================
# 4. Initial correlation / Chi Square tests / ANOVA analysis / Z-test or 
# Z-interval / T-test or T-interval etc. 
# =============================================================================

# -----------------------------------------------------------------------------
# Correlation 
# -----------------------------------------------------------------------------

# Scatter plot points vs. price ft. fitted line 
# Interpretation: Wines over $300 dollars are typically >90pts, not suprising. 
# However, There's a great variation in points among wines less that $100. 
# What counties in California produce the highest rated wines that cost less 
# than $50? 
wine_reviews  %>% 
  filter(price < 500) %>% 
  ggplot(aes(x = price, y= points)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Correlation points vs. price
cor.test(wine_reviews$price, wine_reviews$points)

# Scatter plot year vs. price ft. fitted line 
# Interpretation: Not very fitted, does imply that the year a wine was 
# produced has influence on how it's rated
wine_reviews %>% 
  ggplot(aes(x = year, y= price)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Correlation year vs. points
# Interpretation: No conclusive evidence that true correlation is not equal to 0
cor.test(wine_reviews$year, wine_reviews$points)

# -----------------------------------------------------------------------------
# Two Sample T 
# -----------------------------------------------------------------------------

# Are domestic wines rated higher on average than imported wines 
domestic_wines <- wine_reviews %>% filter(country == "US")
imported_wines <- wine_reviews %>% filter(country != "US")

# t-test 
# Interpretation: The p-value is lower than the usual threshold of 0.05.
# You are confident to say there is a statistical difference between the groups
t.test(domestic_wines$points, imported_wines$points)

# -----------------------------------------------------------------------------
# Anova 
# -----------------------------------------------------------------------------

# largest wine producers 
big_wine <- wine_reviews %>% 
  filter(country %in% c("Italy", "Spain", "France", "US")) 

# Anova 
# Interpretation: The p-value is lower than the usual threshold of 0.05.
# You are confident to say there is a statistical difference between the groups
anova_results <- aov(points ~ country, data = big_wine)
summary(anova_results)       

# -----------------------------------------------------------------------------
# Who makes the most >80pt wine? You'd think it might be Italy but the answer 
# might suprise you
# Source: https://en.wikipedia.org/wiki/List_of_wine-producing_regions
# -----------------------------------------------------------------------------

raw_wine_reviews <- read.csv("data/winemag-data-130k-v2.csv")

wine_count <- raw_wine_reviews %>%
  mutate(country = as.character(country)) %>% # that's the trick 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 

wine_count <- wine_count %>% 
  mutate(country = ifelse(country == "US", "United States", country))

# -----------------------------------------------------------------------------
# Note: Do we think Italy makes less >80pt wine or could it be that 
# Wine Enthusiast is more likely to review a bottle of wine from the United 
# States? This could potentially be a point of bias in our sample. 
# -----------------------------------------------------------------------------

# ============================================================================
# Calfornia wine production 
# ============================================================================







