
# ========================================================================
# functional programming w/ the tidyverse 
# ========================================================================

# setwd("~/Desktop/Git/edwinbet")

# ------------------------------------------------------------------------
# packages 
# ------------------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------------------
# plotting functions
# ------------------------------------------------------------------------

wine_qq <- function(dataframe, variable, title = NULL){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(sample = !! variable))  +
    stat_qq(distribution = stats::qnorm) +
    stat_qq_line(distribution = stats::qnorm) +
    labs(title = paste(title, "QQ Plot") ) + 
    theme_minimal()
  return(p1)
}

wine_box <- function(dataframe, variable, title = NULL){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = " ", y = !! variable)) +
    geom_boxplot() + 
    labs(title = paste(title, "Box Plot"), y = "") + 
    theme_minimal()
  return(p1)
}

wine_hist <- function(dataframe, variable, title = NULL, ...){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = !! variable)) +
    geom_histogram(...) +  
    labs(title = paste(title, "Histogram"), y = "Frequency", x = "") + 
    theme_minimal()
  return(p1)
}

# ------------------------------------------------------------------------
# test plotting function 
# ------------------------------------------------------------------------

# wine <- read.csv("data/winemag-data-130k-v2.csv")
#  wine_box(wine, price)
#  wine_hist(wine, points, bins = 20)

# ------------------------------------------------------------------------
# remove outliers function 
# ------------------------------------------------------------------------

filter_outliers <- function(dataframe, variable){
  variable <- enquo(variable)
  series <- select(dataframe, !! variable) %>% unlist()
  IQR <- IQR(series, na.rm = T)
  Q <- quantile(series, probs=c(.25, .75), na.rm = T)
  LB <- Q[1] - 1.5*IQR
  UB <- Q[2] + 1.5*IQR
  cat(" Upper bound:", UB, "\n Lower bound:", LB)
  return(filter(dataframe, !! variable > LB & 
  !! variable < UB & !is.na(!! variable)))
}

# ------------------------------------------------------------------------
# test outliers function 
# ------------------------------------------------------------------------

# wine <- read.csv("data/winemag-data-130k-v2.csv")
# filter_outliers(wine_reviews, price)
