
# ========================================================================
# functional programming w/ the tidyverse 
# ========================================================================

setwd("~/Desktop/Git/edwinbet")

# ------------------------------------------------------------------------
# packages 
# ------------------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------------------
# plotting function 
# ------------------------------------------------------------------------

wine_qq <- function(dataframe, variable, title = NULL){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(sample = !! variable))  +
    stat_qq(distribution = stats::qnorm) +
    stat_qq_line(distribution = stats::qnorm) +
    labs(title = paste(title, "Box Plot") ) 
  return(p1)
}

wine_box <- function(dataframe, variable, title = NULL){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = " ", y = !! variable)) +
    geom_boxplot() + 
    labs(title = paste(title, "Box Plot"), y = "")
  return(p1)
}


wine_hist <- function(dataframe, variable, title = NULL, ...){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = !! variable)) +
    geom_histogram(...) +  
    labs(title = paste(title, "Box Plot"), y = "Frequency", x = "")
  return(p1)
}

# ------------------------------------------------------------------------
# test plotting function 
# ------------------------------------------------------------------------

#  wine <- read.csv("data/winemag-data-130k-v2.csv")
#  wine_box(wine, price)
#  wine_hist(wine, points, bins = 20)


