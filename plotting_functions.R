
# ========================================================================
# functional programming w/ the tidyverse
# ========================================================================

# ------------------------------------------------------------------------
# packages 
# ------------------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------------------
# plotting function 
# ------------------------------------------------------------------------

wine_qq <- function(dataframe, variable, title = NULL, ...){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(sample = !! variable))  +
    stat_qq(distribution = stats::qnorm) +
    stat_qq_line(distribution = stats::qnorm) +
    labs(title = paste(title, "Box Plot")) 
  return(p1)
}

wine_box <- function(dataframe, variable, title = NULL, ...){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = " ", y = !! variable)) +
    geom_boxplot() + 
    labs(title = paste(title, "Box Plot"), y = "")
  return(p1)
}

wine_hist <- function(dataframe, variable, title = NULL, ...){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(x = !! variable)) +
    geom_histogram() + 
    labs(title = paste(title, "Box Plot"), y = "")
  return(p1)
}

# ------------------------------------------------------------------------
# test plotting function 
# ------------------------------------------------------------------------

# data 
wine <- read.csv("data/winemag-data-130k-v2.csv")

# plot 
# wine_hist(wine, points, bins = 30)
# wine_box(wine, points)
# wine_qq(wine, points)
