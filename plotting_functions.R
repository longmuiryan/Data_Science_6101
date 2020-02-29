
# ========================================================================
# functional programming w/ the tidyverse 
#   ryan 
# ========================================================================

# ------------------------------------------------------------------------
# packages 
# ------------------------------------------------------------------------

library(tidyverse)
library(gridExtra)

# ------------------------------------------------------------------------
# data 
# ------------------------------------------------------------------------

wine <- read.csv("~/Desktop/Git/edwinbet/winemag-data-130k-v2.csv")

# ------------------------------------------------------------------------
# plotting function 
# ------------------------------------------------------------------------

norm_test <- function(dataframe, variable, title = NULL){
  variable <- enquo(variable)
  p1 <- ggplot(dataframe, aes(sample = !! variable))  +
    stat_qq(distribution = stats::qnorm) +
    stat_qq_line(distribution = stats::qnorm) +
    labs(title = paste(title, "Box Plot") ) 
  p2 <- ggplot(dataframe, aes(x = " ", y = !! variable)) +
    geom_boxplot() + 
    labs(title = paste(title, "Box Plot"), y = "")
  return(grid.arrange(p1, p2, nrow = 1))
}

# ------------------------------------------------------------------------
# test plotting function 
# ------------------------------------------------------------------------

norm_test(wine, price, title = "Price")
