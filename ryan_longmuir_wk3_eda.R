
# -----------------------------------------------------------------------------
# =============================================================================

# =============================================================================
# EDA Homework 
# =============================================================================

library(ggplot2)

# ----------------------------------------------------------------------
# Functions 
# ----------------------------------------------------------------------

outlierKD <- function(dt, var, remove = TRUE) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  if(remove){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


# -----------------------------------------------------------------------------
# Question 1 
# -----------------------------------------------------------------------------

setwd("~/Desktop/Data Science/Data Science 6101/week3")
pizza.df <- read.csv("Pizza.csv")

# -----------------------------------------------------------------------------
# Question 2
# -----------------------------------------------------------------------------

length(pizza.df)

# -----------------------------------------------------------------------------
# Question 3
# -----------------------------------------------------------------------------

summary(pizza.df)

# -----------------------------------------------------------------------------
# Question 4
# -----------------------------------------------------------------------------

# Sodium Hist 
ggplot(pizza.df, aes(x = sodium)) +
  labs(title = "Soldium Content", y = "Frequency") + 
  theme(text = element_text(size = 12)) + 
  geom_histogram(bins = 30, color="darkblue", fill="lightblue")

# Caloria Hist 
ggplot(pizza.df, aes(x = cal)) +
  labs(title = "Soldium Content", y = "Frequency") + 
  theme(text = element_text(size = 12)) + 
  geom_histogram(bins = 30, color="darkred", fill="lightpink")

# Sodium Box Plot 
ggplot(pizza.df, aes(x = "", y = sodium)) +
  labs(title = "Sodium", y = "Sodium") + 
  theme(text = element_text(size = 15)) + 
  geom_boxplot(color="darkblue", fill="lightblue")

# Calorie Box Plot 
ggplot(pizza.df, aes(x = "", y = cal)) +
  labs(title = "Calories", y = "Calories") + 
  theme(text = element_text(size = 15)) + 
  geom_boxplot(color="darkred", fill="lightpink")

# Sodium QQ-Plot (w/ Normal Dist)
ggplot(pizza.df, aes(sample = sodium))  +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line(distribution = stats::qnorm) + 
  theme(text = element_text(size = 15)) + 
  labs(title = "Sodium QQ-Plot" )

# Calories QQ-Plot (w/ Normal Dist)
ggplot(pizza.df, aes(sample = cal))  +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line(distribution = stats::qnorm) + 
  theme(text = element_text(size = 15)) + 
  labs(title = "Calorie QQ-Plot" )

# Shapiro test 
# The Shapiro–Wilk test is a test of normality in frequentist statistics. 
shapiro.test(pizza.df$sodium) # reject  p < 0.0001 
shapiro.test(pizza.df$cal) # reject p < 0.0001

# Remove outliers 
sodium_ex_outliers <- outlierKD(pizza.df, sodium)
calories_ex_outliers <- outlierKD(pizza.df, cal)

# Sodium QQ-Plot (w/ Normal Dist)
ggplot(sodium_ex_outliers, aes(sample = sodium))  +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line(distribution = stats::qnorm) + 
  theme(text = element_text(size = 15)) + 
  labs(title = "Sodium QQ-Plot" )

# Calories QQ-Plot (w/ Normal Dist)
ggplot(calories_ex_outliers, aes(sample = cal))  +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line(distribution = stats::qnorm) + 
  theme(text = element_text(size = 15)) + 
  labs(title = "Calorie QQ-Plot" )







 



