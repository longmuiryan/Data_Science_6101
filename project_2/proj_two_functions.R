
# =============================================================================
# Data Wine'ing Functions 
# =============================================================================


# -----------------------------------------------------------------------------
# loadPkg
# -----------------------------------------------------------------------------

loadPkg = function(pkg, character.only = FALSE) { 
  if (!character.only) { pkg <- as.character(substitute(pkg)) }
  pkg <- ifelse(!character.only, as.character(substitute(pkg)) , pkg)  
  if (!require(pkg,character.only=T, quietly =T)) { install.packages(substitute(pkg),dep=T);
    if(!require(pkg,character.only=T)) stop("Package not found") } 
}

# -----------------------------------------------------------------------------
# unloadPkg
# -----------------------------------------------------------------------------

unloadPkg = function(pkg, character.only = FALSE) { 
  if(!character.only) { pkg <- as.character(substitute(pkg)) } 
  search_item <- paste("package", pkg,sep = ":") 
  while(search_item %in% search()) { detach(search_item, unload = TRUE, character.only = TRUE) } 
}

# -----------------------------------------------------------------------------
# xkabledply
# -----------------------------------------------------------------------------

xkabledply = function(modelsmmrytable, title="Table", digits = 4, pos="left", bso="striped") { 
  modelsmmrytable %>%
    xtable() %>% 
    kable(caption = title, digits = digits) %>%
    kable_styling(bootstrap_options = bso, full_width = FALSE, position = pos)
}

# -----------------------------------------------------------------------------
# xkable summary
# -----------------------------------------------------------------------------

xkablesummary <- function(df, ...) {
  # Combining base::summary, xtable, and kableExtra, to easily display numeric variable summary of dataframes.
  # If the categorical variables has less than 6 levels, the function will still run without error.
  # ELo 202003 GWU DATS
  # @param df The dataframe.
  # xkablesummary( faraway::ozone )
  
  s <- summary(df) %>%
    apply(2, function(x) {
      stringr::str_remove_all(x, c(
        "Min.\\s*:\\s*", "1st Qu.\\s*:\\s*", "Median\\s*:\\s*",
        "Mean\\s*:\\s*", "3rd Qu.\\s*:\\s*", "Max.\\s*:\\s*"))
    }) %>% # replace all leading words
    apply(2, function(x) stringr::str_trim(x, "right")) # trim trailing spaces left
  
  colnames(s) <- stringr::str_trim(colnames(s))
  
  if (dim(s)[1] == 6) {rownames(s) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")
  } else if (dim(s)[1] == 7) {rownames(s) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "NA")}
  
  s %>% xkabledply("Table: Statistics summary.", "center", ...)
}

# -----------------------------------------------------------------------------
# uzscale
# -----------------------------------------------------------------------------

uzscale <- function(df, append=0, excl=NULL) { 
  #' Standardize dataframe to z scores, safe for non-numeric variables. 
  #' ELo 201904 GWU DATS
  #' @param df The dataframe.
  #' @param append T/F or 0/1. Option to append scaled columns or replace original columns in the dataframe.
  #' @param excl A list c(a,b,"d","ef") of excluded columns, either by their indexes and/or names.
  #' @return The transformed dataframe, appended or replaced with standardized scores. Non-numeric columns will not be appended, or if "replace option" is chosen, the columns will be untouched.
  #' @examples
  #' library("ISLR")
  #' tmp = uzscale( Hitters )
  #' tmp = uzscale( Hitters, 1 )
  #' tmp = uzscale( Hitters, TRUE, c(19,"NewLeague") )
  
  append = ifelse(append==TRUE || append=="true" || append=="True" || append=="T" || append=="t" || append==1 || append=="1", TRUE, FALSE) # standardize append 
  nmax = length(df)
  if (nmax < 1 || !is.numeric(nmax) ) { return(df) }
  df1 = df
  onames = colnames(df)  # the original column names
  cnames = onames  # the new column names, if needed start with the original ones
  znames = paste("z",cnames, sep="")     # new column names added prefix 'z'. Those are non-numeric will not be used.
  nadd = ifelse(append, nmax, 0) # add to the column index or replace the orig columns
  j=1  # counting index
  for( i in 1:nmax ) {
    if ( is.numeric(df[,i]) && !( i %in% excl || onames[i] %in% excl ) ) { 
      df1[,j+nadd] = scale(df[,i])
      cnames = c(cnames, znames[i])
      j=j+1
    } else if ( !append ) { j=j+1
    } # if append == 1 and (colunm non-numeric or excluded), do not advance j.
  }
  if (append) { colnames(df1) <- cnames }
  return(df1)
}

# -----------------------------------------------------------------------------
# clusterPlot 
# ------------------------------------------------------------------------------

clusterPlot <- function(df, var, title) {
  var <- enquo(var)
  p.info <- df %>%
    mutate(cluster = k$cluster) %>%
    select(cluster, !!var) %>%
    melt(id.vars = "cluster") %>%
    ggplot(aes(y = value, x = paste("Cluster", cluster))) +
    geom_boxplot(color = "#B31B1B", fill = "#F4C2C2", size = 1.3, outlier.colour = "black", outlier.size = 4) +
    labs(x = title) + ggtitle(paste(title, "by Cluster")) + theme(plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size=10), axis.title = element_text(size=12))
  return(p.info)
}

