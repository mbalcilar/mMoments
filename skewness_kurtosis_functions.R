# some functions to compute skewness and kurtosis

skewness <- function (x, na.rm = FALSE) 
{
  if (is.matrix(x)) 
    apply(x, 2, skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    n <- length(x)
    (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
  }
  else if (is.data.frame(x)) 
    sapply(x, skewness, na.rm = na.rm)
  else skewness(as.vector(x), na.rm = na.rm)
}


median_skewness <- function (x, na.rm = FALSE) 
{
  if (is.matrix(x)) 
    apply(x, 2, median_skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    3*(mean(x)-median(x))/sd(x)
  }
  else if (is.data.frame(x)) 
    sapply(x, median_skewness, na.rm = na.rm)
  else median_skewness(as.vector(x), na.rm = na.rm)
}

mode_skewness <- function (x, na.rm = FALSE) 
{
  my_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  if (is.matrix(x)) 
    apply(x, 2, mode_skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    (mean(x)-my_mode(x))/sd(x)
  }
  else if (is.data.frame(x)) 
    sapply(x, mode_skewness, na.rm = na.rm)
  else mode_skewness(as.vector(x), na.rm = na.rm)
}

kurtosis <- function (x, na.rm = FALSE) 
{
  if (is.matrix(x)) 
    apply(x, 2, kurtosis, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    n <- length(x)
    n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
  }
  else if (is.data.frame(x)) 
    sapply(x, kurtosis, na.rm = na.rm)
  else kurtosis(as.vector(x), na.rm = na.rm)
}
