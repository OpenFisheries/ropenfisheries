
#' Simple Moving Average 
#'
#' A function to compute a simple moving average
#' @param x A timeseries of data
#' @param n The number of observations to average. Default is 5 period moving average.
#' @keywords simple moving average
#' @examples
#' sma(c(1:100))

sma <- function(x,n=5) {
  j <- n-1
  NAs <- rep(NA,j)
  return( c(NAs,sapply(1:(length(x)-j), function(X) sum(x[X:(X+j)])/n)) )
}

#' Landings SMA
#' This function is an adjusted SMA with the ends filled.
#' Avoids the case of missing data (NAs) at the beginning and end of the data series.
#' @param x A timeseries of data
#' @param n The number of observations to average. Default is 5 period moving average.
#' @keywords simple moving average
#' @examples
#' sma(c(1:100))

landings_sma <- function(x,n=5) {
  l <- length(x)
  if(l>10) {
    ma <- sma(x, n)[n:l]
    ma2 <- sma(x,3)
    start <- c(x[1],ma2[3])
    end <- c(ma2[length(x)],x[l])
    return( c(start,ma,end) )
  } else {
    return( x )
  }
}

categorize_stock_pct <- function(x) {
  if(is.na(x)) {
    t <- NA
  } else if(x < .1) {
    t <- 0
  } else if(x >= .1 & x < .5) {
    t <- 1
  } else if(x >= .5) {
    t <- 2
  }
  return(t)
}

categorize_pre_peak <- function(X) {
  temp <- X
  temp[which(X==2)] <- "exploited"
  temp[which(X==0 | X==1)] <- "developing"
  return(temp)
}

categorize_post_peak <- function(X) {
  temp <- X
  temp[which(X==0)] <- "collapsed"
  temp[which(X==1)] <- "over-exploited"
  temp[which(X==2)] <- "exploited"
  return(temp)
}

categorize_post_peak_min <- function(X) {
  temp <- X
  temp[which(X==0)] <- "collapsed"
  temp[which(X==1)] <- "recovering"
  temp[which(X==2)] <- "exploited"
  return(temp)
}