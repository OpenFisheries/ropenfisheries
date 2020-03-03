
#' Stock Status
#' This function takes a landings time series from 1950 to 2011 as the argument 
#' Outputs the time-series of stock-status, MSY, and lost potential catch
#' @param data a time-series of landings.
#' @export
stock_status <- function(data, n=5) {
  l <- length(data)
  if(l < 10) {
    results <- list( 
      status=data.frame(
        year = c(1950:2015), 
        catch = data,
        stock_status=rep(NA, l),
        normalized=rep(NA, l),
        lost_catch=rep(NA, l),
        peak_year=NA,  
        msy=NA, 
        max_catch=NA
      )
    )
  } else {
    smoothed <- landings_sma(data,n)
    mx <- max(smoothed,na.rm=TRUE)
    peakBoolean <- smoothed==mx
    peakPos <- length(peakBoolean[1:which(peakBoolean==TRUE)])
    print(peakPos)
    if (peakPos == l) {
      results <- list(
        status=data.frame(
          year = c(1950:2015), 
          catch = data,
          stock_status=rep("developing",l),
          normalized=rep(NA, l),
          lost_catch=rep(NA, l),
          peak_year=NA,  
          msy=NA, 
          max_catch=NA
        )
      )
    } else if (l-peakPos < 10) {
      results <- list(
        status=data.frame(
          year = c(1950:2015), 
          catch = data,
          stock_status=rep(NA, l),
          normalized=rep(NA, l),
          lost_catch=rep(NA, l),
          peak_year=NA,  
          msy=NA, 
          max_catch=NA
        )
      )
    } else {
      pct_of_max <- smoothed/mx
      mn <- min(smoothed[peakPos:l],na.rm=TRUE)
      minBoolean <- smoothed[(peakPos+1):l]==mn
      postPeakMinPos <- length(minBoolean[1:which(minBoolean==TRUE)]) + peakPos
      start <- sapply(pct_of_max[1:peakPos], categorize_stock_pct)
      mid <- sapply(pct_of_max[(peakPos+1):postPeakMinPos], categorize_stock_pct)
      if(postPeakMinPos == l) {
        end <- c()
      } else {
        end <- sapply(pct_of_max[(postPeakMinPos+1):l], categorize_stock_pct)
      }
      
      msy <- estimate_msy(mx)
      
      stock_status=as.factor(
        c( 
          categorize_pre_peak(start), 
          categorize_post_peak(mid), 
          categorize_post_peak_min(end) 
        )
      )
      
      # this is in testing now.
      lost_catch_temp <- ((msy/mx)-pct_of_max)*mx
      lost_catch = ifelse(lost_catch_temp>= 0, lost_catch_temp, 0)
      lost_catch[which(stock_status=="developing"|stock_status=="exploited")] <- 0
      
      results <- list(
        status=data.frame(
          year = c(1950:2015), 
          catch = data,
          stock_status=stock_status,
          normalized=pct_of_max,
          lost_catch=lost_catch,
          peak_year=(peakPos+1949),  
          msy=msy, 
          max_catch=mx
        )
      )
    }
  }
  return( results )
}
