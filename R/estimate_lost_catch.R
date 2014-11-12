

#' Estimate Lost Catch
#' A function to estimate lost catch due to over-fishing
#' Based on the work of Cheung & Srinivasan 
#' The data.frame must also include 
#' @param data A data.frame with two columns: 1. a catch time series normalized to the max catch of the time series. 
#' That is, each year of landings expressed as a % of the maximum annual catch. 2. A column that categorizes each year of catch as
#' either developing, exploited, over-exploited, collapsed, or rebuilding.
#' @keywords MSY, Maximum Sustainable Yield, lost catch
#' @export

estimate_lost_catch <- function(data, msy, max_catch) {
  lost_catch_temp <- ((msy/max_catch)-data$normalized)*max_catch
  data$lost_catch = ifelse(lost_catch_temp>= 0, lost_catch_temp, 0)
  data$lost_catch[which(data$stock_status=="developing"|data$stock_status=="exploited")] <- 0
  return(data$lost_catch)
}
