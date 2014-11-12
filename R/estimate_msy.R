
#' Estimate MSY
#' A function to estimate Maximum Sustainable Yield from a time series of landings
#' The function requires only the maximum catch of a time-series.
#' Based on the work of Cheung & Srinivasan who find a statistical relationship between
#' the maximum annual landings in a time-series for a given species and the MSY as
#' estimated through traditional stock assessment methods.
#' @param max_catch The maximum catch of a time-series
#' @keywords MSY, Maximum Sustainable Yield
#' @export

estimate_msy <- function(max_catch) {
  return(10^(log10(max_catch)*0.8458 + 0.3777))
}
