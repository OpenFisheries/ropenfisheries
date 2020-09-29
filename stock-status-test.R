
library(ropenfisheries)
library(rfisheries)
source('./R/stock_status_utils.R')

skj <- rfisheries::of_landings(species="SKJ")
ropenfisheries::stock_status(skj$catch)

bft <- rfisheries::of_landings(species="BFT")
cod <- rfisheries::of_landings(species="COD")

cc <- skj$catch

smoothed <- landings_sma(cc,5)
mx <- max(smoothed,na.rm=TRUE)
peakBoolean <- smoothed==mx
peakPos <- length(peakBoolean[1:which(peakBoolean==TRUE)])
length(cc)
l <- length(cc)
if (peakPos == l) {
  print('yes')
}

