
library(ropenfisheries)
library(rfisheries)
library(ggplot2)

gl <- of_landings()

sab <- of_landings(species='SAB')$catch
ss <- stock_status(sab)$status
ss$year <- 1:nrow(ss) + 1949

ggplot(ss, aes(x=year, y=catch)) + 
  geom_area(aes(fill=stock_status, group=stock_status)) 
  # geom_line()
  # geom_area(mapping = aes(
  #     x=ifelse(year > 1950 & year<=(1949+23), year, 0)
  #   ), fill='red') + xlim(1949,2020)



stsregime <- data.frame(
  regime=1:5,
  start=c(1950,1969,1999,2005,2007),
  end=c(1969,1999,2005,2007,2018),
  status=c('developing', 'exploited', 'over-exploited', 'exploited', 'over-exploited')
)

ggplot(ss) + 
  geom_rect(aes(xmin=start, xmax=end, fill=status), ymin=-Inf, ymax=Inf, alpha=.2, 
            data=stsregime) +
  geom_line(aes(year, catch))


presidential <- subset(presidential, start > economics$date[1])
# Figure 34
p <- ggplot(economics) +
  geom_rect(aes(xmin = start, xmax = end, fill = party),
            ymin = -Inf, ymax = Inf, alpha = 0.2,
            data = presidential)
print(p)

p <- p + geom_line(aes(date, unemploy))
print(p)
