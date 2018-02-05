# https://www.r-bloggers.com/f-is-for-forecast/
library(fpp2) # loads data and forecast, fma, expsmooth, ggplot

z <- ts(c(123,0,3,4,13,39,78,52,110,33,4,1),start=2010,frequency = 4)
print(z)

m <- data.frame(y=as.matrix(melsyd), date=time(melsyd))

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers") +
  xlab('Year') + ylab('thousands')

ggplot(m, aes(x=date, y=y.First.Class)) +
  geom_point() +
  geom_line() +
  ggtitle('Some stuff') +
  xlab('year') + ylab('thousands')
