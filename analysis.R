filenames <- list.files("data", pattern="*.csv", full.names=TRUE)
stocks <- lapply(filenames, read.csv)
res <- lapply(stocks, summary)

stock = stocks[[1]]

tsStock = ts(rev(stock$Close),start=c(2000, 1),frequency=12)
train <- window(tsStock, end=2013)
test <- window(tsStock, start=2013)
tl = seq(2000,2014,length=length(tsStock))
tl2 = tl^7
polyStock = lm(tsStock ~ tl + tl2)
tsStockTrend1 = ts(polyStock$fit, start=c(2000,1), frequency=12)
plot(tsStock, xlim = c(2000,2015))
abline(v=2014.25)
lines(tsStockTrend)

stlStock = stl(tsStock,s.window="periodic")
plot(stlStock)
tsStockTrend2 = stlStock$time.series[,2]
plot(forecast(stlStock))

plot(tsStock, xlim = c(2000,2015))
abline(v=2014.25)
lines(tsStockTrend1, col = "red")
lines(tsStockTrend2, col = "blue")

HWStock1_ng = HoltWinters(tsStockTrend1,gamma=FALSE)
HWStock1 = HoltWinters(tsStockTrend1)
NETfit1 <- nnetar(tsStockTrend1)
fit12 <-   arima(tsStockTrend1, order=c(1,0,0), list(order=c(2,1,0), period=12))
fitl1 <- tslm(tsStockTrend1 ~ trend + season, lambda=0)
stlStock1 = stl(tsStockTrend1,s.window="periodic")

HWStock2_ng = HoltWinters(tsStockTrend2,gamma=FALSE)
HWStock2 = HoltWinters(tsStockTrend2)
NETfit2 <- nnetar(tsStockTrend2)
fit2 <-    arima(tsStockTrend2, order=c(15,3,3))
fit22 <-    arima(tsStockTrend2, order=c(1,0,0), list(order=c(2,1,0), period=12))
fitl2 <- tslm(tsStockTrend2 ~ trend + season, lambda=0)
stlStock2 = stl(tsStockTrend2,s.window="periodic")
