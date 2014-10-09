## for Arima model with actual data model

filenames <- list.files("data", pattern="*.csv", full.names=TRUE)
stocks <- lapply(filenames, read.csv)
res <- lapply(stocks, summary)
for(stockcnt in 1:length(stocks)){
    stock = stocks[[stockcnt]]
    tsStock = ts(rev(stock$Close),start=c(2000, 1),frequency=12)
    train <- window(tsStock, end=2012)
    test <- window(tsStock, start=2013)
    
    fitr <-arima(train, order=c(15,3,3))
    fitr2 <-  arima(train, order=c(1,0,0), list(order=c(2,1,0), period=12))
    NETfitr <- nnetar(train)
    HWStockr = HoltWinters(train)
    HWStockr_ng = HoltWinters(train,gamma=FALSE)
    fitlr <- tslm(train ~ trend + season, lambda=0)
    stlStockr = stl(train,s.window="periodic")
    
    predictions = vector(mode="list")
    predictions[[1]] =  window(forecast(fitr,h=39)$mean, start=2013)
    predictions[[2]] =  window(forecast(fitr2,h=39)$mean, start=2013)
    predictions[[3]] =  window(forecast(NETfitr,h=39)$mean, start=2013)
    predictions[[4]] =  window(predict(HWStockr,n.ahead=39), start=2013)
    predictions[[5]] =  window(predict(HWStockr_ng,n.ahead=39), start=2013)
    predictions[[6]] =  window(forecast(fitlr, h=39)$mean , start=2013)
    predictions[[7]] =  window(forecast(stlStockr, h=39)$mean, start=2013)
    
    plot(train, xlim=c(2000, 2016), type= "n")
    lines(train, col = "black")
    lines(test, col = "blue")
    lines(predictions[[1]], col = "red")
    lines(predictions[[2]], col="green")
    lines(predictions[[3]], col="yellow")
    lines(predictions[[4]], col="orange")
    lines(predictions[[5]], col="lightblue")
    lines(predictions[[6]], col="lightblue2")
    lines(predictions[[7]], col="violet")
    
    mae = data.frame()
    for(pred in 1:length(predictions)){
      for(i in 1:length(test))
      {
        ##print(predictions[[pred]][i])
        mae[i, pred] <-abs(predictions[[pred]][i]-test[i])
      }  
    }
    
    minimumSqrError = apply(mae, 2, sum)
    best = which.min(minimumSqrError)
    
    plot(train, xlim=c(2000, 2016), type= "n")
    lines(train, col = "black")
    lines(test, col = "blue")
    lines(predictions[[best]], col = "red")
    
}
