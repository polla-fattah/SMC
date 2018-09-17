library('BatchGetSymbols')
#TODO: Harvest Date
#TODO: normalise and transpose datasets

norm <- function(x) as.integer((x - min(x)) / (max(x) - min(x)) * 1000)

transposeAndNormalize <- function(data, col){
  
  tickers <- t(unique(data['ticker']))
   
  listTickers<-lapply(tickers,function(s){
    norm(data[as.character(data$ticker) == s , col])
  })
  tickerlsValue<-as.data.frame(matrix(unlist(listTickers), nrow=length(tickers)))
  tickerlsValue$ticker<-t(tickers)
  
  tickerlsValue
}

normalizeDataFrame<- function(data){
  tickers <- t(unique(data[['ticker']]))
  data["date"] <- rep(0, length(data[,1]))
  for (tt in tickers){
    index <- which(data['ticker'] == tt)
    data[index , "price.close"] <- norm(as.matrix(data[index , "price.close"]))
    data[index ,"date"] <- 1:length(index)
  }
  
  return(data)
}


tickers <- GetSP500Stocks()$tickers
dates <- c("2013-01-01", "2013-04-01", "2013-07-01", "2013-10-01", 
           "2014-01-01", "2014-04-01", "2014-07-01", "2014-10-01", 
           "2015-01-01", "2015-04-01", "2015-07-01", "2015-10-01", 
           "2016-01-01", "2016-04-01", "2016-07-01", "2016-10-01", 
           "2017-01-01", "2017-04-01", "2017-07-01", "2017-10-01", 
           "2018-01-01")
for(k in 1:2){
  i <- 1
  while( i < length(dates)){
    first.date <- dates[i]
    last.date <- dates[ i + k]
    fileName <- paste0(first.date, " to ", last.date, ".csv")
    print(fileName)
    i = i + k
  data <- read.csv(paste0("data/Original/", fileName))

#    result <- BatchGetSymbols(tickers = tickers,
#                              first.date = first.date,
#                             last.date = last.date, 
#                             thresh.bad.data = 0.85,
#                             do.cache=FALSE)
     
     
#    normAndTrans <- transposeAndNormalize(result$df.tickers[,c(4,8)], "price.close")
  #    normalize <- normalizeDataFrame(result$df.tickers[,c(4,8)])
      normalize <- normalizeDataFrame(data[,c(4,8)])
  
    write.csv(x =  normalize, file = paste0("data/normalized/", fileName), row.names = F)
#    write.csv(x =  normAndTrans, file = paste0("data/normalized + Transformed/", fileName), row.names = F)
#    write.csv(x =  result$df.tickers, file = paste0("data/Original/", fileName), row.names = F)
  }
}  


