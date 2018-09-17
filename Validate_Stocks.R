library(pROC)

#TODO: What was that

# Train Test method
trainTest <- function(testIndex, allData, itermax=15, trace = T, costFun=completeDist){
  result <- list()
  
  print('Start Aggrigate')
  aggrigate(allData[-testIndex, ])
  
  print('Start Classification')
  classifierLimits <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                              DEoptim.control(itermax=itermax, trace = trace), 
                              fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
  # classify Training Data 
  
  trainClassLabels <- classify(classifierLimits)
  
  aggrigate(allData[testIndex, ])
  
  testClassLabels <- classify(classifierLimits)
  
  result[['classifierLimits']] <- classifierLimits
  result[['trainClassLabels']] <- trainClassLabels
  result[['testClassLabels']] <- testClassLabels
  #  result[['multiclass.roc']] <- multiclass.roc(result['trainClassLabels'], result['testClassLabels'])
  
  return (result)
  
}

doubleClassify <- function(data1, data2, itermax=15, trace = T, costFun=varSD){
  result <- list()
  
  aggrigate(data1)
  result[['bestLimits1']] <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                                     DEoptim.control(itermax=itermax, trace = trace), 
                                     fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
  result[['classes1']] <- classify(result[['bestLimits1']])
  
  aggrigate(data2)
  result[['bestLimits2']] <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                                     DEoptim.control(itermax=itermax, trace = trace), 
                                     fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
  result[['classes2']] <- classify(result[['bestLimits2']])
  
  
  result
}
multiClassify <- function(allData, itermax=15, trace = T, costFun=varSD){
  timeLength <- as.integer(max(allData$Date) / 2)
  
  
  result <- list()
  
  aggrigate(allData[which(allData$Date <= timeLength) , ])
  result[['bestLimits1']] <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                                     DEoptim.control(itermax=itermax, trace = trace), 
                                     fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
  result[['classes1']] <- classify(result[['bestLimits1']])
  
  
  aggrigate(allData[which(allData$Date > timeLength) , ])
  result[['bestLimits2']] <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                                     DEoptim.control(itermax=itermax, trace = trace), 
                                     fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
  result[['classes2']] <- classify(result[['bestLimits2']])
  
  result
}

multiClassifyOld <- function(segmentNo, allData, itermax=15, trace = T, costFun=varSD){
  timeLength <- max(allData$Date) / segmentNo
  timeSegment <- 1:timeLength
  
  result <- c()
  
  for(i in 1:segmentNo){
    temp <- list()
    currentTime <- allData$Date == timeSegment + ((i - 1) * timeLength)
    print(cbind(allData$Date,head( which(allData$Date == timeSegment))))
    stop('stop')
    aggrigate(allData[currentTime, ])
    
    temp[['bestLimits']] <- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                                    DEoptim.control(itermax=itermax, trace = trace), 
                                    fnMap=function(x) {round(x,0)}, costFun=costFun)$optim$bestmem
    temp[['classes']] <- classify(temp[['bestLimits']])
    
    result[[i]] <- temp
  }
  result
}

testFUn <- function(){
  cat('\t1\t2\t3\t4\t5\n')
  for(i in 1:5){
    cat (i, '\t')
    for(j in 1:5){
      cat(length(which((mcr[[i]]$classes == mcr[[j]]$classes) == T))/ 497, '\t')
    }
    cat('\n')
  }
}



