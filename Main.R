library("stargazer")
source('Aggrigate.R')
source('Classify.R')
source('Evaluate.R')
source('Bruteforce.R')
source('DEvolution.R')
source('Visualize.R')
source('Validate_Stocks.R')

#TODO: Figure out the best periods of time to be classified id:7
 ----
 <https://github.com/pollaeng/SMC/issues/4>
#TODO: Aggrigate the results of the periods (mean) id:6
 # 
 ----
 <https://github.com/pollaeng/SMC/issues/3>

##### Environment Variables #####
setVariables <- function(tp=4){
  TIME_POINT <<-tp
  TIME_COL <<- 'Date'
  ID_COL <<- 'Symbol'
  TEMPORAL_ATTRIBUTES <<- c("Close") # , "HighPercent"

# For full data of 125 time points
  LOWER <<- c( 1100, 500, 1600, 650, 550)
  UPPER <<- c( 1300, 750, 2000, 1000, 800)

  
  paramNames <- c('sdClose-VS', 'sdDiff-VS', 'sdClose-US', 'sdDiff-US', 'sdDiff-RS')
  names(LOWER) <<- paramNames
  names(UPPER) <<- paramNames
}

##### Manipulate ######
##### If needed, please create your own functions here #####

addValPercent <- function(colName, usedCol){
  for(id in ITEM_ID){
    ids <- DATA_FRAME[ID_COL] == id
    DATA_FRAME[ids, colName] <<- round((DATA_FRAME[ids, usedCol] / max(DATA_FRAME[ids, usedCol])) * 100, 0)
    
  }
}
# Difference of consecutive values
valDiffConsec <- function(subdata, usedCol, r=-1, abs=F){
  temp <- subdata[[usedCol]]
  x <- sd(temp[-1] - temp[-length(temp)]) * 10

  if(r != -1)	x <- round(x, r)
  if(abs)	x <- abs(x)
  
  return(x)
}

addConsDiffColumn <- function(){
  column <- rep(0, nrow(DATA_FRAME))

  for(id in ITEM_ID){
    ids <- DATA_FRAME[ID_COL] == id

    temp <- DATA_FRAME[ids,]$Close

    v <- temp[-1] - temp[-length(temp)]
    
    column[ids] <- c(round(mean(v), 0), v)

  }
  DATA_FRAME['closedDiff'] <<- column
  
}

addColumns <- function(){

  addAggrColumn(itemSD, 'ClosedPercentSD', 'Close', r = 3)
  addAggrColumn(valDiffConsec, 'ClosedDiffSD', 'Close', r=3)
}

####### Conditional functions for finding players ########
veryStable <- function(p, curValue){
  return(p$ClosedPercentSD < curValue[1] && p$ClosedDiffSD < curValue[2])
}

unstable <- function(p, curValue){
  return(p$ClosedPercentSD > curValue[3] || p$ClosedDiffSD > curValue[4])
}

ruggedStable <- function(p, curValue){
	if(p$ClosedDiffSD > curValue[5] )
		return (T)
	
	return (F)
}
smoothStable <- function(p, curValue){
	return (T)
}
registerClasses <- function(){
  # registering classes 
  registerNewClass('VS', veryStable)	# VS = Very Stable
  registerNewClass('US', unstable)	  # US = UnStable
  registerNewClass('RS', ruggedStable)# RS = Rough Stable
  registerNewClass('SS', smoothStable)# SS = Smooth Stable
  
}
###### Main Function #######
main <- function(){
	allData <<- NULL
	dataSplit <- 0
	set.seed(999)
	criteria <- c(centroidDist, completeDist, varSD, varSSE, varQuantile)
	
	setVariables(4)
	registerClasses()
	#FIXME the path shoud Change
	allData <<- read.csv('../Data/SP500 1-2015 to 7-2015 Normilized.csv')
	dataSplit <- 63
	
	testIndex <- which(allData$Date > dataSplit)

	ttr.centroidDistStock <<- trainTest(testIndex, allData, costFun=centroidDist)
	dput(ttr.centroidDistStock, file = 'SavedResults/ttr.centroidDistStock.txt')

	ttr.completeDistStock <<- trainTest(testIndex, allData, costFun=completeDist)
	dput(ttr.completeDistStock, file = 'SavedResults/ttr.completeDistStock.txt')

	ttr.varSDStock <<- trainTest(testIndex, allData, costFun=varSD)
	dput(ttr.varSDStock, file = 'SavedResults/ttr.varSDStock.txt')

	ttr.varSSEStock <<- trainTest(testIndex, allData, costFun=varSSE)
	dput(ttr.varSSEStock, file = 'SavedResults/ttr.varSSEStock.txt')

	ttr.varQuantileStock <<- trainTest(testIndex, allData, costFun=varQuantile)
	dput(ttr.varQuantileStock, file = 'SavedResults/ttr.varQuantileStock.txt')

	
##### If want to split more than two parts each
	# mcr.centroidDistStock <<- multiClassify(2, allData,  costFun=centroidDist)
	# dput(mcr.centroidDistStock, file = 'SavedResults/mcr.centroidDistStock.txt')
	# mcr.completeDistStock <<- multiClassify(2, allData, costFun=completeDist)
	# dput(mcr.completeDistStock, file = 'SavedResults/mcr.completeDistStock.txt')
	# mcr.varSDStock <<- multiClassify(2, allData, costFun=varSD)
	# dput(mcr.varSDStock, file = 'SavedResults/mcr.varSDStock.txt')
	# mcr.varSSEStock <<- multiClassify(2, allData, costFun=varSSE)
	# dput(mcr.varSSEStock, file = 'SavedResults/mcr.varSSEStock.txt')
	# mcr.varQuantileStock <<- multiClassify(2, allData, costFun=varQuantile)
	# dput(mcr.varQuantileStock, file = 'SavedResults/mcr.varQuantileStock.txt')
########
	#bestClassifier <- dd$optim$bestmem
	#addClass(classify(bestClassifier))
	#print(table(classify(bestClassifier)))
	
# 	plotTAAC(c('Very Stable', 'Unstable', 'Rough Stable', 'Smooth Stable')
# 				,row = 2, col=2)
# 	
  
	#	bf <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varSD)
}
loadRuleBasedStocks <- function(){
	ttr.centroidDistStock <<- dget(file = 'SavedResults/ttr.centroidDistStock.txt')
	ttr.completeDistStock <<- dget(file = 'SavedResults/ttr.completeDistStock.txt')
	ttr.varSDStock <<- dget(file = 'SavedResults/ttr.varSDStock.txt')
	ttr.varSSEStock <<- dget(file = 'SavedResults/ttr.varSSEStock.txt')
	ttr.varQuantileStock <<- dget(file = 'SavedResults/ttr.varQuantileStock.txt')
}

percentages <- function(){
  countEquals <- function(v1, v2)length(which((v1==v2) == T))
  result <<- list()
  result$centroidDistStock <<- countEquals( ttr.centroidDistStock$trainClassLabels, ttr.centroidDistStock$testClassLabels)/497
  result$completeDistStock <<-countEquals( ttr.completeDistStock$trainClassLabels, ttr.completeDistStock$testClassLabels)/497
  result$varSDStock <<-countEquals( ttr.varSDStock$trainClassLabels, ttr.varSDStock$testClassLabels)/497
  result$varSSEStock <<-countEquals( ttr.varSSEStock$trainClassLabels, ttr.varSSEStock$testClassLabels)/497
  result$varQuantileStock <<- countEquals( ttr.varQuantileStock$trainClassLabels, ttr.varQuantileStock$testClassLabels)/497
  return (result)
}
main()


