library(dtw)
#TODO: Compare Between clustering and classification
source('Aggrigate.R')
transpos <- function (DATA_FRAME) {
	result <<- data.frame()
	
	for( s in unique(DATA_FRAME$Symbol)){
		result <<- rbind(result, t(DATA_FRAME$ClosedPercent[which(DATA_FRAME$Symbol == s)]))
		
	}
	
	result <<- cbind(symbol=unique(DATA_FRAME$Symbol), result)
	return (result)
}


ID_COL <<- 'Symbol'

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

addColumns <- function(){
	
	addValPercent('ClosedPercent', 'Close')
	addAggrColumn(itemSD, 'ClosedPercentSD', 'Close', r = 3)
	addAggrColumn(valDiffConsec, 'ClosedDiffSD', 'Close', r=3)
	
}

getClustData <- function(){
  # From aggrigatedClustering
  kmAgriAll <<- dget(file = 'SavedResults\\kmAgriAll.txt')
  kmAgriH1 <<- dget(file = 'SavedResults\\kmAgriH1.txt')
  kmAgriH2 <<- dget(file = 'SavedResults\\kmAgriH2.txt')
  
  # From closeClustering
  closedKMAll <<- dget(file = 'SavedResults\\closedKMAll.txt')
  closedKMH1 <<- dget(file = 'SavedResults\\closedKMH1.txt')
  closedKMH2 <<- dget(file = 'SavedResults\\closedKMH2.txt')
  
  # From closeTemporalClustering
  dtwHiraClustAll <<- dget(file = 'SavedResults\\dtwHiraClustAll.txt')
  dtwHiraClustH1 <<- dget(file = 'SavedResults\\dtwHiraClustH1.txt')
  dtwHiraClustH2 <<- dget(file = 'SavedResults\\dtwHiraClustH2.txt')
  
  
}

aggrigatedClustering <- function(){
  print('start aggrigatedClustering')
  
  allData <<- NULL
  AGGRI_DATA <<-NULL
  DATA_FRAME <<- NULL
  set.seed(999)
  
  allData <<- read.csv('../Data/SP500 1-2015 to 7-2015 Normilized.csv')
  
  aggrigate(allData)
  kmAgriAll <<- kmeans(AGGRI_DATA[, -1], 4)
  dput(kmAgriAll, file = 'SavedResults\\kmAgriAll.txt')
  
  dataSplit <<- as.integer(max(allData$Date)/2)
  indexOfHalf <<- which(allData$Date <= dataSplit)
  
  firstHalfData <<- allData[indexOfHalf, ]
  secondHalfData <<- allData[-indexOfHalf, ]
  
  AGGRI_DATA <<-NULL
  DATA_FRAME <<- NULL
  aggrigate(firstHalfData)
  
  kmAgriH1 <<- kmeans(AGGRI_DATA[, -1], 4)
  dput(kmAgriH1, file = 'SavedResults\\kmAgriH1.txt')
  
  AGGRI_DATA <<-NULL
  DATA_FRAME <<- NULL
  aggrigate(secondHalfData)
  
  kmAgriH2 <<- kmeans(AGGRI_DATA[, -1], 4)
  dput(kmAgriH2, file = 'SavedResults\\kmAgriH2.txt')
}

closeClustering <- function(){
  print('start closeClustering')
  
  closedData <<- read.csv('../Data/SP500 Close Price 1-2015 to 7-2015 Normalized Transposed.csv')
  closedKMAll <<- kmeans(closedData[, -1], 4)
  dput(closedKMAll, file = 'SavedResults\\closedKMAll.txt')
  
  firstHalf <- 2:(ncol(closedData)/2)
  secondHalf <- (ncol(closedData)/2):ncol(closedData)
  
  closedKMH1 <<- kmeans(closedData[, firstHalf], 4)
  dput(closedKMH1, file = 'SavedResults\\closedKMH1.txt')
  
  closedKMH2 <<- kmeans(closedData[, secondHalf], 4)
  dput(closedKMH2, file = 'SavedResults\\closedKMH2.txt')
}
closeTemporalClustering <- function(){
  print('start closeTemporalClustering')
  
  closedData <<- read.csv('../Data/SP500 Close Price 1-2015 to 7-2015 Normalized Transposed.csv')
  
  dtwDist <- dist(closedData[, -1], method="DTW")
  dtwHiraClustAll <<- hclust(dtwDist, method="average")
  dtwHiraClustAll <<- cutree(dtwHiraClustAll, k=4)
  dput(dtwHiraClustAll, file = 'SavedResults\\dtwHiraClustAll.txt')
  
  
  firstHalf <- 2:(ncol(closedData)/2)
  dtwDist <- dist(closedData[, firstHalf], method="DTW")
  dtwHiraClustH1 <<- hclust(dtwDist, method="average")
  dtwHiraClustH1 <<- cutree(dtwHiraClustH1, k=4)
  dput(dtwHiraClustH1, file = 'SavedResults\\dtwHiraClustH1.txt')
  
  
  secondHalf <- (ncol(closedData)/2):ncol(closedData)
  dtwDist <- dist(closedData[, secondHalf], method="DTW")
  dtwHiraClustH2 <<- hclust(dtwDist, method="average")
  dtwHiraClustH2 <<- cutree(dtwHiraClustH2, k=4)
  dput(dtwHiraClustH2, file = 'SavedResults\\dtwHiraClustH2.txt')
  
}
main <- function(calculate=FALSE){
  if(calculate){
    aggrigatedClustering()
    closeClustering()
    closeTemporalClustering()  
  }
  else{
    getClustData()
  }

}
percentages <- function(){ # Comparison between first and second quarters
  require(clv)

  Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
  FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
  countEquals <- function(v1, v2)length(which((v1==v2) == T))
  
  result <<- list()
  result$kmAgri_FM <<- FM( kmAgriH1$cluster, kmAgriH2$cluster)
  result$closedKM_FM <<-FM( closedKMH1$cluster, closedKMH2$cluster)
  result$dtwHiraClust_FM <<-FM( dtwHiraClustH1, dtwHiraClustH2)
  
  result$kmAgri_Ja <<- Jaccard( kmAgriH1$cluster, kmAgriH2$cluster)
  result$closedKM_Ja <<-Jaccard( closedKMH1$cluster, closedKMH2$cluster)
  result$dtwHiraClust_Ja <<-Jaccard( dtwHiraClustH1, dtwHiraClustH2)
  
  result$kmAgri_Pre <<- countEquals( kmAgriH1$cluster, kmAgriH2$cluster) / 497
  result$closedKM_Pre <<-countEquals( closedKMH1$cluster, closedKMH2$cluster) / 497
  result$dtwHiraClust_Pre <<-countEquals( dtwHiraClustH1, dtwHiraClustH2) / 497
  
  
  return (result)
}

#main(FALSE)
