
library('clv')
library('mcclust')

# External clustering validity measures function with single function call
Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)

#This function specially customised for Simons' Request
evaluateDiff <- function(curValue, costFun=FM, ...){
  clust <- classify(curValue)
  class <- DATA_FRAME$idtyp[match(AGGRI_DATA$idsubj, DATA_FRAME$idsubj)]
  costFun(class, clust)

}


evaluate <- function(curValue, costFun=SD, ...){
	clust <- classify(curValue)
	total <- 0
	for(i in unique(DATA_FRAME[,TIME_COL])){
		data <- DATA_FRAME[(DATA_FRAME[TIME_COL] == i ), TEMPORAL_ATTRIBUTES, FALSE]
		total <- total + costFun(data, clust, ...)
	}
	c(total)
}

###### Single Temporal Variable #######
singleVariableTest <- function (fn, data, clust){
	total <- 0
	clusters <- unique(clust)
	t <- table(clust)

	for(i in clusters){
		r <- fn(data[clust == i,])
		if(is.na(r)) r <- 0
		
		total <- total + (r * t[names(t) == i])
	}
	total
}

varQuantile <- function (data, clust){
	fn <- function(var){q <- quantile(var); return (q[4] - q[2])}
	total <- 0
	for(i in TEMPORAL_ATTRIBUTES)
		total <- total + singleVariableTest(fn , data[, i, F], clust)
	total
}

varSSE <- function (data, clust){
	fn <- function(var){sum((mean(var) - var) ^ 2)}
	total <- 0
	for(i in TEMPORAL_ATTRIBUTES)
		total <- total + singleVariableTest(fn , data[, i, F], clust)
	total
}
	
varSD <- function(data, clust){
	total <- 0
	for(i in TEMPORAL_ATTRIBUTES)
		total <- total + singleVariableTest(sd , data[, i, F], clust)
	total
}

######## Distance Criteria ########
completeDist <- function(data, clust){
	fn <- function(x) sum(dist(x))
	singleVariableTest(fn , data, clust)
}

centroidDist <- function(data, clust){
	fn <- function(x) {
		if(is.null(nrow(x))) 
			return (1000000);
		sum(sqrt(rowSums(t(t(x)-colMeans(x))^2)))
	}
	singleVariableTest(fn , data, clust)
	
}
###### Internal Criteria #######
Dunn <- function(data, clust, intracls="centroid", intercls="centroid"){
	data <- sapply(X=data, FUN= as.double)
	
	clust <- as.integer(clust)
	if(length(table(clust)) < 4)
	  return(1000000)
	scatt <- cls.scatt.data(data, clust)

	clv.Dunn(scatt, intracls = intracls, intercls = intercls)
}

Davies.Bouldin <- function(data,clust, intracls="centroid", intercls="centroid"){
	data <- sapply(X=data, FUN= as.double) 
	clust <- as.integer(clust)
	if(length(table(clust)) < 4)
	  return(1000000)
	scatt <- cls.scatt.data(data, clust)
	clv.Davies.Bouldin( scatt, intracls = intracls, intercls = intercls)
}

SD <- function(data, clust, alfa = 4){
	data <- sapply(X=data, FUN= as.double) 
	clust <- as.integer(clust)
	
	scatt <- clv.Scatt(data, clust)
	dis <- clv.Dis(scatt$cluster.center)
	
	SD <- clv.SD(scatt$Scatt, dis, alfa=alfa) # alfa is equal to number of clusters 
}
SDbw <- function(data, clust){
	data <- sapply(X=data, FUN= as.double) 
	clust <- as.integer(clust)

	scatt <- clv.Scatt(data, clust)
	dens.bw <- clv.DensBw(data, clust, scatt)
	SDbw <- clv.SDbw(scatt$Scatt, dens.bw)
}
