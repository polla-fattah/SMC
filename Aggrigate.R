#==== 
# This file is optional it can be used to create aggregative columns before classification 
# as the aggregative columns can be created using other means like Excel, SPSS or Matlab.
#
# This file presents some generic aggregative functions for each item. These functions 
# aggregate temporal values of all items which occur in each time point. If you need a more
# specific function which is not presented here, you can simply define them.
#
# You can start the process by running the 'main' function which it will ask for a temporal 
# data which you want to add new aggregative attributes to it then it will run 
# 'addColumns' function which it should call all necessary functions.

#==== override this function to add columes this will be called by main function ====
addColumns <- function(){

}

#==== This function is core function wich calls other aggrigative functions =====
addAggrColumn <- function(fun, colName, dataCol, r=-1, abs=F){
	column <- rep(0, nrow(DATA_FRAME))
	AGGRI_DATA[, colName] <- rep(0, nrow(AGGRI_DATA))
	
	for(id in ITEM_ID){
		ids <- DATA_FRAME[ID_COL] == id
		aid <- AGGRI_DATA[ID_COL] == id
		val <- fun(DATA_FRAME[ids,], dataCol, r, abs)
		
		column[ids] <- val
		AGGRI_DATA[aid, colName] <<- val

	}
	DATA_FRAME[colName] <<- column
}

#==== Generic Aggrigative Functions ====
itemSD <- function(subdata, usedCol,  r, abs){
	x <- sd(unlist(subdata[usedCol]))
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}
itemMean <- function(subdata, usedCol, r, abs){
	x <- mean(unlist(subdata[usedCol]))
	
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}
itemMax <- function(subdata, usedCol, r, abs){
	x <- max(unlist(subdata[usedCol]))
	
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}
itemMin <- function(subdata, usedCol, r, abs){
	x <- min(subdata[usedCol])
	
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}
itemMode <- function(subdata, usedCol, r, abs){
	x <- as.integer(names(which.max(table(subdata[usedCol]))))
	
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}
itemTotal <- function(subdata, usedCol, r, abs){
	x <- sum(subdata[usedCol])
	
	if(r != -1)	x <- round(x, r)
	if(abs)	x <- abs(x)
	
	return(x)
}

itemValCount <- function(subdata, usedCol, r, abs){
	x <- table(subdata[usedCol])
	
	if(!(as.character(r) %in% names(x)))	return(0)
	x = c(x[names(x) == r])
	return (x)
}
#==== Non-Aggrigative Functions ====
# Non-Aggrigative functions which add column by calculating other columns
# regardless to the items' id
addColDiff <- function(dataCol1, dataCol2, colName, r=-1, abs=F){
	column <- DATA_FRAME[[dataCol1]] - DATA_FRAME[[dataCol2]]
	
	if(r != -1) column <- round(column, r)
	if(abs) column <- abs(column)
	
	DATA_FRAME[colName] <<- column
}
addColSum <- function(dataCol1, dataCol2, colName, r=-1, abs=F){
	column <- DATA_FRAME[[dataCol1]] + DATA_FRAME[[dataCol2]]
	
	if(r != -1) column <- round(column, r)
	if(abs) column <- abs(column)
	
	DATA_FRAME[colName] <<- column
}

copyCol <- function(dataCol, colName, r=-1, abs=F){
	column <- DATA_FRAME[dataCol]
	
	if(r != -1) column <- round(column, r)
	if(abs) column <- abs(column)
	
	DATA_FRAME[colName] <<- column
}
addColsMean <- function(dataCols, colName, r=-1, abs=F){
	column <- apply(apply(DATA_FRAME[,dataCols],1,unlist),2,mean)
	if(r != -1) column <- round(column, r)
	if(abs) column <- abs(column)
	
	DATA_FRAME[colName] <<- column
}

removeCol <- function(colName){
	DATA_FRAME[colName] <<- NULL	
}
###################################
addClass <- function(class, className='class'){
	AGGRI_DATA[className] <<- class
	DATA_FRAME[className] <<- rep(0, nrow(DATA_FRAME))
	for(i in 1:length(class)){
		DATA_FRAME[DATA_FRAME[[ID_COL]] == AGGRI_DATA[i, ID_COL], className] <<- AGGRI_DATA[i, className]
	}


}
#==== Main function ====

aggrigate <- function(data){

	DATA_FRAME <<- data
	# if there is need the data to be sorted 
	#DATA_FRAME[with(DATA_FRAME, order(idsubj, period)), ], 20	
	
	ITEM_ID <<- unique(DATA_FRAME[[ID_COL]])
	
	AGGRI_DATA <<- unique(DATA_FRAME[ID_COL])
	rownames(AGGRI_DATA) <<- NULL
	
	# if there is need the data to be sorted 
	#DATA_FRAME[with(DATA_FRAME, order(idsubj, period)), ], 20
	addColumns()

}
