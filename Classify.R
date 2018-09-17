conditionalFUNs <- c()
classNames <- c()
# TODO: Optimise the method of classification if possible and give more comments

registerNewClass <- function(name, conditionFUN){
	conditionalFUNs <<- c(conditionalFUNs, conditionFUN)
	classNames <<- c(classNames, name)
	
	names(conditionalFUNs) <<- classNames
}
###### Classifier #########
classify <- function(curValue){
	items <- nrow(AGGRI_DATA)
	class <- rep(0, items)
	
	for(i in 1:items){
		for(classNum in 1:length(conditionalFUNs)){
			if(conditionalFUNs[[classNum]](AGGRI_DATA[i, ], curValue)){
				class[i] <- classNum
				break
			}
		}
	}
	return (class)
}
