#TODO: Get rid of unused commented methods id:15
 ----
 <https://github.com/pollaeng/SMC/issues/13>
 Polla A. Fattah
 pollaeng@gmail.com
#TODO: Better commenting id:19
 # 
 ----
 <https://github.com/pollaeng/SMC/issues/17>
 Polla A. Fattah
 pollaeng@gmail.com

#boxplots for Temporal Atributes Acording to Classes
plotTAAC <- function(classes, className='class', row=2, col=2){
	DF <- split(DATA_FRAME[,c(TEMPORAL_ATTRIBUTES, TIME_COL)], DATA_FRAME[[className]])
	
	sdMean = c()
	for(ta in TEMPORAL_ATTRIBUTES){
		i <- 1
		
		par(mfrow=c(row,col),
		    oma = c(2,1,0,0),
		    mar = c(1.7,1.7,1.3,0.5),
		    mgp = c(0, 0.3, 0))
		
		for(df in DF){
			dat <- split(df[, ta], df[TIME_COL])
			
			sdMean[i] <- calculateSTD(dat)
			boxplot(dat, main=classes[i], axes = F, ylab=NA, xlab=NA)
			box()
			axis(side = 1, tck = -.01)
			axis(side = 2, las = 1, tck = -.01)
			i <- i + 1
		}

		mtext(ta, side=2, outer=T, line=-0.3, cex=1.2)
		mtext('Time Points', side=1, outer=T, line=0, cex=1.2)
		 
	}
	print('--------------------')
	print(mean(sdMean))
	print('--------------------')
	
}
calculateSTD <- function(dat){
  result = c()
  i = 1
  for (d in dat){
    result[i] <- sd(d)
    i <- i + 1
  }
  print(mean(result))
  return(mean(result))
   
}
attachClass <- function(cls, rounds = 10){
  DATA_FRAME$class <<- rep(cls, each = rounds)
}


#plotTAAC(c('Free Rider', 'Weak Contributor', 'Normal Contributor', 'Strong Contributor'))
#plotTAAC(c('Conditional Contributor', 'Free Rider', 'Triangle Contributor', 'Others')
#         , className =  'idtyp')

# library(plotrix)
# library(gplots)
# library(Hmisc)
# 
# #setwd("C://Users/pqf/Google Drive/PhD/Data/")
# #setwd("D://polla/GoogleDrive/PhD/Data/")
# #data <- read.csv(file.choose())
# 
# PERIOD <- 27
# 
# getPeriodData <- function(data, p){
# 
#   periodIndex <- which(data$period == p, arr.ind=T)
#   temp <- data[periodIndex,]
#   return (temp[order(temp$idsubj), ])
# 
# }
# 
# getPersonData <- function(p){
#   periodIndex <- which(data$idsubj == p, arr.ind=T)
#   temp <- data[periodIndex,]
#   return (temp[order(temp$period), ])
# }
# 
# dencityMatrix <- function(x, y){
# 	result <- matrix(0, nrow=21, ncol=21)
# 
# 	for(i in 1:length(x)){
# 		inc(result[x[i] + 1, y[i] + 1]) <-  1
# 	}
# 	
# 	return(result)
# }
# 
# belief2Contrib <- function (data) {
#   for(i in 1:PERIOD){
# 
#     perdiodData <- getPeriodData(data, i)
#     
#     plot(x=perdiodData$belief, y=perdiodData$contribution, col=perdiodData$idtyp
#          , xlim=c(-1,20), ylim=c(-1,20), main= paste0("Game round #", i),
#          xlab = "Players' belief about others contribution", 
#          ylab="Players' Contribution")
#   }
# }
# 
# belief2ContribHeat <- function (data) {
# 	for(i in 1:PERIOD){
# 		perdiodData <- getPeriodData(data,i)
# 		
# 		heatMap(x= dencityMatrix( x=perdiodData$contribution, y=perdiodData$belief), 
# 						xlab = "Players' belief about others contribution", 
# 						ylab="Players' Contribution", main= paste0("Game round #", i))
# 	
# 	}
# }
# 
# period2Contrib <- function(data, main=""){
#   perdiodContrib <- list()
#   for(i in 1:10)
#     perdiodContrib[[i]] <- getPeriodData(data, i)$contribution
#   
#   boxplot(x= perdiodContrib, main = main,
#           xlab="Time", ylab="Average amount of contribution")
# }
# 
# period2Payoff <- function(data){
# 	perdiodContrib <- list()
# 	for(i in 1:10)
# 		perdiodContrib[[i]] <- getPeriodData(data, i)$payoff
# 	
# 	boxplot(x= perdiodContrib)
# }
# 
# period2Belief <- function(data){
#   perdiodBelief <- list()
#   for(i in 1:10)
#     perdiodBelief[[i]] <- getPeriodData(data,i)$belief
#   
#   boxplot(x= perdiodBelief, main = "Player's average belief during 10 game sessions ",
#           xlab="Time", ylab="belief amount of other players contribution")
# }
# 
# leanierRegresion <- function(data){
#   slop <- c()
#   for(s in unique(data$idsubj)){
#     index = which(data$idsubj == s)
#     y = data$belief[index]
#     x = 1:10
#     reg <- lm(y ~ x)
# #    plot (x, y)
# #    abline(reg)
#     slop <- c(slop, reg$coefficients[[2]])
#   }
# #  print(length(which(slop < 0)))
# #  print(mean(slop))
#   hist(slop)
# }
# 
# playerInitialIntent <- function(data){
#   x = 0:20
#   for(s in unique(data$idsubj)){
#     index = which(data$idsubj == s)
#     y = data[index[1],]
#     y = c(y$b0, y$b1, y$b2, y$b3, y$b4, y$b5, y$b6, y$b7, y$b8, y$b9, y$b10, 
#           y$b11, y$b12, y$b13, y$b14, y$b15, y$b16, y$b17, y$b18, y$b19, y$b20)
#     plot(x, y, xlab="Others contribusion", xlim=c(0, 20), 
#          ylab="player contribution", ylim=c(0, 20), 
#          main=paste("player's ", s, " initial willingness to contribute"))
#     lines(x, y)
#   }
# }
# playerContribution <- function(data){
#   x = 1:10
# 
#   for(s in sort(unique(data$idsubj))){
# 
#     index = which(data$idsubj == s)
#     y = data$contribution[index]
# 
#     plot(x, y, xlab="Time", xlim=c(1, 10), 
#          ylab="player contribution", ylim=c(0, 20), 
#          main=paste0("player #", s, "'s contribution during the game rounds"))
#     lines(x, y)
#   }
# 
# }
# playerBelief <- function(data){
#   x = 1:10
#   
#   for(s in sort(unique(data$idsubj))){
#     
#     index = which(data$idsubj == s)
#     y = data$belief[index]
#     
#     plot(x, y, xlab="Time", xlim=c(1, 10), 
#          ylab="player Belief", ylim=c(0, 20), 
#          main=paste0("player #", s, "'s beleif during the game rounds"))
#     lines(x, y)
#   }
#   
# }
# 
# 
# 
# heatMap <- function(x, xlab="Column", ylab="Row", main="Main"){
#   
#   
#   #Build the matrix data to look like a correlation matrix
# 
# 	
#   x <- (x / sum(x)) * 100
#   x <- apply(x, 2, rev)
# 	x[which(x==0)] <- NA
#   xmin <- 0
#   xmax <- 1
#   
#   #Generate the palette for the matrix and the legend.  Generate labels for the legend
#   palmat <- color.scale(x, c(1, 0.4), c(1, 0.4), c(0.96, 1))
#   palleg <- color.gradient(c(1, 0.4), c(1, 0.4), c(0.96, 1), nslices=100)
#   lableg <- c(formatC(xmin, format="f", digits=2), 
#   						formatC(1*(xmax-xmin)/4, format="f", digits=2), 
#   						formatC(2*(xmax-xmin)/4, format="f", digits=2), 
#   						formatC(3*(xmax-xmin)/4, format="f", digits=2), 
#   						formatC(xmax, format="f", digits=2))
#   
#   #Set up the plot area and plot the matrix
#   par(mar=c(5, 5, 5, 8))
#   color2D.matplot(x, cellcolors=palmat, main=main, xlab=xlab, ylab=ylab,
#   								show.values=2, vcol=rgb(0,0,0), axes=FALSE, vcex=0.7)
#   axis(1, at=seq(0, 21, 1)-0.5, labels=seq(-1, 20, 1), tck=-0.01, padj=-1)
#   
#   #In the axis() statement below, note that the labels are decreasing.  This is because
#   #the above color2D.matplot() statement has "axes=FALSE" and a normal axis()
#   #statement was used.
#   axis(2, at=seq(0, 21, 1)-0.5, labels=seq(-1, 20, 1), tck=-0.01, padj=0.7)
#   
#   #Plot the legend
#   pardat <- par()
#   color.legend(pardat$usr[2]+0.5, 0, pardat$usr[2]+1, pardat$usr[2], 
#   						 paste(" ", lableg, sep=""), palleg, align="rb", 
#   						 gradient="y", cex=0.7)
#   
# }
# 
# eachPesronContrib <- function(data){
# 	subjects <- unique(data$idsubj)
# 	for(s in subjects){
# 		subjectRecords <- which(data$idsubj == s)
# 		cat("=====================================================\n")
# 		cat(s, " , ", data$idtyp[subjectRecords[1]], "\n") 
# 		cat("contribution = ", data$contribution[subjectRecords], "\n")
# 		cat("belief       = ", data$belief[subjectRecords], "\n")
# 		cat("payoff       = ", data$payoff[subjectRecords], "\n")
# 		cat("=====================================================\n\n")
# 	}
# }
# 
#playerParameter <- function(player){
#  s <- player$idsubj
#  png(paste0('P',s ,'.png'), width=800, height=400)
#  par(mfrow= c(1, 2))
#  
#  # Plot
#  plot(0:20, player$initial, main=paste0("predisposition for contribution\n idtype = ",player$idtyp ),
#       ylim=c(0,20), xlim=c(0,20), ylab="claiming Contribution",
#       xlab=paste0("Assuming Contribution of other players\n",
#                   "Claim contribution (mean = ", round(player$initialMean, 3),
#                   ", slop = ", round(player$initialSlop, 3), ")"))
#  lines(0:20, player$initial)
#  
#  abline(player$initialReg)
#  
#  # plot beleif
#  plot(1:10, player$belief, xlim=c(1, 10),
#       xlab= paste("Time\nMean of (contrib=", round(player$contribMean, 3),
#                   " ,belief=", round(player$beliefMean, 3), ")"),
#       ylab="player Belief", ylim=c(0, 20), col="blue")
#  lines(1:10, player$belief, col="blue")
#  
#  abline(player$beliefReg, col="blue")
#  
#  # plot Contributions
#  
#  
#  points(1:10, player$contribution, xlim=c(1, 10),
#         ylab="player Belief", ylim=c(0, 20),
#         main=paste0("player #", s, "'s beleif during the game rounds"), col="red")
#  lines(1:10, player$contribution, col="red", lty=2, type="o", pch=22)
#  abline(player$contiribReg, col="red", lty=2)
#  
#  legend(7, 20.5, c("Beleif","Contribution"), cex=0.8,
#         col=c("blue","red"), pch=21:22, lty=1:2);
#  
#  title(paste0("\nUser #", s), outer=TRUE)
#  title (main=paste0("\nContrbution Slop =",
#                     round(player$contiribSlop, 3), ", Belief Slop =", round(player$beliefSlop, 3), "\n newClass = ", player$newClass))
#  
#  
#  dev.off()
#  par(mfrow= c(1, 1))
#}
# 
# 
# period2BeliefCustom <- function(data, cls){
# 	perdiodBelief <- list()
# 	for(i in 1:10)
# 		perdiodBelief[[i]] <- getPeriodData(data,i)$belief
# 	
# 	boxplot(x= perdiodBelief, main = "Player's average belief during 10 game sessions ",
# 			  xlab="Time", ylab="belief amount of other players contribution")
# }
# period2ContribCustom <- function(data, cls, main=""){
# 	perdiodBelief <- list()
# 	which
# 	for(i in 1:10)
# 		perdiodBelief[[i]] <- getPeriodData(data[which(data[,1] ==cls),],i)$contribution
# 	
# 	boxplot(x= perdiodBelief, main = main,
# 			  xlab="Time", ylab="Contribution")
# }
# period2Contribution4all <- function(){#
# 	for(i in 1:4)
# 		period2ContribCustom(dataOld_type, i , bquote("Player's contribution during 10 game sessions of category " ~ alpha ~ . (i)))
# 	
# 	
# 	for(i in 1:5)
# 		period2ContribCustom(dataNew_class, i , bquote("Player's contribution during 10 game sessions of category " ~ beta ~ . (i)))
# 
# }
# main <-function(){
#   d = data
#  # belief2Contrib(d)
#   
#  belief2ContribHeat(d)
#   
# #  period2Belief(d)
# #  period2Contrib(dataNew_class)
#   #period2Payoff(d)
# 
# 
# #playerParameter(players)
# }
# main()
# #leanierRegresion(data)
# #period2Contribution4all()
