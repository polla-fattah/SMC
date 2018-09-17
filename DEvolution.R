
library("DEoptim")

#TODO: Use other optimisation methods other than DE id:18
 ----
 <https://github.com/pollaeng/SMC/issues/16>
 Polla A. Fattah
 pollaeng@gmail.com
#LATER: Compare between these optimisation methos id:9
 ----
 <https://github.com/pollaeng/SMC/issues/7>
 Polla A. Fattah
 pollaeng@gmail.com

fnm <- function(x) {round(x,0)}
#fnm <- function(x, Len) matrix(round(x, 2), nrow=10*Len, ncol=Len,byrow=TRUE)


#fn <- function (x) x ^ 2

#upper <- 100
#lower <- -100

#dd = DEoptim(fn=fn, lower=lower, upper=upper, DEoptim.control( storepopfrom = 1))

#dd = DEoptim(fn=fn, lower=lower, upper=upper, fnMap=function(x) fnm(x,length(upper)))

#dd =DEoptim(fn=fn, lower=lower, upper=upper, fnMap=fnm,control=DEoptim.control(NP=1))
#print(dd$optim)

#plot(dd, plot.type="storepop")
# Genrose <- function(x) {
# 	## One generalization of the Rosenbrock banana valley function (n parameters)
# 	n <- length(x)
# 	## make it take some time ...
# 	Sys.sleep(.001)
# 	1.0 + sum (100 * (x[-n]^2 - x[-1])^2 + (x[-1] - 1)^2)
# }
# # get some run-time on simple problems
# maxIt <- 250
# n <- 5
# oneCore <- system.time( DEoptim(fn=Genrose, lower=rep(-25, n), upper=rep(25, n),
# 										  control=list(NP=10*n, itermax=maxIt,trace=F)))
# withParallel <- system.time( DEoptim(fn=Genrose, lower=rep(-25, n), upper=rep(25, n),
# 												 control=list(NP=10*n, itermax=maxIt, parallelType=1,trace=F)))
# ## Compare timings
# print(oneCore)
# print(withParallel)
# ## End(Not run)
# 

