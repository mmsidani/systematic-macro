compareErRiskRatio.test <- function(e1,e2,sig1,sig2,rho,w1,w2,hurdle){
# e1 is expected return of first asset
# e2 expected return of second asset
# sig1 is volatility (standard deviation) of first asset
# sig2 is vol of second asset
# rho is correlation between first and second assets
# w1 is portfolio weight in first asset
# w2 is portfolio weight in second asset
# hurdle is the rebalancing hurdle

	library(quadprog)
	
	if(w1+w2 != 1){
		stop(paste("Error in compareErRiskRatio.test(): weights should sum to 1. you passed:",w1,w2))
	}

	erRiskRatio <- function(e1,e2,sig1,sig2,rho,w1,w2,hurdle){
		return((w1*e1 + w2 * e2 + hurdle) / sqrt(w1^2 * sig1^2 + w2^2 * sig2^2 + 2 * w1 * w2 * rho * sig1 * sig2))
	}
	
	optimPort <- function(e1,e2,sig1,sig2,rho){
		cov12 <- rho * sig1 * sig2
		
		constraintsMat <- t(matrix(c(1,1,-1,0,1,0,0,-1,0,1), byrow=TRUE,ncol=2))
		constraintsVec <- c(1,-1,0,-1,0)
		
		varCovar <- matrix(c(sig1^2, cov12, cov12,sig2^2),nrow=2)
		
		optWeights <- solve.QP(varCovar,c(0,0),constraintsMat,constraintsVec,meq=1)
		
		return(optWeights[["solution"]])
	}
	
	optWeights <- optimPort(e1,e2,sig1,sig2,rho)
	
	currentRatio <- erRiskRatio(e1,e2,sig1,sig2,rho,w1,w2,hurdle)
	newRatio <- erRiskRatio(e1,e2,sig1,sig2,rho,optWeights[1],optWeights[2],hurdle)
	
	return(list(currentRatio = currentRatio,newRatio=newRatio,newOptWeights=optWeights))
}
