mvOpt.getSolverObject <- function(riskMeasure,rateData,numAssets,rollingWindow,halfLife,varScaleFactor){
# calculate the var-covar matrix ignoring fx risk since we unit-hedge and the fx risk only concerns unexpected profit/loss at end of period, which should be small
	
	return(list(coVar = varianceCovariance.riskMatrix(riskMeasure,rateData,numAssets,rollingWindow,halfLife,varScaleFactor)))
}

mvOpt.getWeights<-function(solverObject,targetRet,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs){
# this solves the QP problem; i.e., it gets one point on the efficient frontier
# targetRet is a scalar; the target return for which we want a portfolio
# expectedRets is the vector of expected returns (ER's) of the assets
# varcovar is the var-covar matrix of the assets
# returns a list() with 3 components: portRet, the portfolio return; portRisk, the portfolio stdev; portWeights, the weights in each asset
	
	# instead of passing yet one more argument to tell us how many assets we have, we use the length of expectedRets to guess that
	numAssets<-length(expectedRets)
	if( !is.null(targetRet ) ){
		# the constraints are: return of the portfolio should be targetRet plus the constraints from the constraints file		
		constraintsLhs<-rbind(expectedRets,constraintsLhs,deparse.level=0)
		constraintsRhs<-c(targetRet,constraintsRhs)
		constraintsNumEqs<-constraintsNumEqs+1
	}
	
	# the quadprog QP solver requires the transpose of the matrix we just built
	constraintsLhs<-t(constraintsLhs)
	# now solve it. rep(0,numAssets) is a vector of 0's; we have that because there's no linear term in our objective function. Note: passing the inverse of the choleski triangle of varCovar and setting factorized to TRUE did not produce any gain in performance
	qpResults<-solve.QP(solverObject[["coVar"]],rep(0,numAssets),constraintsLhs,constraintsRhs,constraintsNumEqs,factorized=FALSE)
	
	if( is.null(targetRet)){
		targetRet=sum(qpResults[["solution"]]*expectedRets)
		riskAversionFactor <- NA # we do not have an expected return constraint when calculating the minimum variance portfolio
	} else {
		# we piggy-back on the optimization to find the appropriate factor to use in our quadratic (expected) utility
		riskAversionFactor <- 1 / qpResults[["Lagrangian"]][1]
	}
	
	return(list(portRet=targetRet,portRisk=sqrt(2*qpResults[["value"]]),portWeights=qpResults[["solution"]],riskAversionFactor=riskAversionFactor))
}

mvOpt.getMaxUWeights<-function(solverObject,riskAversion,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs){
# this solves the QP problem; i.e., it gets one point on the efficient frontier
# targetRet is a scalar; the target return for which we want a portfolio
# expectedRets is the vector of expected returns (ER's) of the assets
# varcovar is the var-covar matrix of the assets
# returns a list() with 3 components: portRet, the portfolio return; portRisk, the portfolio stdev; portWeights, the weights in each asset
	
	# instead of passing yet one more argument to tell us how many assets we have, we use the length of expectedRets to guess that
	numAssets<-length(expectedRets)
	
	# the quadprog QP solver requires the transpose of the matrix we just built
	constraintsLhs<-t(constraintsLhs)
	# now solve it. Note: passing the inverse of the choleski triangle of varCovar and setting factorized to TRUE did not produce any gain in performance
	qpResults<-solve.QP(riskAversion * solverObject[["coVar"]],expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs,factorized=FALSE)
	
	portRet <- sum( expectedRets * qpResults[["solution"]])
	portRisk <- sqrt( matrix(qpResults[["solution"]], nrow = 1) %*% solverObject[["coVar"]] %*% matrix(qpResults[["solution"]], ncol = 1) )
	
	return(list(portRet=portRet, portRisk=portRisk, portWeights=qpResults[["solution"]], riskAversionFactor=riskAversion))
}

mvOpt.getRisk <- function(solverObject, portfolio ){
	
	return(sqrt(matrix(portfolio,nrow=1) %*% solverObject[["coVar"]] %*% matrix(portfolio,ncol=1)))
}
