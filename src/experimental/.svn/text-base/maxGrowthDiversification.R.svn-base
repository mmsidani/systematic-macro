maxGrowthDiversification.solve <- function( covMat){
# minimize ( Sigma_i w_i sig_i ) - ( Sigma_i_j rho_i_j w_i w_j sig_i sig_j ). then the weights maximize excess growth and diversification
	
	library(quadprog)
	
	numWeights <- nrow( covMat)
	
	# budget
	constraintsMat <- matrix(1, nrow=1, ncol=numWeights)
	constraintsVals <- 1
	# upper bounds
	constraintsMat <- rbind( constraintsMat, diag(-1, numWeights, numWeights) )
	constraintsVals <- c( constraintsVals, rep( -1, numWeights ) )
	# lower bounds
	constraintsMat <- rbind( constraintsMat, diag(1, numWeights, numWeights) )
	constraintsVals <- c( constraintsVals, rep(0, numWeights))

	solveQPRes <-  solve.QP( covMat, 0.5 * sqrt(diag(covMat)), meq = 1, t(constraintsMat), constraintsVals)
	ret <- data.frame( solveQPRes[["solution"]])
	rownames(ret) <- rownames( covMat )
	
	return( list( solution=ret, value=solveQPRes[["value"]]) )
}