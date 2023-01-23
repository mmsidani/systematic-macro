hmOptPrivate.getMaxMoments <- function( parameters) {
# what the largest moment specified in any set of parameters for hmOpt, 3 or 4? also check that all lambda's are positive

	maxDoMoments <- 3
	for(i in 1:length(parameters)){
		maxDoMoments <- max(maxDoMoments, parameters[[i]][["doMoments"]])
		if( parameters[[i]][["lambda"]] <= 0 ){
			stop("ERROR in hmOptPrivate.getMaxMoments(): all moments coefficients must be positive.")
		}
	}
	
	return( maxDoMoments )
}

hmOptPrivate.getLabel <- function( uParameters ){
# construct the "label" for frontierBuilder: this is used for instance in the names of output files
	
	return( paste(uParameters, collapse=".") )
}

hmOpt.getSolverObject <- function(numAssets, rateData, constraintsLhs, constraintsRhs, constraintsNumEqs,parameters, optimParams,riskMeasure,rollingWindow,halfLife,varScaleFactor){
	
	if(constraintsNumEqs != 1){
		stop("ERROR in downsideOpt.getSolverObject(): exactly 1 equality constraint is permitted.")
	} 
	
	# Note: we do this for every date when back-testing even though we need it once. shouldn't matter computationally but still...
	maxDoMoments <- hmOptPrivate.getMaxMoments( parameters )

	# row 1 is expected to have the budget constraint. it can't be an equality because of the requirements of R's constrOptim() so change into double inequalities (with an "interior")
	constraintsLhs[1,] <- -constraintsLhs[1,]
	leverage <- constraintsRhs[1]
	constraintsRhs[1] <- -constraintsRhs[1]
	constraintsLhs <- rbind(matrix(rep(1,numAssets),nrow=1),constraintsLhs,deparse.level=0)
	constraintsRhs <- c((1 - optimParams[["interiorWidth"]]) * leverage, constraintsRhs)
	
	# get covariance matrix
	coVar <- varianceCovariance.riskMatrix(riskMeasure, rateData,numAssets,rollingWindow,halfLife,varScaleFactor)
	# remove durations
	rateData <- rateData[1:rollingWindow, 1:numAssets]
	# get 3rd central co-moments and change format for better performance when we use constrOptim(). we get a numAssets * numAssets^2 matrix
	coSkew <- utils.3DimTo2( utils.3Moment(rateData, na.rm = FALSE) )
	coKurt <- NULL
	if(maxDoMoments == 4 ){
		# get 4th central co-moments and change format for better performance when we use constrOptim(). we get a numAssets^2 &* numAssets^2 matrix
		coKurt <- utils.4DimTo2( utils.4Moment(rateData, na.rm = FALSE) )
	} else if ( maxDoMoments  != 3 ){
		stop(paste("ERROR in hmOpt.getSolverObject(): doMoments must be 3 or 4. you passed", maxDoMoments))
	}
	
	weightsSum <- (1 + 1 - optimParams[["interiorWidth"]]) / 2
	
	return( list(outer.eps = optimParams[["outer.eps"]], reltol = optimParams[["reltol"]], leverage= leverage, weightsSum= weightsSum, coSkew = coSkew, coKurt = coKurt, coVar = coVar, constraintsLhs = constraintsLhs, constraintsRhs = constraintsRhs))
}

hmOpt.getStateObject <- function( parameters, numDates, assetNames ){
	
	ret <- list()
	# we want to store the 3rd and 4th moments because the rest of what is computed here (portfolio weights, returns, risks) is processed into the output structure of in the calling function
	for( i in 1:length(parameters) ){
		mat <- matrix(nrow = numDates, ncol = 5 + length( assetNames ) )
		colnames(mat ) <- c("Date", "portRet", "portRisk", "portM3", "portM4", assetNames)
		ret <- append(ret, list(mat))
		names(ret)[length(ret)] <- hmOptPrivate.getLabel( parameters[[i]] )
	}
	
	return( ret )
}

hmOpt.updateStateObject <- function( thisDate, hmStateObject, hmOutput ){

	index <- match( NA, hmStateObject[[ hmOutput[["label"]] ]][,1])
	if( !is.finite(index)){
		stop("ERROR in hmOpt.updateStateObject(): something off. was the state object not initialized with the proper number of rows? ")
	}
	
	hmStateObject[[ hmOutput[["label"]] ]][index, c("Date", "portRet", "portRisk", "portM3", "portM4", hmOutput[["assetNames"]])] <- c( thisDate,  hmOutput[["portRet"]],  hmOutput[["portRisk"]], hmOutput[["portM3"]], hmOutput[["portM4"]], hmOutput[["portWeights"]] )
	
	return( hmStateObject )
}

hmOpt.getMaxUWeights<-function(solverObject, uParameters, expectedRets){
# for a risk aversion parameter optimizes approximation of exponential utility. expansion is written in terms of central moments 2, 3 and possibly 4
	
	uParameters <- uParameters[[1]]
	# instead of passing yet one more argument to tell us how many assets we have, we use the length of expectedRets to guess that
	numAssets<-length(expectedRets)
	
	lambda <-uParameters[["lambda"]]
	lambda2 <- lambda^2
	lambda3 <- lambda2 * lambda
	if(uParameters[["doMoments"]] == 4){
		# include kurtosis
		lambda4 <- lambda3 * lambda
		# Note: inlined vol/m3/m4 for performance reasons
		objective <- function(theta) {  thetaBig <- matrix( kronecker( theta, theta ), nrow = 1); thetaRow <- matrix(theta, nrow = 1); return( - ( -exp(-lambda*sum(expectedRets*theta) ) * (1 + lambda2/2 * thetaRow %*% solverObject[["coVar"]] %*% t(thetaRow) - lambda3 / 6 * thetaRow %*% solverObject[["coSkew"]] %*% t(thetaBig) + lambda4/24.0 * thetaBig %*% solverObject[["coKurt"]] %*% t(thetaBig)) )) }
	} else if (uParameters[["doMoments"]] == 3){
		objective <- function(theta) { thetaBig <- matrix( kronecker( theta, theta ), ncol = 1); thetaRow <- matrix(theta, nrow = 1); return( - ( -exp(-lambda*sum(expectedRets*theta) ) * (1 + lambda2/2 * thetaRow %*% solverObject[["coVar"]] %*% t(thetaRow) - lambda3 / 6 * thetaRow %*% solverObject[["coSkew"]] %*% thetaBig) )) }
	}
	
	# minimize objective function. hence the mysterious "-" in the definition of the objective function
	thetaStar <- constrOptim(rep(solverObject[["weightsSum"]]/numAssets,numAssets),objective, NULL, solverObject[["constraintsLhs"]], solverObject[["constraintsRhs"]], outer.eps = solverObject[["outer.eps"]],control=list(reltol = solverObject[["reltol"]]) )
	
	thetaStar$par <- thetaStar$par / sum(thetaStar$par)
	targetRet <- sum(expectedRets * thetaStar$par)
	
	thetaStarBig <- matrix( kronecker( thetaStar$par, thetaStar$par ), nrow = 1)
	
	portM4 <- ifelse( uParameters[["doMoments"]] == 4, thetaStarBig %*% solverObject[["coKurt"]] %*% t(thetaStarBig), NA)
	return(list(portRet=targetRet, portRisk=sqrt(matrix(thetaStar$par,nrow=1) %*% solverObject[["coVar"]] %*% matrix(thetaStar$par, ncol=1)), portWeights=thetaStar$par, label=hmOptPrivate.getLabel( uParameters ), portM3 =matrix(thetaStar$par,nrow=1) %*% solverObject[["coSkew"]] %*% t(thetaStarBig), portM4 = portM4, assetNames= colnames(expectedRets) ))
}

hmOpt.getRisk <- function(solverObject, portfolio ){
	
	return(sqrt(matrix(portfolio,nrow=1) %*% solverObject[["coVar"]] %*% matrix(portfolio,ncol=1)))
}
