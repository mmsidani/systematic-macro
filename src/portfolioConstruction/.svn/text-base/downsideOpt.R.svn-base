downsideOpt.getSolverObject <- function(thisDate, method, parameters, numAssets,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs,optimParams,riskMeasure,rollingWindow,rateData,halfLife,varScaleFactor,rfRates,cashBenchmark){
	
	if(constraintsNumEqs != 1){
		stop("ERROR in downsideOpt.getSolverObject(): exactly 1 equality constraint is permitted.")
	}
	
	maxRet<-fbUtils.getMaxTargetReturn(expectedRets, constraintsLhs,constraintsRhs,constraintsNumEqs)[["maxRet"]]
	thisDate <- utils.ymdToHyphens(thisDate)
	indDate <- match(thisDate, rownames( rfRates ))
	rollingInds <- indDate:(indDate + rollingWindow -1 )
	if( !is.finite( indDate) || rollingInds[ length(rollingInds) ] > nrow( rfRates) || sum( !is.finite( rfRates[ rollingInds, cashBenchmark ] ) ) != 0 ){
		stop(paste("ERROR in downsideOpt.getSolverObject(): the cash benchmark does not have enough data for ",thisDate,"and/or",rollingWindow,"dates prior." ))
	}
	
	cannotMeet <- NULL
	if(sum(parameters + rfRates[thisDate, cashBenchmark ] > maxRet) != 0 ){
		inds <- grep(T, parameters + rfRates[thisDate, cashBenchmark ] > maxRet )
		cannotMeet <- parameters[ inds ]
		print(paste("WARNING in downsideOpt.getSolverObject(): the target return cannot be above the maximum achievable return. cash yield is: ", rfRates[thisDate, cashBenchmark ],"on",thisDate,". cannot meet a spread of: ", paste(parameters[inds], collapse=", "),". shifting to cash benchmark."))
	}

	# get covariance matrix
	coVar <- varianceCovariance.riskMatrix(riskMeasure, rateData, numAssets,rollingWindow,halfLife,varScaleFactor)
	# row 1 is expected to have the budget constraint
	leverage <- constraintsRhs[1]
	
	if( method == "1" ){
		benchmarkRets <- rfRates[ indDate, cashBenchmark ]
	} else if( method == "2" ) {
		benchmarkRets <- rfRates[ rollingInds, cashBenchmark ]
	} else {
		stop(paste("ERROR in downsideOpt.getSolverObject(): the method you passed,",method,", is not supported."))
	}
	
	return(list(cannotMeet = cannotMeet, outer.eps = optimParams[["outer.eps"]], reltol = optimParams[["reltol"]], coVar=coVar, targetRet= rfRates[thisDate, cashBenchmark ], benchmarkRets= benchmarkRets,interiorWidth=optimParams[["interiorWidth"]], leverage= leverage,constraintsNumEqs=constraintsNumEqs, constraintsLhs=constraintsLhs, constraintsRhs=constraintsRhs, rateData=rateData[1:rollingWindow, 1:numAssets]))
}

downsideOpt.getWeights <- function(solverObject, spreadOverBenchmark, expectedRets) {
# this solves the SV problem; i.e., it gets one point on the efficient frontier
# targetRet is a scalar; the target return for which we want a portfolio
# expectedRets is the vector of expected returns (ER's) of the assets
# returns a list() with 3 components: portRet, the portfolio return; portRisk, the portfolio stdev; portWeights, the weights in each asset
	
	# instead of passing yet one more argument to tell us how many assets we have, we use the length of expectedRets to guess that
	numAssets<-length(expectedRets)
	
	if( is.element(spreadOverBenchmark, solverObject[["cannotMeet"]])){
		# note that portSemiRisk is not defined in this case: since we're in cash, it doesn't make sense to talk about downside risk to cash + x%
		return(list( portRet=0, portRisk=0, portSemiRisk = 0, assetNames= colnames(expectedRets), portWeights = rep(0, numAssets) , label=as.character(spreadOverBenchmark)))
	}
	
	# use quadprog to get an initial guess, so first modify input by adding target return constraint
	solverObject[["constraintsLhs"]] <- rbind(solverObject[["constraintsLhs"]], matrix(expectedRets, nrow=1))
	solverObject[["constraintsRhs"]] <- c(solverObject[["constraintsRhs"]], solverObject[["targetRet"]] + spreadOverBenchmark )

	initialGuess <- solve.QP( solverObject[["coVar"]],rep(0,numAssets),t(solverObject[["constraintsLhs"]]),solverObject[["constraintsRhs"]],solverObject[["constraintsNumEqs"]],factorized=FALSE )[["solution"]]
	
	# now on to constrOptim(): row 1 in constraints is expected to have the budget constraint but for constrOptim(), it can't be an equality so change into double inequalities (with an "interior")
	solverObject[["constraintsLhs"]][1,] <- -solverObject[["constraintsLhs"]][1,]
	solverObject[["constraintsRhs"]][1] <- -solverObject[["constraintsRhs"]][1]
	solverObject[["constraintsLhs"]] <- rbind(matrix(rep(1,numAssets),nrow=1),solverObject[["constraintsLhs"]],deparse.level=0)
	solverObject[["constraintsRhs"]] <- c((1 - solverObject[["interiorWidth"]]) * solverObject[["leverage"]], solverObject[["constraintsRhs"]])
	# the initial guess also has to be in the interior
	initialGuess[ abs( initialGuess ) < 1.0e-10 ] <- solverObject[["interiorWidth"]] / 2.0
	initialGuess <- initialGuess/sum(initialGuess) * (1 - solverObject[["interiorWidth"]] / 2.0)
	# and target return should also be adjusted for the initial guess to remain feasible
	solverObject[["constraintsRhs"]][ length( solverObject[["constraintsRhs"]] )] <- sum( expectedRets * initialGuess) * (1 - solverObject[["interiorWidth"]] )
	
	rateData <- solverObject[["rateData"]]
	
	objective <- function(theta){
		return(utils.semiVariance(rateData, theta, solverObject[["benchmarkRets"]] + spreadOverBenchmark))
	}
	
	thetaStar <- constrOptim(initialGuess,objective, NULL, solverObject[["constraintsLhs"]], solverObject[["constraintsRhs"]], outer.eps = solverObject[["outer.eps"]], control=list(reltol = solverObject[["reltol"]]) )
	
	if( solverObject[["leverage"]] == 1){
		thetaStar$par <- thetaStar$par / sum(thetaStar$par)
	}

	return(list( portRet=sum(expectedRets * thetaStar$par), portRisk= sqrt(matrix(thetaStar$par,nrow=1) %*% solverObject[["coVar"]] %*% matrix(thetaStar$par,ncol=1)), portSemiRisk=downsideOpt.getRisk(solverObject, thetaStar$par, spreadOverBenchmark), label=as.character(spreadOverBenchmark), assetNames= colnames(expectedRets), portWeights=thetaStar$par ))
}

downsideOpt.getStateObject <- function( parameters, numDates, assetNames ){
	
	ret <- list()
	# we want to store the 3rd and 4th moments because the rest of what is computed here (portfolio weights, returns, risks) is processed into the output structure of in the calling function
	for( i in 1:length(parameters) ){
		mat <- matrix(nrow = numDates, ncol = 4 + length( assetNames ) )
		colnames(mat ) <- c("Date", "portRet", "portRisk", "portSemiRisk", assetNames)
		ret <- append(ret, list(mat))
		names(ret)[length(ret)] <- as.character(parameters[i])
	}
	
	return( ret )
}

downsideOpt.updateStateObject <- function( thisDate, dsStateObject, dsOutput ){
print(thisDate)
	index <- match( NA, dsStateObject[[ dsOutput[["label"]] ]][,1])
	if( !is.finite(index)){
		stop("ERROR in downsideOpt.updateStateObject(): something off. was the state object not initialized with the proper number of rows? ")
	}
	
	dsStateObject[[ dsOutput[["label"]] ]][index, c("Date", "portRet", "portRisk", "portSemiRisk",dsOutput[["assetNames"]] ) ] <- c( thisDate,  dsOutput[["portRet"]],  dsOutput[["portRisk"]], dsOutput[["portSemiRisk"]], dsOutput[["portWeights"]] )
	
	return( dsStateObject )
}

downsideOpt.getRisk <- function(solverObject, portfolio, spreadOverBenchmark){
	
	return( sqrt(utils.semiVariance(solverObject[["rateData"]], portfolio,  solverObject[["benchmarkRets"]] + spreadOverBenchmark)) )
}