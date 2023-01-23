constructionMethods.getWeights <- function(method,solverObject,parameter,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs){
	
	if ( method == "MV") {
		return( mvOpt.getWeights(solverObject,parameter,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs) )
	} else if (method == "MAD") {
		return( madOpt.getWeights(solverObject,parameter,expectedRets) )
	} else if (method == "MVU") {
		return( mvOpt.getMaxUWeights(solverObject,parameter,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs) )
	} else if (method == "MADU") {
		return( madOpt.getMaxUWeights(solverObject, parameter, expectedRets) )
	} else if (method == "HMU") {
		return(hmOpt.getMaxUWeights(solverObject, parameter, expectedRets))
	} else if (method == "DS1" || method == "DS2") {
		return( downsideOpt.getWeights(solverObject, parameter, expectedRets) )
	} else {
		stop(paste("ERROR in constructionMethods.getWeights(): method",method,"is not supported."))
	}
}

constructionMethods.getStateObject <- function( method, parameters, numberOfDates, assetNames ){
	if(method == "HMU"){
		return( hmOpt.getStateObject( parameters, numberOfDates, assetNames ) )
	} else if (method == "DS1" || method == "DS2" ){
		return( downsideOpt.getStateObject( parameters, numberOfDates, assetNames ) )
	} else {
		return( NULL )
	}
}

constructionMethods.updateStateObject <- function( method, thisDate, stateObject, output ) {
	if(method == "HMU") {
		return( hmOpt.updateStateObject( thisDate, stateObject, output ))
	} else if ( method == "DS1" || method == "DS2" ) {
		return( downsideOpt.updateStateObject( thisDate, stateObject, output ) )
	} else {
		# for those methods for which we don't maintain state we just return the stateObject
		return( stateObject )
	}
}

constructionMethods.getRisk <- function(method,solverObject, portfolio, parameter){
	if( method == "MV" || method == "MVU"){
		return(mvOpt.getRisk(solverObject, portfolio ) )
	} else if (method == "MAD" || method == "MADU"){
		return(madOpt.getRisk( solverObject, portfolio ))
	} else if (method == "HMU") {
		return(hmOpt.getRisk(solverObject, portfolio ))
	} else if (method == "DS1" || method == "DS2") {
		return(downsideOpt.getRisk(solverObject, portfolio, parameter))
	} else {
		stop(paste("ERROR in constructionMethods.getRisk(): method",method,"is not supported."))
	}
}

constructionMethods.getSolverObject <- function(method, riskMeasure,rateData,numAssets,rollingWindow,halfLife,varScaleFactor,constraintsLhs, constraintsRhs, constraintsNumEqs, uParameters,optimParams,rfRates,cashBenchmark, thisDate){
	if( method == "MV" || method == "MVU"){
		return( mvOpt.getSolverObject(riskMeasure,rateData,numAssets,rollingWindow,halfLife,varScaleFactor) )
	} else if (method == "MAD" || method == "MADU"){
		return(madOpt.getSolverObject( numAssets, constraintsLhs, constraintsRhs,constraintsNumEqs,  rollingWindow, rateData))
	} else if (method == "HMU") {
		return(hmOpt.getSolverObject(numAssets, rateData, constraintsLhs, constraintsRhs, constraintsNumEqs,uParameters, optimParams,riskMeasure,rollingWindow,halfLife,varScaleFactor))
	} else if (method == "DS1") {
		return(downsideOpt.getSolverObject(thisDate, "1", uParameters, numAssets,rateData[1, 1:numAssets],constraintsLhs,constraintsRhs,constraintsNumEqs,optimParams,riskMeasure, rollingWindow,rateData,halfLife,varScaleFactor,rfRates,cashBenchmark))
	} else if (method == "DS2") {
		return(downsideOpt.getSolverObject(thisDate, "2", uParameters, numAssets,rateData[1, 1:numAssets],constraintsLhs,constraintsRhs,constraintsNumEqs,optimParams,riskMeasure, rollingWindow,rateData,halfLife,varScaleFactor,rfRates,cashBenchmark))
	} else {
		stop(paste("ERROR in constructionMethods.getSolverObject(): method",method,"is not supported."))
	}
}

constructionMethods.doOneDate <- function(method,solverObject,parameters,rateData,rollingWindow,halfLife,varScaleFactor,numFrontierPoints,constraintsLhs,constraintsRhs,constraintsNumEqs, stateObject, thisDate) {
	
	numCols<-ncol(rateData)
	numAssets<-numCols/2
	
	expectedRets<-rateData[1,1:numAssets]
	
	# get the asset names from the ER names; this is only used for the headings of the returned data frame
	assetNames<-names(rateData)[1:numAssets]
	
	if( method == "MV" || method == "MAD"){
		return(fbex.getFrontier(method,solverObject,numAssets,expectedRets,assetNames,numFrontierPoints,constraintsLhs,constraintsRhs,constraintsNumEqs, stateObject, thisDate))
	} else if (method == "MVU" || method == "HMU" || method == "MADU" || method == "DS1" || method == "DS2" ) {
		return(fbex.getPortfolios(method,solverObject, parameters,numAssets,expectedRets,assetNames,constraintsLhs,constraintsRhs,constraintsNumEqs, stateObject, thisDate))
	} else {
		stop(paste("ERROR in constructionMethods.doOneDate(): method",method,"is not supported."))
	}
}
