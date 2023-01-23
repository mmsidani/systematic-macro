madOpt.getSolverObject <- function( numAssets, constraintsLhs, constraintsRhs, constraintsNumEqs, rollingWindow, rateData){
	
	rateData<- rateData[1:rollingWindow,1:numAssets]
	colmeans <- apply(rateData, 2, mean)
	rateData <- rateData - matrix(colmeans,nrow=1)[rep(1,nrow(rateData)),]	
	# add columns to allow what comes next
	constraintsLhs <- cbind(diag(0,nrow(constraintsLhs),nrow(rateData)),constraintsLhs,deparse.level=0)
	
	temp <- cbind(diag(nrow(rateData)),rateData,deparse.level=0)
	constraintsLhs <- rbind(constraintsLhs, temp ,deparse.level=0)
	constraintsRhs <- c(constraintsRhs, rep(0, nrow(rateData)))
	
	temp <- cbind(diag(nrow(rateData)), -rateData,deparse.level=0)
	constraintsLhs <- rbind(constraintsLhs, temp,deparse.level=0 )
	constraintsRhs <- c(constraintsRhs, rep(0, nrow(rateData)))
	
	constraintsLhs <- as.matrix(constraintsLhs)

	return( list(rollingWindow = rollingWindow, constraintsLhs = constraintsLhs, constraintsRhs= constraintsRhs, constraintsNumEqs=constraintsNumEqs,rateData= rateData) )
}

madOpt.getWeights<-function(solverObject,targetRet,expectedRets){
	
	numAssets<-length(expectedRets)
	
	if(!is.null(targetRet )){
		solverObject[["constraintsLhs"]] <- rbind(solverObject[["constraintsLhs"]], matrix(c(rep(0,solverObject[["rollingWindow"]]), expectedRets), nrow=1),deparse.level=0)
		solverObject[["constraintsRhs"]] <- c(solverObject[["constraintsRhs"]],targetRet)
	}
	
	efgh <- fbUtils.setLPConstraints(solverObject[["constraintsLhs"]], solverObject[["constraintsRhs"]], solverObject[["constraintsNumEqs"]])
	lpRes <- linp(E=efgh[["e"]],F=efgh[["f"]],G=efgh[["g"]],H=efgh[["h"]],c(rep(1,solverObject[["rollingWindow"]]),rep(0,numAssets)),ispos=TRUE,verbose=TRUE)
	
	assetWeights <- lpRes[["X"]][(solverObject[["rollingWindow"]] +1):(solverObject[["rollingWindow"]]+numAssets)]

	if(is.null(targetRet )){
		targetRet <- sum(expectedRets * assetWeights)
	}
	
	portRisk <- 1 / solverObject[["rollingWindow"]] * sum(abs(solverObject[["rateData"]] %*% matrix(assetWeights, ncol= 1)))
	
	return(list(portRet=targetRet,portRisk= portRisk,portWeights= assetWeights, riskAversionFactor = NA))
}

madOpt.getMaxUWeights<-function(solverObject, riskAversion, expectedRets){
	
	numAssets<-length( expectedRets )
	
	efgh <- fbUtils.setLPConstraints(solverObject[["constraintsLhs"]], solverObject[["constraintsRhs"]], solverObject[["constraintsNumEqs"]])
	objective <- c( rep(riskAversion,solverObject[["rollingWindow"]]) / solverObject[["rollingWindow"]], -expectedRets )
	lpRes <- linp(E=efgh[["e"]],F=efgh[["f"]],G=efgh[["g"]],H=efgh[["h"]], objective, ispos=TRUE, verbose=TRUE)	
	assetWeights <- lpRes[["X"]][(solverObject[["rollingWindow"]] +1):(solverObject[["rollingWindow"]]+numAssets)]
	
	targetRet <- sum(expectedRets * assetWeights)
	portRisk <- 1 / solverObject[["rollingWindow"]] * sum(abs(solverObject[["rateData"]] %*% matrix(assetWeights, ncol= 1)))
	
	return(list(portRet=targetRet,portRisk= portRisk,portWeights= assetWeights))
}

madOpt.getRisk <- function( solverObject, portfolio ){
	
	return( 1 / solverObject[["rollingWindow"]] * sum(abs(solverObject[["rateData"]] %*% matrix(portfolio, ncol= 1))))
}
