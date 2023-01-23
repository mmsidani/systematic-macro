rebalancing.turnoverHurdle<-function(curPort,targetPort, weightsHurdle){
# curPort and targetPort are 2 1-row data.frame()'s. Note: this is equivalent to rebalancing.euclidean() (except that here we use the 1-norm instead of the 2-norm) minus the check on risks
# returns TRUE if we should rebalance, FALSE otherwise

	if(sum(colnames(curPort)!=colnames(targetPort))!=0){
		stop(c("error in rebalancing.turnoverHurdle(): curPort and targetPort do not have the same assets or are not in the same order. curPort's are:",colnames(curPort)," targetPort's are:",colnames(targetPort)))
	}
	
	# is the total turnover less than the minimum fraction of the portfolio that we would care to rebalance?
	if(sum(abs(as.numeric(curPort)-as.numeric(targetPort))) < weightsHurdle){
		return(FALSE)
	}else{
		return(TRUE)
	}
}

rebalancing.expectedReturnHurdle<-function(curPort,targetPort, expectedRets, erHurdle){
# curPor, targetPort and expectedRets are 2 1-row data.frame()'s
# returns TRUE if we should rebalance, FALSE otherwise

	if(sum(colnames(curPort)!=colnames(targetPort) | colnames(curPort)!=colnames(expectedRets))!=0){
		stop(c("error in rebalancing.expectedReturnHurdle(): curPort, targetPort and expectedRets do not have the same assets or are not in the same order. curPort's are:",colnames(curPort)," targetPort's are:",colnames(targetPort)," expectedRets' are:",colnames(expectedRets)))
	}
	
	# is the expected return of the target portfolio larger than the expected return of the current portfolio + a hurdle?
	if(sum(as.numeric(targetPort)* as.numeric(expectedRets)) < sum(as.numeric(curPort)* as.numeric(expectedRets)) + erHurdle){
		return(FALSE)
	}else{
		return(TRUE)
	}
}

reb.retRisk <- function(curPort,targetPort,ersRisks,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets){
# an auxiliary internal (in our naming conventions) function that is called by a number of rebalancing policies
	
	assetNames<-colnames(curPort)
	if(sum(assetNames!=colnames(targetPort))!=0){
		stop(c("error in rebalancing.erRiskHurdle(): curPort and targetPort do not have the same assets or are not in the same order. curPort's are:",colnames(curPort)," targetPort's are:",colnames(targetPort)))
	}
	
	# get expected returns for all assets in portfolio -- not just riskyAssets
	expectedRets<-as.numeric(ersRisks[1,assetNames])
	numAssets<-length(riskyAssets)
	# we want expected returns columns, followed by risk columns. this is for the variance-covariance matrix so do it only for riskyAssets
	ersRisks<-ersRisks[,c(riskyAssets,paste(riskyAssets,".risk",sep=""))]
	
	# get variance-covariance matrix
	varCovar<-varianceCovariance.riskMatrix(riskMeasure,ersRisks,numAssets,rollingWindow,halfLife,varScaleFactor)
	
	# auxiliary function to calculate return/risk of portfolios
	retRisk<-function(port){
		portRet<-sum(as.numeric(port)*expectedRets)
		
		port<-as.numeric(port[1,riskyAssets])
		if(sum(port)==0){
			# nothing left: all non-zero positions are in non-risky assets
			return(NULL)
		}
		
		matrixPortWeights<-matrix(port,nrow=1)
		portRisk<-sqrt(matrixPortWeights%*%varCovar%*% t(matrixPortWeights))
		
		return(c(portRet,portRisk))
	}
	
	# calculate expected rets and volatilities for curPort and targetPort
	curPortRetRisk<-retRisk(curPort)
	targetPortRetRisk<-retRisk(targetPort)
	
	return(list(curPortRetRisk=curPortRetRisk, targetPortRetRisk=targetPortRetRisk))
}

rebalancing.erRiskHurdle<-function(curPort,targetPort,ersRisks,erRiskRatioHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets){
# curPort and targetPort are 2 1-row data.frame()'s; ersRisks is a timeSeries() in the frontierBuilder format (assets followed by assets.risk). NOTE: ersRisks should NOT have NA's, otherwise results will have NA's. change NA's to 0's before calling. the same rolling window that was used in building the efficient frontier should be used here.
# returns TRUE if we should rebalance, FALSE otherwise

	retRisks <- reb.retRisk(curPort,targetPort,ersRisks,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets)
	curPortRetRisk<- retRisks$curPortRetRisk
	targetPortRetRisk <- retRisks$targetPortRetRisk
	# a NULL return means we are in cash, or target is cash. rebalance without further checks. in other words, ALWAYS switch to or away from cash.
	if(is.null(curPortRetRisk)||is.null(targetPortRetRisk)){
		return(TRUE)
	}
	
	# is the current portfolio expected return / forecast risk larger than the same ratio for the target portfolio?
	if(curPortRetRisk[2] == 0 || targetPortRetRisk[2]==0){
		stop(c("error in rebalancing.erRiskHurdle(): current or target portfolio risk is 0",curPort,targetPort))
	}else{
		if((curPortRetRisk[1]+ erRiskRatioHurdle)/curPortRetRisk[2] >= targetPortRetRisk[1]/targetPortRetRisk[2]){
			return(FALSE)
		}else{
			return(TRUE)
		}
	}
}

rebalancing.kullbackLeibler <- function(curPort,targetPort,ersRisks, klHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets){
# do rebalance if kullback-leibler divergence between current and target weights is greater than tolerance. argument list as above except for tolerance. NOTE: only works if we're long only in risky assets
	
	retRisks <- reb.retRisk(curPort,targetPort,ersRisks,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets)
	curPortRetRisk<- retRisks$curPortRetRisk
	targetPortRetRisk <- retRisks$targetPortRetRisk
	# a NULL return means we are in cash, or target is cash. rebalance without further checks. in other words, ALWAYS switch to or away from cash.
	if(is.null(curPortRetRisk)||is.null(targetPortRetRisk)){
		return(TRUE)
	}
	
	curPort <- round(as.numeric(curPort[,riskyAssets]),3)
	curPort <- curPort / sum(curPort)
	targetPort <- round(as.numeric(targetPort[,riskyAssets]),3)
	targetPort <- targetPort / sum(targetPort)
	
	if(any(curPort < 0) || any(targetPort < 0) ){
		stop("ERROR in rebalancing.kullbackLeibler(): it only works for long only...")
	}
	
	temp <- targetPort > 0
	if(any(curPort[temp] == 0)){
		# a new asset came into the target so rebalance; this kind of takes care of the fact that KL is not defined in this case
		return(T)
	}
	
	# return TRUE if KL divergence between sets of weights is larger than tolerance or current port's vol is larger than target port's vol
	return(sum(targetPort[temp] * log(targetPort[temp]/curPort[temp])) > klHurdle || curPortRetRisk[2] > targetPortRetRisk[2])
	
}

rebalancing.euclideanLowerRisk <- function(curPort,targetPort,ersRisks, euclidHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets){
# like KL but here we use euclidean distance and we so don't care if we have short positions in risky assets. Note: this is equivalent to doing rebalancing.turnoverHurdle() (which uses the 1-norm instead of the 2-norm) combined with the check on the risks
	
	retRisks <- reb.retRisk(curPort,targetPort,ersRisks,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets)
	curPortRetRisk<- retRisks$curPortRetRisk
	targetPortRetRisk <- retRisks$targetPortRetRisk
	# a NULL return means we are in cash, or target is cash. rebalance without further checks. in other words, ALWAYS switch to or away from cash.
	if(is.null(curPortRetRisk)||is.null(targetPortRetRisk)){
		return(TRUE)
	}
		
	# return TRUE if the euclidean distance between sets of weights is larger than tolerance or current port's vol is larger than target port's vol
	return(sqrt(sum((as.numeric(targetPort)-as.numeric(curPort))^2)) > euclidHurdle || curPortRetRisk[2] > targetPortRetRisk[2])
	
}

rebalancing.euclidean <- function(curPort,targetPort, euclidHurdle){
# like KL but here we use euclidean distance and we so don't care if we have short positions in risky assets. Note: this is equivalent to doing rebalancing.turnoverHurdle() (which uses the 1-norm instead of the 2-norm) 
	
	# return TRUE if the euclidean distance between sets of weights is larger than tolerance or current port's vol is larger than target port's vol
	return(sqrt(sum((as.numeric(targetPort)-as.numeric(curPort))^2)) > euclidHurdle)
	
}

rebalancing.leverageCreep<-function(curPort,leverage,leverageThreshold,riskyAssets){
# some unintended leverage does creep into our positions. it happens when the net interest on our cash positions is negative and exceeds (in absolute value) the net interest and dividends on our non-cash positions. if, at a rebalancing date, we deviate from our target leverage by leverageThreshold (specified in GlobalVariables.R) we automatically rebalance. Note that we could exceed this threshold and carry on if it first happens between 2 rebalancing dates.
# curPort is a 1-row data.frame(); cashNameSuffix is a string
# returns TRUE/FALSE
	
	if(sum(abs(curPort[1,riskyAssets]))>leverage+leverageThreshold){
		# leverage exceeded our tolerance. note that leverageThreshold is expressed as percentage of portfolio value
		return(TRUE)
	}else{
		return(FALSE)
	}
}

rebalancing.reallocHurdle<-function(thisDate,curPort,targetPort,ersRisks,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,leverage,leverageThreshold,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets){
# main entry point. thisDate is a string; for the others see above comments
# which of the functions above is called is controlled with reallocDecision

	# leverage is within our tolerance, now implement the specified rebalancing policy
	if(reallocDecision == "turnover"){
		return(rebalancing.turnoverHurdle(curPort,targetPort,weightsHurdle))
	}else if(reallocDecision == "expectedReturn"){
		dateInd<-(1:nrow(ersRisks))[thisDate>=rownames(ersRisks)][1]
		return(rebalancing.expectedReturnHurdle(curPort,targetPort, ersRisks[dateInd,1:(ncol(ersRisks)/2)],erHurdle))
	}else if(reallocDecision =="erRiskRatio"){
		dateInd<-(1:nrow(ersRisks))[thisDate>=rownames(ersRisks)][1]
		rebalancing.erRiskHurdle(curPort,targetPort, ersRisks[dateInd:(dateInd+rollingWindow),],erRiskRatioHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets)
	}else if(reallocDecision == "KL"){
		dateInd<-(1:nrow(ersRisks))[thisDate>=rownames(ersRisks)][1]
		return(rebalancing.kullbackLeibler(curPort,targetPort, ersRisks[dateInd:(dateInd+rollingWindow),], klHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets))
	} else if(reallocDecision == "euclideanLowerRisk"){
		dateInd<-(1:nrow(ersRisks))[thisDate>=rownames(ersRisks)][1]
		return(rebalancing.euclideanLowerRisk(curPort,targetPort, ersRisks[dateInd:(dateInd+rollingWindow),], euclidHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets))		
	} else if(reallocDecision == "euclidean"){
		return(rebalancing.euclidean(curPort,targetPort,euclidHurdle))
	} else if(reallocDecision == "always"){
		return(TRUE)
	}else{
		stop(paste("error in rebalancing.R: unknown reallocation policy reallocDecision",reallocDecision))
	}
}

