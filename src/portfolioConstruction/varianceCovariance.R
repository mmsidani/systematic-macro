varianceCovariance.varCovar<-function(rateData,numAssets,rollingWindow){
# this calculates the variance-covariance matrix and allows us to get the volatility of the portfolio. it's dependent on the format of rateData.
# this version assumes the first numAssets columns of rateData contains ER's and the next numAssets columns contain variances of the assets
# returns the var-covar matrix to be used in the QP problem

	if(nrow(rateData)<(rollingWindow+1)){
		stop(paste("error in varianceCovariance.varCovar(): fewer than rollingWindow+1 rows"))
	}

	levelChange<-as.matrix(rateData[1:rollingWindow,1:numAssets])-as.matrix(rateData[2:(rollingWindow+1),1:numAssets])
	# proprietary durations (risks)
	durs<-as.numeric(rateData[1,(numAssets+1):ncol(rateData)])

	if(ncol(levelChange)!=1){
		covs<-cov(levelChange)
		# now scale covs by the durations to get assets returns covariances. Note: we could use diag() and %*% here to scale. when we do that however, R doesn't see the 0's and 0*NA becomes NA and all of covs becomes NA if there's a single NA column. in the trick next, NA's remain localized since we avoid the summation that comes with %*%
		covs<-as.matrix(durs)[,rep(1,numAssets)] * covs * t(durs)[rep(1,numAssets),]
	} else {
		covs<-durs[1]^2 * var(levelChange)
	}

	return(as.matrix(covs))
}

varianceCovariance.varCovarNoDuration<-function(rateData,numAssets,rollingWindow){
# this calculates the variance-covariance matrix and allows us to get the volatility of the portfolio. it's dependent on the format of rateData.
# this version assumes the first numAssets columns of rateData contains ER's and the next numAssets columns contain variances of the assets
# returns the var-covar matrix to be used in the QP problem
	
	if(nrow(rateData)<rollingWindow){
		stop(paste("error in varianceCovariance.varCovar(): fewer than rollingWindow+1 rows"))
	}

	rateData <- rateData[1:rollingWindow,1:numAssets]
	covs<-cov(rateData)
		
	return(as.matrix(covs))
}

varianceCovariance.varCovarShocks<-function(durations,numAssets,rollingWindow){
# first 3 args as above. NOTE: shocks are assumed to have already been converted to a domestic perspective

	if(nrow(shocks) < rollingWindow ){
		stop(paste("error in varianceCovariance.varCovarShocks(): fewer than rollingWindow rows"))
	}

	# retrieve current asset durations
	durs<-as.numeric(durations)

	# convert the history of shocks into a history of asset returns using current durations
	shocks <- shocks[1:rollingWindow,] * matrix(durs,nrow=1)[rep(1,rollingWindow),]
	# calculate the covariances. cov() gives population covariances -- not sample
	covs<-cov(shocks)

	return(as.matrix(covs))
}

varianceCovariance.varCovarShrink<-function(rateData,numAssets,rollingWindow){
# this gets the covariance matrix from varianceCovariance.varCovar() but then shrinks to a constant correlation target

	# calculate covariance matrix
	covs<-varianceCovariance.varCovar(rateData,numAssets,rollingWindow)	
	# vector of asset variances
	vars<-diag(covs)
	# vector of asset standard deviations
	stds<-sqrt(vars)
	
	# Note: the following logic doesn't hurt anything but is confusing. we should not have 0 std for a risky asset. the only time this shows up is when we don't have data for that risky asset and we replace NA with 0 in the calling program for example (we do this in portfolioSimulation.R, for example)
	isNotZero <- stds != 0
	numAssetsNotZero <- sum(isNotZero)
	# temp object
	stdsInv<-as.matrix(1/stds[isNotZero])[,rep(1,numAssetsNotZero)]
	# get correlation matrix from covariance matrix
	cors<-stdsInv * covs[isNotZero, isNotZero] * t(stdsInv)
	
	# get average correlation: average of all off-diagonal numbers. Note: we substract numAssets because that is the sum of the diagonal of the correlation matrix
	rho<-(sum(cors)-numAssetsNotZero)/(numAssetsNotZero*(numAssetsNotZero-1))
	# build constant correlation matrix and scale it to get covariances
	prior <- matrix(rep(0, numAssets^2),nrow=numAssets)
	prior[isNotZero, isNotZero] <-as.matrix(stds[isNotZero])[,rep(1,numAssetsNotZero)] * matrix(rep(rho,numAssetsNotZero^2),nrow=numAssetsNotZero) * t(stds[isNotZero])[rep(1,numAssetsNotZero),]
	# reset diagonal entries
	diag(prior)<-vars

	# "sort of agnostic" final estimator: just average the 2
	return(0.5*prior+0.5*covs)
}

varianceCovariance.varCovarHistorical<-function(rateData,numAssets,rollingWindow){
# this gets us the volatility of the portfolio based entirely on the history of returns. it's intended for the returns to be historical returns but the code will equally work with er's (although interpreting what we get in this case is possibly a bit problematic). NOTE: only the 1:numAssets columns of rateData are referenced
# input/output as above

	if(nrow(rateData)<(rollingWindow+1)){
		stop(paste("error in varianceCovariance.varCovarHistorical(): fewer than rollingWindow+1 rows"))
	}

	assetReturns<-as.matrix(rateData[1:rollingWindow,1:numAssets])

	# risk is the covariance of returns
	if(ncol(assetReturns)!=1){
		covs<-cov(assetReturns)
	} else {
		covs<-var(assetReturns)
	}

	return(as.matrix(covs))
}

varianceCovariance.varCovarDuration<-function(rateData,numAssets,rollingWindow,varScaleFactor){
# this calculates the variance-covariance matrix and allows us to get the duration of the portfolio. it's dependent on the format of rateData.
# this version assumes the first numAssets columns of rateData contains ER's and the next numAssets columns contain variances of the assets
# returns the var-covar matrix to be used in the QP problem

	if(nrow(rateData)<(rollingWindow+1)){
		stop(paste("error in varianceCovariance.varCovarDuration(): fewer than rollingWindow+1 rows"))
	}

	percentChange<-as.matrix(rateData[1:rollingWindow,1:numAssets])/as.matrix(rateData[2:(rollingWindow+1),1:numAssets])-1
	# proprietary durations (risks)
	durs<-as.numeric(rateData[1,(numAssets+1):ncol(rateData)])/varScaleFactor

	if(ncol(percentChange)!=1){
		cors<-cor(percentChange)
		# now scale cors by the standard deviations
		covs<-as.matrix(durs)[,rep(1,numAssets)] * cors * t(durs)[rep(1,numAssets),]
	} else {
		covs<-durs[1]^2 
	}

	return(as.matrix(covs))
}

varianceCovariance.expWeightedVarCovarDuration<-function(rateData,numAssets,rollingWindow,halfLife,varScaleFactor){
# this calculates the variance-covariance matrix and allows us to get the duration of the portfolio.
# if t is any time, then the weight given to the observation at t is twice that given to the observation at time t-halfLife
# if halfLife ==0, we should get "almost" the same output as from varianceCovariance.varCovar(); the difference is that here we calculate sample correlations, whereas the R function cor() that we call in varianceCovariance.varCovar() estimates the population correlations

	if(nrow(rateData)<(rollingWindow+1)){
		stop(paste("error in varianceCovariance.expWeightedVarCovarDuration(): fewer than rollingWindow+1 rows"))
	}
	
	#calculate the exponentially decreasing weights 
	if(halfLife == 0){
		expWeights<-rep(1.,rollingWindow)/(rollingWindow-1)
	} else {
		expWeights<-2.^((rollingWindow:1)/halfLife)
		# scale the weights so they sum to 1
		expWeights<-expWeights/sum(expWeights)
	}	
	percentChange<-as.matrix(rateData[1:rollingWindow,1:numAssets])/as.matrix(rateData[2:(rollingWindow+1),1:numAssets])-1.
	
	# calculate the exponentially-weighted means
	weightedMeans <- matrix(expWeights,nrow=1) %*% percentChange
	# de-mean the historical observations (Note: cor() in R returns population, not sample correlations)
	percentChange<-percentChange - matrix(rep(1,rollingWindow),ncol=1) %*% weightedMeans
	# sd(percentChange) returns the sd of each column in percentChange
	invStds<-as.matrix(1./apply(percentChange,2,sd))[,rep(1,numAssets)]
	# get the correlations
	cors<- invStds * t(percentChange) %*% diag(expWeights) %*% percentChange * t(invStds)
	# now use predicted durations to get covariances
	durs<-as.matrix(as.numeric(rateData[1,(numAssets+1):(2*numAssets)])/varScaleFactor)[,rep(1,numAssets)]
	covs<- durs * cors * t(durs)

	return(as.matrix(covs))
}

varianceCovariance.varCovarGlobalDuration<-function(rateData,numAssets,rollingWindow){
# this builds a block diagonal var-covar matrix assuming 0 correlation between different countries

	headers<-names(rateData)
	countryCodes<-unique(substr(headers,1,2))
	
	ret<-matrix(rep(0,numAssets*numAssets),nrow=numAssets)
	diagInd<-1
	for(c in countryCodes){
		countryAssetsInds<-grep(paste("^",c,"\\.",sep=""),headers)
		numCountryAssets<-length(countryAssetsInds)/2
		varCovar<-varianceCovariance.varCovar(rateData[,countryAssetsInds],numCountryAssets,rollingWindow)
		ret[diagInd:(diagInd+numCountryAssets-1), diagInd:(diagInd+numCountryAssets-1)]<-varCovar
		diagInd<-diagInd+numCountryAssets
	}
	
	return(ret)
}

varianceCovariance.riskMatrix<-function(riskMeasure,rateData,numAssets,rollingWindow,halfLife,varScaleFactor){
# main entry point
# which of the functions above it calls is controlled with portfolio.inList[["riskMeasure"]]

	if(riskMeasure=="duration"){
		return(varianceCovariance.expWeightedVarCovarDuration(rateData,numAssets,rollingWindow,halfLife,varScaleFactor))
	}else if(riskMeasure=="volatility"){
		return(varianceCovariance.varCovar(rateData,numAssets,rollingWindow))
	}else if(riskMeasure=="erCovariance"){
		return(varianceCovariance.varCovarNoDuration(rateData,numAssets,rollingWindow))
	}else if(riskMeasure=="volatilityShrink"){
		return(varianceCovariance.varCovarShrink(rateData,numAssets,rollingWindow))
	}else if(riskMeasure=="historical"){
		return(varianceCovariance.varCovarHistorical(rateData,numAssets,rollingWindow))
	} else if(riskMeasure == "shocks"){
		return(varianceCovariance.varCovarShocks(rateData[1,(1+numAssets):ncol(rateData)],numAssets,rollingWindow))
	}else{
		stop(paste("error in varianceCovariance.riskMatrix(): unknown riskMeasure",riskMeasure))
	}
}

