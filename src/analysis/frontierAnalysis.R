frontierAnalysis.input<-function(settings){
# input function
	
	inputDir<-settings[["faex.inputDir"]]
	frontiersFile<-settings[["faex.frontiersFile"]]
	
	# the file we read here typically is the *-Opt-FULL.csv that is output by fontierBuilder
	# returns data.frame() with the entire file content as is
	return(read.csv(paste(inputDir,frontiersFile,sep=""),header=TRUE))
}

frontierAnalysis.output<-function(output,settings){
# output function
	
	outputDir<-settings[["faex.outputDir"]]
	frontiersFile<-settings[["faex.frontiersFile"]]
	
	frontiersFile<-sub("\\.csv","", frontiersFile)
	write.csv(output[["output1"]],paste(outputDir, frontiersFile,"-frontierAnalysis1.csv",sep=""),row.names=FALSE)
	write.csv(output[["output2"]],paste(outputDir, frontiersFile,"-frontierAnalysis2.csv",sep=""),row.names=FALSE)
	write.csv(output[["output3"]],paste(outputDir, frontiersFile,"-frontierAnalysis3.csv",sep=""),row.names=FALSE)
}

faex.frontierSlopes<-function(frontier){
# calculate the slopes that determine some of the special portfolios we use: max_sharpe_ratio_portfolio to max_return_portfolio (Aggressive point); min_variance_portfolio to max_return_portfolio (Moderate point)
# frontier is a data.frame() containing the efficient frontier for a single date.
	
	if(length(unique(frontier$Date))!=1){
		stop(c("error in faex.frontierSlopes(): frontier must correspond to one date only. what was passed had multiple dates",paste(unique(frontier$Date),collapse=",")))
	}
	
	# get the maximum sharpe ratio point on the frontier
	sharpePoint<-order(frontier$portRet/frontier$portRisk,decreasing=TRUE)[1]
	# join it to maximum return point and get the slope of the tangent to the frontier at the agressive portfolio point
	aggressiveSlope<-(frontier$portRet[nrow(frontier)]-frontier$portRet[sharpePoint])/(frontier$portRisk[nrow(frontier)]-frontier$portRisk[sharpePoint])
	
	# now join the minimum variance point (the first point on the frontier) to the maximum return point (the last point on the frontier) and get the slope of the tangent to the frontier at the moderate portfolio point
	moderateSlope<-(frontier$portRet[nrow(frontier)]-frontier$portRet[1])/(frontier$portRisk[nrow(frontier)]-frontier$portRisk[1])
	
	return(data.frame(aggressiveSlope=aggressiveSlope,moderateSlope=moderateSlope))
}

faex.portfolioReturnDispersion<-function(frontier){
# calculate the dispersion of the returns of the different portfolios on the efficicent frontier
# frontier is a data.frame() with the frontier for a single date
# returns scalar, the dispersion
	
	return(sqrt(sum((frontier$portRet-mean(frontier$portRet))^2)/nrow(frontier)))	
}

faex.portfolioRiskDispersion<-function(frontier){
# calculate the dispersion of the risks of the different portfolios on the efficicent frontier
# frontier is a data.frame() with the frontier for a single date
# returns scalar, the dispersion
	
	return(sqrt(sum((frontier$portRisk-mean(frontier$portRisk))^2)/nrow(frontier)))	
}

faex.pcaFactor<-function(frontiers,assetWeightsBeginInColumn, numFrontierPoints){
	dates<-unique(frontiers$Date)
	if(dates[1]<dates[2]){
		stop("error in faex.pcaFactor(): the efficient frontiers should be in REVERSE chronological order.")
	}

	# form the matrix of returns: time index applies to rows
	rets<-matrix(nrow=length(dates),ncol=numFrontierPoints)
	for(i in 1:length(dates)) {
		d <- dates[i]
		frontier<-frontiers[(1+(i-1)*numFrontierPoints):(i*numFrontierPoints),]
		rets[i,]<-matrix(frontier[["portRet"]],nrow=1)
	}
	
	# de-mean the returns. Note: Karhunen-Loeve says to take covariance of X_t with X_s for the stochastic process X on (Omega, F, P). X_s and X_t are i.d. and so the variables observed at different times can be thought of as different realizations of the variables for 1 given time. We use that in calculating the mean and the var-covariances below
	retsMeans<-matrix(colMeans(rets),nrow=1)
	rets<-rets-retsMeans[rep(1,nrow(rets)),]
	
	# get only 1 right singular vector, since we're looking for 1 factor only. recall right singular vector is also right eigenvector of t(rets)%*%rets, the covariance matrix. this assumes svd(rets) = U * Sigma * V^T
	svdRets<-svd(rets,nu=0,nv=1)
	# square the largest singular value to get the largest variance.  
	retVariance<-svdRets[["d"]][1]^2
	# calculate fraction explained
	relVal<-retVariance/sum(svdRets[["d"]]^2)
	# reshape
	singularVector<-matrix(svdRets[["v"]][,1],ncol=1)
	# scale vector to make into weights summing to 1
	singularVector<-singularVector/sum(singularVector)
	# the (first) factor portfolio for frontier points (which is also the principal component by definition). Note: the variance of factor is NOT equal to the largest eigenvalue since we did not scale singularVector to make its 2-norm equal 1
	factor<-rets %*% singularVector
	
	# the corresponding portfolio
	assetWeights<-t(singularVector)%*%as.matrix(frontiers[frontiers$Date==dates[1],assetWeightsBeginInColumn:ncol(frontiers)])
	# recall rets were demeaned. for the next statement we need the weighted average of the original returns of the points on the frontier
	portRet <- sum(frontiers[["portRet"]][1:numFrontierPoints] * as.numeric(singularVector) )
	# NOTE: portRisk is of course incorrect since we don't take correlations into account. Its value doesn't matter if we're just simulating later since the simulator ignores portRisk (24/08/12)
	portRisk <- sum(frontiers[["portRisk"]][1:numFrontierPoints] * as.numeric(singularVector) )
	
	return(list(stats=data.frame(Date=dates[1],variance= retVariance,relVal=relVal,singularVector = t(singularVector),factor=t(factor)),portfolio=data.frame(Date=dates[1],portRet=portRet,portRisk=portRisk,assetWeights)))
}

# main logic
frontierAnalysis.calculate<-function(input,settings){
	pcaRollingWindow<-settings[["faex.pcaRollingWindow"]]
	assetWeightsBeginInColumn<-settings[["faex.assetWeightsBeginInColumn"]]
	numFrontierPoints <- settings[["faex.numFrontierPoints"]]
	
	frontiers<-input
	
	dates<-unique(frontiers$Date)
	output1<-NULL
	output2<-NULL
	output3<-NULL
	for(i in 1:length(dates)){
		frontier<-frontiers[grep(dates[i],frontiers$Date),]
		
		slopes<-faex.frontierSlopes(frontier)
		retDisps<-faex.portfolioReturnDispersion(frontier)
		riskDisps<-faex.portfolioRiskDispersion(frontier)
		
		output1<-rbind(output1,data.frame(Date=dates[i],slopes, retDisps, riskDisps))
		
		if(i+pcaRollingWindow-1<=length(dates)){
			pcaFactors<-faex.pcaFactor(frontiers[is.element(frontiers$Date,dates[i:(i+pcaRollingWindow-1)]),], assetWeightsBeginInColumn, numFrontierPoints)
			
			output2<-rbind(output2,pcaFactors[["stats"]])
			output3<-rbind(output3,pcaFactors[["portfolio"]])
		}
	}

	return(list(output1=output1, output2 =output2, output3 =output3))
}


