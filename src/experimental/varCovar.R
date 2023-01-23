

fb.ledoitWolf<-function(rateData,numAssets,rollingWindow){
	if(nrow(rateData)<(rollingWindow+1)){
		stop(paste("error in fb.varCovar(): fewer than rollingWindow+1 rows"))
	}

	percentChange<-as.matrix(rateData[1:rollingWindow,1:numAssets])/as.matrix(rateData[2:(rollingWindow+1),1:numAssets])-1
	# proprietary standard deviations (risks)
	stds<-as.numeric(rateData[1,(numAssets+1):ncol(rateData)])/fb.varScaleFactor
	# the R cor() function calculates the correlation matrix of the vectors of historical ER's
##########	cors<-cor(rateData[1:rollingWindow,1:numAssets])

	if(ncol(percentChange)!=1){
		cors<-cor(percentChange)
		# now scale cors by the standard deviations
		covs<-diag(stds)%*%cors%*%diag(stds)
	} else {
		covs<-stds[1]^2 
	}
	
	# sum all off-diagonal correlations; get average
	rho<-(sum(cors) - numAssets) / (numAssets*(numAssets-1))
	prior<-diag(stds)%*%diag(rep(rho,numAssets))%*%diag(stds)
	diag(prior)<-stds^2
	
	c<-sum((covs-prior)^2)
	vars<-matrix(diag(covs),ncol=1)
	sqtVars<-sqrt(vars)
	meansX<-colMeans(percentChange)
	x<-percentChange-meansX[rep(1,rollingWindow),]
	y<-x^2
	p<-1/rollingWindow*sum(t(y)%*%y)-sum(covs^2)
	rdiag<-1/rollingWindow*sum(y^2)-sum(vars^2)
	v<-(t(x^3)%*%x)/rollingWindow-vars[,rep(1,numAssets)]*covs
	diag(v)<-0
	roff<-sum(v*(t(sqtVars[,rep(1,numAssets)])/sqtVars[,rep(1,numAssets)]))
	r<-rdiag+rho*roff
	k=(p-r)/c
	
	delta<-max(0,min(1,k/rollingWindow))
	
	return(delta*prior+(1-delta)*covs)
}

