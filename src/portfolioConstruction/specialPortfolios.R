sprt.specialOptPort<-function(portRets,portRisks){
# get some particular points (portfolios) from the frontiers
# portRets, portRisks are 2 matrices with each column corresponding to a frontier on a particular date. the number of rows is equal to the number of points we chose for the frontiers and the number of columns is equal the number of dates in the history for which we built frontiers
# returns a list() of vectors of indices of the rows corresponding to the frontier points of interest. each vector is of length ncol(portRets) (and ncol(portRisks)) and each value is between 1 and nrow(portRets) (and nrow(portRisks))
	
	# how many frontiers? recall, ncol(portRets)== ncol(portRisks)
	numFrontiers<-ncol(portRets)
	# how many points are there on the efficient frontier?
	numPorts<-nrow(portRets)
	# if fewer than 2 we don't have anything to do. just return
	if(numPorts<=2){
		print("WARNING in sprt.specialOptPort(): 2 or fewer portfolios. nothing to do. returning")
		return(NULL)
	}
	
	# get the row in which the minimum risk portfolio appears; this is usually the first row in the output of the frontier; however, we're trying to be careful here
	indsMinRiskPort<-apply(portRisks,2,order)[1,]
	#get the portfolio resulting in the max sharpe ratio
	indsMaxSharpePort<-apply(portRets/portRisks,2,order)[numPorts,]
	# get the row in which the maximum return portfolio appears; this is normally the last row in the output of getFrontier()
	indsMaxRetPort<-apply(portRets,2,order)[numPorts,]
	# now build matrices of indices to extract 3 points from each column (frontier), minimum variance, maximum sharpe and maximum return. each row in each matrix indicates the row and column of the point we want to pick.
	indicesMatrixMaxRetPort<-matrix(c(indsMaxRetPort,1:numFrontiers),ncol=2,byrow=FALSE)
	indicesMatrixMinRiskPort<-matrix(c(indsMinRiskPort,1:numFrontiers),ncol=2,byrow=FALSE)
	indicesMatrixMaxSharpePort<-matrix(c(indsMaxSharpePort,1:numFrontiers),ncol=2,byrow=FALSE)
	# now calculate the slopes of the lines that join the max return portfolio to the other two
	maxRetRets<-portRets[indicesMatrixMaxRetPort]
	maxRetRisks<-portRisks[indicesMatrixMaxRetPort]
	slopesToMatch1<-matrix((maxRetRets-portRets[indicesMatrixMinRiskPort])/(maxRetRisks-portRisks[indicesMatrixMinRiskPort]),nrow=1)
	slopesToMatch2<-matrix((maxRetRets-portRets[indicesMatrixMaxSharpePort])/(maxRetRisks-portRisks[indicesMatrixMaxSharpePort]),nrow=1)
	
	# use forward differences to approximate the slope of the tangents at each (except last) of the points of the efficient frontier
	allRetRisks<-(portRets[2:numPorts,,drop=FALSE]-portRets[1:(numPorts-1),,drop=FALSE])/(portRisks[2:numPorts,,drop=FALSE]-portRisks[1:(numPorts-1),,drop=FALSE])
	
	# now find the point at which slopesToMatch* are best matched. we subtract slopesToMatch* from each row of allRetRisks. Note: we change negative values to Inf to prevent them from being chosen when we take minima
	slopesDiffs1<-allRetRisks-slopesToMatch1[rep(1,numPorts-1),]	
	slopesDiffs1[slopesDiffs1<0]<-Inf
	indsOptPort1<-apply(slopesDiffs1,2,order)[1,]
	slopesDiffs2<-allRetRisks-slopesToMatch2[rep(1,numPorts-1),]
	slopesDiffs2[slopesDiffs2<0]<-Inf
	indsOptPort2<-apply(slopesDiffs2,2,order)[1,]
	
	# return the indices
	return(list(minSMaxR=indsOptPort1+1,sharpeMaxR=indsOptPort2+1,sharpe=indsMaxSharpePort,mSlope=as.numeric(slopesToMatch1),aSlope=as.numeric(slopesToMatch2)))
}

sprt.maxEntropyPortfolio <- function(allFrontiers,cashNameSuffix,numFrontierPoints){
# calculate the maximum entropy point on every frontier. the results are meaningless if we short or have leverage > 1. TODO: add logic to check that there's no shorting and no leverage but NOT an issue for now
	
	frontierHeaders <- colnames(allFrontiers)
	# cash assets are excluded from calculation
	isNonCashAsset <- !grepl(paste(cashNameSuffix,"$",sep=""), frontierHeaders) & !is.element(frontierHeaders,c("Date","portRet","portRisk","Port"))
	allFrontiers <- allFrontiers[,isNonCashAsset]
	# replace NA's and negative weights with a tiny number. in fact 1.e-30 log(1.e-30) is 0 to machine precision. Note: the negative weights we're looking for are really 0 to machine precision if there's no shorting. If in future we add shorting, then this will distort things. in fact, if we add shorting we cannot calculate the E point
	allFrontiers[ !is.finite(allFrontiers) | allFrontiers <= 0 ] <- 1.e-30
	# calculate the entropy of each portfolio in each frontier and normalize it by log(number_of_assets)
	entropies <- matrix(-rowSums(allFrontiers * log(allFrontiers)), nrow=numFrontierPoints,byrow=FALSE) / log(sum(isNonCashAsset))
	
	# find the index of the portfolio with the maximum entropy in each frontier 
	indsMaxEntropyPorts <- apply(entropies,2,order,decreasing=TRUE)[1,]
	
	return(list(maxEntropy=indsMaxEntropyPorts,eEntropy=entropies[matrix(c(indsMaxEntropyPorts,1:ncol(entropies)),ncol=2,byrow=FALSE)]))
}

specialPortfolios.VCEMA<-function(allFrontiers,riskAversionFactors,cashNameSuffix,numFrontierPoints){
# frontierOutput is a list() as output from mvFrontier.R indexed by date of data.frame()'s of columns "portRet","portRisk",asset1, etc.
# returns list() of all frontiers for entire history in one table and tables for points "A", "M", "C"
	
	orgDates<-as.numeric(allFrontiers[,"Date"])
	# get the dates and the number of frontiers
	uniqueDates<-utils.ymdToHyphens(unique(orgDates))
	numFrontiers<-length(uniqueDates)
	columnNames<-colnames(allFrontiers)
	
	# initialize some matrices to calculate the special points on the frontiers. retsMatrix (risksMatrix, resp.) contains in its columns the "portRet" ("portRisk", resp.) columns of all frontiers in the history
	retsMatrix<-matrix(allFrontiers[,"portRet"],nrow=numFrontierPoints,byrow=FALSE)
	risksMatrix<-matrix(allFrontiers[,"portRisk"],nrow=numFrontierPoints,byrow=FALSE)
	
	# now the special points
	optPortsInds<-sprt.specialOptPort(retsMatrix,risksMatrix)
	# sprt.speicalOptPort() returns vectors of indices into each frontier. frontierStarts helps map those into indices into allEffFontiers where all frontiers were stacked
	frontierStarts<-(0:(numFrontiers-1))*numFrontierPoints
	allBestSlopePorts<-data.frame(allFrontiers[optPortsInds[["sharpeMaxR"]]+frontierStarts,,drop=FALSE])
	allMinMaxPorts<-data.frame(allFrontiers[optPortsInds[["minSMaxR"]]+frontierStarts,,drop=FALSE])
	allSharpePorts<-data.frame(allFrontiers[optPortsInds[["sharpe"]]+frontierStarts,,drop=FALSE])
	# now minimum variance portfolios. the first portfolio for every date
	allMinVPorts <- data.frame(allFrontiers[frontierStarts+1,,drop=FALSE])
	# now maximum entropy points. Note: this only works with no shorting, no leverage, but we do not check for that in sprt.maxEntropyPortfolio()
	maxEntropyInds <- sprt.maxEntropyPortfolio(allFrontiers,cashNameSuffix,numFrontierPoints)
	allMaxEntropyPorts <- data.frame(allFrontiers[frontierStarts+maxEntropyInds[["maxEntropy"]],,drop=FALSE])
	# change date format
	allBestSlopePorts[,"Date"]<-allMinMaxPorts[,"Date"]<-allSharpePorts[,"Date"]<-allMinVPorts[,"Date"]<-allMaxEntropyPorts[,"Date"] <- uniqueDates
	
	# now retrieve the risk aversion factors
	mPointRAs <- riskAversionFactors[optPortsInds[["minSMaxR"]]+frontierStarts,"riskAversion",drop=FALSE]
	aPointRAs <- riskAversionFactors[optPortsInds[["sharpeMaxR"]]+frontierStarts,"riskAversion",drop=FALSE]
	ePointRAs <- riskAversionFactors[frontierStarts+maxEntropyInds[["maxEntropy"]],"riskAversion",drop=FALSE]
	# which point on the frontier was picked on a given date? other info also. but first this: without it the last 3 columns in allLabeledPoints would be named "riskAversion"
	colnames(mPointRAs) <- colnames(aPointRAs) <- colnames(ePointRAs) <- NULL
	allLabeledPoints<-data.frame(Date=uniqueDates,C=optPortsInds[["sharpe"]],M=optPortsInds[["minSMaxR"]],A=optPortsInds[["sharpeMaxR"]],E=maxEntropyInds[["maxEntropy"]],MSlope=optPortsInds[["mSlope"]],ASlope=optPortsInds[["aSlope"]],EEntropy=maxEntropyInds[["eEntropy"]],MRiskAversion=mPointRAs,ARiskAversion=aPointRAs,ERiskAversion=ePointRAs)
	
	return(list(summary=allLabeledPoints,A= allBestSlopePorts,M= allMinMaxPorts,C= allSharpePorts,V=allMinVPorts,E=allMaxEntropyPorts))
}
