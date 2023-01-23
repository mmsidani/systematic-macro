multiStrategy.input<-function(settings){
# simFiles is a vector of portfolio names (as strings); typically an element of simFiles suffixed with ".sim.csv" gives us the name of the file that contains the results of simulating the portfolio
# returns a list() of data.frame()'s each with the entire content of an element of simFiles, in chronological order
	
	simFiles<-settings[["msex.portfolios"]]
	inputDir<-settings[["msex.inputDir"]]
	inputDirER<-settings[["msex.inputDirER"]]
	simFileSuffix<-settings[["msex.simFileSuffix"]]
	reallocFileSuffix <- settings[["msex.reallocFileSuffix"]]
	useDB <- settings[["msex.useDB"]]
	dtrCrFile <- settings[["msex.dtrCrFile"]]
	dbSettings <- settings[["dbSettings"]]
	dateFormat <- settings[["msex.dateFormat"]]
	cashNameSuffix <- settings[["msex.cashNameSuffix"]]
	
	sims<-NULL
	reallocs <- NULL
	for(f in simFiles){
		newSimDF<-utils.read(paste(inputDir,f,simFileSuffix,sep=""))
		newReallocDF <- utils.read(paste(inputDir,f,reallocFileSuffix,sep=""))
		# reverse data to get it in chronological order
		sims<-append(sims,list(newDF=newSimDF[nrow(newSimDF):1,]))
		names(sims)[length(sims)]<-f
		reallocs <- append(reallocs, list(newDF=newReallocDF[nrow(newReallocDF):1,]))
		names(reallocs)[length(reallocs)]<-f
	}
	
	# ACHTUNG: we could go to constraints files to get the cash name but we know it's the first cash asset in the sim. This assumption is actually also hard-coded in portfolioSimulation.R. Another thing to keep in mind is that in multiStrategy.R we do not mix portfolios with different base currencies. Hence it suffices to look at the very first sim
	assets1Names <- names(sims[[1]])
	baseCashName <- assets1Names[grep(cashNameSuffix, assets1Names)][1]
	
	if(useDB){
		baseCashDtrs <- utils.getDataMatrixFromDB(baseCashName,".dtr",dbSettings)
	}else{
		baseCashDtrs <- utils.getDataMatrixFromFile(baseCashName, ".dtr", paste(inputDirER,dtrCrFile,sep=""),dateFormat)
	}
	
	return(list(sims=sims,reallocs=reallocs,baseCashDtrs=baseCashDtrs[nrow(baseCashDtrs):1,,drop=F], baseCashName=baseCashName))
}

multiStrategy.output<-function(output,settings){
	outputDir<-settings[["msex.outputDir"]]
	outputFileName<-settings[["msex.outputFileName"]]
	portfolioReturnHistFileSuffix<-settings[["msex.portfolioReturnHistFileSuffix"]]
	portfolioReallocationHistFileSuffix<-settings[["msex.portfolioReallocationHistFileSuffix"]]
	mixedPolicy <-settings[["msex.policy"]]
	
	portHist<-output[["portRetHist"]]
	reallocHist<-output[["reallocHist"]]
	
	outputFileName <- paste(sub(".csv","", outputFileName),".",sub("msex\\.","",mixedPolicy),sep="")
	
	outputRetFile<-paste(outputFileName,portfolioReturnHistFileSuffix,sep="")
	outputReallocFile<-paste(outputFileName,portfolioReallocationHistFileSuffix,sep="")
	
	portHist<-portHist[nrow(portHist):1,]
	reallocHist<-reallocHist[nrow(reallocHist):1,]
	write.csv(portHist,paste(outputDir,outputRetFile,sep=""),row.names=FALSE)
	save(portHist,file=paste(outputDir,sub("\\.csv",".RData", outputRetFile),sep=""))
	write.csv(reallocHist,paste(outputDir,outputReallocFile,sep=""),row.names=FALSE)
	save(reallocHist,file=paste(outputDir,sub("\\.csv",".RData", outputReallocFile),sep=""))
	
	for( n in setdiff(names(output),c("portRetHist", "reallocHist"))) {
		temp <- output[[n]][nrow(output[[n]]):1, ]
		outputNFile <- paste(outputFileName,".",n,sep="")
		write.csv(temp,paste(outputDir,outputNFile,".csv",sep=""),row.names=FALSE)
		save(temp,file =paste(outputDir,outputNFile,".RData",sep=""))
	}
}

msex.policyBestReturn<-function(thisDate, cumRets, simDataDFs, currentPortfolioInd, basePortfolio, performancePeriod, bestReturnHurdle){
# thisDate is a string "YYYY-mm-dd" ; cumRets is a list() indexed by portfolio name of cumulative returns of portfolios as data.frame() with $Date and $ret; simDataDFs is a list() indexed by portfolio name of portfolio simulation history; currentPortfolioInd is an integer for the portfolio in the simDataDFs list() that is currently invested. Note: the last 2 arguments are not used in this function. they're here because they might be needed in future multi-trading policies and we want all policies to have the same interface
	
	# initialize vector to contain performance of different portfolios
	allPortPerfs<-rep(0,length(cumRets))
	for(i in 1:length(cumRets)){
		dateInd<-match(thisDate,row.names(cumRets[[i]]))
		portPerformance<-0
		# we want to compare performance over a period of performancePeriod; dateInd is NA when history starts before this portfolio 
		if(is.finite(dateInd) && dateInd >= performancePeriod+1){
			portPerformance<-cumRets[[i]]$ret[dateInd]/cumRets[[i]]$ret[dateInd-performancePeriod]
		}else if(i==basePortfolio){
			if(!is.finite(dateInd)){
				# we're here if history (union of all dates of all portfolios in the simulation) starts before the base portfolio
				return(NA)
			}else if(dateInd < performancePeriod+1){
				# if history is shorter than performancePeriod we stick with base portfolio
				return(basePortfolio)
			}
		}
		allPortPerfs[i]<-portPerformance
	}
	
	basePortPerformance<-allPortPerfs[basePortfolio]
	# find best realized performance
	orderedInds<-order(allPortPerfs,decreasing=TRUE)
	# check if best performance passes hurdle to switch away from base portfolio
	if(allPortPerfs[orderedInds[1]]>basePortPerformance+bestReturnHurdle){
		return(orderedInds[1])
	}else{
		return(basePortfolio)
	}
}

msex.policyDownAndOut <- function(activeTraders, isBelowThreshold, eopIndices, controlIndices, cumRets){
# if a trader is down below a threshold from the beginning of the period (for example, the year) he's taken out of the mix and remains out for the rest of the year
# activeTraders is a 0/1 matrix, one column per trader, where 0 means trader's positions are not to be considered in the final mix (because for example he had NA's); isBelowThreshold is a matrix of nrow=length(controlIndices) and ncol=ncol(activeTraders) which shows which trader's return dipped below threshold year-to-date ( or quarter-to-date) on the control dates; eopIndices gives periods boundaries indices 
# returns matrix of weights, 1 per trader/ per date
	
	numTraders <- ncol(activeTraders )
	weights <- matrix( 0, nrow = nrow(activeTraders), ncol = numTraders )
	nextEopInd <- match(T, controlIndices[1] <= eopIndices )
	if( controlIndices[1] < eopIndices[ nextEopInd]){
		nextEopInd <- nextEopInd - 1
	}
	
	weights[1:controlIndices[1],  ] <- activeTraders[ 1:controlIndices[1], ] * cumRets[1:controlIndices[1],  ] / cumRets[rep(1, controlIndices[1]),  ]
	activeThisYear <- matrix(nrow=1, ncol= numTraders )
	for(i in 1:( length(controlIndices) -1) ){
		if( i == 1 || controlIndices[i] >= eopIndices[ nextEopInd ]  ){
			weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- activeTraders[ (controlIndices[i]+1):controlIndices[i+1], ] + 0
			weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- weights[(controlIndices[i]+1):controlIndices[i+1],  ] * cumRets[(controlIndices[i]+1):controlIndices[i+1],  ] / cumRets[rep(controlIndices[i]+1, controlIndices[i+1]-controlIndices[i]),  ]
			activeThisYear[1, ] <- rep(T, numTraders )
			nextEopInd <- nextEopInd + 1
			next
		}
		
		# recall isBelowThreshold has length( controlIndices) rows
		activeThisYear[1, ] <- activeThisYear[1, , drop=F] &  !isBelowThreshold[i, , drop=F ]
		weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- activeThisYear[1, , drop=F][rep(1, controlIndices[i+1] - controlIndices[i] ),] & activeTraders[(controlIndices[i]+1):controlIndices[i+1],  ]+ 0
		weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- weights[(controlIndices[i]+1):controlIndices[i+1],  ] * cumRets[(controlIndices[i]+1):controlIndices[i+1],  ] / cumRets[rep(controlIndices[i]+1, controlIndices[i+1]-controlIndices[i]),  ]
		
	}
		
	numAllocatees <- rowSums( weights )
	isNotZeros <- numAllocatees != 0
	weights[isNotZeros, ] <- weights[isNotZeros, ] / matrix(numAllocatees[isNotZeros], ncol=1)[ , rep(1, numTraders) ]
	
	return(weights)
}

msex.policyDownAndSuspend <- function(activeTraders, isBelowThreshold, eopIndices, controlIndices, cumRets){
# same as above but now trader is out for the rest of the control period; so eopIndices in msex.policyDownAndOut() can be EOY indices, e.g., and controlIndices are the indices of the dates on which we record performance. in this policy here, the trader is out only to the end of the observation period (for example, a month) not the performance period (for example, a year)	
# return as above
	
	numTraders <- ncol(activeTraders )
	weights <- matrix( 0, nrow = nrow(activeTraders), ncol = numTraders )
	nextEopInd <- match(T, controlIndices[1] <= eopIndices )
	if( controlIndices[1] < eopIndices[ nextEopInd]){
		nextEopInd <- nextEopInd - 1
	}
	
	weights[1:controlIndices[1],  ] <- activeTraders[ 1:controlIndices[1], ] * cumRets[1:controlIndices[1],  ] / cumRets[rep(1, controlIndices[1]),  ]
	for(i in 1:( length(controlIndices) -1) ){
		if( i == 1 || controlIndices[i] >= eopIndices[ nextEopInd ]  ){
			weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- activeTraders[ (controlIndices[i]+1):controlIndices[i+1], ] + 0
			weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- weights[(controlIndices[i]+1):controlIndices[i+1],  ] * cumRets[(controlIndices[i]+1):controlIndices[i+1],  ] / cumRets[rep(controlIndices[i]+1, controlIndices[i+1]-controlIndices[i]),  ]
			nextEopInd <- nextEopInd + 1
			next
		}
		
		# again, as in downandout, isBelowThreshold has length(controlIndices) rows
		weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- !isBelowThreshold[i, , drop=F ][rep(1, controlIndices[i+1] - controlIndices[i] ),] & activeTraders[(controlIndices[i]+1):controlIndices[i+1],  ]+ 0
		weights[(controlIndices[i]+1):controlIndices[i+1],  ] <- weights[(controlIndices[i]+1):controlIndices[i+1],  ] * cumRets[(controlIndices[i]+1):controlIndices[i+1],  ] / cumRets[rep(controlIndices[i]+1, controlIndices[i+1]-controlIndices[i]),  ]
		
	}
	
	numAllocatees <- rowSums( weights )
	isNotZeros <- numAllocatees != 0
	weights[isNotZeros, ] <- weights[isNotZeros, ] / matrix(numAllocatees[isNotZeros], ncol=1)[ , rep(1, numTraders) ]
	
	return(weights)
}

msex.policyProgressiveRisk2 <- function( portCumRets, eopIndices, controlIndices, basePortfolio, perfToPortTable ){
	
	numBrackets <- nrow( perfToPortTable )
	weights <- matrix( 0, nrow = nrow(portCumRets), ncol = ncol(portCumRets) )
	weights[1:controlIndices[1], basePortfolio] <- 1.0
	combinedRets <- rep( 0, nrow(portCumRets) )
	nextEopInd <- match(T, controlIndices[1] <= eopIndices )
	if( controlIndices[1] < eopIndices[ nextEopInd]){
		nextEopInd <- nextEopInd - 1
	}
	
	for(i in 1:( length(controlIndices) -1) ){
		if( i == 1 || controlIndices[i] >= eopIndices[ nextEopInd ]  ){
			weights[(controlIndices[i]+1):controlIndices[i+1], basePortfolio ] <- 1.0
			combinedRets[ (controlIndices[i]+1):controlIndices[i+1] ] <- portCumRets[ (controlIndices[i]+1):controlIndices[i+1], basePortfolio ]
			nextEopInd <- nextEopInd + 1
			next
		}
		
		# period-to-date performance
		inds <- grep( T, ( (1 + as.numeric(perfToPortTable[1:(numBrackets-1),1]) ) - combinedRets[controlIndices[i]] ) * ( (1 + as.numeric(perfToPortTable[2:numBrackets,1]) ) - combinedRets[controlIndices[i]] ) <= 0  )
		if( length(inds) == 0 ){
			if( 1 + perfToPortTable[1,1] > combinedRets[controlIndices[i]] ){
				currentInd <- perfToPortTable[1, 2]
			} else {
				# we must be above the highest
				currentInd <- perfToPortTable[numBrackets, 2]
			}
		} else if (length(inds) > 1 ){
			# so it coincided with one of the nodes
			currentInd <- perfToPortTable[ match(0, 1 + as.numeric( perfToPortTable[, 1]) - combinedRets[controlIndices[i]] ), 2 ]
			
		} else {
			# length(inds) ==1 
			currentInd <- perfToPortTable[ inds, 2]
		}

		weights[ (controlIndices[i]+1):controlIndices[i+1], currentInd ] <- 1.0
		combinedRets[ (controlIndices[i]+1):controlIndices[i+1]] <- combinedRets[controlIndices[i]] * portCumRets[ (controlIndices[i]+1):controlIndices[i+1], currentInd ] / portCumRets[ controlIndices[i], currentInd ]
	}
	
	return( weights )
}

msex.policyProgressiveRisk1 <- function(isBelowNextThreshold, isAbovePrevThreshold, controlIndices, cumRets ){
# start the year conservatively and when you outperform, get more agressive and when you underperform get less agressive. NOTE: isBelowNextThreshold applies to portfolios 1:(n-1) and tells us which remained below the threshold required to move to a more agressive portfolio; isAbovePrevThreshold applies to portfolios 2:n and tells us where the portfolio remained above the threshold that moved us into it (i.e., the portfolio). controlIndices = indices of dates on which we recorded performance for switching decisions
# returns matrix with 1 weight per trader/ per day, although here one trader is assigned 1 and the others 0, whereas in the downAndOut and downAndSuspend policies we spread wealth evenly
	
	# the number of portfolios is actually the columns of isBelowNextThreshold (same as isAbovePrevThreshold) + 1
	numCols <- ncol(isBelowNextThreshold) + 1
	weights <- matrix(0, nrow= nrow(isBelowNextThreshold), ncol = numCols )
	# first column applies to first portfolio which is the portfolio we start the year with
	weights[,1] <- 1
	
	tempControlIndices <- controlIndices[1:(length(controlIndices)-1)]
	for( i in 2:ncol(weights)){
		# controlIndices are end-of-period indices. add 1 to get beginning of year indices
		temp <- !isBelowNextThreshold[tempControlIndices+1, i-1] & isAbovePrevThreshold[tempControlIndices+1, i-1]
		# where temp we want to move into a more agressive posture
		weights[tempControlIndices+1, i-1][temp==T] <- 0
		weights[tempControlIndices+1, i][temp==T] <- 1
	}
	
	for(i in 2:length(controlIndices)){
		# maintain the same weights throughout the observation period (i.e., until the next observation date)
		weights[(controlIndices[i-1]+1):controlIndices[i], ] <- weights[controlIndices[i-1]+1,,drop=F][rep(1, controlIndices[i] - controlIndices[i-1 ]), ]
	}
	
	return(weights)
}

msex.policyConstantProportions <- function(allReallocDates, commonSimDates, assets, simDataDFs, reallocDataDFs, strategyWeights, baseCashName, cashNameSuffix, reinvestmentDecision,baseCashRets){
# commonDates is a vector of the intersection of all dates in simDataDFs; assets is a vector of all assets in all portfolios (i.e., simDataDFs); simDataDFs is a list() of portfolios; strategyWeight is a vector of weights to apply to the different portfolios
# returns 
	
	# initialize vector to contain combined portfolio
	numAssets <- length(assets)
	numSimDates <- length(commonSimDates)
	numReallocDates <- length(allReallocDates)
	numPorts <- length(simDataDFs)
	if(length(strategyWeights) != numPorts){
		stop(paste("ERROR in msex.policyConstantProportions(): you passed",length(strategyWeights),"weights and",numPorts,"portfolios. they should be the same"))
	}
	
	portWeights <- matrix(0,nrow=numSimDates, ncol=numAssets)
	colnames(portWeights) <- assets
	portDiffs <- matrix(0, nrow=numReallocDates, ncol=numAssets)
	portRet <- rep(0, numSimDates)
	realloc <- rep(0, numSimDates)
	portTc <- rep(0, numReallocDates)
	originalCashAllocation <- rep(0, numSimDates)
	for(i in 1:numPorts){
		if(sum(as.numeric(simDataDFs[[i]][,baseCashName]) < 0) !=0){
			# ACHTUNG until we have it
			stop("ERROR in multiStrategy.R: the logic to handle portfolios with short base cash positions still needs work. Do not pass portfolios with short base cash positions")
		}
		
		isToFill <- is.element(assets,names(simDataDFs[[i]]))
		isSimDate <- is.element(simDataDFs[[i]][["Date"]], commonSimDates)
		tempSim <- as.matrix(simDataDFs[[i]][ isSimDate ,assets[isToFill] ])
		portWeights[,isToFill] <- portWeights[,isToFill] + strategyWeights[i] * tempSim
		portRet <- portRet + strategyWeights[i] * simDataDFs[[i]][ isSimDate, "ret"]
		realloc <- pmax(realloc, simDataDFs[[i]][isSimDate, "realloc"] )
		# NOTE: one portfolio could have realloc=2, which means cash was reinvested and another could have realloc=1, which means rebalancing was done. We're opting to label everything as 1 here. This has no impact on the stats that are output from SIMSTATS. It's just that when reading sim.csv we would see 1 where, possibly, a cash reinvestment was done in one of the portfolios being combined
		realloc[realloc == 2] <- 1
		# we allow there to be an allocation to base cash. as of today, June 2012, the position in base cash is only there to net the short positions in foreign cash. But we want to be flexible, so we consider any allocation to base cash in excess of the sum (absolute value) of (all) short positions to be a desired allocation to cash that should be maintained. First zero positive weights for convenience to be able to use rowSums()
		tempSim[tempSim >0] <- 0
		originalCashAllocation <- originalCashAllocation + strategyWeights[i] * (simDataDFs[[i]][ isSimDate, baseCashName] + rowSums(tempSim))
		
		intersection <- intersect(allReallocDates, reallocDataDFs[[i]][["Date"]])
		isPortDiffDateIntersection <- is.element(allReallocDates,intersection)
		isDFDateIntersection <- is.element(reallocDataDFs[[i]][["Date"]], intersection)
		portDiffs[isPortDiffDateIntersection, isToFill] <- as.matrix(reallocDataDFs[[i]][isDFDateIntersection, assets[isToFill] ])
		portTc[isPortDiffDateIntersection] <- portTc[isPortDiffDateIntersection] + strategyWeights[i] * reallocDataDFs[[i]][isDFDateIntersection, "tc"]
	}
	
	if(reinvestmentDecision != "noReinvestment"){
		# check for excess cash that in this case could have resulted from netting long and short positions in the same asset across portfolios. 
		nonBaseCashAssets <- setdiff(assets,baseCashName)
		nonBaseCashWeights <- portWeights[, nonBaseCashAssets]
		nonBaseCashWeights[nonBaseCashWeights > 0] <- 0
		netShortPositions <- rowSums(nonBaseCashWeights)
		excessCash <- portWeights[ , baseCashName] + netShortPositions - originalCashAllocation
		if(sum(excessCash < -1.0e-10) != 0){
			stop(paste("ERROR in multiStrategy.R: something odd, some excessCash =",paste(excessCash < -1.0e-10,collapse=",")))
		}else if (sum(excessCash < 0) != 0){
			excessCash[excessCash < 0] <- 0
		}
		
		baseCc <- substr(baseCashName, 1, 2)
		countryCodes <- substr(colnames(portWeights), 1, 2)
		cashNames <- paste(countryCodes, cashNameSuffix, sep="")
		cashNames[ countryCodes == baseCc | grepl(cashNameSuffix, colnames(portWeights)) ] <- NA
		nonBaseAggregationList <- list(by= cashNames)
		if(sum(!grepl(cashNameSuffix, assets))==0){
			stop(paste("ERROR in multiStrategy.R: There are no non-cash assets in those portfolios?"))
		}
		if(reinvestmentDecision != "riskyAssets"){
			# do this check right where it matters: in the loop we calculate ratios that might not be correct with a different reinvestment decision
			stop(paste("ERROR in multiStrategy.R: the reinvestment logic can only handle the riskyAssets reinvestmentDecision. nothing is known about:",reinvestmentDecision))
		}	
		for( d in 2:nrow(portWeights)){ 
			reinvestedPort <- reinvesting.reinvest(portWeights[d-1,,drop=F], excessCash[d], originalCashAllocation[d], baseCashName, nonBaseAggregationList, reinvestmentDecision, 0)
			if(!is.null(reinvestedPort)){
				# the non-baseCash assets get scaled in the call to reinvesting.riskyAssets()
				nonCashReturn <- portRet[d] - portWeights[d-1,baseCashName] * baseCashRets[d]
				firstNonCashAsset <- assets[!grepl(cashNameSuffix, assets) & portWeights[d-1,]!=0][1]
				portRet[d] <- reinvestedPort[1, baseCashName] * baseCashRets[d] + nonCashReturn * reinvestedPort[1,firstNonCashAsset] / portWeights[d-1,firstNonCashAsset]
				portWeights[d-1,] <- reinvestedPort
			}
		}
	}
	
	portRetHist=cbind(data.frame(Date=commonSimDates, realloc=realloc, ret = portRet), data.frame(portWeights))
	names(portRetHist) <- c("Date", "realloc", "ret", assets)
	reallocHist = cbind(data.frame(Date=allReallocDates, tc=portTc), data.frame(portDiffs))
	names(reallocHist) <- c("Date", "tc", assets)
	
	return(list(portRetHist=portRetHist,reallocHist=reallocHist))
}

msex.alignRets <- function(cumRets, dates, freq ){
# define performance period: if we're monitoring performance year-to-date, reset cumulative performance at the beginning of each year to 1; similarly with quarters, etc.
# returns list()

	if( freq == "year"){	
		# end of period indices
		eopIndices <- utils.getEOYDatesIndices(dates, FALSE)
	} else if( freq == "quarter"){
		eopIndices <- utils.getEOQDatesIndices(dates)
	} else {
		stop(paste("ERROR in msex.alignRets(): the only frequencies allowed are: year and quarter. you passed:", freq))
	}
	
	numEopIndices <- length(eopIndices)
	cumRets <- as.matrix( cumRets )
	# so: time starts at the end of the very first period
	for(j in numEopIndices:1){
		if(j < numEopIndices){
			# Note: possible division by NA. Note 2: add 1 to indices to get beginning of next period
			cumRets[ (eopIndices[j]+1):eopIndices[j+1], ] <- cumRets[(eopIndices[j]+1):eopIndices[j+1], ,drop=F] / cumRets[eopIndices[j]+1, ,drop=F][rep(1, eopIndices[j+1] - eopIndices[j]), ]
		}
	}
	
	return(list(cumRets=cumRets,periodBoundaryIndices=eopIndices) )
}

msex.getControlIndices <- function(dates, controlFrequency, dayOfWeek){
# this now defines observation period. for example, we might be interested in year-to-date performance (performance period defined in msex.alignRets()) and we record it monthly (control or observation period)
# returns vector
	
	if(controlFrequency == "weekly"){
		return(utils.getWeeklyDatesIndices(dates, dayOfWeek))
	} else if(controlFrequency == "monthly"){
		# dates is in chronological order here and utils.getEOMDatesIndices() wants them in reverse order
		indices <- utils.getEOMDatesIndices(rev(dates), F)
		# convert indices properly
		return( length(dates)-rev( indices ) +1)
	} else if( controlFrequency == "daily") {
		return( 1:length(dates))
	} else {
		stop(paste("ERROR in msex.getControlIndices(): only weekly and monthly are permitted. you passed:", controlFrequency))
	}
}

msex.checkSingleCc<-function(simDataDFs, firstAssetWeightColumn){
# simDataDFs is a list() of data.frame()'s as, for example, output by msex.getSimFiles()
# no return value; crashes if the base currency, determined by the country code of the very first asset in the portfolio, is different between any 2 portfolios
	
	baseCcOld<-NULL
	for(f in simDataDFs){
		# get the country code of the first asset in the list of assets of the portfolio
		baseCc<-substr(names(f)[firstAssetWeightColumn],1,2)
		if(!is.null(baseCcOld) && baseCc!=baseCcOld){
			stop(paste("error in multiStrategyExec.R: all portfolios must be from the same currency perspective. there are at least 2 different currencies:",baseCc,", and ",baseCcOld))
		}
		baseCcOld<-baseCc
	}
	
	# nothing returned
}

msex.checkForGaps <- function(dates, simDataDFs){
# Note: the error we're checking for here (just like the attack in Dr. Strangelove) can never occur in theory 
# dates is a vector and simDataDFs a list() of data.frame()'s
# NO return value
	
	minDate <- min(dates)
	maxDate <- max(dates)
	for(i in 1:length(simDataDFs)){
		f <- simDataDFs[[i]]
		fDates <- f[["Date"]]
		tempDates <- fDates[fDates >= minDate & fDates <= maxDate]
		if(sum(tempDates != dates) != 0){
			stop("ERROR in multiStrategy.R: not all portfolios are defined for a common, contiguous, set of dates")
		}
	}
}

msex.getCumRets<-function(simDataDFs,dates){
# simDataDFs is a list() of data.frame()'s of portfolios simulations; dates are those for which we want to know the cum rets, typically dates on which we consider switching portfolios
# returns list() indexed by portfolio names, each item being a timeSeries() of portfolio cumulative returns
	
	ret<-NULL
	for(i in 1:length(simDataDFs)){
		f<-simDataDFs[[i]]
		dfDates<-f$Date
		# cumulative gross returns 
		cumRets<-cumprod(1+f$ret)
		# which of dfDates do we keep?
		isDateToKeep<-is.element(dfDates,dates)
		newItem<-timeSeries(cumRets[isDateToKeep],charvec=dfDates[isDateToKeep])
		names(newItem)<-"ret"
		ret<-append(ret,list(newItem=newItem))
		names(ret)[length(ret)]<-names(simDataDFs)[i]
	}
	
	return(ret)
}

msex.getAllDates<-function(simDataDFs){
# simDataDFs is a list() of data.frame()'s 
# returns a vector with the sorted combined $Date from all the data.frame()'s in chronological order
	
	ret<-c()
	for(f in simDataDFs){
		ret<-union(ret,f$Date)
	}
	
	if( length(ret) == 0 ){
		stop("ERROR in msex.getAllDates(): all data.frame()'s are empty.")
	}
	
	# sort in chronological order and return
	return(sort(ret,decreasing=FALSE))
}

msex.getCommonDates <- function(simDataDFs){
# simDataDFs is a list() of data.frame()'s 
# returns a vector with the sorted common $Date from all the data.frame()'s in chronological order
	
	ret<-simDataDFs[[1]][["Date"]]
	numDFs <- length(simDataDFs)
	if(numDFs > 1){
		for(i in 2:numDFs){
			ret<-intersect(ret,simDataDFs[[i]][["Date"]])
		}
		
		# sort in chronological order and return
		return(sort(ret,decreasing=FALSE))
	} else {
		return(ret)
	}
}

msex.getAllAssets<-function(simDataDFs,cashNameSuffix,firstAssetWeightColumn){
# simDataDFs is a list() indexed by portfolio name of portfolio history returns. only used here to get asset universe for each portfolio
	
	cashInds<-grep(cashNameSuffix,names(simDataDFs[[1]]))
	# Note: recall we've already asserted all portfolios have the same base currency
	baseCashName<-names(simDataDFs[[1]])[cashInds[1]]
	# take union of all assets in all portfolios
	ret<-c()
	for(f in simDataDFs){
		ret<-union(ret,names(f)[firstAssetWeightColumn:ncol(f)])
	}
	
	# baseCashName should be the first asset because of some legacy stuff
	ret<-c(baseCashName,setdiff(ret,baseCashName))
	return(ret)
}

msex.getReallocationDates<-function(dates, reallocationTimeUnit, dayOfWeek, reallocationFrequency){
# on which dates do we test to switch between portfolios?
# dates is a vector of strings "YYYY-mm-dd"
# returns vector of strings of dates on which we will test to switch between portfolios
	
	if(reallocationTimeUnit=="day"){
		# nothing to do
	}else{
		dates<-as.Date(dates)
		if(reallocationTimeUnit=="week"){
			dates<-dates[weekdays(dates)==dayOfWeek]
		}else if(reallocationTimeUnit=="month"){
			dateBlocks<-rle(months(dates))$lengths
			dates<-dates[cumsum(dateBlocks)]
		}else{
			stop(paste("error in msex.getReallocationDates(): reallocationTimeUnit was set to an unrecognized value,", reallocationTimeUnit))
		}
	}
	
	if(reallocationFrequency==1){
		return(as.character(dates))
	}else{
		# select dates at the specified frequency
		return(as.character(dates[c(TRUE,((2:length(dates))%%reallocationFrequency)==1)]))
	}
}

msex.diffPorts<-function(portfolio1,portfolio2){
# calculate diffs between portfolios. we report this in the .realloc.csv output file and we use it to calculate transaction cost
# portfolio1 and 2 are two data.frame()'s of assets and their weights with nothing extraneous (no Date, ret, etc.)
# returns data.frame() headed by all assets from both portfolios and giving the change in weight in those assets as a fraction of total wealth
	
	assets<-union(names(portfolio1),names(portfolio2))
	commonAssets<-intersect(names(portfolio1),names(portfolio2))
	wDelta<-rep(0,length(assets))
	for(i in 1:length(assets)){
		if(is.element(assets[i],commonAssets)){
			# assets[i] is in both portfolios and its weight might be changing
			wDelta[i]<-portfolio2[1,assets[i]]-portfolio1[1,assets[i]]
		}else if(is.element(assets[i],names(portfolio1))){
			# assets[i] is not in portfolio2 and hence has to be liquidated entirely
			wDelta[i]<--portfolio1[1,assets[i]]
		}else{
			# in portfolio2 but not portfolio1 and has to be bought
			wDelta[i]<-portfolio2[1,assets[i]]
		}
	}
	
	ret<-data.frame(matrix(wDelta,nrow=1))
	names(ret)<-assets
	
	return(ret)
}

msex.getPortfolioDataForDate<-function(thisDate,simDataDF,firstAssetWeightColumn){
# just for convenience this one;
# thisDate is a string of a date; simDataDF is data.frame() of portfolio history (Date, realloc, ret, weights)
	
	ind<-match(thisDate, simDataDF$Date)
	# return the fields that we use elsewhere in the code
	return(list(ret=simDataDF[ind,"ret"],realloc=simDataDF[ind,"realloc"],weights=simDataDF[ind, firstAssetWeightColumn:ncol(simDataDF)]))
}

msex.createMixedPortfolio<-function(assets,portfolio){
# portfolios in the simulation might have different assets; output must be a table headed by a fixed set of assets; here we take portfolio and we assign 0 to assets not in portfolio
# assets is a vector of strings of asset names; portfolio is a data.frame() of some portfolio weights. it's assumed all assets in portfolio are already part of assets
# returns data.frame() with names() equal assets and 0 weights for those assets not in portfolio
	
	ret<-rep(0,length(assets))
	# fit portfolio in the right slots
	isToFill <- is.element(assets,names(portfolio))
	ret[isToFill] <- as.numeric(portfolio[assets[isToFill]])
	
	return(ret)
}

msex.buildHistBestReturn <-function(simDataDFs,tc,cashNameSuffix,basePortfolio,performancePeriod,bestReturnHurdle,firstAssetWeightColumn, reallocationTimeUnit, dayOfWeek, reallocationFrequency,defaultTransactionCost){
# simDataDFs is a list() indexed by portfolio name of portfolio simulation history (as output by portfolioSimulationExecImpl for example); tc is a list() indexed by asset names of transations costs
	
	dates<-msex.getAllDates(simDataDFs)
	# on which dates do we consider reallocating, i.e., switching between portfolios
	reallocationDates<-msex.getReallocationDates(dates, reallocationTimeUnit, dayOfWeek, reallocationFrequency)
	cumRets<-msex.getCumRets(simDataDFs,reallocationDates)
	assets<-msex.getAllAssets(simDataDFs,cashNameSuffix,firstAssetWeightColumn)
	
	# put default values for transaction costs if we didn't specify them
	tc<-transactions.fillMissingTC(assets,tc,defaultTransactionCost)
	
	numDates <- length(dates)
	numAssets <- length(assets)
	
	currentPortfolioInd<-NA
	newInd<-NA
	portRetHist<-matrix(nrow = numDates, ncol = numAssets + 2)
	colnames(portRetHist) <- c("realloc", "ret", assets)
	# Important Note: in portRetHist and reallocHist below we do not convert the dates by removing hyphens as we do with matrices elsewhere in the code. Why? because we're not really doing any arithmetic with the dates and so it's stupid to just remove the hyphens and then put them back in right before we output. If we start doing any date arithmetic in the future we should put the logic to convert the dates to integers. 
	rownames(portRetHist) <- dates
	keepPortRetHistRows <- rep(FALSE, numDates)
	reallocHist<-matrix(nrow = numDates, ncol = numAssets + 1)
	colnames(reallocHist) <- c("tc",assets)
	rownames(reallocHist) <- dates
	keepReallocHistRows <- rep(FALSE, numDates)
	for(i in 1:numDates){
		d <- dates[i]
		if(is.element(d,reallocationDates)){
			newInd<-msex.policyBestReturn(d, cumRets, simDataDFs, currentPortfolioInd,basePortfolio,performancePeriod,bestReturnHurdle)
		}
		
		if(is.finite(newInd)){
			if(is.finite(currentPortfolioInd) && newInd!=currentPortfolioInd){
				# we're here if we have to switch portfolios
				currentPort<-msex.getPortfolioDataForDate(d,simDataDFs[[currentPortfolioInd]], firstAssetWeightColumn)
				newPort<-msex.getPortfolioDataForDate(d,simDataDFs[[newInd]], firstAssetWeightColumn)
				turnovers<-msex.diffPorts(currentPort[["weights"]],newPort[["weights"]])
				transactPenalty<-transactions.getTransactPenalty(turnovers,tc,cashNameSuffix)
				
				portRetHist[i, ] <-c(1,currentPort[["ret"]]+transactPenalty, msex.createMixedPortfolio(assets,newPort[["weights"]]))
				keepPortRetHistRows[i] <- T
				reallocHist[i, ] <-c(transactPenalty, msex.createMixedPortfolio(assets,turnovers))
				keepReallocHistRows[i] <- T
				
				currentPortfolioInd<-newInd
			}else if(!is.finite(currentPortfolioInd)){
				# we're here at the very first reallocation
				newPort<-msex.getPortfolioDataForDate(d,simDataDFs[[newInd]], firstAssetWeightColumn)
				transactPenalty<-transactions.getTransactPenalty(newPort[["weights"]],tc,cashNameSuffix)
				
				mixedPort <- msex.createMixedPortfolio(assets,newPort[["weights"]])
				portRetHist[i, ] <-c(1,transactPenalty, mixedPort)
				keepPortRetHistRows[i] <- T
				reallocHist[i, ] <- c(transactPenalty, mixedPort)
				keepReallocHistRows[i] <- T
				
				currentPortfolioInd<-newInd
			}else{
				# we're here if newInd == currentPortfolioInd, and so it's a reallocation date but we're staying in the same portfolio or it's not a reallocation date
				newPort<-msex.getPortfolioDataForDate(d,simDataDFs[[currentPortfolioInd]], firstAssetWeightColumn)
				portRetHist[i,] <- c(newPort[["realloc"]],newPort[["ret"]], msex.createMixedPortfolio(assets,newPort[["weights"]]))
				keepPortRetHistRows[i] <- T
			}
		}
	}
	
	portRetHist<-portRetHist[keepPortRetHistRows,]
	reallocHist<-reallocHist[keepReallocHistRows,]
	
	return(list(portRetHist=cbind(data.frame(Date=rownames(portRetHist)),data.frame(portRetHist)),reallocHist=cbind(data.frame(Date=rownames(reallocHist)),data.frame(reallocHist))))
}

msex.buildHistNew <- function(mixedPolicy, simDataDFs, reallocDFs, downThreshold, transactionCostConstant,cashNameSuffix,firstAssetWeightColumn, freq, baseCashDtrs, controlFrequency,progressiveThresholds1,progressiveThresholds2, dayOfWeek, basePortfolio){
# the new logic to manage policies
	
	dates<-msex.getAllDates(simDataDFs)
	cumRets <- msex.getCumRets(simDataDFs,dates)
	alignedCumRets <- utils.alignSeries(cumRets, FALSE)
	
	# start every performance period with a cumulative return of 1
	alignList <- msex.alignRets( alignedCumRets , dates, freq )
	cumRets <- alignList[["cumRets"]]
	# indices of performance periods
	eopIndices <- alignList[["periodBoundaryIndices"]]
	# indices of observations/reallocation periods
	controlIndices <- msex.getControlIndices(dates, controlFrequency, dayOfWeek)
	eoyIndices <- utils.getEOYDatesIndices(dates, FALSE)
	
	if( mixedPolicy == "msex.policyDownAndOut" || mixedPolicy == "msex.policyDownAndSuspend" ){
		isBelowThreshold <- cumRets[controlIndices, , drop=F] < 1 + downThreshold
		# the following has this side effect: if trader is not active at the beginning of the period, he's suspended for the whole period
		isBelowThreshold[is.na(isBelowThreshold)] <- 0
		
		activeTraders <- !is.na(cumRets)
		# the specifics of the policy: spread the funds evenly across active trades who have not violated the threshold
		if(mixedPolicy == "msex.policyDownAndOut"){
			weights <- msex.policyDownAndOut(activeTraders, isBelowThreshold, eopIndices, controlIndices, cumRets)
		} else if( mixedPolicy == "msex.policyDownAndSuspend"){
			weights <- msex.policyDownAndSuspend(activeTraders, isBelowThreshold, eopIndices, controlIndices, cumRets)
		}
		
	} else if (mixedPolicy == "msex.policyProgressiveRisk2"){
		if(length(progressiveThresholds2) != ncol(cumRets)){
			stop(paste("ERROR in msex.buildHistNew(): number of thresholds should be equal to number of portfolios. you passed",length(progressiveThresholds2),"thresholds and",ncol(cumRets),"portfolios."))
		}
		if( sum( sort( progressiveThresholds2) != progressiveThresholds2 )){
			stop(paste("ERROR in msex.buildHistNew(): thresholds are not properly ordered. you passed", paste(progressiveThresholds2, collapse=".")) )
		}
		
		perfToPortTable <- matrix(c(progressiveThresholds2, 1:ncol(cumRets)), nrow = length(progressiveThresholds2), byrow = F)
		
		# get the weights
		weights <- msex.policyProgressiveRisk2(cumRets, eopIndices, controlIndices, basePortfolio, perfToPortTable)
	} else if ( mixedPolicy == "msex.policyProgressiveRisk1") {
		if(length(progressiveThresholds1) != ncol(cumRets) - 1){
			stop(paste("ERROR in msex.buildHistNew(): number of thresholds should be equal to number of portfolios minus 1. you passed",length(progressiveThresholds1),"thresholds and",ncol(cumRets),"portfolios."))
		}
		
		cumProgThresholds <- cumsum(progressiveThresholds1)
		# despite the name isBelowThreshold here is very different from the one above since we have a different threshold for every column (except the last)
		isBelowNextThreshold <- matrix(F, nrow=nrow(cumRets), ncol=ncol(cumRets)-1)
		isAbovePrevThreshold <- matrix(F, nrow=nrow(cumRets), ncol=ncol(cumRets)-1)
		for(i in 1:(ncol(cumRets) -1)){
			# if it goes above this threshold we move into a more agressive portfolio
			isBelowNextThreshold[,i] <- cumRets[,i] < 1+cumProgThresholds[i]
		}
		for(i in 2:ncol(cumRets)){
			# if it moves below this threhold we move into a less agressive portfolio
			isAbovePrevThreshold[,i-1] <- cumRets[, i] > 1+cumProgThresholds[i-1]
		}
		
		# get the weights
		weights <- msex.policyProgressiveRisk1(isBelowNextThreshold, isAbovePrevThreshold, controlIndices)
	}
	
	assets <- msex.getAllAssets(simDataDFs,cashNameSuffix,firstAssetWeightColumn)
	
	numDates <- length(dates)
	# combine all positions into 1 portfolio
	combinedPortfolio <- matrix(0,nrow=numDates,ncol=length(assets))
	portRet <- rep(0, numDates)
	colnames(combinedPortfolio) <- assets
	for (i in 1:length(simDataDFs)){
		commonAssets <- intersect(assets, names(simDataDFs[[i]]))
		isCommonDate <- is.element(dates, simDataDFs[[i]]$Date)
		# multiply the positions of the different traders by the appropriate weights and add
		combinedPortfolio[isCommonDate, commonAssets] <- combinedPortfolio[isCommonDate, commonAssets] + weights[isCommonDate,i,drop=F][,rep(1,length(commonAssets))] * as.matrix( simDataDFs[[i]][,commonAssets])
		portRet[isCommonDate] <- portRet[isCommonDate] + weights[isCommonDate,i] * simDataDFs[[i]][, "ret"]
	}

	# by construction the first component in assets is the base cash name
	baseCashName <- assets[1]
	zeroWeights <- rowSums(weights)==0
	if(sum(zeroWeights) != 0){
		# if we're not interested in any trader we move into cash
		combinedPortfolio[ zeroWeights ,baseCashName] <- 1
		ymdDates <- as.character(utils.hyphensToYmd(dates))
		portRet[ zeroWeights ] <- baseCashDtrs[ ymdDates, 1][zeroWeights]
	}
	
	# in addition to the reallocation dates for the individual traders, we have to reallocate on those dates when the allocation to the traders changes
	weightsChanged <- 1 - apply(weights[controlIndices[2:(length(controlIndices)-1)]+1,] == weights[controlIndices[1:(length(controlIndices)-2)]+1, , drop=F ], 1, prod)

	for (i in 1:length(reallocDFs)) {
		reallocDFs[[i]] <- reallocDFs[[i]][ !is.element( reallocDFs[[i]][["Date"]], dates[ weights[,i]==0] ), ]
	}
	# get all dates on which any reallocation happened
	reallocDates<- msex.getAllDates(reallocDFs)

	# add the dates on which we reallocated among traders
	reallocDates <- sort(union(reallocDates, dates[controlIndices[2:(length(controlIndices)-1)]+1][ weightsChanged != 0 ]), decreasing=FALSE )
	reallocHist <- matrix(0,nrow=length(reallocDates),ncol=length(assets))
	dateIndices <- (1:length(dates))[is.element(dates, reallocDates) ]
	if (dateIndices[1] != 1){
		print("WARNING in msex.buildHistNew(): the index of the first reallocation date is not 1.")
	}
	
	reallocHist[1,] <- combinedPortfolio[dateIndices[1], ]
	dateIndices <- dateIndices[2:length(dateIndices) ]
	reallocHist[2:nrow(reallocHist), ] <- combinedPortfolio[dateIndices,] - combinedPortfolio[dateIndices-1,]
	colnames(reallocHist) <- assets
	tc <- rowSums(abs(reallocHist[, setdiff(assets, assets[grep(cashNameSuffix, assets)])])) * transactionCostConstant
	
	activeTradersWeights <- data.frame(Date=dates,weights)
	names(activeTradersWeights)[2:ncol(activeTradersWeights)] <- names(simDataDFs)
	
	return(list(portRetHist = data.frame(Date=dates, realloc=as.integer(is.element(1:numDates, dateIndices)), ret=portRet, data.frame(combinedPortfolio)), reallocHist = data.frame(Date=reallocDates, tc=tc, reallocHist), activeTraders=activeTradersWeights))
}

msex.buildHistConstantProportions <-function(simDataDFs,reallocDataDFs,tc,cashNameSuffix,firstAssetWeightColumn, defaultTransactionCost, strategyWeights, reinvestmentDecision,baseCashDtrs,baseCashName){
# simDataDFs and reallocDataDFs are list()'s indexed by portfolio name of portfolio simulation and reallocation history (as output by portfolioSimulation for example); tc is a list() indexed by asset names of transations costs; cashNameSuffix is a string; firstAssetWeightColumn is an integer, the index of the first asset weight in the portfolio sim file; defaultTransactionCost is a list(); strategyWeights is an array of numbers. NOTE: the style in this file somewhat lacks consistency. Whereas msex.buildHistBestReturn() does quite a bit and leaves little to the "policy" function, msex.buildHistConstantProportions() leaves most of the work to its policy function
# returns the output from msex.policyConstantProportions(), a list() of sim's and realloc's in the same format as portfolioSimulation.R
	
	allReallocDates <- msex.getAllDates(reallocDataDFs)
	commonSimDates<-msex.getCommonDates(simDataDFs)
	# we want all the rellocation dates that fall within the common simulation period only
	allReallocDates <- allReallocDates[ allReallocDates <= max(commonSimDates) & allReallocDates >= min(commonSimDates) ]
	
	# we can get the assets interchangeably from the reallocs or the sims
	assets<-msex.getAllAssets(simDataDFs, cashNameSuffix, firstAssetWeightColumn)
	
	# put default values for transaction costs if we didn't specify them
	tc<-transactions.fillMissingTC(assets,tc,defaultTransactionCost)
	
	return(msex.policyConstantProportions(allReallocDates, commonSimDates, assets, simDataDFs, reallocDataDFs, strategyWeights, baseCashName,cashNameSuffix, reinvestmentDecision,baseCashDtrs[is.element(utils.ymdToHyphens(rownames(baseCashDtrs)), commonSimDates),]))
}

multiStrategy.calculate<-function(input,settings){
	tc<-settings[["msex.transactionCosts"]]
	defaultTransactionCost<-settings[["msex.defaultTransactionCost"]]
	basePortfolio<-settings[["msex.basePortfolio"]]
	performancePeriod<-settings[["msex.performancePeriod"]]
	bestReturnHurdle<-settings[["msex.bestReturnHurdle"]]
	firstAssetWeightColumn<-settings[["msex.firstAssetWeightColumn"]]
	reallocationTimeUnit<-settings[["msex.reallocationTimeUnit"]]
	dayOfWeek<-settings[["msex.dayOfWeek"]]
	reallocationFrequency<-settings[["msex.reallocationFrequency"]]
	portfolios<-settings[["msex.portfolios"]]
	cashNameSuffix<-settings[["msex.cashNameSuffix"]]
	mixedPolicy <-settings[["msex.policy"]]
	portfolioWeights <- settings[["msex.portfoliosWeights"]]
	reinvestmentDecision <- settings[["msex.reinvestmentDecision"]]
	downThreshold <- settings[["msex.downThreshold"]]
	controlFrequency <- settings[["msex.controlFrequency"]]
	progressiveThresholds1 <- settings[["msex.progressiveThresholds1"]]
	progressiveThresholds2 <- settings[["msex.progressiveThresholds2"]]
	
	simDataDFs <- input[["sims"]]
	reallocDataDFs <- input[["reallocs"]]
	baseCashDtrs <- input[["baseCashDtrs"]]
	baseCashName <- input[["baseCashName"]]
	
	msex.checkSingleCc(simDataDFs,firstAssetWeightColumn)
	
	if(mixedPolicy == "msex.policyBestReturn"){
		output<-msex.buildHistBestReturn(simDataDFs,tc, cashNameSuffix,basePortfolio,performancePeriod,bestReturnHurdle,firstAssetWeightColumn, reallocationTimeUnit, dayOfWeek, reallocationFrequency,defaultTransactionCost)
	} else if( mixedPolicy == "msex.policyConstantProportions"){
		output<-msex.buildHistConstantProportions(simDataDFs,reallocDataDFs,tc,cashNameSuffix,firstAssetWeightColumn, defaultTransactionCost, portfolioWeights, reinvestmentDecision,baseCashDtrs,baseCashName)
	} else if( mixedPolicy == "msex.policyDownAndOut" || mixedPolicy == "msex.policyDownAndSuspend" || mixedPolicy == "msex.policyProgressiveRisk1" || mixedPolicy == "msex.policyProgressiveRisk2" || mixedPolicy == "msex.policyWRONGProgressiveRisk2"){
		output <- msex.buildHistNew(mixedPolicy, simDataDFs, reallocDataDFs, downThreshold, defaultTransactionCost,cashNameSuffix,firstAssetWeightColumn, reallocationTimeUnit, baseCashDtrs,controlFrequency,progressiveThresholds1,progressiveThresholds2,dayOfWeek, basePortfolio)
	} else {
		stop(paste("ERROR in multiStrategy.R: the mixed strategy",mixedPolicy,"is not supported."))
	}
	
	return(output)
}

