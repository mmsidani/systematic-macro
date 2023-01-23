# all the *Model() functions take the same arguments (for convenience) and ignore some of these arguments, calculate a vector of 2 compoments, a growth number and LTPE number, and return the predicted return 
eretm.nModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	return(eretm.predictedRet(growthRates+inflationRates,ltpe,currentPE,divYld,annualize))
}

eretm.rModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	return(eretm.predictedRet(growthRates+growthOffset+inflationRates,ltpe,currentPE,divYld,annualize))
}

eretm.jModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	ltpe<-meanPayoutRatios/(meanDdrs-growthRates)
	isNegative <- ltpe <= 0
	ret<-rep(NA,length(ltpe))
	ret[!isNegative]<-eretm.predictedRet(growthRates[!isNegative]+inflationRates[!isNegative],ltpe[!isNegative],currentPE[!isNegative],divYld[!isNegative],annualize)
	
	return(ret)
}

eretm.sModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	ltpe<-meanPayoutRatios/(meanDdrs-growthRates)
	isNegative <- ltpe <= 0
	ret<-rep(NA,length(ltpe))
	ret[!isNegative]<-eretm.predictedRet(growthRates[!isNegative],ltpe[!isNegative],currentPE[!isNegative],divYld[!isNegative],annualize)
	
	return(ret)
}

eretm.cModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	ltpe<-meanPayoutRatios/(meanDdrs-growthRates)
	isNegative <- ltpe <= 0
	ret<-rep(NA,length(ltpe))
	ret[!isNegative]<-eretm.predictedCapRet(growthRates[!isNegative]+inflationRates[!isNegative],ltpe[!isNegative],currentPE[!isNegative],divYld[!isNegative],annualize)
	
	return(ret)
}

eretm.tModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	ltpe<-meanPayoutRatios/(meanDdrs-growthRates)
	isNegative <- ltpe <= 0
	ret<-rep(NA,length(ltpe))
	ret[!isNegative]<-eretm.predictedRet(growthRates[!isNegative]+inflationRates[!isNegative]+growthOffset[!isNegative],ltpe[!isNegative],currentPE[!isNegative],divYld[!isNegative],annualize)
	
	return(ret)
}

eretm.fModel<-function(currentPE,ltpe,divYld,growthRates,inflationRates,growthOffset,meanPayoutRatios,meanDdrs,annualize){
	return(eretm.predictedRet(growthRates,ltpe,currentPE,divYld,annualize))
}

# given LTG and LTPE parameters calculate our predicted return
eretm.predictedRet<-function(formulaParams1,formulaParams2,currentPE,divYld,annualize){
	return((1+formulaParams1)*(formulaParams2/currentPE)^annualize-1.+0.5*divYld*(1+currentPE/formulaParams2))
}

# given LTG and LTPE parameters calculate our predicted return
eretm.predictedCapRet<-function(formulaParams1,formulaParams2,currentPE,divYld,annualize){
	return((1+formulaParams1)*(formulaParams2/currentPE)^annualize-1.)
}

# another function that is meant to simplify the main asset loops in eretm.calculate() and eretm.doOneAsset()
# thisDate is a string; thisPe and thisDvdYld are scalars; all other arguments are vectors
# returns data.frame() of one row with all the return models predictions
eretm.getAllModelRets<-function(thisDate,thisPe,thisDvdYld,ltg,inflationRate,ltpe,alpha,meanPayoutRatios,meanDdrs,annualize){
	return(data.frame(Date=thisDate,ner=eretm.nModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize),rer=eretm.rModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize),jer=eretm.jModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize),ser=eretm.sModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize),ter=eretm.tModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize),fer=eretm.fModel(thisPe,ltpe,thisDvdYld,ltg,inflationRate,alpha,meanPayoutRatios,meanDdrs,annualize)))
}

# calculate annualized geometric return from each component of v1 to corresponding component of v2; if v2[i]/v1[i] is negative replace with arithmetic ret
# v1 and v2 are 2 vectors; periodInMonths is an integer, for example, 120, when v1 and v2 are 10 years apart
# returns vector of geom or arith rates
eretm.getGeomOrArithRet<-function(v1,v2,periodInMonths){
	ret<-v2/v1
	
	# which components are positive?
	postvs<-ret>=0
	# calculate geometric rates for positive components
	ret[postvs]<-ret[postvs]^(12/periodInMonths)-1
	# calculate arithmetic rates for negative components
	ret[!postvs]<-(ret[!postvs]-1)*(12/periodInMonths)
	
	return(ret)	
}

# asset is a string
# returns list() indexed by "ccInflation", "ccGrowth". if a formula is involved in the calculation of these, this is where we do it
eretm.getMacroSeries<-function(asset,inflationSeries,gdpSeries,gniSeries,nonLocalMacroGrowth, nonLocalInflation,useGNI){
	# country code
	assetCc<-substr(asset,1,2)
	ccGrowth<-NULL
	if(!is.null(gdpSeries) || !is.null(gniSeries)){
		if(is.element(asset,names(nonLocalMacroGrowth))){
			# we want to use "foreign" macro data in addition, possibly, to local macro data for modeling this market. macro data from more than 1 other country is allowed and in that case we take weighted averages where the weights are specified in GlobalVariables.R
			growthList<-NULL
			for(cc in nonLocalMacroGrowth[[asset]][["macroCcs"]]){
				# recall in useGNI we specify country codes -- not assets, hence the call to substr()
				if(!is.element(substr(asset,1,2),useGNI)){
					growthList<-append(growthList,list(gdpSeries[[grep(paste("^",cc,".",sep=""),names(gdpSeries))]]))
				}else{
					growthList<-append(growthList,list(gniSeries[[grep(paste("^",cc,".",sep=""),names(gniSeries))]]))
				}
			}
	
			# where there are NA's we get NA's as a result of the matrix multiplication. nothing should break.
			growthList<-utils.fillGapsSeries(utils.alignSeries(growthList,TRUE),TRUE)
			ccGrowth<-timeSeries(as.matrix(growthList)%*%matrix(nonLocalMacroGrowth[[asset]][["macroCoeffs"]],ncol=1),charvec=row.names(growthList))
		}else{ # this is the simple base case. we use the local gdp/gni and inflation
			# assetCc growth; growth is either gdp or gni as specified in useGNI
			if(!is.element(assetCc,useGNI)){
				ccGrowth<-gdpSeries[[grep(paste("^", assetCc,".",sep=""),names(gdpSeries))]]
			}else{
				ccGrowth<-gniSeries[[grep(paste("^", assetCc,".",sep=""),names(gniSeries))]]
			}
		}
		
		colnames(ccGrowth ) <- asset
	}
	
	ccInflation<-NULL
	if(!is.null(inflationSeries)){
		if(is.element(asset,names(nonLocalInflation))){
			# we want to use "foreign" macro data in addition, possibly, to local macro data for modeling this market. macro data from more than 1 other country is allowed and in that case we take weighted averages where the weights are specified in GlobalVariables.R
			inflList<-NULL
			for(cc in nonLocalInflation[[asset]][["macroCcs"]]){
				inflList<-append(inflList,list(inflationSeries[[grep(paste("^",cc,"\\.",sep=""),names(inflationSeries))]]))
			}
	
			# where there are NA's we get NA's as a result of the matrix multiplication. nothing should break.
			inflList<-utils.fillGapsSeries(utils.alignSeries(inflList,TRUE),TRUE)
			ccInflation<-timeSeries(as.matrix(inflList)%*%matrix(nonLocalInflation[[asset]][["macroCoeffs"]],ncol=1),charvec=row.names(inflList))
		}else{ # this is the simple base case. we use the local gdp/gni and inflation
			# assetCc inflation or growth; growth is either gdp or gni as specified in useGNI
			ccInflation<-inflationSeries[[grep(paste("^", assetCc,"\\.",sep=""),names(inflationSeries))]]
		}
		colnames(ccInflation ) <- asset
	}
	
	return(list(ccInflation=ccInflation,ccGrowth=ccGrowth))
}

# regress earnings. NOTE: uses end of month data only
eretm.regressGrowth<-function(earningsEOM,benchEarningsEOM,regressEarningsInMonths,regMaxIt){
	numPoints<-min(nrow(benchEarningsEOM),nrow(earningsEOM))
	if(numPoints <= regressEarningsInMonths){
		return(0)
	}
	benchEarningsGrowth<-eretm.getGeomOrArithRet(as.numeric(benchEarningsEOM[(1+regressEarningsInMonths):nrow(benchEarningsEOM),1]),as.numeric(benchEarningsEOM[1:(nrow(benchEarningsEOM)-regressEarningsInMonths),1]), regressEarningsInMonths)
	earningsGrowth<-eretm.getGeomOrArithRet(as.numeric(earningsEOM[(1+regressEarningsInMonths):nrow(earningsEOM),1]),as.numeric(earningsEOM[1:(nrow(earningsEOM)-regressEarningsInMonths),1]),regressEarningsInMonths)

	numPoints<-min(nrow(benchEarningsEOM)-regressEarningsInMonths,nrow(earningsEOM)-regressEarningsInMonths)
	return(utils.robustXPlusAlpha(benchEarningsGrowth[1:numPoints], earningsGrowth[1:numPoints],regMaxIt))
}

# returns arithmetic average of YoY growth
# growthSeries is a timeSeries() (in practice, either gdp or gni) of YoY rates, in reverse chronological order
# returns timeSeries() with average growth
eretm.getAverageGrowth<-function(growthSeries,numGrowthYears){
	growthDates<-as.Date(row.names(growthSeries))
	if(growthDates[1]<growthDates[2]){
		stop("error in eretm.getAverageGrowth(): growthSeries is expected to be in reverse order.")
	}
	
	ret<-NULL
	# why these acrobatics instead of just using index offsets to go back? because we expect gdp data to have irregular frequency, quarterly data mixed with yearly data from GFD
	for(i in 1:length(growthDates)){
		dD<-growthDates[i]
		# go back numGrowthYears from this date
		dates<-utils.shiftToEndOfQuarter( seq(dD,by="-1 year",length.out=numGrowthYears) )
		# now find latest available growth rates for each date
		datesInds<-c()
		for(d in dates){
			datesInds<-c(datesInds,match(TRUE,d>=growthDates))
		}
		
		# now get average
		datesInds<-datesInds[is.finite(datesInds)]
		if(length(datesInds)!=0){		
			dateIndGrowth <- growthSeries[datesInds,1] # numbers to use
			dateIndGrowth <- dateIndGrowth[ is.finite( dateIndGrowth) ] # remove na's
			growth <- ( prod( 1 + dateIndGrowth ) )^(1/length(dateIndGrowth)) - 1			
			newRec<-data.frame(Date=as.character(dD),gdp=growth,stringsAsFactors=FALSE)
			ret<-rbind(ret,newRec)
		}
	}

	names(ret)[2]<-names(growthSeries)
	return(timeSeries(ret[2],charvec=ret$Date))
}

# calculate earnings averaged over a number of years
# earningsData is a timeSeries() in reverse chrono order and for only one asset
# returns timeSeries of averaged earnings
eretm.averageEarnings<-function(earningsData,averageEarningsYears,busDaysPerYear,averageEarningsSuffix){
	if(averageEarningsYears <2) return(earningsData)
	nrowEarningsData<-nrow(earningsData)
	temp<-earningsData[is.finite(earningsData[,1]),]
	# data is daily. this is the range over which we can calculate averageEarningsYears yearly averages
	initRange<-1:(nrow(temp)-(averageEarningsYears-1)*busDaysPerYear)
	ret<-rep(0,length(initRange))
	for (i in 1:averageEarningsYears){
		ret<-ret+ as.numeric(temp[initRange,1])
		initRange<-initRange+busDaysPerYear
	}
	# now augment ret with NA's for those dates in earningsData for which we couldn't calculate averages
	earningsData[,1]<-c(ret/averageEarningsYears,rep(NA,nrow(earningsData)-length(ret)))
	names(earningsData)<-paste(names(earningsData),averageEarningsSuffix,sep="")
	return(earningsData)
}

# calculate Price/PriorEarningsPeaks
eretm.getPtoEPeak<-function(secPrices,secEarnings){
	if(nrow(secPrices)!=nrow(secEarnings)){
		stop(paste("error in eretm.getPtoEPeak(): secPrices has",nrow(secPrices),"rows whereas secEarnings has",nrow(secEarnings),"rows. must be equal."))
	}
	
	newCol<-rep(NA,nrow(secPrices))
	nonNAs<-as.logical(is.finite(secPrices[,1]))&as.logical(is.finite(secEarnings[,1]))
	newCol[nonNAs]<-secPrices[nonNAs,1]/rev(cummax(rev(secEarnings[nonNAs,1])))
	secPrices[,1]<-newCol
	
	return(secPrices)
}

# dividend paid as a fraction of the last recorded peak in earnings. NOTE: can easily change to dividend/earnings by replacing peakEarnings[,1] with secData[,2]
eretm.getPayoutRatio<-function(secDvdYlds,pToEPeak,payoutRatioSuffix){
	nonNAs<-as.logical(is.finite(secDvdYlds[,1])) & as.logical(is.finite(pToEPeak[,1]))
	# divYield * price / earnings = dividend / earnings
	ratios<-as.numeric(secDvdYlds[nonNAs,1])*as.numeric(pToEPeak[nonNAs,1])
	ret<-timeSeries(c(ratios,rep(NA,nrow(secDvdYlds)-length(ratios))),charvec=row.names(secDvdYlds))
	names(ret)<-paste(names(pToEPeak)[1],payoutRatioSuffix,sep="")
	
	return(ret)
}

# seed the params history of asset with that of another asset specified in seedSource
# asset and firstData are strings
# returns data.frame() with columns, $Date,$ltg,$ltpe,$payoutRatio,$inflation
eretm.seedAssetParams<-function(asset,firstDate,seedParams,seedSource){
	# for convenience. only used here
	retNULLSeed<-function(){
		ltg<-NULL
		ltpe<-NULL
		
		return(list(ltg=ltg,ltpe=ltpe))
	}
	
	if(!is.element(asset,names(seedSource))){
		return(retNULLSeed())
	}
	
	# make sure we have the seed source params
	if(!is.element(seedSource[[asset]],names(seedParams))){
		stop(paste("error in eretm.seedAssetParams(): there is no data for",seedSource[[asset]],"to seed",asset,". did you forget to run",seedSource[[asset]],"before",asset,"?"))
	}
	
	# retrieve the data to use for seeding
	seedData<-seedParams[[seedSource[[asset]]]]
	ret<-retNULLSeed()
	for(i in 1:length(seedData)){
		index<-(1:nrow(seedData[[i]]))[firstDate > seedData[[i]]$Date][1]
		if(!is.finite(index) || length(index)==0){
			print(paste("WARNING in eretm.seedAssetParams(): cannot seed",asset,"with data from",seedSource[[asset]],"because the latter does not have earlier data. continuing without seeding."))
			return(retNULLSeed())
		}else{
			# get the parameters from seed source that fall before firstDate. Note: seedParams has data in reverse order
			ret[[names(seedData)[i]]]<-seedData[[i]][index:nrow(seedData[[i]]),]
		}
	}
	
	return(ret)
}

# for convenience; having this logic in a separate function aids the readability of the main asset calculation loops below. it updates seedParams in which we save parameters of assets that are needed to seed params of other assets
# a is a string; all other args are vectors, except for seedParams which is a list() indexed by assets, the seeders, of list()'s indexed by the params we use for seeding ("ltg", "ltpe", etc.); seeders is a vector of strings -- the assets that provide seeds to other assets
# returns seedParams updated with a's params if a is in seeders, seedParams unchanged otherwise
eretm.updateSeedParams<-function(a,assetDates,ltgToSeed,seedLtg,ltpeToSeed,seedLtpe,seedParams,seeders,allowGrowthRateSeeding){
	if(is.element(a,seeders)){
		# a is needed to seed some other asset; this is also where all control over which parameters to seed resides
		indexRuns<-rle(ltgToSeed[["indices"]])
		uniqueIndices<-c(1,1+cumsum(indexRuns[["lengths"]]))[1:length(indexRuns[["lengths"]])]
		ltgDF<-data.frame(Date=c(ltgToSeed[["Date"]][uniqueIndices],seedLtg[["Date"]]),values=c(ltgToSeed[["values"]][uniqueIndices],seedLtg[["values"]]))
		ltpeDF<-data.frame(Date=c(assetDates,seedLtpe[["Date"]]),values=c(ltpeToSeed,seedLtpe[["values"]]))
		
		# a bug in an earlier version of the code prevented seeding the growth rate. now seeding the growth rate is an option
		if(allowGrowthRateSeeding){ 
			seedParams<-append(seedParams,list(newData=list(ltg=ltgDF,ltpe=ltpeDF)))
		}else{	
			seedParams<-append(seedParams,list(newData=list(ltpe=ltpeDF)))
		}
		
		names(seedParams)[length(seedParams)]<-a
	}
	
	return(seedParams)
}

# our calculations start after we have enough inflation and growth (i.e., gdp or gni) data and not before we have asset data
# first 3 arguments are vectors of Date objects in reverse chronological order. freq is a string
# returns a vector of indices in increasing order, the indices in assetDates of the dates where we have all the data we need
eretm.getRows<-function(growthDates,inflationDates,assetDates,freq,dayOfWeek){
	# find the first date in assetDates that is posterior to the last (oldest) date in inflationDates
	indInfl<-match(FALSE,assetDates>=inflationDates[length(inflationDates)])
	if(!is.finite(indInfl)){
		indInfl<-length(assetDates)
	}else{
		indInfl<-indInfl-1
	}
	
	# find the first date in assetDates that is posterior to the last date in growthDates
	indGdp<-match(FALSE,assetDates>= growthDates[length(growthDates)])
	if(!is.finite(indGdp)){
		indGdp <-length(assetDates)
	}else{
		indGdp <-indGdp-1
	}
			
	# minimum of the two: min() because data is in reverse chronological order
	startingRow<-min(indInfl,indGdp)
	
	if(freq=="daily"){
		return(1:startingRow)
	}else if(freq=="monthly"){
		# utils.getEOMDatesIndices() requires data in reverse chronological order
		datesIndices<-utils.getEOMDatesIndices(assetDates[1:startingRow],TRUE)
		return(datesIndices)
	}else if(freq=="weekly"){
		# utils.getWeeklyDatesIndices() on the other hand doesn't care about date order
		return(utils.getWeeklyDatesIndices(assetDates[1:startingRow],dayOfWeek))
	}else{
		stop(paste("error in eretm.getRows(): the specified frequency",freq,"is not implemented"))
	}
}

eretm.buildRepeatingRate<-function(assetDates,rates,ratesDates){ # for inflationRate's and ltg
# for each date in assetDates, get the index in ratesDates of the latest date that comes before the date in assetDates. Note: some of these will obviously repeat since inflation/growth numbers do not update daily

	indices<-algos.mesh(assetDates, ratesDates)

	# Note: we need to take rates[indices,] and we want to keep track of which rates repeated. the indices we just calculated point to components in rates -- not rates[indices,]. so now a little trick so that the indices we save point to the right component in rates[indices,] and not rates. we need to keep track of repeats because when the time comes to average rates we want only unique rates to make it into the average. 
	indexBlocks<-rle(indices)	
	indexBlocks[["values"]]<-1:length(indexBlocks[["values"]])
	
	if(max(indices) < length(rates)){
		maxInd<-max(indices)
		maxIndPlus1<-maxInd+1
		numRates<-length(rates)
		maxIndexBlocksPlus1<-length(indexBlocks[["values"]])+1
		ret<-data.frame(Date=c(ratesDates[indices],ratesDates[maxIndPlus1:numRates]),indices=c(inverse.rle(indexBlocks),maxIndexBlocksPlus1:(maxIndexBlocksPlus1+numRates-maxInd-1)),values=c(rates[indices],rates[maxIndPlus1:numRates]))
	}else{
		ret<-data.frame(Date=ratesDates[indices],indices=inverse.rle(indexBlocks),values=rates[indices])
	}
	return(ret)
}

eretm.averageRepeatingRate<-function(repeatingRateDF,seeds, doAverageOverHistory){
	ratesAndSeedRates<-repeatingRateDF[["values"]]
	ratesAndSeedIndices<-repeatingRateDF[["indices"]]
	if(!is.null(seeds)){
		numRates<-nrow(repeatingRateDF)
		index<-(1:nrow(seeds))[repeatingRateDF[["Date"]][ numRates ] > seeds[["Date"]] ][1]
		if(length(index) != 0 && is.finite(index) ){
			numSeeds<-nrow(seeds)
			ratesAndSeedRates<-c(ratesAndSeedRates,seeds[["values"]][index:numSeeds])
			ratesAndSeedIndices<-c(ratesAndSeedIndices,(numRates+1):(numRates+numSeeds-index+1))
		}
	}

	if(doAverageOverHistory){
		# isDataChronological must be FALSE because we stuck the seedRates at the end
		meanRates<-algos.averageUniqueRepeats(ratesAndSeedIndices,ratesAndSeedRates,FALSE)
	} else {
		meanRates <- ratesAndSeedRates
	}
	
	return(meanRates)
}

eretm.truncate<-function(minAssetDate,maxAssetDate,assetDataDates,assetData){
	return(assetData[assetDataDates >= minAssetDate & assetDataDates <= maxAssetDate,])
}

eretm.buildDDR<-function(dvdYld,meanLtgs){
	return(dvdYld + meanLtgs)
}

eretm.buildAlpha<-function(a,assetDates,growthBenchmark,avgEarnings,benchmarkEarningsData,averageEarningsYears,regressEarningsInMonths,busDaysPerYear,averageEarningsSuffix,regMaxIt){
	if(!is.element(a,names(growthBenchmark))){
		numAssetDates<-length(assetDates)
		return(data.frame(Date=assetDates,indices=rep(1,numAssetDates),values=rep(0,numAssetDates)))
	}else if(is.element(a,names(growthBenchmark))){
		avgEarningsEOM<-utils.getEOMData(avgEarnings,TRUE)
		avgEarningsEOM<-avgEarningsEOM[is.finite(avgEarningsEOM[,1]),]
		avgBenchmarkEarningsEOM<-utils.getEOMData(eretm.averageEarnings(benchmarkEarningsData, averageEarningsYears, busDaysPerYear, averageEarningsSuffix),TRUE)
		avgBenchmarkEarningsEOM<-avgBenchmarkEarningsEOM[is.finite(avgBenchmarkEarningsEOM[,1]),]
	}
		
	avgEarningsDates<-utils.hyphensToYmd(row.names(avgEarningsEOM))
	avgBenchDates<-utils.hyphensToYmd(row.names(avgBenchmarkEarningsEOM))
	commonDates<-intersect(avgEarningsDates,avgBenchDates)
	earningsEOM<-avgEarningsEOM[is.element(avgEarningsDates,commonDates),] # reverse order assumption
	benchEarningsEOM<-avgBenchmarkEarningsEOM[is.element(avgBenchDates,commonDates),] # reverse order assumption
	numEarningsPoints<-length(commonDates)
	earningsInds<-algos.mesh(assetDates,commonDates)
	earningsIndsRuns<-rle(earningsInds)
	numRuns<-length(earningsIndsRuns[["lengths"]])	
	alphas<-rep(0,numRuns)
	for(i in 1:numRuns){
		start<-earningsIndsRuns[["values"]][i]
		
		if(is.finite(start) && numEarningsPoints-start >= regressEarningsInMonths){
			benchEarningsGrowth<-eretm.getGeomOrArithRet(as.numeric(benchEarningsEOM[(start+regressEarningsInMonths):numEarningsPoints,1]),as.numeric(benchEarningsEOM[start:(numEarningsPoints-regressEarningsInMonths),1]), regressEarningsInMonths)
			earningsGrowth<-eretm.getGeomOrArithRet(as.numeric(earningsEOM[(start+regressEarningsInMonths):numEarningsPoints,1]),as.numeric(earningsEOM[start:(numEarningsPoints-regressEarningsInMonths),1]),regressEarningsInMonths)
			alphas[i]<-utils.robustXPlusAlpha(benchEarningsGrowth, earningsGrowth,regMaxIt)
		}
	}	
	earningsIndsRuns$values<-alphas
	ret<-data.frame(Date=assetDates,indices=earningsInds,values=inverse.rle(earningsIndsRuns))
	
	return(ret)
}

eretm.partCumulativeMean <- function( x, lengthOfAverage, doAverageOverHistory ){
	if ( doAverageOverHistory ){
		return( algos.cumSampleMean( x, FALSE) )
	}
	
	ret <- rep(NA, length(x))
	for ( i in 1: length(x)){
		endInd <- i + lengthOfAverage -1
		if ( endInd <= length(x) ){
			ret[i] <- mean( x[ i : endInd] )
		} else {
			ret[i ] <- mean( x[ i: length(x)])
		}
	}
	
	return( ret )
}

eretm.doOneAsset<-function(a,assetData,benchmarkEarningsData,averageEarningsYears,busDaysPerYear,averageEarningsSuffix,payoutRatioSuffix,growthBenchmark,numGrowthYears,freq,dayOfWeek,seedParams,seedSource,annualize,regressEarningsInMonths,regMaxIt,seeders,inflationSeries,gdpSeries,gniSeries,nonLocalMacroGrowth,nonLocalInflation,useGNI, allowGrowthRateSeeding,doAverageOverHistory){
	# ACHTUNG: hard-coded 1 and 2 in calculating pe's for this asset. average yearly earnings and benchmark earnings first
	avgEarnings<-eretm.averageEarnings(assetData[,2],averageEarningsYears,busDaysPerYear,averageEarningsSuffix)
	pToEPeak<-eretm.getPtoEPeak(assetData[,1],avgEarnings)
	isNotNA<-is.finite(pToEPeak[,1])
	pToEPeak<-pToEPeak[isNotNA,]
	assetData<-assetData[isNotNA,]
	
	assetDates<-row.names(pToEPeak)
	
	# get macro data for this asset
	macroSeries<-eretm.getMacroSeries(a,inflationSeries,gdpSeries,gniSeries,nonLocalMacroGrowth,nonLocalInflation,useGNI)
	ccInflation<-macroSeries[["ccInflation"]]
	ccGrowth<-macroSeries[["ccGrowth"]]
	avgGrowth<-eretm.getAverageGrowth(ccGrowth,numGrowthYears)
	
	# find dates for which we can do calculations
	assetDates<-utils.hyphensToYmd(assetDates)
	growthDates<-utils.hyphensToYmd(row.names(avgGrowth))
	inflationDates<-utils.hyphensToYmd(row.names(ccInflation))
	allRows<-eretm.getRows(growthDates,inflationDates,assetDates,freq,dayOfWeek)
	assetDates<-assetDates[allRows]
	numDates<-length(assetDates)
	
	# ACHTUNG: hard-coded assumption that dividend yields are in the third column
	assetDvdYlds<-assetData[,3]
	pToEPeak<-pToEPeak[allRows,]
	assetDvdYlds<-assetDvdYlds[allRows,]

	# dividend/earnings ratios
	payoutRatios<-eretm.getPayoutRatio(assetDvdYlds,pToEPeak,payoutRatioSuffix)

	# this returns a list of NULL data frames if the asset is not to be seeded
	seedData<-eretm.seedAssetParams(a,assetDates[numDates],seedParams,seedSource)
	seedLtg<-seedData[["ltg"]]
	seedLtpe<-seedData[["ltpe"]]
	inflationRate<-eretm.buildRepeatingRate(assetDates,as.numeric(ccInflation[,1]),inflationDates)[["values"]][1:numDates]
	ltgToSeed<-eretm.buildRepeatingRate(assetDates,as.numeric(avgGrowth[,1]),growthDates)
	ltg<-eretm.averageRepeatingRate(ltgToSeed,seedLtg,doAverageOverHistory)[1:numDates]
	ddr<-eretm.buildDDR(as.numeric(assetDvdYlds[,1]),ltg)
	alpha<-eretm.buildAlpha(a,assetDates,growthBenchmark,avgEarnings,benchmarkEarningsData,averageEarningsYears,regressEarningsInMonths,busDaysPerYear,averageEarningsSuffix,regMaxIt)[["values"]]
	ltpe<-algos.cumSampleMean(c(as.numeric(pToEPeak[,1]),seedLtpe[["values"]]),FALSE)[1:numDates]
	payoutRatios<-eretm.partCumulativeMean (payoutRatios, numGrowthYears * busDaysPerYear, doAverageOverHistory)
	ddr<-eretm.partCumulativeMean (ddr, numGrowthYears * busDaysPerYear, doAverageOverHistory)
	
	output<-eretm.getAllModelRets(assetDates,as.numeric(pToEPeak[,1]),as.numeric(assetDvdYlds[,1]),as.numeric(ltg),inflationRate,ltpe,alpha,payoutRatios,ddr,annualize)
	isNotDateCol<-!is.element(names(output),"Date")
	names(output)[isNotDateCol]<-paste(a,".",names(output)[isNotDateCol],sep="")
	output[["Date"]]<-utils.ymdToHyphens(output[["Date"]])
	seedParams<-eretm.updateSeedParams(a,assetDates,ltgToSeed,seedLtg,as.numeric(pToEPeak[,1]),seedLtpe,seedParams,seeders, allowGrowthRateSeeding)
	
	return(list(output=output,pToEPeak=pToEPeak,seedParams=seedParams,growth=avgGrowth,inflation=ccInflation))
}

# input is expected to be "prepared" in predict.R
equityReturnModels.calculate<-function(input,settings){
	numGrowthYears<-settings[["eretm.numGrowthYears"]]
	payoutRatioSuffix<-settings[["eretm.payoutRatioSuffix"]]
	averageEarningsSuffix<-settings[["eretm.averageEarningsSuffix"]]
	averageEarningsYears<-settings[["eretm.averageEarningsYears"]]
	busDaysPerYear<-settings[["eretm.busDaysPerYear"]]
	annualize<-settings[["eretm.annualize"]]
	regMaxIt<-settings[["eretm.regMaxIt"]]
	seedSource<-settings[["eretm.seedSource"]]
	growthBenchmark<-settings[["eretm.growthBenchmark"]]
	useGNI<-settings[["eretm.useGNI"]]
	nonLocalMacroGrowth<-settings[["eretm.nonLocalMacroGrowth"]]
	nonLocalInflation<-settings[["eretm.nonLocalInflation"]]
	regressEarningsInMonths<-settings[["eretm.regressEarningsInMonths"]]
	allowGrowthRateSeeding<-settings[["eretm.allowGrowthRateSeeding"]]
	doAverageOverHistory<-settings[["eretm.doAverageGrowthOverHistory"]]

	equityData<-input[["equityData"]]
	inflationSeries<-input[["inflationSeries"]]
	gdpSeries<-input[["gdpSeries"]]
	gniSeries<-input[["gniSeries"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	
	if(length(equityData)==0){
		return(NULL)
	}
	
	# seedParams is used for seedings; seeders are those assets that provide seed params to other assets
	seedParams<-NULL
	seeders<-c()
	for(a in names(seedSource)){
		seeders<-c(seeders,seedSource[[a]])
	}
	
	equityDataNames<-names(equityData)
	# ACHTUNG: hard-coded 3 which is the number of data items we get per equity security
	assetNames<-equityDataNames[1:(length(equityDataNames)/3)]
	
	outputRets<-NULL
	outputPEs<-NULL
	outputInflation<-NULL
	outputGrowth<-NULL
	for (a in assetNames){
		print(paste("now doing",a))
		assetData<-equityData[,grep(a,equityDataNames)]
		benchmarkEarningsData<-NULL
		if(is.element(a,names(growthBenchmark))){
			benchmarkEarningsData <- equityData[,grep(growthBenchmark[[a]],equityDataNames)][,2]
		}
		
		oneAssetOutput<-eretm.doOneAsset(a, assetData, benchmarkEarningsData,averageEarningsYears, busDaysPerYear,averageEarningsSuffix, payoutRatioSuffix,growthBenchmark,numGrowthYears, freq, dayOfWeek, seedParams, seedSource, annualize,regressEarningsInMonths,regMaxIt, seeders,inflationSeries,gdpSeries,gniSeries,nonLocalMacroGrowth,nonLocalInflation,useGNI, allowGrowthRateSeeding,doAverageOverHistory)
		seedParams<-oneAssetOutput[["seedParams"]]
		outputRets<-append(outputRets,list(oneAssetOutput[["output"]]))
		outputPEs<-append(outputPEs,list(oneAssetOutput[["pToEPeak"]]))
		names(outputPEs)[length(outputPEs)]<-a
		outputInflation<-append(outputInflation,list(oneAssetOutput[["inflation"]]))
		names(outputInflation)[length(outputInflation)]<-a
		outputGrowth<-append(outputGrowth,list(oneAssetOutput[["growth"]]))
		names(outputGrowth)[length(outputGrowth)]<-a
	}

	return(list(rets=utils.align(outputRets,TRUE),pes=outputPEs,inflation=outputInflation,growth=outputGrowth))
}
