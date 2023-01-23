psex.getPortHist<-function(inputDir,portHistFile,dateFormat,cashNameSuffix,numberOfWeeks){
# input: 3 strings; a directory, a portfolio history file as output by efficient frontier and a dateFormat
# output: a list of 3 items, a matrix() of portfolio return and risk, a matrix() of portfolio weights, and the base country code (determined by the first cash asset in the array of the names of assets in the portfolio)

	portHist<-read.csv(paste(inputDir,portHistFile,sep=""),header=TRUE,stringsAsFactors=FALSE)
	dates<-portHist[["Date"]]
	
	# begin hack
	portHist <- portHist[(1+numberOfWeeks):length(dates),]
	dates <- dates[1:(length(dates)-numberOfWeeks)]
	# end hack

	
	portHist<-as.matrix(portHist[,setdiff(names(portHist),"Date")])
	rownames(portHist)<-utils.hyphensToYmd(dates)

	portHist<-portHist[nrow(portHist):1,]
	ccs<-unique(substr(setdiff(colnames(portHist),c("Date","portRet","portRisk")),1,2))

	# base currency is the first one in the file by convention
	cashTicker<-paste(ccs[1],cashNameSuffix,sep="")

	# if there's no cash asset (in the base country) we add it; this is only useful if we've built a one-country portfolio because, based on how we do things now, if more than one country is involved, cash positions for every country in the portfolio are necessarily in
	if(!is.finite(match(cashTicker,colnames(portHist)))){
		# we get here possibly only if we're dealing with a one-country portfolio
		tempCash<-1-rowSums(portHist[,3:ncol(portHist),drop=FALSE],na.rm=TRUE)
		portHist<-cbind(portHist,tempCash,deparse.level=0)
		# give it a proper name
		colnames(portHist)[ncol(portHist)]<-cashTicker
	}

	return(list(portRetRisk=portHist[,1:2],portWeights=portHist[,3:ncol(portHist)],baseCountryCode=ccs[1]))
}

psex.getPortfolioData<-function(portfolioName, pathToRSourceCode, namesOfInList, portfolioDesignators, reallocDecision){
# portfolioName is a string, for example, NonUS31
# returns vector of strings of portfolios names that we want to simulate

	# make portfolio.settings NULL before source()'ing as a precaution
	portfolio.settings<-NULL
	portfolioFile<-paste(pathToRSourceCode,"inputFiles/","constraints-",portfolioName,".R",sep="")
	source(portfolioFile)
	portfolioSettings<-portfolio.settings(portfolioName)
	# now make sure portfolioSettings has all the required settings. Note that here we're more flexible than in frontier builder since we allow portfolioSettings to have more fields than is needed for simulation
	if(sum(is.finite(match(namesOfInList,names(portfolioSettings))))!=length(namesOfInList)){
		# yes, we're calculating again something we've just calculated but who cares? we're about to abort anyway
		inds<-match(namesOfInList,names(portfolioSettings))
		stop(paste("error in psex.getPortfolioData(): the following settings were not specified in portfolio.settings()",paste(namesOfInList[!is.finite(inds)],collapse=", ")))
	}
	
	# now form the names of the portfolios that we want to simulate. portfolioDesignators is typically c("A","M","C")
	portfolioNames <- c()
	nonEmptyDesignators <- setdiff(portfolioDesignators, "")
	if(length(nonEmptyDesignators) != 0){
		portfolioNames <-c(portfolioNames,paste(portfolioName,"-", nonEmptyDesignators,"-",ifelse(portfolioSettings[["effFrontierFreq"]]=="weekly", portfolioSettings[["dayOfWeek"]], portfolioSettings[["effFrontierFreq"]]),".csv",sep="") )
	}
	if (is.element("", portfolioDesignators)){
		portfolioNames <-c(portfolioNames, paste(portfolioName,".csv",sep="") )
	}

	# print an info message to make sure there are no surprises.
	print(paste("INFO from portfolioSimulation.R: er/risk mappings in file",portfolioFile,"will be used for portfolio(s)",paste(portfolioNames,collapse=", "),", if rebalancing policy",reallocDecision,"makes use of er/risk"))
	
	return(list(portfolioFileNames=portfolioNames,erMapping=portfolioSettings[["erMapping"]],riskMapping=portfolioSettings[["riskMapping"]],correlationDataFreq=portfolioSettings[["correlationDataFreq"]],dayOfWeek=portfolioSettings[["dayOfWeek"]],baseCountryCode=portfolioSettings[["baseCc"]],riskMeasure= portfolioSettings[["riskMeasure"]],rollingWindow= portfolioSettings[["rollingWindow"]],halfLife= portfolioSettings[["halfLife"]],leverage=portfolioSettings[["leverage"]]))
}

psex.generateIds<-function(assetNames,suffixes,cashNameSuffix,fxDtrSuffix,countryDtrSuffix){
# Why do we have this function instead of using the utilities in data utils? we have an "irregular" way of generating ids in the simulator, e.g., we don't look for cash drc (daily capital return), and we need the dtr's of assets that are not in the portfolio, namely, the dtr's of exchange rates; so to use the utilities in dataUtils we would have to go to the DB or read the file more than once
# input: assetNames of the assets in the portfolio, and suffixes is a list with 2 elements "capRetSuffix" and "totalRetSuffix"
# output: in addition to the ids of dtr's and dcr's of the appropriate assets, this discovers the exchange rates for which dtr's are needed; returns a list of 2 items, a vector of the id's to get from the DB or file, and a list that is later used to rename fx dtr's 

	# get country codes
	ccs<-unique(substr(assetNames,1,2))
	# initialize return vector
	idsToGet<-c()
	# get the suffixes
	capRetSuffix<-suffixes[["capRetSuffix"]]
	totalRetSuffix<-suffixes[["totalRetSuffix"]]
	# we get dtr's for all assets
	idsToGet<-paste(assetNames,totalRetSuffix,sep="")	
	# we get dcr's for non-cash assets only
	cashInds<-grep(cashNameSuffix,assetNames)
	nonCashAssets<-setdiff(assetNames,assetNames[cashInds])
	if(length(nonCashAssets)!=0){
		idsToGet<-c(idsToGet,paste(nonCashAssets,capRetSuffix,sep=""))
	}
	
	# now, the simulation requires dtr's of fx rates, hence the acrobatics that follow
	ccys<-utils.ccyMap(ccs)
	# ccyCcMap is meant to help us rename currency dtr's; for example, we (in the function psex.getAssetRets()) rename gbp.usd.fx.b.dtr to uk.dtr (because it's more convenient to have it that way later in the simulation)
	ccyCcMap<-list()
	nonUsdCcys<-setdiff(ccys,c("usd"))
	if(length(nonUsdCcys)!=0){
		ccyDtrIds<-paste(nonUsdCcys,fxDtrSuffix,sep="")
		idsToGet<-c(idsToGet,ccyDtrIds)
		for(i in 1:length(ccyDtrIds)){
			ccyCcMap<-append(ccyCcMap,list(newEntry=paste(ccs[match(nonUsdCcys[i],ccys)],countryDtrSuffix,sep="")))
			names(ccyCcMap)[length(ccyCcMap)]<-ccyDtrIds[i]
		}
	}

	return(list(idsToGet=idsToGet,ccyCcMap=ccyCcMap,cashDtrs=paste(assetNames[cashInds],totalRetSuffix,sep="")))
}

psex.getAssetRets<-function(useDB,dataFileName,assetNames,suffixes,dateFormat,cashNameSuffix,fxDtrSuffix,countryDtrSuffix,dbSettings){
# input: useDB controls DB usage; dataFileName is file to go to if useDB=FALSE; assets for which we want dtr's and dcr's; suffixes for the dtr's and dcr's; dateFormat is the format of dates in the file, if we're reading from a file
# output: time series of dtr's and dcr's

	# get ids of data including fx dtr's
	listOfIds<-psex.generateIds(assetNames,suffixes,cashNameSuffix,fxDtrSuffix,countryDtrSuffix)
	
	if(useDB){
		retHist<-utils.getDataMatrixFromDB(listOfIds[["idsToGet"]],NULL,dbSettings)
	}else{
		if(is.null(dataFileName)){
			stop("error in psex.getAssetRets(): useDB is FALSE and dataFileName is NULL.")
		}
		retHist<-utils.getDataMatrixFromFile(listOfIds[["idsToGet"]],NULL,dataFileName=dataFileName,dateFormat=dateFormat)
	}
	retHist<-retHist[nrow(retHist):1,]
	
	# now rename fx dtr's; e.g., gbp.usd.fx.b.dtr is renamed simply uk.dtr
	ccyCcMap<-listOfIds[["ccyCcMap"]]
	retHistNames<-colnames(retHist)
	fxDtrInds<-grep(fxDtrSuffix,retHistNames)
	if(length(fxDtrInds)!=0){
		for(i in 1:length(fxDtrInds)){
			colnames(retHist)[fxDtrInds[i]] <- ccyCcMap[[retHistNames[fxDtrInds[i]] ]]
		}
	}
	
	# add a column for usd; its dtr's are 0; if the base currency is not usd, the conversion is done elsewhere in the code
	retHist<-cbind(retHist,rep(0,nrow(retHist)),deparse.level=0)
	colnames(retHist)[ncol(retHist)]<-paste("us",countryDtrSuffix,sep="")
	
	# NOTE: for performance reasons, to avoid conditional searches based on whether an asset is cash or not, we create bogus dcr's for cash and set them equal to the same cash dtr's. we can do that because of the way we use them: when looking for a portfolio day-on-day capital appreciation we use non-cash assets dcr's and cash assets dtr's. with the bogus columns we can, we can just ask for dcr's for all assets. when calculating dividends/interests gains, we take the difference between dcr's and dtr's and given these bogus columns we get 0's for cash assets -- as desired. Changes in use and forgetting about the setting might expose us to bugs in the future though 
	bogusCashDcrs<-retHist[,listOfIds[["cashDtrs"]],drop=FALSE]
	colnames(bogusCashDcrs)<-sub(suffixes[["totalRetSuffix"]],suffixes[["capRetSuffix"]],listOfIds[["cashDtrs"]])
	retHist<-cbind(retHist,bogusCashDcrs,deparse.level=0)
	
	return(retHist)
}

psex.getConvertedForecasts<-function(baseCc,assetNames,erMapping,riskMapping,correlationDataFreq,dayOfWeek,conversionNameSuffix,useDB,inputDir,inputDirER,ersRisksFile,mktDataFile,dateFormat,couponsPerYear,reallocDecision,dbSettings,doRisks){
# baseCc is a string country code; assetNames is a vector of strings
# returns timeSeries() of er/risk's converted to baseCc perspective at a frequency determined by correlationDataFreq

	nAssets<-length(assetNames)
	# form id's
	erIds<-utils.generateIdsFromMapping(assetNames,erMapping)
	riskIds<-utils.generateIdsFromMapping(assetNames,riskMapping)
	ccs<-unique(substr(assetNames,1,2))
	rfIds<-paste(ccs,conversionNameSuffix,sep="")
	
	if(useDB){
		isIRR<-sum(grepl("\\.irr",erIds))!=0
		if(!isIRR){
			allData<-utils.getDataFromDB(c(erIds,riskIds,rfIds),NULL,dbSettings)
		}else{
			erRisks<-utils.getDataFromFile(c(erIds,riskIds),NULL,paste(inputDirER,ersRisksFile,sep=""),dateFormat)
			rfRates<-utils.getDataFromDB(rfIds,NULL,dbSettings)
			allData<-utils.alignSeries(list(erRisks,rfRates),TRUE)
		}
	}else{
		erRisks<-utils.getDataFromFile(c(erIds,riskIds),NULL,paste(inputDirER,ersRisksFile,sep=""),dateFormat)
		rfRates<-utils.getDataFromFile(rfIds,NULL,paste(inputDirER,mktDataFile,sep=""),dateFormat)
		allData<-utils.alignSeries(list(erRisks,rfRates),TRUE)
	}
	erRisks<-allData[,c(erIds,riskIds)]
	# erRiskDomesticEquivalent.convertErsRisks() expects risk to have a ".risk" suffix
	names(erRisks)<-c(assetNames,paste(assetNames,".risk",sep=""))
	ret<-erRiskDomesticEquivalent.convertErsRisks(erRisks,baseCc,allData[,rfIds],couponsPerYear,conversionNameSuffix,doRisks)
	
	# extract data at the desired frequency
	if(correlationDataFreq=="monthly"){
		ret<-utils.getEOMData(ret,TRUE)
	}else if(correlationDataFreq=="weekly"){
		ret<-utils.getWeeklyData(ret,dayOfWeek)
	}else if(correlationDataFreq=="semi-monthly"){
		ret<-utils.getSemiMonthlyData(ret,TRUE)
	}else if(correlationDataFreq=="daily"){
		# nothing to do
	}else{
		stop(paste("error in psex.getConvertedForecasts(): unknown data frequency. you passed",correlationDataFreq))
	}
			
	# convert to a matrix and return. TODO: ideally we should call utils.getDataMatrixFrom*() above. can be done after we have changed erRiskDomesticEquivalent.convertErsRisks() to accept matrices or, most likely, after we have stopped converting risks and started calling erRiskDomesticEquivalent.convertErs() (and it already accepts matrices)
	dates<-utils.hyphensToYmd(row.names(ret))
	ret<-as.matrix(ret)
	rownames(ret)<-dates
	
	return(ret)
}

portfolioSimulation.input<-function(settings){
	# portfolioName is essentially an asset mix and a specification of vol, er/risk mappings, etc.
	portfolioName<-settings[["portfolioName"]]
	pathToRSourceCode<-settings[["pathToRSourceCode"]]
	dateFormat<-settings[["psex.dateFormat"]]
	cashNameSuffix<-settings[["psex.cashNameSuffix"]]
	conversionNameSuffix<-settings[["psex.conversionRateNameSuffix"]]
	inputDir<-settings[["psex.inputDir"]]
	inputDirER<-settings[["psex.inputDirER"]]
	inputRetFile<-settings[["psex.retHistFile"]]
	fxDtrSuffix<-settings[["psex.fxDtrSuffix"]]
	countryDtrSuffix<-settings[["psex.countryDtrSuffix"]]
	useDB<-settings[["psex.useDB"]]
	ersRisksFile<-settings[["psex.ersRisksFile"]]
	mktDataFile<-settings[["psex.mktDataFile"]]
	couponsPerYear<-settings[["psex.couponsPerYear"]]
	pathToRSourceCode<-settings[["pathToRSourceCode"]]
	namesOfInList<-settings[["psex.namesOfInList"]]
	portfolioDesignators<-settings[["psex.portfolioDesignators"]]
	reallocDecision<-settings[["reb.reallocDecision"]]
	dbSettings<-settings[["dbSettings"]]
	suffixes<-settings[["psex.listOfSuffixes"]]
	doRisks<-settings[["psex.doRisks"]]
	
	numberOfWeeks <- settings[["psex.hack.numberOfWeeksToShift"]]

	# this returns the names of the files that contain the history of weights of portfolios we want to simulate all of which correspond to the same asset mix; for example, they are, the "A", "M", etc., points corresponding to portfolioName; this also returns data from the "constraints-[portfolioName].R" file
	portfolioData<-psex.getPortfolioData(portfolioName,pathToRSourceCode, namesOfInList, portfolioDesignators, reallocDecision)
	
	# now read the files containing asset weights history
	portWeightsHist <-NULL
	for(p in portfolioData[["portfolioFileNames"]]){
		# portfolio weights history for all portfolio built with 	
		portWeightsHist<-append(portWeightsHist,list(psex.getPortHist(inputDir,p,dateFormat,cashNameSuffix,numberOfWeeks)))
		names(portWeightsHist)[length(portWeightsHist)]<-p
	}
	
	# now get asset dcr's and dtr's but first get asset names. portWeightsHist is a list() of, for example, "A", "M", "C" portfolios corresponding to the same asset mix. so we can get the assets names from any one of them
	assetNames<-colnames(portWeightsHist[[1]][["portWeights"]])
	assetData<-psex.getAssetRets(useDB,paste(inputDirER,inputRetFile,sep=""),assetNames,suffixes,dateFormat,cashNameSuffix,fxDtrSuffix,countryDtrSuffix, dbSettings)
	
	# now get er/risk's if necessary and convert them to domestic currency perspective 
	forecasts<-psex.getConvertedForecasts(portfolioData[["baseCountryCode"]],assetNames,portfolioData[["erMapping"]],portfolioData[["riskMapping"]],portfolioData[["correlationDataFreq"]],portfolioData[["dayOfWeek"]],conversionNameSuffix,useDB,inputDir,inputDirER,ersRisksFile,mktDataFile,dateFormat,couponsPerYear,reallocDecision, dbSettings,doRisks)

	return(list(portWeightsHist=portWeightsHist,assetData=assetData,forecasts=forecasts,assetNames=assetNames,riskMeasure=portfolioData[["riskMeasure"]],rollingWindow=portfolioData[["rollingWindow"]],halfLife=portfolioData[["halfLife"]],leverage=portfolioData[["leverage"]]))
}

portfolioSimulation.output<-function(output,settings){
	outputDir<-settings[["psex.outputDir"]]
	portfolioReturnHistFileSuffix<-settings[["psex.portfolioReturnHistFileSuffix"]]
	portfolioReallocationHistFileSuffix<-settings[["psex.portfolioReallocationHistFileSuffix"]]
	rebalPolicy <- settings[["reb.reallocDecision"]]
	
	for(portName in names(output)){
		outputRetFile<-paste(portName,".",rebalPolicy,portfolioReturnHistFileSuffix,sep="")
		outputReallocFile<-paste(portName,".",rebalPolicy,portfolioReallocationHistFileSuffix,sep="")
	
		portHist<-output[[portName]][["portSimHist"]]
		reallocHist<-output[[portName]][["portReallocHist"]]
		portHist<-portHist[nrow(portHist):1,]
		reallocHist<-reallocHist[nrow(reallocHist):1,]
		write.csv(portHist,paste(outputDir,outputRetFile,sep=""),row.names=FALSE)
		save(portHist,file=paste(outputDir,sub("\\.csv",".RData", outputRetFile),sep=""))
		write.csv(reallocHist,paste(outputDir,outputReallocFile,sep=""),row.names=FALSE)
		save(reallocHist,file=paste(outputDir,sub("\\.csv",".RData", outputReallocFile),sep=""))
	}
}

psex.adjustRetsForFX<-function(assetRets, ccs, countryDtrSuffix) {
# this converts dtr's and dcr's to base currency; this assumes that the appropriate dtr for the country, relative to the appropriate base country, has been calculated elsewhere (in fact, it's done in psex.convertToBaseCurrency())
# input: assetRets is a timeSeries (only 1 row is needed) of asset dtr's and dcr's; usedAssetDcr and usedAssetDtr are character arrays with the dtr and dcr names we want to convert
# output: list indexed by c(usedAssetDcr, usedAssetsDtr) that gives the converted dcr's and dtr's

	assetRetNames<-colnames(assetRets)
	isNotCountryDtr<-!grepl(paste(countryDtrSuffix,"$",sep=""),assetRetNames)
	for(cc in ccs){
		isCcAsset<-grepl(paste("^",cc,"\\.",sep=""), assetRetNames) & isNotCountryDtr
		assetRets[,isCcAsset]<-(1+assetRets[,isCcAsset,drop=FALSE]) * (1+assetRets[, paste(cc,countryDtrSuffix,sep=""),drop=FALSE])[,rep(1,sum(isCcAsset))] - 1
	}
	
	return(assetRets)
}

psex.buildHist<-function(portDates,portWeightsHist,assetRetDates,assetRetHist,ersRisks,tc,cashNameSuffix,assetNames,nonCashAssets,assetsDcrNames,assetsDtrNames,baseCashName,sortedCashAssets,ccsList,sortedNonBaseCashAssets,nonCashNonBaseCcsList,negativeCashDailyInterest,reinvestmentHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,leverageThreshold,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,reinvestmentDecision,leverage,rebalanceOnly){
# loop through time

	# some more variables we're going to need
	baseCashInd<-match(baseCashName,assetNames)
	
	# init return structures
	numDates<-length(assetRetDates)
	numAssets<-length(assetNames)
	# this will contain the return and weights history of the portfolio
	portRetHist<-matrix(nrow=numDates+1,ncol=numAssets+2) 
	colnames(portRetHist)<-c("realloc","ret",assetNames)
	rownames(portRetHist)<-c(portDates[1],assetRetDates)
	# this one will have the rellocation history
	reallocHist<-matrix(nrow=numDates+1,ncol=numAssets+1)
	colnames(reallocHist)<-c("tc",assetNames)
	rownames(reallocHist)<-c(portDates[1],assetRetDates)
	# this is to keep track of the rows in reallocHist that we want to keep. these are the ones corresponding to dates on which reallocation took place
	keepReallocHistRows<-rep(FALSE,numDates+1)
	portInd<-match(TRUE,assetRetDates[1] <= portDates)-1
	currentPort<-portWeightsHist[portInd,,drop=FALSE]
	initTC<-transactions.getTransactPenalty(currentPort,tc,cashNameSuffix)
	# record the first portfolio
	portRetHist[1,]<-c(1,initTC,as.numeric(currentPort))
	reallocHist[1,]<-c(initTC,as.numeric(currentPort))
	keepReallocHistRows[1]<-TRUE
	reallocate<-FALSE
	originalCashAllocation<-sum(currentPort[,sortedCashAssets])
	
	for(i in 1:numDates){
		# # # if(abs(sum(currentPort[1,])-leverage) > 1.e-6){
			# # # stop(paste("ERROR in buildHist(): weights do not sum to leverage of",leverage,"on", portDates[i]," sum = ", sum(currentPort[1,])))
		# # # }

		# Important Note: When is the following if() clause TRUE? FX moves may increase the weight of foreign positions in a way that is not reflected in the return of the portfolio (portRet) returns. For example: if we're 100% in a foreign position and only fx moves to make it 102%, it remains 102% after the scaling by portRet below because the corresponding foreign cash position would have moved to -102% and the net return is 0 and so portRet is 1
		if(sum(abs(currentPort[1,nonCashAssets]))>leverage){
			print(paste("WARNING in buildHist(): on", assetRetDates[i],"weights of non-cash assets sum to more than leverage of",leverage,". we have sum =",sum(abs(currentPort[1,nonCashAssets]))))
		}

		# calculate new weights. here we get them as percentages of initial portfolio value
		updatedPort <- currentPort
		assetsDcr<-assetRetHist[i,assetsDcrNames]
		updatedPort[1,]<-updatedPort[1,] * (1+assetsDcr)
		assetsDtr<-assetRetHist[i,assetsDtrNames]
		isNegativeWeight <- updatedPort[1,] < 0
		assetsDtr[isNegativeWeight] <- assetsDtr[isNegativeWeight] + negativeCashDailyInterest
		# Note: portfolio return is the net return of all assets including the short cash positions part of the FX hedge. When the only move is a FX move the long and short foreign positions returns cancel out and are not reflected in the portfolio return
		portRet<-sum(currentPort[1,] * assetsDtr)
		# add cashAdjustments (dividend gains, etc; any discrepancy between dtr and dcr) to the cash positions. aggregate() sorts the ccs in ccsList in its output. so we use here a sorted array of cash assets in updatedPort to map things right
		updatedPort[1,sortedCashAssets]<-updatedPort[1,sortedCashAssets]+aggregate(updatedPort[1,]*(assetsDtr-assetsDcr),by=ccsList,FUN="sum")[[2]]
		# now scale to get weights as percentages of current portfolio value
		updatedPort[1,]<-updatedPort[1,]/(1+portRet)
		# adjust cash positions to maintain hedge
		originalCashPosition<-sum(updatedPort[1,sortedNonBaseCashAssets])
		if(length(sortedNonBaseCashAssets) != 0){
			updatedPort[1,sortedNonBaseCashAssets]<- -aggregate(updatedPort[1,],by=nonCashNonBaseCcsList,FUN="sum")[[2]]
		}
		updatedPort[1,baseCashName]<-updatedPort[1,baseCashName]-sum(updatedPort[1,sortedNonBaseCashAssets])+originalCashPosition
		
		# # # # # # updatedPort[1,baseCashName]<-updatedPort[1,baseCashName]+(1-sum(updatedPort))		
		# end new weights
	
		reallocCode<-0
		if(portInd!=length(portDates) && portDates[portInd+1]<=assetRetDates[i]){
			# check for leverage creep
			reallocate <- rebalancing.leverageCreep(updatedPort,leverage,leverageThreshold,nonCashAssets)
			if ( reallocate ) {
				reallocCode <- 9
			} else {
				# we have a new target portfolio; do we reallocate? Note: must exclude cash positions
				reallocate<-rebalancing.reallocHurdle(assetRetDates[i],updatedPort,portWeightsHist[portInd+1,,drop=FALSE],ersRisks,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,leverage,leverageThreshold,riskMeasure,rollingWindow,halfLife,varScaleFactor,nonCashAssets)
				if ( reallocate ) reallocCode <- 1
			}
			if( !reallocate && rebalanceOnly){
				reallocate<- !is.null(reinvesting.reinvest(updatedPort,sum(updatedPort[1,sortedCashAssets])-originalCashAllocation,originalCashAllocation,baseCashInd,nonCashNonBaseCcsList,reinvestmentDecision,reinvestmentHurdle))
			} else if(!reallocate){
				# didn't pass hurdle -- start looking for the next port to rebal
				portInd<-portInd+1	
			}
		}
		
		if(!reallocate){
			currentPort<-updatedPort
			# check if cash has strayed too far from goal and reinvest cash if that's the case
			transactPenalty<-0
			if(!rebalanceOnly) {
				excessCash<-sum(currentPort[1,sortedCashAssets])-originalCashAllocation
				if( excessCash >= reinvestmentHurdle){
					reinvestedCashPort<-reinvesting.reinvest(currentPort,excessCash,originalCashAllocation,baseCashInd,nonCashNonBaseCcsList,reinvestmentDecision,reinvestmentHurdle)
					
					# if reinvestedCashPort is NULL, then we're NOT reinvesting the cash on this date
					if(!is.null(reinvestedCashPort)){
						diffPorts<-reinvestedCashPort[1,,drop=FALSE]-currentPort[1,,drop=FALSE]
						transactPenalty<-transactions.getTransactPenalty(diffPorts,tc,cashNameSuffix)
						reallocHist[i+1,]<-c(transactPenalty,diffPorts)
						keepReallocHistRows[i+1]<-TRUE
						# use reallocation code 2 to signify reinvestment of cash
						reallocCode<-2
						currentPort<-reinvestedCashPort
					}
				}
			}
			portRetHist[i+1,]<-c(reallocCode,portRet+transactPenalty,as.numeric(currentPort))
		}else{
			diffPorts<-portWeightsHist[portInd+1,,drop=FALSE]-updatedPort[1,,drop=FALSE]
			transactPenalty<-transactions.getTransactPenalty(diffPorts,tc,cashNameSuffix)
			
			portRetHist[i+1,]<-c(reallocCode,portRet+transactPenalty,as.numeric(portWeightsHist[portInd+1,]))
			reallocHist[i+1,]<-c(transactPenalty,as.numeric(diffPorts))
			keepReallocHistRows[i+1]<-TRUE	
			currentPort<-portWeightsHist[portInd+1,,drop=FALSE]
			originalCashAllocation<-sum(currentPort[1,sortedCashAssets])
			portInd<-portInd+1
			reallocate<-FALSE
		}	
	}
	reallocHist<-reallocHist[keepReallocHistRows,]
	return(list(portRetHist=cbind(data.frame(Date=utils.ymdToHyphens(rownames(portRetHist))),data.frame(portRetHist)),reallocHist=cbind(data.frame(Date=utils.ymdToHyphens(rownames(reallocHist))),data.frame(reallocHist))))
}

psex.simulate<-function(baseCountryCode,portHist,assetRetHist,suffixes,tc,cashNameSuffix,negativeCashDailyInterest,ersRisks,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,leverageThreshold,reinvestmentDecision,reinvestmentHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,countryDtrSuffix,leverage,rebalanceOnly){
# set things up for the simulation

	portRetRiskHist<-portHist[["portRetRisk"]] # not used
	portWeightsHist<-portHist[["portWeights"]]

	assetNames<-colnames(portWeightsHist)
	# replace NA's with 0
	portWeightsHist[!is.finite(portWeightsHist)]<-0

	# restrict dates to what we need
	portDates<-as.numeric(rownames(portWeightsHist))
	assetRetDates<-as.numeric(rownames(assetRetHist))
	requiredDatesInds <- assetRetDates > portDates[1]
	assetRetDates<-assetRetDates[requiredDatesInds]
	assetRetHist<-assetRetHist[requiredDatesInds,]
	# yes, no unique(), repeating, needed for aggregate() later
	countryCodes<- substr(assetNames,1,2)
	# use country dtr's to convert asset dcr's and dtr's to base currency
	assetRetHist<-psex.adjustRetsForFX(assetRetHist, unique(countryCodes),countryDtrSuffix)
	# remove NA's
	assetRetHist[!is.finite(assetRetHist)]<-0
	# remove NA's
	ersRisks[!is.finite(ersRisks)]<-0
	
	# now a bunch of arrays that are needed repeatedly
	nonCashAssets<-assetNames[!grepl(cashNameSuffix,assetNames)]
	cashAssets<-setdiff(assetNames,nonCashAssets)
	assetsDcrNames<-paste(assetNames,suffixes[["capRetSuffix"]],sep="")
	assetsDtrNames<-paste(assetNames,suffixes[["totalRetSuffix"]],sep="")
	# remove extraneous columns and order remaining columns in the order of the portfolio assets and dcr's followed by dtr's
	assetRetHist<-assetRetHist[,c(assetsDcrNames,assetsDtrNames)]
	
	# some arrays needed when we aggregate()
	baseCashName<-paste(baseCountryCode,cashNameSuffix,sep="")
	sortedCashAssets<-sort(cashAssets)
	sortedNonBaseCashAssets<-sort(setdiff(cashAssets,baseCashName))
	ccsList <- list(by=countryCodes)
	nonCashNonBaseCcs <- paste(countryCodes,cashNameSuffix,sep="")
	nonCashNonBaseCcs[is.element(assetNames,cashAssets) | countryCodes == baseCountryCode]<-NA
	nonCashNonBaseCcsList<-list(by=nonCashNonBaseCcs)
	allHist<-psex.buildHist(portDates,portWeightsHist,assetRetDates, assetRetHist,ersRisks, tc,cashNameSuffix,assetNames,nonCashAssets,assetsDcrNames,assetsDtrNames,baseCashName,sortedCashAssets, ccsList,sortedNonBaseCashAssets,nonCashNonBaseCcsList,negativeCashDailyInterest,reinvestmentHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,leverageThreshold,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,reinvestmentDecision,leverage,rebalanceOnly)
		
	return(list(portRetHist=allHist[["portRetHist"]],reallocHist=allHist[["reallocHist"]]))
}

psex.convertToBaseCurrency<-function(baseCountryCode, assetRetHist,countryDtrSuffix){
# the country dtr's that we have in the DB (and hence the files) are relative to USD. if the base country is not US, convert those to another currency
# input: baseCountryCode is a 2-letter country code, "uk", "us", etc.; assetRetHist is a timeSeries of asset dtr's and dcr's and country (i.e., fx) dtr's relative to USD
# output: country dtr's will have been converted to be dtr's relative to the currency of baseCountryCode; so we return a modified assetRetHist

	if(baseCountryCode == "us"){
		# nothing to do
		return(assetRetHist)
	}
	
	# find the base country dtr relative to usd
	baseCountryDtrs<-assetRetHist[,paste(baseCountryCode,countryDtrSuffix,sep=""),drop=FALSE]
	# find all countries
	countryDtrInds <- grep(countryDtrSuffix,colnames(assetRetHist))
	# convert. Note: it's component-wise so we can afford to have NA's, we just get NA's -- as desired
	assetRetHist[,countryDtrInds] <- (1 + assetRetHist[,countryDtrInds]) / (1 + baseCountryDtrs[,rep(1,length(countryDtrInds))]) - 1
	
	# return assetRetHist with now converted non-base country dtr's
	return(assetRetHist)
}

portfolioSimulation.calculate<-function(input, settings){
# main entry point

	tc<-settings[["psex.transactionCosts"]]
	suffixes<-settings[["psex.listOfSuffixes"]]
	cashNameSuffix<-settings[["psex.cashNameSuffix"]]
	negativeCashDailyInterest<-settings[["psex.negativeCashDailyInterest"]]
	countryDtrSuffix<-settings[["psex.countryDtrSuffix"]]
	defaultTransactionCost<-settings[["psex.defaultTransactionCost"]]
	varScaleFactor<-settings[["vc.varScaleFactor"]]
	reallocDecision<-settings[["reb.reallocDecision"]]
	weightsHurdle<-settings[["reb.weightsHurdle"]]
	erHurdle<-settings[["reb.erHurdle"]]
	erRiskRatioHurdle<-settings[["reb.erRiskRatioHurdle"]]
	klHurdle <- settings[["reb.klHurdle"]]
	euclidHurdle <- settings[["reb.euclidHurdle"]]
	leverageThreshold<-settings[["reb.leverageThreshold"]]
	reinvestmentDecision<-settings[["reinv.reinvestmentDecision"]]
	reinvestmentHurdle<-settings[["reinv.reinvestmentHurdle"]]
	rebalanceOnly <- settings[["psex.rebalanceOnly"]]
	
	portHistFiles<-input[["portWeightsHist"]]
	assetNames<-input[["assetNames"]]
	assetRetHist <-input[["assetData"]]
	forecasts<-input[["forecasts"]]
	riskMeasure<-input[["riskMeasure"]]
	rollingWindow<-input[["rollingWindow"]]
	halfLife<-input[["halfLife"]]
	leverage <- input[["leverage"]]
	
	ret<-NULL
	for(i in 1:length(portHistFiles)){			
		portHist<-portHistFiles[[i]]
		portName<-sub(".csv","",names(portHistFiles)[i])
		baseCountryCode<-portHist[["baseCountryCode"]]
		
		# all items in list() have same length() so can convert to data.frame()
		tc<-transactions.fillMissingTC(assetNames,tc,defaultTransactionCost)
		
		# convert country dtr's to the base country (if not US)
		assetRetHist<-psex.convertToBaseCurrency(baseCountryCode, assetRetHist,countryDtrSuffix)

		execTime<-system.time(results<-psex.simulate(baseCountryCode,portHist,assetRetHist,suffixes,tc,cashNameSuffix,negativeCashDailyInterest,forecasts,reallocDecision,weightsHurdle,erHurdle,erRiskRatioHurdle,klHurdle,euclidHurdle,leverageThreshold,reinvestmentDecision,reinvestmentHurdle,riskMeasure,rollingWindow,halfLife,varScaleFactor,countryDtrSuffix,leverage,rebalanceOnly))[["elapsed"]]
		print(paste("INFO from portfolioSimulation.calculate(): the simulation of",portName,"took",execTime,"secs."))

		ret<-append(ret,list(list(portSimHist=results[["portRetHist"]],portReallocHist=results[["reallocHist"]])))
		names(ret)[length(ret)]<-portName
	}

	return(ret)
}
