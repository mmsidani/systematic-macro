sharpeRatios.input<-function(settings){
	inputDir<-settings[["shr.inputDir"]]
	mktDataFile<-settings[["shr.mktDataFile"]]
	retHistFile<-settings[["shr.retHistFile"]]
	dateFormat<-settings[["shr.dateFormat"]]
	useDB<-settings[["shr.useDB"]]
	baseCc<-settings[["shr.baseCc"]]
	assets<-settings[["shr.assets"]]
	retSuffix<-settings[["shr.retSuffix"]]
	cashNameSuffix<-settings[["shr.cashNameSuffix"]]
	dbSettings<-settings[["dbSettings"]]
	
	# we need cash levels and the dtr of base cash; the former to convert to baseCc perspective and the latter to calculate average cash returns
	ccs<-substr(assets,1,2)
	cashNames<-paste(union(ccs,baseCc),cashNameSuffix,sep="")
	baseCashRetName<-paste(baseCc,cashNameSuffix,retSuffix,sep="")
	assetRetNames<-paste(assets,retSuffix,sep="")
	
	if(useDB){
		allData<-utils.getDataMatrixFromDB(union(assetRetNames,union(baseCashRetName, cashNames)),NULL,dbSettings)
		assetRets<-allData[,assetRetNames]
		baseCashRets<-allData[, baseCashRetName]
		cashLevels<-allData[,cashNames]
	}else{
		# the data we need lives in separate files
		assetRets<-utils.getDataMatrixFromFile(union(assetRetNames, baseCashRetName),NULL,paste(inputDir,retHistFile,sep=""),dateFormat)
		cashLevels<-utils.getDataMatrixFromFile(cashNames,NULL,paste(inputDir, mktDataFile,sep=""),dateFormat)
		numRows<-min(nrow(assetRets),nrow(cashLevels))
		if(sum(row.names(assetRets)[1:numRows]!=row.names(cashLevels)[1:numRows])!=0){
			stop(paste("error in sharpeRatios.input(): the dates in",retHistFile,"and",mktDataFile,"do not match"))
		}
		baseCashRets<-assetRets[1:numRows,baseCashRetName]
		assetRets<-assetRets[1:numRows,assetRetNames]
		cashLevels<-cashLevels[1:numRows,]
	}
	
	return(list(assetRets=assetRets,baseCashRets=baseCashRets,cashLevels=cashLevels))
}

sharpeRatios.output<-function(output,settings){
	outputDir<-settings[["shr.outputDir"]]
	outputFile<-settings[["shr.outputFile"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

shr.sharpeRatios<-function(cumAssetReturns, cumBaseCashRets, periodsInYear){
# cumAssetReturns and cumBaseCashRets are named matrices for the dates of interest in chronological order; for example, if we're interested in yearly averages, we year end data; if monthly, we should pass month end data (essentially, call the appropriate utils.() function before calling this function). periodsInYear is 1 for yearly data, 12 for monthly, etc.

	assetPeriodRets<-cumAssetReturns[2:nrow(cumAssetReturns),]/cumAssetReturns[1:(nrow(cumAssetReturns)-1),]-1
	# cashPeriodRets will be coerced to a vector because we're not specifying drop=FALSE in `['
	cashPeriodRets<-cumBaseCashRets[2:nrow(cumBaseCashRets),]/cumBaseCashRets[1:(nrow(cumBaseCashRets)-1),]-1
	
	cashIsNotNA<-as.logical(is.finite(cashPeriodRets))
	allDataNotNA<-is.finite(assetPeriodRets) & cashIsNotNA
	ret<-matrix(data=0,nrow=1,ncol=ncol(assetPeriodRets))
	for(i in 1:ncol(assetPeriodRets)){
		iColumn<-assetPeriodRets[allDataNotNA[,i],i]
		meanAssetRets<-mean(iColumn,na.rm=TRUE)
		sdAssetRets<-sd(iColumn,na.rm=TRUE) * sqrt(periodsInYear)
		meanCashRets<-mean(cashPeriodRets[allDataNotNA[,i]],na.rm=TRUE)
		
		ret[1,i]<-((1+meanAssetRets)^periodsInYear-(1+meanCashRets)^periodsInYear)/sdAssetRets
	}
	
	colnames(ret)<-colnames(cumAssetReturns)
	return(ret)
}

sharpeRatios.calculate<-function(input,settings){
	rfCounponsPerYear<-settings[["shr.couponsPerYear"]]
	cashNameSuffix<-settings[["shr.cashNameSuffix"]]
	baseCc<-settings[["shr.baseCc"]]
	frequency<-settings[["shr.frequency"]]
	retSuffix<-settings[["shr.retSuffix"]]
	
	cashLevels<-as.matrix(input[["cashLevels"]])
	assetRets<-as.matrix(input[["assetRets"]])
	baseCashRets<-as.matrix(input[["baseCashRets"]])
	
	# this, typically, removes ".dtr" from the name of the final output
	colnames(assetRets)<-sub(retSuffix,"",colnames(assetRets))
	
	# we take cumulative products and such, so reverse all of them
	cashLevels<-cashLevels[nrow(cashLevels):1,]
	assetRets<-assetRets[nrow(assetRets):1,]
	# because baseCashRets has 1 column only, `[' will coerce it to a vector and we lose rownames(baseCashRets). drop=FALSE prevents that. why do we care? because we need rownames(baseCashRets) in utils.cumprodMatrixPlus1()
	baseCashRets <-baseCashRets[nrow(baseCashRets):1,,drop=FALSE]
	
	convertedAssetReturns<-erRiskDomesticEquivalent.convertReturnsMatrix(assetRets,baseCc,cashLevels,rfCounponsPerYear,cashNameSuffix)
	cumAssetRets<-utils.cumprodMatrixPlus1(convertedAssetReturns)
	cumBaseCashRets<-utils.cumprodMatrixPlus1(baseCashRets)

	if(frequency=="yearly"){
		sharpeRatios<-shr.sharpeRatios(utils.timeSeriesToMatrix(utils.getYearData(utils.matrixToTimeSeries(cumAssetRets))), utils.timeSeriesToMatrix(utils.getYearData(utils.matrixToTimeSeries(cumBaseCashRets))), 1)
	}else if(frequency=="monthly"){
		sharpeRatios<-shr.sharpeRatios(utils.timeSeriesToMatrix(utils.getEOMData(utils.matrixToTimeSeries(cumAssetRets),TRUE)), utils.timeSeriesToMatrix(utils.getEOMData(utils.matrixToTimeSeries(cumBaseCashRets),TRUE)), 12)
	}
	
	return(sharpeRatios)
}
