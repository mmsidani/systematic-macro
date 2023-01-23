bexec.constantPortfolio <- function(asset1, holding1, asset2, holding2, cashAsset, dates){
	numDates <- length(dates)
	
	ret <- data.frame(Date=dates,portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=rep(0,numDates),asset1=rep(holding1, numDates),asset2=rep(holding2, numDates))
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

bexec.constantPortfolioShort <- function(asset1, holding1, asset2, holding2, cashAsset, dates){
	numDates <- length(dates)
	
	ret <- data.frame(Date=dates,portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=rep(holding1+holding2,numDates),asset1=rep(-holding1, numDates),asset2=rep(-holding2, numDates))
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

bexec.riskParityRolling <- function(asset1, asset2, assetData, rollingWindow, cashAsset, dates){
	assetData <- assetData[,c(asset1, asset2)]
	
	assetDateIndices <- (1:nrow(assetData))[is.element(rownames(assetData), dates)]
	numDates <- length(assetDateIndices)
	
	asset1W <- rep(NA, numDates)
	asset2W <- rep(NA, numDates)
	for (i in assetDateIndices){
		if(i+rollingWindow-1 > length(assetDateIndices)) break
		sds <- apply(as.matrix(assetData[i:(i+rollingWindow-1),]), 2, sd, na.rm=T)

		asset1W[i] <- sds[2] / (sds[1] + sds[2])
		asset2W[i] <- sds[1] / (sds[1] + sds[2])
	}
	
	ret <- data.frame(Date= rownames(assetData)[assetDateIndices], portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=rep(0,numDates), asset1 = asset1W, asset2 = asset2W)
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

bexec.riskParity <- function(asset1, asset2, assetData,cashAsset, dates){
	assetData <- assetData[,c(asset1, asset2)]

	assetDateIndices <- (1:nrow(assetData))[is.element(rownames(assetData), dates)]
	numDates <- length(assetDateIndices)
	
	sds <- apply(as.matrix(assetData[rownames(assetData) <= max(dates), ]), 2, sd, na.rm=T)
	asset1W <- rep(sds[2] / (sds[1] + sds[2]), numDates)
	asset2W <- rep(sds[1] / (sds[1] + sds[2]), numDates)
	
	ret <- data.frame(Date= rownames(assetData)[assetDateIndices], portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=rep(0,numDates), asset1 = asset1W, asset2 = asset2W)
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

bexec.riskParityRollingLeveraged <- function(asset1, asset2, holding, assetData, rollingWindow, cashAsset, dates){
# leverage asset1 to have the same risk as asset2. holding is the desired holding in asset2. assetData has return data for asset1 and asset2. rollingWindow is the window over which we calculate risk. cashAsset is the name of the cash asset. dates is the array of the dates for which we want the portfolio

	assetData <- assetData[,c(asset1, asset2)]
	
	assetDateIndices <- (1:nrow(assetData))[is.element(rownames(assetData), dates)]
	numDates <- length(assetDateIndices)
	
	asset1W <- rep(NA, numDates)
	for (i in assetDateIndices){
		if(i+rollingWindow-1 > length(assetDateIndices)) break
		sds <- apply(as.matrix(assetData[i:(i+rollingWindow-1),]), 2, sd, na.rm=T)

		asset1W[i] <- sds[2]/sds[1]
	}
	
	asset1W <- holding * asset1W
	asset2W <- rep(holding, numDates)
	cash <- 1- asset1W - asset2W
	
	ret <- data.frame(Date= rownames(assetData)[assetDateIndices], portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=cash, asset1 = asset1W, asset2 = asset2W)
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

bexec.riskParityLeveraged <- function(asset1, asset2, holding, assetData,cashAsset, dates){
# leverage asset1 to have the same risk as asset2. holding is the desired holding in asset2. assetData has return data for asset1 and asset2. cashAsset is the name of the cash asset. dates is the array of the dates for which we want the portfolio

	assetData <- assetData[ , c(asset1, asset2)]

	assetDateIndices <- (1:nrow(assetData))[is.element(rownames(assetData), dates)]
	numDates <- length(assetDateIndices)
	
	sds <- apply(as.matrix(assetData[rownames(assetData) <= max(dates), ]), 2, sd, na.rm=T)
	asset1W <- rep(holding * sds[2]/sds[1] , numDates)
	asset2W <- rep(holding , numDates)
		
	cash <- 1- asset1W - asset2W
	
	ret <- data.frame(Date= rownames(assetData)[assetDateIndices], portRet=rep(0,numDates),portRisk=rep(0,numDates),cash=cash, asset1 = asset1W, asset2 = asset2W)
	names(ret) <- c("Date","portRet","portRisk",cashAsset,asset1, asset2)
	
	return(ret)
}

benchmarkExec.main <- function(asset1, asset2, cashAsset, startDate, endDate, dateFrequency, holdingAsset1InConstantPortfolio, holdingAsset2InConstantPortfolio, holdingHigherRiskAssetInLeveragedPortfolio, rollingWindow){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	
	library(timeSeries)
	
	coreSettings <- global.core()
	
	assets <- c(asset1, asset2)
	
	assetData <- utils.getDataFromFile(c(asset1,asset2),NULL,paste(coreSettings[["dataOutput"]],"prdInputs.RData",sep=""),"%Y-%m-%d")
	assetData <- assetData[rownames(assetData) >= startDate & rownames(assetData) <= endDate, ]
		
	isBond <- grepl("\\.gg\\.",assets)
	# for equities we have levels so the daily return is straightforward
	assetData[1:(nrow(assetData)-1),!isBond] <- (as.matrix(assetData[1:(nrow(assetData)-1),!isBond]) / as.matrix(assetData[2:nrow(assetData),!isBond]) - 1)
	if(sum(isBond) != 0){
		durs <- utils.getDataFromFile(assets[isBond], ".orisk", paste(coreSettings[["dataOutput"]],"forecasts.RData",sep=""))
		# NOTE: we want the duration at the beginning of the period to apply to the change in the period. hence the next statement
		durs <- durs[rownames(assetData)[2:nrow(assetData)],]
		# now get daily bond returns from yield changes and durations
		assetData[1:(nrow(assetData)-1),isBond] <- -durs *(as.matrix(assetData[1:(nrow(assetData)-1),isBond]) - as.matrix(assetData[2:nrow(assetData),isBond]))
		
	}
	assetData <- assetData[1:(nrow(assetData)-1),]
		
	if(dateFrequency =="monthly"){
		assetData <- utils.getEOMData(assetData, TRUE)
	} else if (dateFrequency == "quarterly") {
		assetData <- utils.getEOQData(assetData)
	} else if (dateFrequency =="daily"){
		# nothing to do
	} else {
		stop(paste("ERROR in benchmarkExec.R: the frequency you passed is not supported. you passed",dateFrequency))
	}
	
	dates <- rownames(assetData)
	
	write.csv(bexec.constantPortfolio(asset1,holdingAsset1InConstantPortfolio,asset2,holdingAsset2InConstantPortfolio,cashAsset,dates), paste(coreSettings[["dataOutput"]],"constantProportionBenchmark.csv",sep=""),row.names=F)
	
	write.csv(bexec.constantPortfolioShort(asset1,holdingAsset1InConstantPortfolio,asset2,holdingAsset2InConstantPortfolio,cashAsset,dates), paste(coreSettings[["dataOutput"]],"constantProportionBenchmarkShort.csv",sep=""),row.names=F)
	
	write.csv(bexec.riskParityRolling(asset1,asset2,assetData,rollingWindow,cashAsset,dates), paste(coreSettings[["dataOutput"]],"riskParityRollingBenchmark.csv",sep=""),row.names=F)
	
	write.csv(bexec.riskParity(asset1,asset2,assetData,cashAsset,dates), paste(coreSettings[["dataOutput"]],"riskParityBenchmark.csv",sep=""),row.names=F)
	
	write.csv(bexec.riskParityRollingLeveraged(asset1,asset2,holdingHigherRiskAssetInLeveragedPortfolio, assetData,rollingWindow,cashAsset,dates), paste(coreSettings[["dataOutput"]],"riskParityRollingBenchmarkLeveraged.csv",sep=""),row.names=F)
	
	write.csv(bexec.riskParityLeveraged(asset1,asset2,holdingHigherRiskAssetInLeveragedPortfolio,assetData, cashAsset,dates), paste(coreSettings[["dataOutput"]],"riskParityBenchmarkLeveraged.csv",sep=""),row.names=F)
}

# execute
# arguments are: first asset; second asset; cash name; start date; end date; frequency of portfolio to be built; weight of first asset in constant proportion portfolio; weight of second asset in constant proportion portfolio; weight of higher risk asset in leveraged RP portfolios; rolling window over which to calculate standard deviations
benchmarkExec.main("us.gg.10y","us.msciworld","us.gg.3m","1969-12-31","2012-06-22","monthly",0.4,0.6,0.6, 120)