# get the maximum of the shift in weight in any one asset, including cash, over the history of the portfolio. portfolioFile should be as the A, C, M files output by frontierBuilder.R
portfolioDistanceExec.main <- function(portfolioFile,numberOfWeeks,cashNameSuffix){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	
	coreSettings <- global.core()
	portfolioHist <- read.csv(paste(coreSettings[["optOutput"]],portfolioFile,sep=""),stringsAsFactors=FALSE)
	# ignore the "Date", "portRet", "portRisk" columns
	dates <- portfolioHist[["Date"]]
	portfolioHistNames <- names(portfolioHist)
	assets <- setdiff(portfolioHistNames,union(c("Date","portRet","portRisk"),portfolioHistNames[grepl(cashNameSuffix,portfolioHistNames)]))
	portfolioHist <- as.matrix(portfolioHist[,assets])
	
	if(coreSettings[["useDB"]]){
		assetData <- utils.getDataMatrixFromDB(assets,NULL,coreSettings[["dbSettings"]])
	}else{
		assetData <- utils.getDataMatrixFromFile(assets,NULL,paste(coreSettings[["input"]],"prdInputs.RData",sep=""),"%Y-%m-%d")
	}
	assetData<- assetData[is.element(rownames(assetData),utils.hyphensToYmd(dates)),]
	
	shiftedPortfolioHist <- portfolioHist[(1+numberOfWeeks):nrow(portfolioHist),]
	portfolioHist <- portfolioHist[1:(nrow(portfolioHist)-numberOfWeeks),]
	dates <- dates[1:(length(dates)-numberOfWeeks)]
	shiftedAssetData <- assetData[(1+numberOfWeeks):nrow(assetData),]
	assetData <- assetData[1:(nrow(assetData)-numberOfWeeks),]
	
	assetReturns <- assetData / shiftedAssetData -1 
	diffPorts <- portfolioHist - shiftedPortfolioHist
	absDiffPorts <- abs(diffPorts)
	maxes <- apply(absDiffPorts, 2, "max",na.rm=TRUE)
	maxDates <- c()
	maxMoves <- c()
	for (i in 1:length(maxes)){
		ind <- match(maxes[i],absDiffPorts[,i])
		maxMoves <- c(maxMoves, diffPorts[ind,i])
		maxDates <- c(maxDates, dates[ind])
	}
	
	means <- colMeans(diffPorts, na.rm=TRUE)
	sds <- apply(diffPorts, 2, "sd", na.rm=TRUE)
	assetSds <- apply(assetReturns, 2, "sd", na.rm=TRUE)
	
	output <- data.frame(means=means,sds=sds,max=maxMoves,max.date=maxDates,index.sd=assetSds)
	row.names(output) <- assets
	
	print(output)
	write.csv(output,paste(coreSettings[["dataOutput"]],"portfolioDistance.output.csv",sep=""),row.names=TRUE)
}

# execute
portfolioDistanceExec.main("Base-M-Monday.csv",4,".gg.3m")