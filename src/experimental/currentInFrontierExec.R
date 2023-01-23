currentInFrontier.main <- function(frontiersFile, portfolioFile, portfolioConstraintsFile, simFile, reallocFile, covariancesFile, forecastsFile, numFrontierPoints, cashNameSuffix){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	source(paste(pathToMe,"/../simulation/portfolioSimulation.R",sep=""))
	source(paste(pathToMe,"/../utils/erRiskDomesticEquivalent.R",sep=""))
	
	library(timeSeries)
	
	portfolio.settings<-NULL
	source(paste(pathToMe,"/../inputFiles/",portfolioConstraintsFile,sep=""))
	portfolioSettings <- portfolio.settings()
	
	coreSettings <- global.core()
	
	frontiers <- data.frame(utils.load(paste(coreSettings[["optOutput"]],frontiersFile,sep="")))
	portfolioHist <- data.frame(utils.load(paste(coreSettings[["optOutput"]],portfolioFile,sep="")))
	simulation <- data.frame(utils.load(paste(coreSettings[["simOutput"]],simFile,sep="")))
	reallocation <- data.frame(utils.load(paste(coreSettings[["simOutput"]],reallocFile,sep="")))
	covariances <- utils.load(paste(coreSettings[["dataOutput"]], covariancesFile, sep=""))
	
	simSettings<-global.portfolioSimulationExecSettings()
	portfolioName <- sub("constraints-","",sub("\\.r","",portfolioConstraintsFile))
	simSettings[["portfolioName"]] <- portfolioName
	simSettings[["pathToRSourceCode"]]<-paste(pathToMe,"/../",sep="")
	forecasts <- portfolioSimulation.input(simSettings)[["forecasts"]]
	forecasts[!is.finite(as.matrix(forecasts))] <- 0
	rownames(forecasts) <- utils.ymdToHyphens(rownames(forecasts))

	assets<-setdiff(names(portfolioHist),c("Date","portRet","portRisk"))
	minCovarianceDate <- utils.ymdToHyphens(min(names(covariances)))
				
	calculateRisk <- function(port, portDate){
		covMat <- as.matrix(covariances[[portDate]])
		covMat[!is.finite(covMat)] <- 0
		
		return( as.numeric(sqrt(matrix(port,nrow=1) %*% covMat %*% matrix(port, ncol=1))))
	}
	
	frontierDates<- frontiers[["Date"]]
	frontierInds <- match(unique(frontierDates),frontierDates)
	
	notKept <- function(){
		reallocationInds <- (1:nrow(simulation))[simulation[["realloc"]] == 1] 
		reallocationDiffs <- reallocation[is.element(reallocation[["Date"]], simulation[["Date"]][reallocationInds]),]
		portsNotKept <- simulation[reallocationInds, ]
		portsNotKept[,4:ncol(portsNotKept)] <- portsNotKept[,4:ncol(portsNotKept)] - reallocationDiffs[,3:ncol(reallocationDiffs)]
		targetsChosen <- portfolioHist[is.element(portfolioHist[["Date"]], simulation[["Date"]][reallocationInds]), ]

		numCols<-ncol(portsNotKept)
		notKeptDates <- portsNotKept[["Date"]]
		notKeptDates <- notKeptDates[notKeptDates >= minCovarianceDate]
		notKeptForecasts <- forecasts[is.element(rownames(forecasts), notKeptDates), ]
		frontierChosenInds <- frontierInds[is.element(frontierDates[frontierInds], notKeptDates)]
		numDates<-length(notKeptDates)
		ret <- matrix(nrow=4*numDates,ncol=numCols+2)
		notKeptDates <- as.character(utils.hyphensToYmd(notKeptDates))
		portsNonCash <- portsNotKept[,!grepl(cashNameSuffix,names(portsNotKept))]
		for (i in 1:numDates){
			targetRisk <- targetsChosen[["portRisk"]][i]
			portRisk <- calculateRisk(as.numeric(portsNonCash[i, 4:ncol(portsNonCash)]),notKeptDates[i])
			portRet <- sum(portsNotKept[i,4:numCols]*notKeptForecasts[i,])
			
			frontier <- frontiers[frontierChosenInds[i]:(frontierChosenInds[i]+numFrontierPoints-1), ]
			enclose <- (portRisk - frontier[["portRisk"]][1:(numFrontierPoints-1)]) * (portRisk - frontier[["portRisk"]][2:numFrontierPoints])
			lower <- (1:(numFrontierPoints-1))[enclose <0]
			closestEfficientPort1 <- frontier[lower,]
			closestEfficientPort2 <- frontier[lower+1,]
			closestTargetPort <- frontier[order(abs(frontier[["portRisk"]]-targetRisk))[1],]
			ret[4*(i-1)+1,]<-c(100,notKeptDates[i],as.numeric(closestEfficientPort1[2:length(closestEfficientPort1)]))
			ret[4*(i-1)+2,]<-c(200,notKeptDates[i],NA,portRet,portRisk,as.numeric(portsNotKept[i,4:numCols]))
			ret[4*(i-1)+3,]<-c(300,notKeptDates[i],as.numeric(closestEfficientPort2[2:length(closestEfficientPort2)]))
			ret[4*(i-1)+4,]<-c(400,notKeptDates[i],as.numeric(closestTargetPort[2:length(closestTargetPort)]))
		}
		
		colnames(ret) <- c("which","Date","point on eff","portRet","portRisk",names(portsNotKept)[4:numCols])
		
		return(data.frame(ret))
	}
	
	kept<- function(){
		noRebalancingDates <- simulation[["Date"]][simulation[["realloc"]] != 1 & is.element(simulation[["Date"]], portfolioHist[["Date"]])]
		targetsNotChosen <- portfolioHist[is.element(portfolioHist[["Date"]],noRebalancingDates), ]
		portsKept <- simulation[is.element(simulation[["Date"]], noRebalancingDates), ]

		keptDates <- portsKept[["Date"]]
		keptDates <- keptDates[keptDates >= minCovarianceDate]
		keptForecasts <- forecasts[is.element(rownames(forecasts), keptDates),]
		frontierNotChosenInds <- frontierInds[is.element(frontierDates[frontierInds], keptDates)]
		numDates <- length(keptDates)
		numCols <- ncol(portsKept)
		ret <- matrix(nrow=4*numDates,ncol=numCols+2)
		keptDates <- as.character(utils.hyphensToYmd(keptDates))
		portsNonCash <- portsKept[,!grepl(cashNameSuffix,names(portsKept))]
		for (i in 1:numDates){
			targetRisk <- targetsNotChosen[["portRisk"]][i]
			portRisk <- calculateRisk(as.numeric(portsNonCash[i, 4:ncol(portsNonCash)]),keptDates[i])
			portRet <- sum(portsKept[i,4:numCols]*keptForecasts[i,])
			
			frontier <- frontiers[frontierNotChosenInds[i]:(frontierNotChosenInds[i]+numFrontierPoints-1), ]
			enclose <- (portRisk - frontier[["portRisk"]][1:(numFrontierPoints-1)]) * (portRisk - frontier[["portRisk"]][2:numFrontierPoints])
			lower <- (1:(numFrontierPoints-1))[enclose <0]
			closestEfficientPort1 <- frontier[lower,]
			closestEfficientPort2 <- frontier[lower+1,]
			closestTargetPort <- frontier[order(abs(frontier[["portRisk"]]-targetRisk))[1],]

			ret[4*(i-1)+1,]<-c(100,keptDates[i],as.numeric(closestEfficientPort1[2:length(closestEfficientPort1)]))
			ret[4*(i-1)+2,]<-c(200,keptDates[i],NA,portRet,portRisk,as.numeric(portsKept[i,4:numCols]))
			ret[4*(i-1)+3,]<-c(300,keptDates[i],as.numeric(closestEfficientPort2[2:length(closestEfficientPort2)]))
			ret[4*(i-1)+4,]<-c(400,keptDates[i],as.numeric(closestTargetPort[2:length(closestTargetPort)]))
		}
		
		colnames(ret) <- c("which","Date","point on eff","portRet","portRisk",names(portsKept)[4:numCols])

		return(data.frame(ret))
	}

	print("now doing kept()")	
	ret <- kept()
	ret[ret[["which"]]==100,"which"] <- "lower"
	ret[ret[["which"]]==200,"which"] <- "current"
	ret[ret[["which"]]==300,"which"] <- "upper"
	ret[ret[["which"]]==400,"which"] <- "target"
	write.csv(ret,paste(coreSettings[["dataOutput"]],portfolioName,".norebalancing.csv",sep=""),row.names=FALSE)
	print("now doing notKept()")
	ret <- notKept()
	ret[ret[["which"]]==100,"which"] <- "lower"
	ret[ret[["which"]]==200,"which"] <- "current"
	ret[ret[["which"]]==300,"which"] <- "upper"
	ret[ret[["which"]]==400,"which"] <- "target"
	write.csv(ret,paste(coreSettings[["dataOutput"]],portfolioName,".rebalancing.csv",sep=""),row.names=FALSE)
}

# execute
currentInFrontier.main("Extended-Monday-Opt-FULL.RData","Extended-M-Monday.RData","constraints-Extended.r","Extended-M-Monday.sim.RData","Extended-M-Monday.realloc.RData","Extended.cov.RData","forecasts.RData",31,".gg.3m")