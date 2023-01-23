allocationToContractsExec.main <- function(assetMix,portfolioFileName,futuresNotionalsFile,forecastsFile,baseCc,cashNameSuffix,budget,budgetTolerance,allocationTolerance){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../portfolioManagement/futuresContracts.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	
	library(limSolve)

	coreSettings<-global.core()
	
	constraintsFiles.portfolioNames <- NULL
	source(paste(pathToMe,"/../inputFiles/constraintsFiles.R",sep=""))
	
	forecasts<-utils.load(paste(coreSettings[["dataOutput"]],forecastsFile,sep=""))
	futuresNotionals <- t(read.csv(paste(coreSettings[["input"]],futuresNotionalsFile,sep=""),header=F,stringsAsFactors=FALSE))
	futuresNames <- futuresNotionals[1,]
	futuresNotionals <- futuresNotionals[2,]
	names(futuresNotionals) <- futuresNames

	portfolio.settings <- NULL
	source(paste(pathToMe,"/../inputFiles/constraints-",assetMix,".r",sep=""))
	portfolio.inList <- portfolio.settings()
	
	allocation<-utils.load(paste(coreSettings[["optOutput"]],portfolioFileName,sep=""))[1,]
	thisDate<-allocation[1,"Date"]
	# remove the "Date", "portRet", "portRisk" columns
	allocation<-allocation[1,4:ncol(allocation)]
	assetNames<-names(allocation)
	cashAssets<-assetNames[grep(cashNameSuffix,assetNames)]
	cashAllocation<-sum(as.numeric(allocation[,cashAssets]))
	nonCashAssets<-setdiff(assetNames,cashAssets)
	allocation<-allocation[,nonCashAssets]		
	allocation[,allocation * budget * (1-cashAllocation) <= 1000 ] <- 0
	allocation <- allocation[,allocation !=0]
	nonCashAssets<-names(allocation)
		
	print(paste("INFO from allocationToContractsExec.main(): calculating for",thisDate,". allocation is:",paste(paste(nonCashAssets,round(as.numeric(allocation),digits=2),sep=","),collapse=","),"cash allocation is:",round(cashAllocation,digits=2)))

	expectedRets<-forecasts[forecasts$Date==thisDate,utils.generateIdsFromMapping(nonCashAssets,portfolio.inList[["erMapping"]])]
	colnames(expectedRets) <- nonCashAssets
	contractAllocation1<-futuresContracts.optimizeReturn(nonCashAssets,expectedRets,allocation, budget,futuresNotionals)
	print(paste("INFO from allocationToContractsExec.main(): total cost of futures contracts:",contractAllocation1[["cost"]],". contract allocation from futuresContracts.optimizeReturn() is:"))
	print(contractAllocation1[["contracts"]])
	contractAllocation2<-futuresContracts.optimizeBudget(nonCashAssets,allocation,budget,futuresNotionals)
	print(paste("INFO from allocationToContractsExec.main(): total cost of futures contracts:",contractAllocation2[["cost"]],". contract allocation from futuresContracts.optimizeReturn() is:"))
	print(contractAllocation2[["contracts"]])
	
	return(list(returnOptimized=contractAllocation1,budgetOptimized=contractAllocation2))
}

# execute
allocationToContractsExec.main("NonUSFutures2-noNL-JTO-104-Monday-vol","NonUSFutures2-noNL-JTO-104-Monday-vol-M-Monday.RData","futuresSpecs.csv","forecasts.RData","us",".gg.3m",1000000.,0.1,0.05)