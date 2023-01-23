createLivePX.input <- function( settings ){
	constraintsFile <- settings[["clpx.constraints"]]
	pathToRSourceCode <- settings[["pathToRSourceCode"]]
	
	constraintsFiles.portfolioNames <- NULL
	source(paste(pathToRSourceCode,"inputFiles/",constraintsFile,sep=""))
	
	return( list(portfolios = constraintsFiles.portfolioNames() ))
}

createLivePX.output <- function( output, settings ){
	outputDir <- settings[["clpx.outputDir"]]	
	
	for(portfolio in names(output)){
		write.table(t(output[[portfolio]]),paste(outputDir,"LIVE-PX-",portfolio,".csv",sep=""),col.names=FALSE,sep=",")
	}
}

createLivePX.calculate <- function( input, settings ){

	seedList <- settings[["clpx.seedSource"]]
	growthBenchmarkList <- settings[["clpx.growthBenchmark"]]
	suffixes <- settings[["clpx.listOfSuffixes"]]
	classIdentifiers <- settings[["clpx.assetIdentifiers"]]
	cashNameSuffix <- settings[["clpx.cashNameSuffix"]]
	pathToRSourceCode <- settings[["pathToRSourceCode"]]
	
	portfolios <- input[["portfolios"]]
	
	outputs <- list()
	for(portfolio in portfolios){
		portfolio.settings <- NULL
		source(paste(pathToRSourceCode,"inputFiles/constraints-",portfolio,".r",sep=""))
		portSettings<-portfolio.settings()
		
		orgAssets <- names(utils.getConstraints(paste(pathToRSourceCode,"inputFiles/",sep=""),paste("constraints-",portSettings[["constraintsNAME"]],".csv",sep="")))
		assets <- utils.dependencies(orgAssets, list(seedList, growthBenchmarkList))
		assetClasses <- utils.assetsByClass(assets, classIdentifiers)
		cash <- paste(unique(substr(assets,1,2)),cashNameSuffix,sep="")
		headers <- c("Date",utils.generateCrossIds(assetClasses[["equity"]],suffixes[["equity"]]),utils.generateCrossIds(assetClasses[["bond"]],suffixes[["bond"]]),cash, utils.generateCrossIds(c(orgAssets,cash),".pos"))
		output <- matrix("",ncol=length(headers))
		colnames(output) <- headers
		
		outputs <- append(outputs, list(newItem=output))
		names(outputs)[length(outputs)] <- portfolio
	}

	return( outputs )
}