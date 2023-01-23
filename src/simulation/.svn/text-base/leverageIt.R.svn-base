leverageIt.input <- function( settings ){
	
	inputDir <- settings[["lit.inputDir"]]
	inputDirER <- settings[["lit.inputDirER"]]
	portSimHistFile <- settings[["lit.portfolioSimFile"]]
	useDB <- settings[["lit.useDB"]]
	
	baseCashName <- settings[["lit.baseCashName"]]
	dtrSuffix <- settings[["lit.dtrSuffix"]]
	dtrCrFile <- settings[["lit.dtrCrFile"]]
	dateFormat <- settings[["lit.dateFormat"]]
	dbSettings <- settings[["dbSettings"]]
	
	isCsv <- grepl("\\.csv$", portSimHistFile)
	reallocFile <- sub("\\.sim",".realloc",portSimHistFile)
	portSimHistFile <- paste(inputDir, portSimHistFile, sep="")
	reallocFile <- paste(inputDir, reallocFile, sep="")
	if( isCsv ){
		portSimHist <- read.csv(portSimHistFile)
		portReallocHist <-  read.csv(reallocFile)
	}else {
		portSimHist <- utils.load(portSimHistFile)
		portReallocHist <-  utils.load(reallocFile)
	}

	if(useDB ){
		baseCashDtrs <- utils.getDataFromDB( baseCashName, dtrSuffix, dbSettings )
	} else {
		baseCashDtrs <- utils.getDataFromFile( baseCashName, dtrSuffix, paste(inputDirER, dtrCrFile, sep = ""), dateFormat )
	}
	
	return( list( baseCashDtrs=baseCashDtrs, portSimHist=portSimHist,portReallocHist=portReallocHist ))
}

leverageIt.output <- function(output, settings){
	
	outputDir <- settings[["lit.outputDir"]]
	portSimHistFile <- settings[["lit.portfolioSimFile"]]
	leverage <- settings[["lit.leverage"]]
	
	portSimHist <- output[["portSimHist"]]
	portReallocHist <- output[["portReallocHist"]]
	
	isCsv <- grepl("\\.csv$", portSimHistFile)
	rootFileName <- ifelse(isCsv, sub("\\.sim\\.csv","",portSimHistFile), sub("\\.sim\\.RData","",portSimHistFile) )
	outputFile <- paste(outputDir,rootFileName,".leveraged.",leverage,sep="")
	write.csv(portSimHist, paste(outputFile,".sim.csv",sep=""), row.names = FALSE )
	save(portSimHist, file=paste(outputFile,".sim.RData",sep=""))
	write.csv(portReallocHist, paste(outputFile,".realloc.csv",sep=""), row.names = FALSE )
	save(portReallocHist, file=paste(outputFile,".realloc.RData",sep=""))
}

leverageIt.calculate <- function(input, settings ){
# TODO handle reallocations properly
	
	leverage <- settings[["lit.leverage"]]
	spread <- settings[["lit.financingSpread"]]
	baseCashName <- settings[["lit.baseCashName"]]
	busDaysPerYear <- settings[["lit.busDaysPerYear"]]
	
	baseCashDtrs <- input[["baseCashDtrs"]]
	portSimHistory <- input[["portSimHist"]]
	portReallocHist <- input[["portReallocHist"]]
	
	financing <- 1 - leverage 
	
	portSimHistory[, setdiff(names(portSimHistory), c("Date", "realloc"))] <- leverage * portSimHistory[, setdiff(names(portSimHistory), c("Date", "realloc"))]
	
	dates <- as.Date(portSimHistory[["Date"]])
	# dates are in reverse chronological order
	datesDiff <- as.numeric(dates[1:(length(dates)-1)] - dates[2:length(dates)] )
	portSimHistory[1:(nrow(portSimHistory) - 1),"ret"] <- portSimHistory[1:(nrow(portSimHistory) - 1),"ret"] + financing * ( baseCashDtrs[is.element(rownames(baseCashDtrs), portSimHistory[["Date"]][1:(nrow(portSimHistory) - 1)]), 1] + spread * datesDiff/busDaysPerYear )
	portSimHistory[, baseCashName ] <- portSimHistory[, baseCashName ] + financing
	
	print("INFO from leverageItExec.calculate(): The output reallocation stats are exactly those of the original (unleveraged) portfolio. We have not implemented logic to calculate the right realloc stats for the leveraged portfolio yet.")
	
	return( list(portSimHist=portSimHistory, portReallocHist=portReallocHist) )
}
