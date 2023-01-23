clpf.specialRequestTable <- function( specialTreatmentTickers, suffix){
# no qualms about hard-coding here. that's its purpose
	
	for( i in 1:length(specialTreatmentTickers) ){
		ticker <- specialTreatmentTickers[ i ]
		if(ticker == "MXSG INDEX" && suffix == ".earnings"){
			return( data.frame( bbTicker=c("MXSG INDEX"), bbField=c("PE_RATIO"), SA2Code= c("SIMSCI.pe_ratio"), divisors=c(1,1)) )
		} else{
			stop( paste( "ERROR in clpf.specialRequestTable(): there's no logic for the special treatment of", ticker) )
		}
	}
}

clpf.specialDataTreatment <- function( bbData, sa2Names, marketDataDBSettings){
	
	sgs <- c("sg.mscifree", "sg.2mscifree")
	
	if(any( is.element( sgs , sa2Names) ) ){
		inds <- match(sgs, sa2Names)
		simsciYestClose <- as.numeric( utils.getDataFromDB( sgs[ is.finite( inds) ], NULL, marketDataDBSettings)[1, ] )
		
		ind <- match("SIMSCI.pe_ratio", bbData[, 2])
		if( !is.finite( ind)){
			stop(paste("ERROR in clpf.specialDataTreatment(): 'SIMSCI.pe_ratio' is missing from the returned bb data"))
		}
		
		simsciData <- bbData[ ind, ]
		bbData <- bbData[ setdiff(1:nrow(bbData), ind), ]
		names(bbData ) <- c("Date", "sa2Name", "value")
		return( rbind( bbData, data.frame( Date=simsciData[, 1], sa2Name= paste( sgs[ is.finite( inds)], ".earnings", sep=""), value = simsciYestClose / simsciData[ 1, 3 ] )) )
	} else {
		stop(paste("ERROR in clpf.specialDataTreatment(): not sure why clpf.specialDataTreatment() was called. no special logic available for what was passed."))
	}
}

clpf.createCashRequestTable <- function( requestIndexesTable, countryCodes, cashNameSuffix ){
	cashNames <- paste(countryCodes, cashNameSuffix, sep="" )
	inds <- match( cashNames, requestIndexesTable[, 3])
	
	if( any(!is.finite(inds))){
		stop(paste("ERROR in clpf.createCashRequestTable(): some cash names could not be mapped to BB tickers:", paste( cashNames[!is.finite(inds)], collapse=", ")))
	}
	
	return( requestIndexesTable[ inds, ])
}

clpf.createRequestTable <- function( univ2Ticker, instrumentBBData, currencyBBField, specialTreatmentItems, cashNameSuffix, requestIndexesTable, bbFieldOtherHoldings, bbDivisorOtherHoldings, expectedColumnNames, miniContracts ){

	requestTable <- NULL
	securityTypes <- univ2Ticker[["priceType"]]
	for( securityType in unique(securityTypes) ){
		isTypeRow <-  securityTypes == securityType
		
		if(!is.element( securityType, names(instrumentBBData))){
			stop(paste("ERROR in clpf.createRequestTable(): There's no spec for the fields needed for", securityType))
		}
		
		suffixes <- instrumentBBData[[securityType]][["suffixes"]]
		bbFields <- instrumentBBData[[securityType]][["fields"]]
		divisors <- instrumentBBData[[securityType]][["divisors"]]
		securityColumn <- instrumentBBData[[securityType]][["securityColumn"]]
		
		if(any( !is.element( securityColumn, expectedColumnNames))){
			stop(paste("ERROR in clpf.createRequestTable(): these strings in 'securityColumn' refer to columns in univ2Ticker that don't exist:", paste(securityColumn[!is.element( securityColumn, expectedColumnNames)], collapse=", ")))
		}
		
		if(length(suffixes) != length(bbFields) || length(suffixes) != length(divisors) || length(suffixes) != length(securityColumn) ){
			stop(paste("ERROR in clpf.createRequestTable(): there's a mismatch between the items in the spec specified for", securityType))
		}
		
		allSecs <- univ2Ticker[ isTypeRow, "SA2Name" ]
		for (i in 1:length(suffixes)){
			newSecs <- paste(allSecs, suffixes[i], sep= "" )
			bbTickers <- univ2Ticker[isTypeRow, securityColumn[i] ]
			isSpecial <- is.element( bbTickers, specialTreatmentItems[["name"]][ specialTreatmentItems[["suffix"]] == suffixes[i] ] )
			if( any( isSpecial )){
				specialTickers <- bbTickers[ isSpecial ]
				requestTable <- rbind( requestTable, clpf.specialRequestTable( specialTickers, suffixes[i]) )
			}
			requestTable <- rbind( requestTable, data.frame(bbTicker = bbTickers[ !isSpecial ], bbField=bbFields[i], SA2Code=newSecs[ !isSpecial ], divisors=divisors[i]))
		}
	}
	
	# NOTE: logic for other holdings assumes that they are futures contracts
	otherHoldings <- univ2Ticker[["otherHolding"]]
	if( any( !is.na(otherHoldings)) ){
		otherHoldings <- otherHoldings[ !is.na(otherHoldings) ]
		# NOTE: SA2Code here is NOT a SA2Code. the instruments in univ2Ticker are already mapped to SA2 codes so these we map to themselves
		requestTable <- rbind( requestTable, data.frame(bbTicker=otherHoldings, bbField=bbFieldOtherHoldings, SA2Code=otherHoldings, divisors=bbDivisorOtherHoldings) )
	}
	
	ccyPairs <- paste(unique(univ2Ticker[["currency"]]),"USD", sep="")
	ccyPairs <- ccyPairs[ ccyPairs != "USDUSD" ]
	fxRates <- paste(ccyPairs," CURNCY", sep="")
	requestTable <- rbind(requestTable, data.frame(bbTicker=fxRates, bbField=currencyBBField, SA2Code=ccyPairs, divisors=1))
	
	cashRequest <- clpf.createCashRequestTable( requestIndexesTable, substr( univ2Ticker[["SA2Name"]], 1, 2), cashNameSuffix  )
	names(cashRequest) <- c("bbTicker", "bbField", "SA2Code", "divisors")
	requestTable <- rbind(requestTable,  cashRequest)
	
	names(requestTable)[1] <- paste("#", names(requestTable)[1])
	
	return( requestTable )
}

clpf.applyBasisAdjustments <- function( bbData, univ2Ticker, futureContractBasis, priceSuffix ){
	
	isEquityIndexFuture <- univ2Ticker[, "priceType"] == "equityIndexFuture"
	sa2Names <- univ2Ticker[ isEquityIndexFuture, "SA2Name" ]
	bbTickers <- univ2Ticker[ isEquityIndexFuture, "price"]
	futurePrefixes <- substr( bbTickers, 1, nchar(bbTickers)-8)
	basisAdjustments <- rep( 0, length(sa2Names))
	for( p in 1:length(futurePrefixes) ){
		basisAdjustments[p] <- futureContractBasis[[futurePrefixes[p]]]
	}
	
	inds <- match( paste(sa2Names, priceSuffix, sep=""), bbData[, 2])
	bbData[ inds, 3] <- bbData[ inds, 3] / basisAdjustments

	return( bbData )
}

createLivePriceFilesExec.main <- function( ){
	
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste( pathToMe, "/../", sep="" )
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	settings <- global.createLivePriceFilesExecSettings()
	
	inputDir <- settings[["clpf.inputDir"]]
	outputDir <- settings[["clpf.outputDir"]]
	universeToSecuritiesMappingFile <- settings[["clpf.universeToSecuritiesMappingFile"]]
	instrumentBBData <-settings[["clpf.instrumentBBData"]]
	currencyBBField <-settings[["clpf.currencyBBField"]]
	pathToJava <-settings[["clpf.pathToJava"]]
	pathToBlpJar <-settings[["clpf.pathToBlpJar"]]
	javaClass <-settings[["clpf.javaClass"]]
	fillNonTrading <-settings[["clpf.fillNonTrading"]]
	classpathSeparator <- settings[["clpf.classpathSeparator"]]
	specialTreatmentItems <- settings[["clpf.specialTreatmentItems"]]
	futureContractBasis <- settings[["clpf.futureContractBasis"]]
	cashNameSuffix <- settings[["clpf.cashNameSuffix"]]
	requestIndexesFile <- settings[["clpf.requestIndexesFile"]]
	marketDataDBSettings <- settings[["clpf.marketDataDBSettings"]]
	priceSuffix <- settings[["clpf.priceSuffix"]]
	livePricesFileNamePrefix <- settings[["clpf.livePricesFileNamePrefix"]]
	bbOtherHoldings <- settings[["clpf.bbOtherHoldings"]]
	expectedColumnNames <- settings[["clpf.expectedColumnNames"]]
	minis <- settings[["clpf.minis"]]
	
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/pmUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/createLivePX.R",sep=""))
	source(paste(pathToRSourceCode,"marketData/bbLivePrices.R",sep=""))
	
	library( RODBC )
	library( timeSeries )
		
	pathToClasses <- paste(pathToRSourceCode,"../java/",classpathSeparator, pathToBlpJar,sep="")
	requestIndexesTable <- read.csv( paste(pathToRSourceCode, "inputFiles/",requestIndexesFile, sep=""), stringsAsFactors=F)
	
	today <- utils.today()
	
	todayObj = as.Date( today )
	if ( weekdays( todayObj ) == "Saturday"   ) {
		
		today = as.character( todayObj - 1 )
		
	} else if ( weekdays( todayObj ) == "Sunday" ) {
		
		today = as.character( todayObj - 2 )
	}
	
	univ2Ticker <- read.csv( paste(inputDir,universeToSecuritiesMappingFile,sep=""), stringsAsFactors=FALSE )
	if( any( !is.element( expectedColumnNames, names(univ2Ticker))) ) {
		stop(paste("ERROR in createLivePriceFilesExec.main(): the universe to securities mapping file is in the wrong format. it must have columns: ",paste( expectedColumnNames, collapse=", ")))
	}
	if(length(bbOtherHoldings[["field"]])!= length( bbOtherHoldings[["divisor"]]) || length(bbOtherHoldings[["field"]])!=1 ){
		stop(paste("ERROR in createLivePriceFilesExec.main(): the code currently allows only ONE field and ONE divisor to be specified for other holdings.") )
	}
	
	bbDataRequestTable <- clpf.createRequestTable( univ2Ticker, instrumentBBData, currencyBBField, specialTreatmentItems, cashNameSuffix, requestIndexesTable, bbOtherHoldings[["field"]], bbOtherHoldings[["divisor"]], expectedColumnNames, minis )
	bbLiveData <- bbLivePrices.retrieve( today, outputDir, bbDataRequestTable, pathToJava, pathToClasses, javaClass, fillNonTrading )
	adjustedLiveData <- clpf.applyBasisAdjustments( bbLiveData, univ2Ticker, futureContractBasis, priceSuffix)
	adjustedLiveData <- clpf.specialDataTreatment( adjustedLiveData, univ2Ticker[["SA2Name"]], marketDataDBSettings )
	
	write.csv( adjustedLiveData, paste(outputDir, livePricesFileNamePrefix, today, ".csv", sep=""), row.names = F, quote=F )
	
	print(paste(sys.frame(1)$ofile,"done."))	
}

# execute
createLivePriceFilesExec.main()