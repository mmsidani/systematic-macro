bloombergData.input <- function(settings){
	
	fieldMappingsFile <- settings[["bbd.fieldMappings"]]
	ccMappingsFile <- settings[["bbd.ccMappings"]]
	pathToRSource <- settings[["pathToRSourceCode"]]
	
	fieldMappings <- read.csv( paste(pathToRSource,"inputFiles/", fieldMappingsFile, sep=""), header=T, stringsAsFactors = F)
	ccMappings <- read.csv( paste(pathToRSource,"inputFiles/", ccMappingsFile, sep=""), header=T, stringsAsFactors = F)
	
	retD <- list()
	retS <- list()
	for( i in 1:nrow(fieldMappings)){
		retD <- append(retD, list( fieldMappings[i,3]))
		retS <- append(retS, list( fieldMappings[i,2]))
	}
	names(retD) <- names(retS) <- fieldMappings[,1]
	
	retC <- list()
	for( i in 1:nrow(ccMappings)){
		retC <- append(retC, list(ccMappings[i, 2]))
	}
	names(retC) <- ccMappings[, 1]
	
	return( list(field2Divisor = retD, field2SA2Suffix = retS, exch2Cc= retC ))
}

bloombergData.output <- function(output, settings ){
	
	outputDir <- settings[["bbd.outputDir"]]
	for (i in 1:length( output)){
		iOutput <- output[[i]]
		for( n in names(iOutput)){
			temp <- iOutput[[n]]
			outputFile <- paste(outputDir, utils.today(),".bbd.",n,".csv",sep="")
			
			write.csv( temp, outputFile, quote=FALSE, row.names = FALSE )
			save( temp, file = sub("\\.csv","\\.RData", outputFile) )
		}	
	}
}

bbd.getJavaCodeOutput <- function( outputFileName, requestType, today, tableSettings ){
# read the data that was received from Bloomberg. Note: It is through this mechanism that R code gets the output from our Java code
	
	ret <- NULL
	try( ret <-  read.csv(outputFileName, header= F, stringsAsFactors = F) )
	if(!is.null(ret )){		
		if(ncol(ret) != length( tableSettings[["tblColumns"]])){
			stop(paste("ERROR in bbd.getJavaCodeOutput(): number of columns in the output from the java code does not match the expected number of columns. java code produced",ncol( ret ), "columns whereas we expected", length(tableSettings[["tblColumns"]])))
		}
		headers <- tableSettings[["tblColumns"]]
		if( tableSettings[["dateEntered"]] ){
			ret <- cbind(ret, data.frame(rep( today, nrow(ret))))
			names(ret) <- c(headers, "Date_Entered")
		} else {
			names(ret) <- headers
		}
	}
	
	return( ret )
}

bbd.reformatOutput <- function(output){
# change the data obtained from Bloomberg into a table headed by the data items names
# data.frame() on input and output
	
	dates <- sort(unique(output[,1]), decreasing=T)
	weekDaysDates <- weekdays( as.Date( dates ))
	dates <- dates[ !is.element(weekDaysDates, c( "Saturday", "Sunday" ))]
	
	secIds <- unique(output[,2])
	
	ret <- matrix(nrow=length(dates),ncol=length(secIds))
	colnames(ret) <- secIds
	rownames(ret) <- dates
	
	for (id in secIds){
		temp <- output[output[,2] == id, c(1, 3)]
		# so temp has 2 columns now
		ret[,id] <- temp[match(dates, temp[,1]), 2]
	}
	
	# convert output to data.frame()
	ret <- cbind(data.frame(Date=rownames(ret)),data.frame(ret),row.names=NULL)
	
	return(ret)
}

bbd.load <- function(javaOutput, channel, data2Key, destinationTable, override, isRevisable, dbLabel, sourceName ){
# load the data obtained from bloomberg (and still in its intermediate form) into the DB
	
	if(isRevisable){
		if( is.null( dbLabel )){
			stop("ERROR in bbd.load(): dbLabel must be specified when the table is revisable.")
		}
		
		javaOutput <- dbDataLoader.addLabels(javaOutput, dbLabel)
	}
	
	return( dbDataLoader.updateTable( channel, javaOutput, destinationTable, data2Key, override, isRevisable, sourceName  ))
}

bbd.launchJava <- function( shellCommand ){
	
	print(paste("INFO from bloombergData.launchJava(): launching java executable with command:", shellCommand))
	shellOutput <- shell(shellCommand, intern= T, wait = T, translate = T, mustWork = T)
	print(paste("INFO from bloombergData.launchJava(): java executable returned."))
	if(length(shellOutput ) != 0){
		print(shellOutput)
	}
}

bbd.getDatesAtFrequency <- function( startDate, today, frequency, dayOfWeek ){
	dates <- rev(seq(from=as.Date(startDate),to=as.Date(today),by="1 day"))
	
	if( frequency == "MONTHLY"){
		return( utils.hyphensToYmd( rev(dates[utils.getEOMDatesIndices(dates, TRUE)]) ) )
	} else if (frequency == "WEEKLY"){
		return( utils.hyphensToYmd( rev(dates[utils.getWeeklyDatesIndices(dates, dayOfWeek )]) ) )
	} else if( frequency == "DAILY"){
		return( utils.hyphensToYmd( rev(dates ) ) )
	} else {
		stop(paste("ERROR in bbd.getDatesAtFrequency():",frequency," frequency is not supported."))
	}
}

bbd.mapEquities <- function( bbEquityNames, asOfDate, field2Divisor, field2SA2Suffix, exch2Cc, header ) {	

	splits <- strsplit(bbEquityNames, " ")
	sa2EquityNames <- c()
	missingExch <- c()
	missingInds <- c()
	for( e in 1:length( bbEquityNames )){
		bbEx <- tolower( splits[[e]] [2] )
		if( is.element( bbEx, names( exch2Cc ))){
			sa2EquityNames <- c( sa2EquityNames, paste( exch2Cc[[ bbEx ]], ".", tolower(splits[[e]] [1]), sep = "" ))
		} else {
			missingExch <- union( missingExch, bbEx )
			missingInds <- c(missingInds, e )
		}
	}
	missingExch <- data.frame( missingExch )
	if(!is.null(missingExch) && nrow(missingExch) > 0 ){
		names(missingExch) <- "missingExchanges"
	}
	
	bbEquityNames <- bbEquityNames[ !is.element(1:length(bbEquityNames), missingInds)]
	uniqueSA2Names <- unique(sa2EquityNames)
	inds <- match(uniqueSA2Names, sa2EquityNames)
	bbEquityNames <- bbEquityNames[ inds ]
	sa2EquityNames <- uniqueSA2Names
	
	# Note: the earliest (as in: with smallest index) security in bbEquityNames that mapped to a given SA2 name is the one that survived after the unique()/match() manipulations we just did. so we cannot have XYZ UN surviving in bbEquityNames even though it comes after XYZ UR in asOfDate[, 2]. the order of the appearance of XYZ UN and XYZ UR will be the same in the original bbEquityNames and asOfDate[, 2]. See also the Note where we do allEquities <- union(allEquities, ...)
	asOfDate <- asOfDate[ is.element( asOfDate[, 2], bbEquityNames), ]
	asOfDate[, 2 ] <- sa2EquityNames[ match( asOfDate[, 2], bbEquityNames )]
	
	numRows <- length( bbEquityNames )
	ret <- NULL
	for( f in names(field2Divisor)  ){
		ret <- rbind(ret, data.frame( bbEquityNames, rep(f, numRows), paste(sa2EquityNames, field2SA2Suffix[[f]], sep=""), rep(field2Divisor[[f]], numRows)) )
	}
	names(ret ) <- header
	
	# add type to please bloomberg
	ret[, 1] <- paste(ret[, 1 ], "EQUITY" )
	
	return( list( equitiesRequest=ret, missingExchanges=missingExch, asOfDate=asOfDate ) )
}

bbd.execHistoricalDataRequest <- function(today, numberOfDates, outputDir, intermediateOutputFile, pathToJava, pathToBlpJar, javaClass, frequency, referenceDataFile, fillNonTrading, loadDB,channel, data2Key, destinationTable, overrideExistingData, tableSettings, request, sourceName, isIntraDay){
	if (!isIntraDay){
		endDate <- utils.getPreviousWeekDay( today )
		startDate <- utils.hyphensToYmd(utils.subtractWeekDays( endDate, numberOfDates ))
		endDate <- utils.hyphensToYmd(endDate)
	} else {
		endDate <- today
		startDate <- utils.hyphensToYmd(utils.subtractWeekDays( endDate, numberOfDates ))
		endDate <- utils.hyphensToYmd(endDate)
	}
	
	intermediateOutputFile <- paste(outputDir, today, ".", intermediateOutputFile,sep="")
	shellCommand <- paste(pathToJava, "-cp ", pathToBlpJar, javaClass, startDate, endDate, frequency, referenceDataFile, intermediateOutputFile, "HistoricalDataRequest", fillNonTrading )
	bbd.launchJava( shellCommand )
	javaOutput <- bbd.getJavaCodeOutput( intermediateOutputFile, "HistoricalDataRequest", utils.today() , tableSettings )
	dbRet <- list()
	if(!is.null(javaOutput)){
		if( loadDB){
			dbRet <- bbd.load( javaOutput, channel, data2Key, destinationTable, overrideExistingData, tableSettings[["ld.isRevisable"]], request[["bbd.dbLabel"]], sourceName  )
		}
		
		return( list(append(dbRet, list( reformatted=bbd.reformatOutput( javaOutput))) ))
	}
	
	return( NULL )
}

bbd.execReferenceDataRequest <- function(today, outputDir, intermediateOutputFile, pathToJava, pathToBlpJar, javaClass, frequency, referenceDataFile, loadDB,channel, data2Key, destinationTable, overrideExistingData, tableSettings, request, sourceName, startDateForReferenceData, equityColNumber, exch2Cc, field2Divisor, field2SA2Suffix, requestHeader){
# Important Note: logic that follows is really specific to INDEX_MWEIGHT_HIST. sub-clause if other fields are requested under "ReferenceDataRequest"
	
	# execution logic starts here
	startDate <- startDateForReferenceData
	dates <- bbd.getDatesAtFrequency( startDate, today, frequency, dayOfWeek )
	startDate <- utils.hyphensToYmd( startDate )
	allEquities <- c()
	asOfDate <- NULL
	outputs <- list()
	# dates are in chronological order
	for (i in 1:length(dates) ) {
		thisIntermediateOutputFile <- paste(outputDir, dates[i], ".", intermediateOutputFile,sep="")
		shellCommand <- paste(pathToJava, "-cp ", pathToBlpJar, javaClass, dates[i], dates[i], frequency, referenceDataFile, thisIntermediateOutputFile, "ReferenceDataRequest" )
		bbd.launchJava( shellCommand )		
		
		javaOutput <- bbd.getJavaCodeOutput( thisIntermediateOutputFile, "ReferenceDataRequest", utils.today() , tableSettings  )
		
		equitiesInfo <- NULL
		# NOTE: the next if block should only be entered when we're processing index members; the clause is not a solid one to distinguish that case but that's what we go with for now.
		if (!is.null( javaOutput )){
			if( is.element( "Member", tableSettings[["tblColumns"]]) && is.element( "Weight", tableSettings[["tblColumns"]])){
				dEquities <-  javaOutput[, equityColNumber]
				isNewEquity <- !is.element(dEquities, allEquities )
				# remember that dates are in chronological order so if this is the first we have seen a security, it must be its as-of-date
				asOfDate <- rbind( asOfDate, data.frame(AsOfDate= rep(dates[i], sum(isNewEquity)), name = dEquities[isNewEquity] ))
				# Note: the following appends new securities to the end of allEquities and the order of securities already in allSecurities will be preserved. this is important: later after we map exchanges to our country codes and get duplicate SA2 names, we know the first in the list will give us the correct as-of-date
				allEquities <- union( allEquities, dEquities )
				equitiesInfo <- bbd.mapEquities( allEquities, asOfDate, field2Divisor, field2SA2Suffix, exch2Cc, requestHeader )
			}
			
			dbRet <- list()
			if(loadDB){
				dbRet <- bbd.load( javaOutput, channel, data2Key, destinationTable, overrideExistingData, tableSettings[["ld.isRevisable"]], request[["bbd.dbLabel"]], sourceName  )
			}
			
			outputs <- append(outputs,  list(append( dbRet, equitiesInfo )  ))
		}
	}
	
	return( outputs )
}

bloombergData.calculate <- function(input, settings){
	
	javaClass <- settings[["bbd.javaClass"]]
	pathToJava <- settings[["bbd.pathToJava"]]
	pathToBlpJar <- settings[["bbd.pathToBlpJar"]]
	classpathSeparator <- settings[["bbd.classpathSeparator"]]
	
	equityColNumber <- settings[["bbd.equityColNumber"]]
	requestHeader <- settings[["bbd.requestHeader"]]
	pathToRSourceCode <- settings[["pathToRSourceCode"]]
	
	outputDir <- settings[["bbd.outputDir"]]
	dayOfWeek <- settings[["bbd.dayOfWeek"]]
	requests <- settings[["bbd.requests"]]
	dbTableSettings <- settings[["bbd.dbTableSettings"]]
	sourceName <- settings[["bbd.sourceName"]]
	
	field2Divisor <- input[["field2Divisor"]]
	field2SA2Suffix <- input[["field2SA2Suffix"]]
	exch2Cc <- input[["exch2Cc"]]
	
	today <- as.Date( utils.today() )

	pathToBlpJar <- paste(pathToRSourceCode,"../java/",classpathSeparator, pathToBlpJar,sep="")
	outputs <- list()
	for (i in 1:length(requests)){
		
		request <- requests[[i]]
		
		overrideExistingData <- request[["bbd.overrideExistingData"]]
		destinationTable <- request[["bbd.destinationTable"]]
		numberOfDates <- request[["bbd.numberOfDates"]]
		intermediateOutputFile <- request[["bbd.intermediateOutputFile"]]
		referenceDataFile <- request[["bbd.referenceDataFile"]]
		referenceDataDir <- request[["bbd.referenceDataDir"]]
		frequency <- request[["bbd.dataFrequency"]]
		fillNonTrading <- request[["bbd.fillNonTrading"]]
		loadDB <- request[["bbd.loadDB"]]
		requestType <- request[["bbd.requestType"]]
		startDateForReferenceData <- request[["bbd.referenceDataStartDate"]]
		tableSettings <- dbTableSettings[[destinationTable]]
		
		channel <- dbDataLoader.getConnection( tableSettings[["ld.uid"]], tableSettings[["ld.dsnName"]])
		data2Key <- dbDataLoader.getTable( channel, tableSettings[["ld.rateDescrTable"]] )
		
		if(is.null( referenceDataDir) ){
			referenceDataFile <- paste(pathToRSourceCode,"inputFiles/", referenceDataFile, sep="" )
		} else {
			referenceDataFile <- paste(referenceDataDir, referenceDataFile, sep="" )
		}
		
		isIntraDay <- FALSE
		if ( requestType == "IntradayDataRequest"){
			isIntraDay <- TRUE
			requestType <- "HistoricalDataRequest"
			if(numberOfDates!=1){
				print(paste("WARNING in bloombergData.calculate(): bbd.numberOfDates must be 1 for IntradayDataRequest. you passed:", numberOfDates,". resetting to 1 and proceeding."))
				numberOfDates <- 1
			}
			if(fillNonTrading){
				print(paste("WARNING in bloombergData.calculate(): bbd.fillNonTrading must be FALSE for IntradayDataRequest. you passed:", fillNonTrading,". resetting to FALSE and proceeding."))
				fillNonTrading <- F
			}
		}
		
		if( requestType == "HistoricalDataRequest" ){
			histDataRet <- bbd.execHistoricalDataRequest(today, numberOfDates, outputDir, intermediateOutputFile, pathToJava, pathToBlpJar, javaClass, frequency, referenceDataFile, fillNonTrading, loadDB,channel, data2Key, destinationTable, overrideExistingData, tableSettings, request, sourceName, isIntraDay)
			if(!is.null(histDataRet)){
				outputs <- append(outputs, histDataRet )
			}
		} else if( requestType == "ReferenceDataRequest" ) {
			# Important Note: logic that follows is really specific to INDEX_MWEIGHT_HIST. sub-clause if other fields are requested under "ReferenceDataRequest"
			outputs <- append(outputs,  bbd.execReferenceDataRequest(today, outputDir, intermediateOutputFile, pathToJava, pathToBlpJar, javaClass, frequency, referenceDataFile, loadDB,channel, data2Key, destinationTable, overrideExistingData, tableSettings, request, sourceName, startDateForReferenceData, equityColNumber, exch2Cc, field2Divisor, field2SA2Suffix, requestHeader))
		} else {		
			stop(paste("ERROR in bloombergData.calculate():", requestType,"is not supported."))
		}
		
		dbDataLoader.close( channel )
	}
	
	return( outputs )
}
