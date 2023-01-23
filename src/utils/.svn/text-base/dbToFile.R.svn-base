dbToFile.input <- function( settings ){
	# nothing to do for now
	
	return(NULL)
}

dbToFile.output <- function( output, settings) {
	outputDir<-settings[["dbf.outputDir"]]
	
	today <- utils.today()
	fileNameRoot <- paste(outputDir, "DBF.", sep="")
	for( nm in names(output)){
		tempDF <- output[[nm]]
		tempFileName <- paste(fileNameRoot, nm, sep="")
		write.csv(tempDF, paste(tempFileName, ".csv", sep=""), row.names=F)
		save(tempDF, file=paste(tempFileName, ".RData", sep=""))
	}
}

dbf.getMktData <- function(assets, otherIds, classIdentifiers, suffixes, dbSettings){
	
	# identify the classes (i.e., "equity", "bond", etc.) of the assets for which we're getting data
	assetsByClass<-utils.assetsByClass(assets,classIdentifiers)
	idsToGet<-c()
	# form all the id's of raw data that we need anywhere in our code
	for(cl in names(assetsByClass)){
		idsToGet<-c(idsToGet,utils.generateCrossIds(assetsByClass[[cl]],suffixes[[cl]]))
	}
	
	# now add the ids
	idsToGet <- c(idsToGet, otherIds)
	
	# now get the data from the DB
	assetData<-utils.getDataFromDB(idsToGet,NULL,dbSettings)
	
	# convert it to a data.frame() and return
	return( list(marketData=cbind(data.frame(Date=row.names(assetData)),data.frame(assetData)) ))
}

dbf.getPositionsData <- function(positionsDBSettings){
	
	channel <- dbDataLoader.getConnection(positionsDBSettings[["uid"]], positionsDBSettings[["dsnName"]] )
	positionsDF <- dbDataLoader.getTable( channel, positionsDBSettings[["positionsTableName"]] )
	dbDataLoader.close( channel )
	
	return(list(positions=positionsDF))
}

dbf.getMacroData <- function(assets, gdpSuffix, gniSuffix, inflationSuffix, useGNI, macroDBSettings){
	ccs <- unique(substr(assets, 1, 2) )
	inflationIds <- paste(ccs, inflationSuffix, sep="")
	
	useGDP <- setdiff(ccs,useGNI)
	gdpIds <- gniIds <- NULL
	if(length(useGDP) != 0 ){
		gdpIds <- paste(useGDP, gdpSuffix, sep="")
	}
	if(length(useGNI) != 0){
		gniIds <- paste(useGNI, gniSuffix, sep="")
	}
	
	inflationData <- utils.getDataFromDB(inflationIds, NULL, macroDBSettings)
	macroData <- utils.getDataFromDB(c( gdpIds, gniIds), NULL, macroDBSettings)
	ret <- list( inflation =cbind(data.frame(Date=rownames(inflationData)), data.frame(inflationData[, inflationIds])) )
	if(length(useGDP) != 0 ){
		ret <- append(ret, list(gdp=cbind(data.frame(Date=rownames(macroData)), data.frame(macroData[, gdpIds]))))
	}
	if(length(useGNI) != 0){
		ret <- append(ret, list(gni=cbind(data.frame(Date=rownames(macroData)), data.frame(macroData[, gniIds]))))
	}
	
	return( ret )
}

dbToFile.calculate <- function( input, settings){
	dtrAssets <- settings[["dbf.dtrAssets"]]
	prdAssets <- settings[["dbf.prdAssets"]]
	classIdentifiers<-settings[["dbf.classIdentifiers"]]
	suffixes<-settings[["dbf.suffixes"]]
	otherIds <- settings[["dbf.otherIds"]]
	marketDataDBSettings<-settings[["dbf.marketDataDBSettings"]]
	macroDBSettings <- settings[["dbf.macroDataDBSettings"]]
	positionsDBSettings <- settings[["dbf.positionsDBSettings"]]
	gdpSuffix <- settings[["dbf.gdpSuffix"]]
	gniSuffix <- settings[["dbf.gniSuffix"]]
	inflationSuffix <- settings[["dbf.inflationSuffix"]]
	useGNI <- settings[["dbf.useGNI"]]
	
	# market data
	assets <- setdiff( union(dtrAssets, prdAssets), otherIds)
	ret <- dbf.getMktData(assets, otherIds, classIdentifiers, suffixes, marketDataDBSettings)
	# macro data
	ret <- append(ret, dbf.getMacroData(prdAssets, gdpSuffix, gniSuffix, inflationSuffix, useGNI, macroDBSettings))
	# positions data
	ret <- append( ret, dbf.getPositionsData(positionsDBSettings))
		
	return(ret)
}

