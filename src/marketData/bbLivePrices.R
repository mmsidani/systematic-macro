bblp.launchJava <- function( shellCommand ){
# execute java code to get live bb data
	
	print(paste("INFO from bblp.launchJava(): launching java executable to retrieve live Bloomberg data:", shellCommand))
	shellOutput <- shell(shellCommand, intern= T, wait = T, translate = T, mustWork = T)
	if(length(shellOutput ) != 0){
		print("WARNING from bblp.launchJava(): java executable might have had an abnormal return. Here's the message:")
		print(shellOutput)
	} else {
		print(paste("INFO from bblp.launchJava(): java executable returned normally."))
	}
}

bbLivePrices.retrieve <- function( today, outputDir, requestTable, pathToJava, pathToClasses, javaClass, fillNonTrading ){
# only "HistoricalDataRequest"
	
	intermediateOutputFile <- paste(outputDir, "bbLivePrices.rawBBDataFile.csv", sep="" )
	requestFileName <- paste(outputDir, "bbLivePrices.dataRequestFile.csv", sep="")
	
	# create request file
	write.csv(requestTable, requestFileName, row.names=FALSE, quote=F )
	
	today <- utils.hyphensToYmd( today )
	# yes, "DAILY" is hardcoded. not sure what we put matters. we want data for today anyway
	shellCommand <- paste(pathToJava, "-cp ", pathToClasses, javaClass, today, today, "DAILY", requestFileName, intermediateOutputFile, "HistoricalDataRequest", fillNonTrading )
	bblp.launchJava( shellCommand)
	
	# read java code output
	javaOutput <- read.csv(intermediateOutputFile, header= F, stringsAsFactors = F)
	
	return(javaOutput)
}