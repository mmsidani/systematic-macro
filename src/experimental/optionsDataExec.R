optionsDataExec.main <- function(){
	# source global variables
	pathToMe<-dirname(sys.frame(1)$ofile)
	
	pathToRSourceCode <- paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"marketData/bloombergData.R", sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R", sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R", sep=""))
	
	library(RODBC)
	library(timeSeries)
	
	settings <- global.bloombergDataExecSettings()
	
	inputDir <- "//Terra/Users/Majed/devel/InOut/"

	requestSx5eOptions<-list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=inputDir,bbd.referenceDataFile="request-sx5eOptions-march.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=FALSE, bbd.intermediateOutputFile="rawsx5eoptionsData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=150, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	requestOptionsOpInt<-list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=inputDir,bbd.referenceDataFile="request-vol-opint-sx5eOptions.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=FALSE, bbd.intermediateOutputFile="rawsx5eoptionsOpIntData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=150, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	requestUnderlyings<-list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=inputDir,bbd.referenceDataFile="request-optUnds.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=FALSE, bbd.intermediateOutputFile="rawIndexData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=150, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	settings[["bbd.requests"]] <- list( requestSx5eOptions,  requestOptionsOpInt, requestUnderlyings )
	settings[["pathToRSourceCode"]] <- pathToRSourceCode
	
	input <- bloombergData.input( settings )
	output <- bloombergData.calculate( input, settings)
	bloombergData.output( output, settings )
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
optionsDataExec.main()