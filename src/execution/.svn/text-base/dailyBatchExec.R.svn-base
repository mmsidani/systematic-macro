dailyBatchExec.main<-function(){
	
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode <- paste( pathToMe, "/../", sep = "")
	
	source(paste(pathToRSourceCode, "settings/GlobalVariables.R", sep = ""))
	source(paste(pathToRSourceCode,"marketData/bloombergData.R", sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R", sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R", sep=""))
	source(paste(pathToRSourceCode,"utils/algorithms.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"utils/draw3.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbToFile.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/securityClasses.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/modelEvaluator.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/totalReturn.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/earnFromPE.R",sep=""))	
	source(paste(pathToRSourceCode,"dataProcessing/slopes.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/predict.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/eurolegacy.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/dailyTotalCapitalRets.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/frontierBuilder.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/varianceCovariance.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/specialPortfolios.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/constructionMethods.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/fbUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/mvOpt.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimulation.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimStats.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/leverageIt.R", sep = ""))
	source(paste(pathToRSourceCode,"portfolioManagement/rebalancing.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/reinvesting.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/transactions.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/multiStrategy.R",sep=""))
	source(paste(pathToRSourceCode,"riskManagement/var.R",sep=""))
	
	library(MASS)
	library(RODBC)
	library(timeSeries)
	library(quadprog)
	library(limSolve)
	library(RODBC)
	
	# modify global.core() to write output to dated folders. to be restored before we exit this function normally
	original.global.core <- global.core
	coreSettings <- global.core()
	today <- utils.today()
	# NOTE: why the 2-step paste()-ing below? because file.exists() returns FALSE if we add the slash at the end of the folder name even if the folder exists
	coreSettings[["dataOutput"]] <- utils.createSubDir(today, coreSettings[["dataOutput"]] )	
	coreSettings[["optOutput"]] <- utils.createSubDir(today, coreSettings[["optOutput"]] )	
	coreSettings[["simOutput"]] <- utils.createSubDir(today, coreSettings[["simOutput"]] )
	
	global.core <<- function(){		
		return( coreSettings )
	}
	
	# we had to modify global.core() above this line because the next line calls global.core()
	settings <- global.dailyBatchExecSettings()
	
	filesDir <- settings[["dabaex.outputFilesDir"]]
	sourceName <-  settings[["dabaex.sourceName"]]
	override <- settings[["dabaex.override"]]
	dbSettings <- settings[["dbSettings"]]
	
	# get bloomberg data
	settings <- global.bloombergDataExecSettings()
	requestIndexMktData <- list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-Indexes.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=TRUE,bbd.dbLabel=NULL, bbd.intermediateOutputFile="rawIndexMktData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=100, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	requestGDP <- list( bbd.requestType="HistoricalDataRequest",bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-GDP.csv",bbd.destinationTable="tbl_sa2_macro_data",bbd.loadDB=TRUE,bbd.dbLabel="YQ",bbd.intermediateOutputFile="rawGDPData.csv",bbd.overrideExistingData=FALSE, bbd.numberOfDates=300, bbd.dataFrequency="QUARTERLY", bbd.fillNonTrading="false")
	requestCPI <- list( bbd.requestType="HistoricalDataRequest",bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-CPIRate.csv",bbd.destinationTable="tbl_sa2_macro_data",bbd.loadDB=TRUE,bbd.dbLabel="YM",bbd.intermediateOutputFile="rawCPIData.csv",bbd.overrideExistingData=FALSE, bbd.numberOfDates=300, bbd.dataFrequency="MONTHLY", bbd.fillNonTrading="false")
	settings[["bbd.requests"]] <- list( requestIndexMktData, requestCPI, requestGDP )
	settings[["pathToRSourceCode"]] <- pathToRSourceCode
	input <- bloombergData.input( settings )
	output <- bloombergData.calculate( input, settings)
	bloombergData.output( output, settings )
	
	# korea special
	print("now doing Korea special")
	settings<-global.totalReturnExecSettings()
	settings[["totret.outputFile"]] <-"totalReturn-korea.csv"
	input<-totalReturn.input(settings)
	output<-totalReturn.calculate(input,settings)
	totalReturn.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latesttotalReturn-korea.csv", sep=""), sourceName, override, dbSettings, NULL)
	
	# singapore special
	print("now doing singapore special")
	settings<-global.earnFromPEExecSettings()
	settings[["earnFromPE.outputFile"]]<-"earningsfromPE-singapore.csv"
	input<-earnFromPE.input(settings)
	output<-earnFromPE.calculate(input,settings)
	earnFromPE.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latestearningsfromPE-singapore.csv", sep=""), sourceName, override, dbSettings, NULL)

	# slopes
	print("now doing slopes")
	settings<-global.slopesExecSettings()
	settings[["slex.outputFile"]]<-"slopes.csv"
	input<-slopes.input(settings)
	output<-slopes.calculate(input,settings)
	slopes.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latestslopes.csv", sep=""), sourceName, override, dbSettings, NULL)
	
	# er's / risks
	print("now doing er's risks")
	settings<-global.predictExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	settings[["prd.outputFile"]]<-"forecasts.csv"
	settings[["prd.gdpFile"]]<-c("DBF.gdp.csv")
	settings[["prd.gdpSuffix"]]<-c(".gdprate")
	settings[["eretm.numGrowthYears"]]<-10
	settings[["eretm.doAverageGrowthOverHistory"]] <- TRUE
	settings[["prd.useMacroDB"]]<- TRUE
	settings[["eretm.nonLocalMacroGrowth"]]<-list(ca.tsx60=list(macroCcs=c("us","ca"),macroCoeffs=c(1,0)),sg.mscifree=list(macroCcs=c("us","jp","sg"),macroCoeffs=c(.4,.5,.1)),tw.mscifree=list(macroCcs=c("us","jp","tw"),macroCoeffs=c(.3,.7,0)),hk.equity=list(macroCcs=c("us","jp","hk"),macroCoeffs=c(.4,.2,.4)))
	settings[["eretm.nonLocalInflation"]]<-list(ca.tsx60=list(macroCcs=c("us","ca"),macroCoeffs=c(1,0)),sg.mscifree=list(macroCcs=c("us","jp","sg"),macroCoeffs=c(.4,.5,.1)),tw.mscifree=list(macroCcs=c("us","jp","tw"),macroCoeffs=c(.3,.7,0)),hk.equity=list(macroCcs=c("us","jp","hk"),macroCoeffs=c(.4,.2,.4)))	
	input<-predict.input(settings)
	output<-predict.calculate(input,settings)
	predict.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latestforecasts.csv", sep=""), sourceName, override, dbSettings, NULL)
	
	# er's / risks  temporary TODO .ber hack
	settings<-global.predictExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	settings[["prd.outputFile"]]<-"forecasts.csv"
	settings[["prd.gdpSuffix"]]<-c(".gdprate")
	settings[["eretm.numGrowthYears"]]<-10
	settings[["eretm.doAverageGrowthOverHistory"]] <- FALSE
	settings[["prd.useMacroDB"]]<- TRUE
	settings[["eretm.nonLocalMacroGrowth"]]<-list()
	settings[["eretm.nonLocalInflation"]]<-list()
	settings[["eretm.seedSource"]]<-list()
	settings[["eretm.growthBenchmark"]]<-list()
	input<-predict.input(settings)
	output<-predict.calculate(input,settings)
	predict.output(output,settings)
	latestForecasts <- read.csv(paste(filesDir, "latestforecasts.csv", sep=""), stringsAsFactors=F)
	latestForecasts <- latestForecasts[ , c("Date", names(latestForecasts)[grepl("\\.ner$", names(latestForecasts))] )]
	names(latestForecasts) <- gsub( "\\.ner$", ".ber", names(latestForecasts) )
	utils.uploadDataFromDataFrame( latestForecasts, sourceName, override, dbSettings, NULL)
	
	# fx special
	print("now doing fx special")
	settings<-global.eurolegacyExecSettings()
	settings[["euroleg.outputFile"]]<-"eurolegacy.csv"
	input<-eurolegacy.input(settings)
	output<-eurolegacy.calculate(input,settings)
	eurolegacy.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latesteurolegacy.csv", sep=""), sourceName, override, dbSettings, NULL)
	
	# dtr's
	print("now doing dtr's")
	settings<-global.dailyTotalCapitalExecSettings()
	settings[["dtcex.outputFile"]] <-"dTrCr.csv"
	input<-dailyTotalCapitalRets.input(settings)
	output<-dailyTotalCapitalRets.calculate(input,settings)
	dailyTotalCapitalRets.output(output,settings)
	utils.uploadDataFromFile(paste(filesDir, "latestdTrCr.csv", sep=""), sourceName, override, dbSettings, NULL)
	
	# frontier builder
	print("now doing frontier builder")
	settings <- global.frontierBuilderExecSettings()
	settings[["fbex.methods"]] <- c("MV","MVU")
	settings[["fbex.parametersList"]] <- list(MVU=c(50,450,350,200))
	settings[["fbex.shortingControl"]] <- list( doShort="none", threshold=list(), securityER=list() )
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/",settings[["fbex.constraints"]],sep=""))
	portfolioNames<-constraintsFiles.portfolioNames()
	for(portfolioName in portfolioNames){
		settings[["portfolioName"]]<-portfolioName
		
		input<-frontierBuilder.input(settings)
		output<-frontierBuilder.calculate(input,settings)
		frontierBuilder.output(output,settings)
	}
	
	# download data to file
	print("now dumping DB data to files")
	settings<-global.dbToFileExecSettings()
	input <- dbToFile.input( settings )
	output <- dbToFile.calculate( input, settings )
	dbToFile.output( output, settings )

	# restore global.core()
	global.core <<- original.global.core
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
dailyBatchExec.main()