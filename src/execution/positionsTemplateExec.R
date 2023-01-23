ptex.getInvestableUniverse <- function(pathToRSourceCode){
	constraintsFiles.portfolioNames <- NULL	
	source(paste(pathToRSourceCode,"inputFiles/constraintsFiles.R", sep=""))
	portfolioNames <- constraintsFiles.portfolioNames()
	
	ret <- c()
	for (portfolio in portfolioNames ){
		portfolio.settings <- NULL
		source(paste(pathToRSourceCode,"inputFiles/constraints-",portfolio,".r", sep=""))
		constraintsFile <- paste("inputFiles/constraints-",portfolio.settings()[["constraintsNAME"]],".csv", sep="")
		ret <- union( ret, names( utils.getConstraints(pathToRSourceCode, constraintsFile ) ) )
	}
	
	return( ret )
}

ptex.getColumnNames <- function(positionsTableSettings){
	
	channel <- dbDataLoader.getConnection( positionsTableSettings[["uid"]], positionsTableSettings[["dsnName"]])
	sqlStmt <- paste("select column_name from INFORMATION_SCHEMA.columns where table_name='", positionsTableSettings[["positionsTableName"]],"'", sep="")
	columnNames <- sqlQuery(channel, sqlStmt, stringsAsFactors=F )
	dbDataLoader.close( channel )
	
	return( columnNames[[1]] )
}

ptex.getRateIdsMap <- function(investableUniverse, rateHeaderTableSettings, bondIdentifier, equityIsEquityIndexFuture, univ2Ticker){
	
	channel <- dbDataLoader.getConnection( rateHeaderTableSettings[["uid"]], rateHeaderTableSettings[["dsnName"]])
	headerTable <- dbDataLoader.getTable(channel, rateHeaderTableSettings[["headerTableName"]])
	dbDataLoader.close(channel)
	
	inds <- match(investableUniverse, headerTable[["descr"]])
	if( any(!is.finite(inds))){
		stop(paste("ERROR in ptex.getRateIds(): the following instruments are not in the header table:", paste(investableUniverse[!is.finite(inds)], collapse=", ")))
	}
	
	sa2Types <- rep("", length(investableUniverse))
	isBond <- grepl(bondIdentifier, investableUniverse)
	if( any( isBond)){
		sa2Types[isBond] <- "bond"
	}
	
	if(any(!isBond) && equityIsEquityIndexFuture){
		sa2Types[!isBond] <- "equityIndexFuture"
	} else if( any(!isBond)){
		sa2Types[!isBond] <- "equity"
	}
	
	inds2 <- match(investableUniverse, univ2Ticker[["SA2Name"]])
	if(any(!is.finite(inds2))){
		stop(paste("ERROR in ptex.getRateIds(): the universe to ticker mapping file is missing some names in the investable universe",paste(investableUniverse[!is.finite(inds2)], collapse=", ")))
	}
	ccys <- univ2Ticker[["currency"]][inds2]
	bbTickers <- univ2Ticker[["price"]][inds2]
	
	ret <-  data.frame(SA2_Name= investableUniverse, BB_Ticker=bbTickers, Rate_Id=headerTable[["Rate_Id"]][inds], Type=sa2Types, Currency=ccys)
	
	otherHoldings <- univ2Ticker[ !is.na(univ2Ticker[["otherHolding"]]), ]

	for( i in 1:nrow(otherHoldings)){
		sa2Name <- otherHoldings[["SA2Name"]][i]
		ret <- rbind(ret, data.frame( SA2_Name=sa2Name, BB_Ticker=otherHoldings[["otherHolding"]][i],Rate_Id = headerTable[["Rate_Id"]][match(sa2Name,headerTable[["descr"]])] , Type=otherHoldings[["otherHoldingType"]][i], Currency=otherHoldings[["currency"]][i]))
	}
	return(ret)
}

ptex.fillTable <- function(columnNames, rateIdsMap ){
	
	output <- data.frame( matrix(NA, ncol=length(columnNames), nrow=nrow(rateIdsMap)))
	colnames(output) <- columnNames

	inds <- match(colnames(rateIdsMap), columnNames)
	if( any( !is.finite(inds))){
		stop(paste("ERROR in ptex.fillTable(): some columns are missing from the DB table:", paste(colnames(rateIdsMap)[!is.finite(inds)], collapse=", ")))
	}
	
	for( i in 1:length(inds)){
		output[[inds[i]]] <- rateIdsMap[[i]]
	}
	
	return(output)
}

positionsTemplateExec.main <- function(){
	
	pathToMe <- dirname( sys.frame(1)$ofile )
	pathToRSourceCode <- paste( pathToMe, "/../", sep="" )
	
	source(paste(pathToRSourceCode, "settings/GlobalVariables.R", sep=""))
	source(paste(pathToRSourceCode, "utils/miscUtils.R", sep=""))
	source(paste(pathToRSourceCode, "utils/dbDataLoader.R", sep=""))
	
	library(RODBC)
	
	settings <- global.positionsTemplateExecSettings()
	positionsTableSettings <- settings[["ptex.positionsTableSettings"]]
	rateHeaderTableSettings <- settings[["ptex.rateHeaderTableSettings"]]
	outputDir <- settings[["ptex.outputDir"]]
	bondIdentifier <- settings[["ptex.bondIdentifier"]]
	equityIsEquityIndexFuture <- settings[["ptex.equityIsEquityIndexFuture"]]
	inputDir <- settings[["ptex.inputDir"]]
	universeToSecuritiesMappingFile <- settings[["ptex.universeToSecuritiesMappingFile"]]
	
	columnNames <- ptex.getColumnNames(positionsTableSettings)
	
	investableUniverse <- ptex.getInvestableUniverse( pathToRSourceCode )
	univ2Ticker <- read.csv(paste(inputDir, universeToSecuritiesMappingFile, sep=""), stringsAsFactors=F)
	if( any( !is.element(c("SA2Name","price","currency","otherHolding"), names(univ2Ticker))) ){
		stop(paste("ERROR in ptex.getRateIds(): the universe to ticker mapping file is in the wrong format"))
	}
	rateIdsMap <- ptex.getRateIdsMap( investableUniverse, rateHeaderTableSettings, bondIdentifier, equityIsEquityIndexFuture, univ2Ticker )
	
	output <- ptex.fillTable( columnNames, rateIdsMap )
	fileName <- paste(outputDir,"positionsTableLoad-",utils.today(),".csv", sep="")
	rootFileName <- sub("\\.csv$", "", fileName)
	counter <- 1
	while (file.exists(fileName)){
		fileName <- paste( rootFileName ,".", counter, ".csv", sep="" )
		counter <- counter + 1
	}
	write.table(output, file= fileName, row.names=F, col.names=T, quote=F, na="", sep=",")
	
	print( paste(sys.frame(1)$ofile,"done.") )
}

# execute
positionsTemplateExec.main()