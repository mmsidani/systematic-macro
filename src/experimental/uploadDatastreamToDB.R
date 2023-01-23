uploadDatastreamToDB.main <- function(){
	
	# input
	rawGDPBBFile <- "//SOL/Users/majed/devel/InOut/2012-10-15.rawGDPData.csv"
	rawCPIBBFile <- "//SOL/Users/majed/devel/InOut/2012-10-15.rawCPIData.csv"
	overrideExistingDataInDB <- FALSE
	sourceName <- "R_DS"
	
	pathToMe <- dirname( sys.frame(1)$ofile )
	pathToData <- paste(pathToMe,"/../../data/", sep = "")
	
	source(paste(pathToMe, "/../settings/GlobalVariables.R", sep=""))
	source(paste(pathToMe, "/../utils/miscUtils.R", sep="" ))
	source(paste(pathToMe, "/../utils/dbDataLoader.R", sep="" ))
	
	library(RODBC)
	
	coreSettings <- global.core()
	inOutDir <- coreSettings[["dataOutput"]]
	macroDbSettings <- coreSettings[["macroDbSettings"]]
	
	# DS data are in trunk/data. read and put in CHRONOLOGICAL order
	gdpDSData <- read.csv(paste(pathToData, "GDP-new.csv", sep="" ))
	gdpDSData <- gdpDSData[ nrow(gdpDSData):1, ]
	cpiDSData <- read.csv(paste(pathToData, "CPI-new.csv", sep="" ))
	cpiDSData <- cpiDSData[ nrow(cpiDSData):1, ]
	
	## read raw bb data. these are the files the java code outputs
	gdpBBData <- read.csv(rawGDPBBFile, header=F, stringsAsFactors=F)
	cpiBBData <- read.csv(rawCPIBBFile, header=F, stringsAsFactors=F)
	
	# work on GDP data first
	toLoad <- NULL
	gdpDF <- NULL
	toWrite <- NULL
	gdpDSDates <- gdpDSData[, "Date"]
	for( n in setdiff( names(gdpDSData), "Date")){
		isNotNA <- is.finite( gdpDSData[, n] )
		temp <- as.numeric(gdpDSData[ isNotNA, n] )
		if(length(temp) < 5){
			stop(paste(" ds series for ",n, "too short."))
			
		}
		
		# calculate year-on-year
		dsRates <- temp[5:length(temp) ] / temp[1:(length(temp) -4) ] - 1.0 
		dsDates <- gdpDSDates[isNotNA][5:sum(isNotNA) ]
		# only way to compare dates in DS data with bb data is to bring them back to years/quarters because former has last calendar day of quarter, latter last business day
		dsYearQuarter <- paste(substr(dsDates, 1, 4), quarters( as.Date( dsDates)), sep="" )
		
		rateName <- paste(n,"rate", sep="")
		if( !is.element( rateName, gdpBBData[,2])){
			print(paste("skipping", rateName))
			next
		}
		bbRates <- gdpBBData[ gdpBBData[ , 2] == rateName, ]
		indsInOrder <- order( bbRates[, 1 ], decreasing = F )
		bbRates <- bbRates[ indsInOrder, ]
		names(bbRates ) <- c("Date","name", "Rate")
		bbDates <- bbRates[ , 1 ]

		secDSDF <- data.frame(Date = dsDates, name=rep(rateName, length(dsDates) ), Rate= as.numeric(dsRates ), Label=paste(substr(dsDates, 1, 4), quarters( as.Date( dsDates)), sep="" ))
		secDSDF$Date <- utils.changeToPrevWeekday( secDSDF$Date )
		secDSDF <- secDSDF[ secDSDF$Date < "2012-01-01", ]
			
		toLoad <- rbind(toLoad, secDSDF )
		gdpDF <- rbind(gdpDF, secDSDF )
		toWrite <- rbind(toWrite, secDSDF )
		
		bbDatesTrunc <- bbDates[ bbDates >= "2012-01-01" ]
		toWrite <- rbind(toWrite, cbind(bbRates[ bbDates >= "2012-01-01",], data.frame(Label=paste(substr(bbDatesTrunc, 1, 4), quarters( as.Date( bbDatesTrunc)), sep="" )) ))
		gdpDF <- rbind(gdpDF, cbind(bbRates[ bbDates >= "2012-01-01",], data.frame(Label=paste(substr(bbDatesTrunc, 1, 4), quarters( as.Date( bbDatesTrunc)), sep="" ))) )
		
	}
	
	# repeat work for CPI
	allCpiDSDates <- cpiDSData[, "Date"]
	cpiDF <-NULL
	for( n in setdiff( names(cpiDSData), "Date")){
		if( !is.element( n, cpiBBData[,2])){
			print(paste("skipping", n))
			next
		}
		isNotNA <- is.finite( cpiDSData[, n ])
		cpiDSLevs <- as.numeric(cpiDSData[ isNotNA, n ])
		
		cpiDSDates <- allCpiDSDates[isNotNA]
		
		
		cpiDSMonths <- substr(cpiDSDates, 1, 7)
		bbLevs <- cpiBBData[ cpiBBData[, 2] == n, ]
		indsInOrder <- order( bbLevs[, 1], decreasing = F)
		bbLevs <- bbLevs[ indsInOrder, ]
		names(bbLevs) <- c("Date","name", "Rate")
		bbDates <- bbLevs[ , 1 ]
		
		secCPIDF <- data.frame(Date = cpiDSDates, name=rep(n, length(cpiDSDates)), Rate=as.numeric(cpiDSLevs), Label=cpiDSMonths)
		secCPIDF$Date <- utils.changeToPrevWeekday( secCPIDF$Date )
		secCPIDF <- secCPIDF[ secCPIDF$Date < "2012-01-01", ]
		
		toLoad <- rbind(toLoad, secCPIDF)
		cpiDF <- rbind(cpiDF, secCPIDF)
		toWrite <- rbind(toWrite, secCPIDF)

		bbDatesTrunc <- bbDates[ bbLevs$Date >= "2012-01-01" ]
		toWrite <- rbind(toWrite, cbind(bbLevs[bbLevs$Date >= "2012-01-01", ],data.frame(Label=substr(bbDatesTrunc, 1, 7)) ))
		cpiDF <- rbind(cpiDF,cbind(bbLevs[bbLevs$Date >= "2012-01-01", ],data.frame(Label=substr(bbDatesTrunc, 1, 7)) ) )
	}
	
	write.csv(toWrite, paste(inOutDir,"dbReady.csv",sep=""), row.names = F )
	
	# now add the columns the db expects
	toLoad <- cbind(toLoad, data.frame(Date_Entered= toLoad[["Date"]] ))
	
	toLoad <- toLoad[toLoad[["Date"]] < "1900-01-01", ]
	toLoad[["Date_Entered"]][ toLoad[["Date_Entered"]] <= "1753-01-01" ] <- "1753-01-01"

	toLoad <- cbind(toLoad, data.frame(Is_Revisable=rep(0,nrow(toLoad))))
	write.csv(toLoad, paste(inOutDir,"dbReadytoload.csv",sep=""), row.names = F, quote=F )
	
	uniqueGDPNames <- unique(gdpDF[,"name"])
	uniqueCPINames <- unique(cpiDF[,"name"])
	for( nn in uniqueGDPNames){
		ttt <- gdpDF[gdpDF[,2]==nn, ]
		tdiff <- abs(ttt[2:nrow(ttt),"Rate"] - ttt[1:(nrow(ttt)-1),"Rate"])
		torder <- order(tdiff,decreasing=T)
		print(c(nn,ttt[["Date"]][2:nrow(ttt)][torder[1] ],tdiff[torder[1] ]))
	}
	for(nn in uniqueCPINames){
		ttt<-cpiDF[cpiDF[,2]==nn,]
		tdiff <- abs(ttt[2:nrow(ttt),"Rate"] / ttt[1:(nrow(ttt)-1),"Rate"] -1)
		torder <- order(tdiff,decreasing=T)
		print(c(nn,ttt[["Date"]][2:nrow(ttt)][torder[1] ],tdiff[torder[1] ] ))
	}
	

	dbDataLoader.pushData( toLoad, macroDbSettings[["ld.uid"]], macroDbSettings[["ld.dsnName"]], macroDbSettings[["ld.rateDescrTable"]], macroDbSettings[["ld.rateDataTable"]], overrideExistingDataInDB, macroDbSettings[["ld.isRevisable"]], sourceName )
}

# execute
uploadDatastreamToDB.main()