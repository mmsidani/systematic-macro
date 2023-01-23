wbData.input <- function(settings){
	dataFile <- settings["wbd.inputFile"]
	inputDir <- settings[["wbd.inputDir"]]
	
	# header is FALSE because otherwise R will replace blanks with "." in the names of countries for example. we need the header from WB to be returned as is. hence the trick
	columnNames <- read.csv(paste(inputDir, dataFile, sep=""), stringsAsFactors=F, header=F )[1, ]
	# now read it again with the header to have a proper data.frame() where numbers are numbers and not strings (in the previous read.csv(), because the header was made a row, numbers became strings. we avoid the big mess of reconverting by reading file twice.)
	wbData <- read.csv(paste(inputDir, dataFile, sep=""), stringsAsFactors=F )
	
	return(list(wbData=wbData, wbColumnNames=as.character(columnNames)) )
}

wbData.output <- function(output, settings){
	outputDir <- settings[["wbd.outputDir"]]
	
	today <- utils.today()
	for( n in names(output)){
		write.csv( output[[n]][nrow(output[[n]]):1, ], paste(outputDir, n, ".csv", sep=""), row.names=F)
	}
}

wbd.basicCheck <- function( countries, seriesNameMap){
	# basic check. basic because if mapping is out of order we wouldn't detect it
	if(length(unique(countries[[1]])) != length(unique(countries[[2]])) || length(unique(seriesNameMap[[1]])) != length(unique(seriesNameMap[[2]])) ){
		stop(paste("ERROR in wbd.check(): mismatch in one/both mappings: ",length(unique(countries[[1]])),length(unique(countries[[2]])),length(unique(seriesNameMap[[1]])),length(unique(seriesNameMap[[2]]))))
	}
}

wbd.makeDBReady <- function( wbSeries) {
	
	ret <- NULL
	dates <- wbSeries[["Date"]]
	nonDateCols <- setdiff(names(wbSeries), "Date")
	for ( n in nonDateCols) {
		isNotNA <- is.finite( wbSeries[,n] )
		newCol <- rep(n, sum(isNotNA))
		ret <- rbind(ret,data.frame(Date=dates[isNotNA],Rate=wbSeries[isNotNA,n],name=newCol))
	}
	
	return( ret )
}

wbData.calculate <- function(input, settings){
	
	countriesMap <- settings[["wbd.countriesMap"]]
	seriesNameMap <- settings[["wbd.seriesNameMap"]]
	indicatorColumnName <- settings[["wbd.indicatorColumnName"]]
	yearColumnName <- settings[["wbd.yearColumnName"]]
	
	wbData <- input[["wbData"]]
	wbColumnNames <- input[["wbColumnNames"]]

	wbd.basicCheck( countriesMap, seriesNameMap)
	
	# recall we read wbData without a header. what would have been a header is now the first row
	# it's easiest to download a given series for all countries on the WB web site; we use the first column in countriesMap to keep those of interest to us
	countriesMap <- countriesMap[is.element( countriesMap[, 1], wbColumnNames), ]
	ccs <- countriesMap[, 1]

	wbIndicators <- wbData[[grep(indicatorColumnName, wbColumnNames)]]
	uniqueSeriesNames <- unique( wbIndicators )
	yearColumnIndex <- match(yearColumnName, wbColumnNames)
	
	mapWBNames <- c()
	for(s in 1:nrow(seriesNameMap) ){
		mapWBNames <- c(mapWBNames, match( seriesNameMap[s, 1], uniqueSeriesNames ) )
	}
	
	dbData <- NULL
	ret <- NULL
	for(i in 1:length(mapWBNames)){
		# did we get data from WB for this series name (i.e., seriesNameMap[[1]][i])?
		if( is.na(mapWBNames[i] ) ) next
	
		wbIndicator <- uniqueSeriesNames[ mapWBNames[i] ]
		inds <- match(wbColumnNames, ccs )

		wbSeries <- wbData[ wbIndicators == wbIndicator, c(yearColumnIndex, (1:ncol(wbData))[is.finite(inds)]) ]

		wbSeries[,1] <- paste(wbSeries[,1], "-12-31", sep= "")
		names(wbSeries)[1] <- "Date"
		names(wbSeries)[2:ncol(wbSeries)] <- paste( as.character(countriesMap[inds[is.finite(inds)], 2]), seriesNameMap[i, 2], sep="")
		
		# wb might return an empty series for a country or region. example gdp (constant LCU) for euro area
		notFinites <- colSums(is.finite(as.matrix(wbSeries[, 2:ncol(wbSeries)]))) == 0
		wbSeries <- wbSeries[, c(1, (2:ncol(wbSeries))[ !notFinites])]
		ret <- append(ret, list(wbSeries))
		names(ret)[length(ret)] <- gsub(" ", ".", seriesNameMap[i, 1])
		
		dbData <- rbind( dbData, wbd.makeDBReady( wbSeries))
	}
	
	ret <- append( ret, list(dataToDB=dbData))
	
	return( ret )
}
