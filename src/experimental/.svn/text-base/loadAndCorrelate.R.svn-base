# input
sa2BellwetherReturns <- c( 0.0331, 0.0603, 0.0014, -0.0036, 0.0057, 0.0298, 0.017, 0.009, 0.0071, -0.0087, 0.0405, -0.0259, -0.01, 0.04041)
lastMonth <- "2013-07"

indices <-c("us.equity", "us.msciworld", "uk.asx", "eu.stxe600")

pathToMarketDataFile <- "//mercury/Users/sebastian/return composite/DBF.marketData.RData"

# end input

loadMarketData <- function( ) {

	marketData <- get ( load( pathToMarketDataFile ) ) 
	dates <- sort( as.Date(marketData$Date), decreasing = T)
	nextDate <- dates[1] + 1
	while( weekdays(nextDate) == "Saturday" || weekdays(nextDate) == "Sunday" ) {
		nextDate <- nextDate + 1
	}
	
	prevDate <- dates[length(dates)] - 1
	while( weekdays(prevDate) == "Saturday" || weekdays(prevDate) == "Sunday" ) {
		prevDate <- prevDate - 1
	}
	
	dates <- c( nextDate, dates, prevDate )
	datePairs <- data.frame( d1= dates[1: (length(dates)-1)], d2=dates[2:length(dates)] )
	differentMonths <- months(datePairs[, 2]) != months(datePairs[,1])
	dates <- as.character( dates[ c( F, differentMonths[1:( length(differentMonths) -1 )] ) ] )

	ret <- marketData[is.element( rownames(marketData), dates) , ]

	if ( !grepl( lastMonth, as.character(rownames(ret)[1] )) ){
		stop(paste ("error: there's no data for", lastMonth ) )
	}
	
	return ( ret )
}

marketData <- loadMarketData()

correlation <- function ( x ) {

	numPoints <- length(sa2BellwetherReturns)


	if(numPoints+1 > nrow(marketData)){
		stop("error: not enough marketData")
	}
	
	rets <- as.matrix(marketData[ 1:numPoints, x] ) / as.matrix(marketData[ 2:(numPoints+1), x] ) -1
	return ( cor (rets, rev(sa2BellwetherReturns), use = "na.or.complete"))
	
}
for (index in indices){

print (paste (index, correlation (index)))
}
	