pmUtils.getCurrentPositions <- function( positionsDBSettings){
	if( any( !is.element( c( "uid", "dsnName", "positionsTableName" ), names(positionsDBSettings))) ){
		stop("ERROR in pmUtils.getCurrentPositions(): positionsDBSettings list is expected to have 3 columns named 'uid', 'positionsTableName' and 'dsnName' ")
	}
	
	channel <- dbDataLoader.getConnection(positionsDBSettings[["uid"]], positionsDBSettings[["dsnName"]] )
	positionsDF <- dbDataLoader.getTable( channel, positionsDBSettings[["positionsTableName"]] )
	dbDataLoader.close( channel )
	
	if( any( !is.element( c("BB_Ticker", "Quantity" ), names(positionsDF)))){
		stop("ERROR in pmUtils.getCurrentPositions(): the positions DB table is expected to have 2 columns named 'BB_Ticker' and 'Quantity' ")
	}
	
	traders <- unique(positionsDF[, "Trader"])
	ret <- list()
	for(trader in traders){
		temp <- positionsDF[ positionsDF[, "Trader"]== trader,  ]
		ret <- append(ret, list(pmUtils.netHoldings( temp, "BB_Ticker", "Quantity", "SA2_Name", "Type" )))
		names(ret)[length( ret)] <- trader
	}
	
	return( ret )
}

pmUtils.netHoldings <- function( positionsDF, nameColumn, quantityColumn, mapColumn, typeColumn ){
# typically: nameColumn contains mkt instrument; quantityColumn contains quantity; mapColumn contains sa2 name to which mkt instrument is mapped
	
	qtys <- aggregate( positionsDF[[quantityColumn]], by=list( by=positionsDF[[nameColumn]]), FUN="sum")
	mapTo <- aggregate( positionsDF[[mapColumn]], by=list( by=positionsDF[[nameColumn]]), FUN="unique")
	mapType <- aggregate( positionsDF[[typeColumn]], by=list( by=positionsDF[[nameColumn]]), FUN="unique")

	names(qtys) <- c("name", "qty")
	inds <- match(qtys[["name"]], mapTo[[1]])
	indsType <- match(qtys[["name"]], mapType[[1]])
	
	return( cbind(qtys, data.frame(sa2code=mapTo[inds, 2], type = mapType[indsType, 2]) ))
}

pmUtils.netHoldingsAsOf <- function( positionsDF, nameColumn, quantityColumn, mapColumn, typeColumn, dateColumn, asOfDate){
	
	positionsDF <- positionsDF[ as.character(positionsDF[ , dateColumn ] ) < asOfDate, ]
	
	return( pmUtils.netHoldings( positionsDF, nameColumn, quantityColumn, mapColumn, typeColumn) )
}

pmUtils.getBalance <- function( positionsDF, nameColumn, quantityColumn, priceColumn){
	
	balance <- aggregate( -positionsDF[[quantityColumn]] * positionsDF[[priceColumn]], by=list( by=positionsDF[[nameColumn]]), FUN="sum")
	
	names(balance) <- c("name", "balance")
	return( balance )
}

pmUtils.miniFromMajor <- function( miniContracts, univ2Ticker ){
	
	if( length(miniContracts) !=0){
		indSA2 <- match( names(miniContracts), univ2Ticker[["SA2Name"]])
		
		miniTickers <- c()
		for (i in 1:length(indSA2)){
			if(is.na(indSA2[i])) next
			
			bbPrice <- sub(" INDEX", "", univ2Ticker[indSA2[i], "price"])
			miniTickers <- c( miniTickers, paste( miniContracts[[i]], substr( bbPrice, nchar(bbPrice)-1, nchar(bbPrice) ), " INDEX", sep="") )
			names(miniTickers)[length(miniTickers)] <- univ2Ticker[ indSA2[i], "SA2Name"]
		}
		
		return( miniTickers )
	}
	
	return( NULL )
}