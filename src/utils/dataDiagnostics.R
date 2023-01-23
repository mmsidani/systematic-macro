dataDiagnostics.outliersPrevious <- function( dataDF, numberOfStandardDeviations, numberOfDays ) {
	
	if( 2 * numberOfDays > nrow(dataDF)){
		stop(paste("ERROR in dataDiagnostics.outliers(): not enough data for the required number of days. nrow(dataDF)=",nrow(dataDF),"; numberOfDays=", numberOfDays))
	}
	
	rownames(dataDF) <- NULL
	columnNames <- colnames(dataDF)
	
	dateIndex <- match("Date", names(dataDF))
	# make "Date" the first component
	columnNames <- c(columnNames[dateIndex], columnNames[setdiff(1:length(columnNames), dateIndex)])
	dates <- dataDF[, dateIndex ]
	
	dataMat <- as.matrix(dataDF[, setdiff(1:ncol(dataDF), dateIndex)] )
	dataMat[1:(nrow(dataMat)-1), ] <- dataMat[1:(nrow(dataMat)-1), ] / dataMat[2:nrow(dataMat), ] -1
	dataMat[nrow(dataMat), ] <- NA

	dfRowCounter <- 1
	ret <- matrix(nrow=nrow(dataDF) * ncol(dataDF), ncol=4 )
	rownames(ret) <- rep(NA, nrow(dataDF) * ncol(dataDF))
	ids <- rep(NA, nrow(dataDF) * ncol(dataDF))
	retRowCounter <- 1
	while( dfRowCounter + 2*numberOfDays -1 <= nrow(dataMat)){
		
		# test these ...
		testDataMat <- dataMat[dfRowCounter:(dfRowCounter + numberOfDays -1), ]
		testDates <- dates[dfRowCounter:(dfRowCounter + numberOfDays -1)]
		# ... against the stats of these
		subDataMat <- dataMat[(dfRowCounter + numberOfDays ):ifelse(dfRowCounter + 3 * numberOfDays -1 <= nrow(dataMat), dfRowCounter + 2* numberOfDays -1, nrow(dataMat)), ]
		
		# look for outliers
		means <- apply( subDataMat, 2, FUN= mean, na.rm = T ) 
		sds <- apply( subDataMat, 2, FUN= sd, na.rm = T ) 
		
		# determine bands
		upBand <- means + numberOfStandardDeviations * sds
		lowBand <- means - numberOfStandardDeviations * sds
		
		outliers <- testDataMat < matrix(lowBand, nrow=1)[rep(1,nrow(testDataMat)), ] | testDataMat > matrix(upBand, nrow=1)[rep(1,nrow(testDataMat)), ] 

		for( i in 1:ncol(testDataMat)){
			isOut <- is.finite( outliers[, i]) & as.logical(outliers[, i]) == T
			numOut <- sum(isOut) 
			
			if( numOut != 0 ){
				ret[retRowCounter:(retRowCounter+numOut-1), 1] <- testDataMat[ isOut, i ]
				ret[retRowCounter:(retRowCounter+numOut-1), 2] <- lowBand[i]
				ret[retRowCounter:(retRowCounter+numOut-1), 3] <- means[i]
				ret[retRowCounter:(retRowCounter+numOut-1), 4] <- upBand[i]
				rownames(ret)[retRowCounter:(retRowCounter+numOut-1)] <- testDates[ isOut ]
				
				ids[retRowCounter:(retRowCounter+numOut-1) ] <- columnNames[i+1]
				
				retRowCounter <- retRowCounter + numOut
			}
		}
		
		dfRowCounter <- dfRowCounter + numberOfDays 
	}
	
	# remove extraneous rows
	ret <- ret[1:(retRowCounter-1), ]
	ids <- ids[1:(retRowCounter-1) ]
	
	# convert to data.frame()
	ret <- cbind(data.frame(Date=rownames(ret)), data.frame(id=ids),data.frame(value=ret[,1]), data.frame(lower.bound=ret[,2]), data.frame(mean= ret[, 3]), data.frame(upper.bound= ret[, 4]))
	
	return(ret)
}

dataDiagnostics.outliersConcurrent <- function( dataDF, numberOfStandardDeviations, numberOfDays ) {
	
	rownames(dataDF) <- NULL
	columnNames <- colnames(dataDF)
	
	dateIndex <- match("Date", names(dataDF))
	# make "Date" the first component
	columnNames <- c(columnNames[dateIndex], columnNames[setdiff(1:length(columnNames), dateIndex)])
	dates <- dataDF[, dateIndex ]
	
	dataMat <- as.matrix(dataDF[, setdiff(1:ncol(dataDF), dateIndex)] )
	dataMat[1:(nrow(dataMat)-1), ] <- dataMat[1:(nrow(dataMat)-1), ] / dataMat[2:nrow(dataMat), ] -1
	dataMat[nrow(dataMat), ] <- NA
	
	dfRowCounter <- 1
	ret <- matrix(nrow=nrow(dataDF) * ncol(dataDF), ncol=4 )
	rownames(ret) <- rep(NA, nrow(dataDF) * ncol(dataDF))
	ids <- rep(NA, nrow(dataDF) * ncol(dataDF))
	retRowCounter <- 1
	while( dfRowCounter < nrow(dataMat)){
		
		subDataMat <- dataMat[dfRowCounter:ifelse(dfRowCounter + numberOfDays <= nrow(dataMat), dfRowCounter + numberOfDays, nrow(dataMat)), ]
		subDates <- dates[ dfRowCounter:ifelse(dfRowCounter + numberOfDays <= nrow(dataMat), dfRowCounter + numberOfDays, nrow(dataMat))]
		
		# look for outliers
		means <- apply( subDataMat, 2, FUN= mean, na.rm = T ) 
		sds <- apply( subDataMat, 2, FUN= sd, na.rm = T ) 
		
		# determine bands
		upBand <- means + numberOfStandardDeviations * sds
		lowBand <- means - numberOfStandardDeviations * sds
		
		outliers <- subDataMat < matrix(lowBand, nrow=1)[rep(1,nrow(subDataMat)), ] | subDataMat > matrix(upBand, nrow=1)[rep(1,nrow(subDataMat)), ] 
		
		for( i in 1:ncol(subDataMat)){
			isOut <- is.finite( outliers[, i]) & as.logical(outliers[, i]) == T
			numOut <- sum(isOut) 
			
			if( numOut != 0 ){
				ret[retRowCounter:(retRowCounter+numOut-1), 1] <- subDataMat[ isOut, i ]
				ret[retRowCounter:(retRowCounter+numOut-1), 2] <- lowBand[i]
				ret[retRowCounter:(retRowCounter+numOut-1), 3] <- means[i]
				ret[retRowCounter:(retRowCounter+numOut-1), 4] <- upBand[i]
				rownames(ret)[retRowCounter:(retRowCounter+numOut-1)] <- subDates[ isOut ]
				
				ids[retRowCounter:(retRowCounter+numOut-1) ] <- columnNames[i+1]
				
				retRowCounter <- retRowCounter + numOut
			}
		}
		
		# why -1 now? because i want periods to overlap (in fact, have a point in common)
		dfRowCounter <- dfRowCounter + numberOfDays - 1
	}
	
	# remove extraneous rows
	ret <- ret[1:(retRowCounter-1), ]
	ids <- ids[1:(retRowCounter-1) ]
	
	# convert to data.frame()
	ret <- cbind(data.frame(Date=rownames(ret)), data.frame(id=ids),data.frame(value=ret[,1]), data.frame(lower.bound=ret[,2]), data.frame(mean= ret[, 3]), data.frame(upper.bound= ret[, 4]))
	
	return(ret)
}