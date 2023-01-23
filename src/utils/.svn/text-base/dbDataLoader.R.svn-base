dbDataLoader.getConnection<-function(uid,dsnName){
#establish connection
	
	#create connection
	channel<-odbcConnect(dsnName,uid=uid,pwd="31July!",case="nochange")
	return(channel)
}

dbDataLoader.getTable<-function(channel,dbTable){
# this retrieves the table dbTable. channel argument as returned by ld.getConnection()
# returns a data frame with headings equal to table headings
	
	return(sqlFetch(channel,dbTable,rownames=TRUE))
}

ld.getData<-function(channel,allIDs,dataTable,listOfIds, isRevisable, asOfDate ){
# this retrieves Date and Rate for ids in listOfIds
# NOTE: if the output of this is to be used in efficient frontier builder, the ER's should list first and then corresponding risks should be listed in the SAME order
# channel is the output from ld.getConnection(); allIDs is the data frame that we use to map id names to keys; dataTable is the db table to get values from; listOfIds are the ids of the data items; isRevisable is logical whether table has Date_Entered column or not; asOfDate only used for isRevisable and directs to get data that was entered before asOfDate
# returns a list each element is a data frame with a Date column and the values in the second column
	
	output<-list()

	inds<-match(listOfIds, allIDs$descr)
	if(any( is.na(inds))){
		stop(paste("ERROR in ld.getData(): no key was found in the header table for:", paste(listOfIds[is.na(inds)], collapse=", ")))
	}
	
	for(i in 1:length(listOfIds)){
		print(paste("INFO from ld.getData(): retrieving",listOfIds[i],"from",dataTable))
		# find this id in allIDs$descr
		index<-inds[i]
		# now find the primary key
		thisKey<-allIDs$Rate_Id[index]
		rateName<-as.character(listOfIds[i])
		
		if( !isRevisable ){
			# set things up for sql query
			selectStmnt<-paste("SELECT Date, Rate  FROM ",dataTable,  " WHERE Rate_Id =",thisKey," ORDER BY Date DESC",sep="")
			tempRates<-sqlQuery(channel,selectStmnt,stringsAsFactors=FALSE,max=0,rows_at_time=1024, as.is=T)
			tempRates$Date<-substr(tempRates$Date,1,10)
			tempRates$Rate <- as.numeric( tempRates$Rate )
		} else {
			selectStmnt<-paste("SELECT Date, Rate, Date_Entered, Label  FROM ",dataTable,  " WHERE Rate_Id =",thisKey," ORDER BY Date_Entered DESC",sep="" )
			tempRates<-sqlQuery(channel,selectStmnt,stringsAsFactors=FALSE,max=0,rows_at_time=1024, as.is=T)
			# align on labels
			if (!is.null( asOfDate )){
				# asOfDate is not null, therefore restrict to data that was entered into the table before asOfDate
				tempRates <- tempRates[ tempRates$Date_Entered <= asOfDate, ]
			}
			uniqueLabels <- unique( tempRates[["Label"]] )
			tempRates <- tempRates[  match( uniqueLabels, tempRates[["Label"]] ), ]
			tempRates$Date<-substr(tempRates$Date,1,10)	
			tempRates$Rate <- as.numeric( tempRates$Rate )
			# now push to weekdays: i have the macro table in mind where data is either monthly or quarterly
			tempRates$Date <- utils.changeToPrevWeekday( tempRates$Date )
			tempRates <- tempRates[ order(tempRates$Date, decreasing=T), ]
		}
		
		# the first column in tempRates is $Date; rename the second to the name of the data item.  NOTE: when used normally from dbDataLoader.getDataList() this data.frame() loses all columns but the first 2, "Date" and "Rate" (which is renamed here)
		names(tempRates)[2]<-rateName
		# append to output
		output<-append(output,list(tempRates))
		
	}

	return(output)
}

ld.checkNAs<-function(col,varName,dates){
# check for missing values; it checks if the missing values form one block that precedes a certain date in the past
# does not return a value
	
	nonNAs<-is.finite(col[,1])
	# rle() returns runs through the data; length(rle(nonNAs)$lengths) tells us how many continuous blocks of TRUE's and FALSE's are in nonNAs; if we have more than 2 blocks, then we know that the TRUE and FALSE blocks are intermixed; if 2 or less, we make sure we have a block of TRUE's and we use match() to make sure that the block of TRUE's is first
	firstTRUE<-match(TRUE,nonNAs)
	
	if(length(rle(nonNAs)$lengths)>2 || !is.finite(firstTRUE) || firstTRUE!=1){
		firstBAD<-match(FALSE,nonNAs)
		print(paste("WARNING in ld.checkNAs:",varName,"has good data intermixed with NA at ",dates[firstBAD]," is missing data"))
	}
}

ld.listToDataFrame<-function(dataList,dbDateFormat){
# NOTE: data with more than 2 columns, i.e., with more than just Date, value, will lose additional columns. Only 1rst and 2nd columns read and processed here (this is true in particular for macro data)
# this converts a list of data.frames, of 2 columns each, to a single data frame; it appends NA's to shorter data frames
# returns data frame with headings, Date, and the headings of the second columns of the data frames in the list
	
	# collect all dates on which any (at least one) of the variables had data
	dates<-NULL
	for(i in 1:length(dataList)){
		#union is exactly like a union of sets: if an element is in both sets, it is not duplicated in the union
		dates<-union(dates,dataList[[i]]$Date)
	}
	
	# convert to R Date object just to be able to sort the dates properly
	dates<-sort(as.Date(dates,format=dbDateFormat),decreasing=TRUE)

	dates<-dates[weekdays(dates)!="Saturday"]
	dates<-dates[weekdays(dates)!="Sunday"]	

	# start building the data frame to output
	output<-data.frame(Date=dates)
	for(i in 1:length(dataList)){
		tempDataFrame<-dataList[[i]]
		# old iDates<-sort(as.Date(tempDataFrame$Date,format=dbDateFormat),decreasing=TRUE)
		iDates<-as.Date(tempDataFrame$Date,format=dbDateFormat)
		# find out on which of the global dates this item has values
		iIndices<-is.element(dates,iDates)
		jIndices<-is.element(iDates,dates)	
		# initialize a new column to bind to output
		newCol<-data.frame(rep(NA,length(dates)))
		# iIndices are the indices in the vector dates for which this variable has values
		newCol[iIndices,]<-tempDataFrame[jIndices,2]
		# give the new column a proper name
		names(newCol)<-c(names(tempDataFrame)[2])
		ld.checkNAs(newCol,names(newCol),dates)
		# append column
		output<-cbind(output,newCol)
	}

	# write.csv(output,"c:/Users/Bloomberg/Documents/R/RobsWork/BADDATA.csv",row.names=FALSE)
	return(output)
}

ld.dataFrameToTimeSeries<-function(dataFrame){
# convert a data frame to an object of type timeSeries as required by efficient frontier code
	
	# convert data frame to timeSeries object

	tSeries<-timeSeries(dataFrame[,2:ncol(dataFrame)],charvec=dataFrame$Date)
	
	# set the headings on the time series
	names(tSeries)<-names(dataFrame[2:ncol(dataFrame)])
	return(tSeries)
}

dbDataLoader.getDataList<-function(dataList,dbSettings){
# this is the main function that puts it all together
	
	uid<-dbSettings[["ld.uid"]]
	dsnName<-dbSettings[["ld.dsnName"]]
	rateDescrTable<-dbSettings[["ld.rateDescrTable"]]
	rateDataTable<-dbSettings[["ld.rateDataTable"]]
	dbDateFormat<-dbSettings[["ld.dbDateFormat"]]
	asOfDate <- dbSettings[["ld.asOfDate"]]
	
	channel<-dbDataLoader.getConnection(uid,dsnName)
	dataDescr<-dbDataLoader.getTable(channel,rateDescrTable)
	isRevisable <- ifelse( is.element("ld.isRevisable", names( dbSettings)), dbSettings[["ld.isRevisable"]], FALSE )
	dataVals<-ld.getData(channel,dataDescr,rateDataTable,dataList, isRevisable, asOfDate )
	odbcClose(channel)
	return(ld.dataFrameToTimeSeries(ld.listToDataFrame(dataVals,dbDateFormat)))
}

ld.write.timeSeries<-function(tSeries,fileName){
# this writes a timeSeries to a file; tSeries is the timeSeries object; fileName is the file where we want the output
# doesn't return a value
	
	# convert time series tSeries to a data frame so we can use write.csv()
	dFrame<-data.frame(Date=row.names(tSeries),tSeries,row.names=NULL)
	# now write data frame dFrame the usual way
	write.csv(dFrame,fileName,row.names=FALSE)
}

dbDataLoader.pushData <- function( dataDF, uid, dsnName, headerTable, destinationTable, override, isRevisable, sourceName ){
# Note: yes, interface is different from that of dbDataLoader.getDataList() (no dbSettings). if given the time, i would change the latter to look like this 
	
	channel<-dbDataLoader.getConnection(uid,dsnName)
	data2Key<-dbDataLoader.getTable(channel,headerTable)

	if(!dbDataLoader.tableExists(channel, destinationTable )){
		stop(paste("ERROR in dbDataLoader.pushData(): table",destinationTable,"does not exist"))
	}
	
	ret <- dbDataLoader.updateTable( channel, dataDF, destinationTable, data2Key, override, isRevisable, sourceName  )
	
	odbcClose(channel)
	
	return( ret )
}

dbDataLoader.addLabels <- function( dataDF, dbLabel ){
	
	if(!is.element( "Date", names(dataDF))){
		stop("ERROR in dbDataLoader.addLabels(): data frame must have a 'Date' column.")
	}
	
	if(dbLabel =="YQ"){
		dataDF <- cbind(dataDF, data.frame(Label= paste(substr(dataDF[["Date"]], 1, 4), quarters( as.Date( dataDF[["Date"]])), sep="" )))
	} else if (dbLabel == "YM"){
		dataDF <- cbind(dataDF, data.frame(Label=substr(dataDF[["Date"]], 1, 7)))
	} else if(dbLabel=="DD"){
		dataDF <- cbind(dataDF, data.frame(Label=dataDF[["Date"]]))
	} else {
		stop(paste("ERROR in  dbDataLoader.addLabels(): dbLabel", dbLabel, "is not recognised."))
	}
	
	return( dataDF )
}

dbDataLoader.tableExists <- function( channel, tableName ){
	return( is.finite( match( tableName, sqlTables( channel )[["TABLE_NAME"]])))
}

dbDataLoader.updateTable <- function( channel, dataDF, tableName, data2Key, override, isRevisable, sourceName  ){
# ACHTUNG: some column names are hard-coded below, "name", "Date", "Source", "Rate_Id"
	
	if(!is.data.frame( dataDF) || !is.element("Date", names(dataDF)) || !is.element("name", names(dataDF))){
		stop("ERROR in dbDataLoader.updateTable(): the data must be in a data.frame() and must have columns named 'Date' and 'name'.")
	} else if ( isRevisable && ( !is.element("Date_Entered", names(dataDF) ) || !is.element("Label", names(dataDF))) ){
		stop("ERROR in dbDataLoader.updateTable(): the table is revisable, therefore the data.frame() must have columns named 'Date_Entered' and 'Label'.")
	}
	
	# order dates in increasing order
	dataDF <- dataDF[ order( dataDF[["Date"]], decreasing = F), ]
	numRows<- nrow(dataDF )
	
	descrInds <- match(dataDF[["name"]], data2Key[["descr"]])
	# return info on which securities were loaded and which were not
	ret <- list(loaded=dataDF[["name"]][ is.finite(descrInds)], not.loaded= dataDF[["name"]][ !is.finite(descrInds)])
	if( sum( is.finite(descrInds)) != numRows ){
		print(paste("WARNING in dbDataLoader.updateTable(): these data names are not in the header table in the database : ", paste( unique(dataDF[["name"]][ !is.finite(descrInds)]), collapse=", ")))
		
		# now proceed without those
		dataDF <- dataDF[ is.finite(descrInds), ]
		descrInds <- descrInds[ is.finite(descrInds)]
		numRows<- nrow(dataDF )
	}
	
	# remove the "name" column and add "Rate_Id" and "Source" to match the DB table. Note: hardcoded stuff
	dataDF <- cbind( data.frame(Rate_Id= data2Key[ descrInds, "Rate_Id"]), data.frame(Source=rep(sourceName, numRows )), dataDF[, setdiff(names(dataDF), "name")] )
	if(isRevisable && !is.element("Is_Revisable", names(dataDF))){
		dataDF <- cbind( dataDF, data.frame(Is_Revisable= rep(1, nrow(dataDF))))
	}
	
	nKeys <- unique( dataDF[, "Rate_Id"])
	for ( nKey in nKeys ) {
		nKeyDF <- dataDF[ dataDF[["Rate_Id"]] == nKey, ]
		n <- data2Key[ data2Key[["Rate_Id"]] == nKey, "descr"]
		if( !isRevisable ){
			dateQuery <- paste("SELECT Date FROM ",tableName,"WHERE Rate_Id =", nKey )
			nDates <- substr( sqlQuery(channel, dateQuery, stringsAsFactors=FALSE, max=0, rows_at_time=1024 )[["Date"]], 1, 10 )
			isNewDateInds <- !is.element( nKeyDF[["Date"]], nDates )
		} else {
			dateQuery <- paste("SELECT Label, Date_Entered, Is_Revisable, Rate FROM ",tableName,"WHERE Rate_Id =", nKey, " ORDER BY Date_Entered DESC" )
			queryResults <- sqlQuery(channel, dateQuery, stringsAsFactors=FALSE, max=0, rows_at_time=1024 )
			nDates <- substr( queryResults[["Date_Entered"]], 1, 10 )
			
			intersectLabels <- intersect( nKeyDF[["Label"]], queryResults[["Label"]] )
			dfInds <- match( intersectLabels, nKeyDF[["Label"]] )
			dbInds <- match( intersectLabels, queryResults[["Label"]] )
			hasOldValue <- rep(F, nrow(nKeyDF ))
			hasOldValue[ dfInds ] <-  nKeyDF[["Rate"]][ dfInds ] == queryResults[["Rate"]][ dbInds ]
			
			isNewDateInds <- !is.element( nKeyDF[["Date_Entered"]], nDates ) & !is.element(nKeyDF[["Label"]], queryResults[["Label"]][ queryResults[["Is_Revisable"]] == 0 ]) & !hasOldValue
		}
		numNewDates <- sum( isNewDateInds )
		
		if( numNewDates != 0 ){
			retCode <- dbDataLoader.save( channel, tableName, nKeyDF[ isNewDateInds, ] )
			if ( retCode != 1 ){
				stop("ERROR in dbDataLoader.updateTable(): sqlSave() did not succeed...")
			}
		}
		
		if(  numNewDates != nrow(nKeyDF) ){
			if(isRevisable && override){
				print(paste("WARNING in dbDataLoader.updateTable():",n,"already had data for some of the dates. we do NOT allow overriding for revisable tables"))
			} else {
				print(paste("WARNING in dbDataLoader.updateTable():",n,"already had data for some of the dates.",ifelse(override,"overriding.","NO overriding.")))
				if (override && !isRevisable){
					retCode <- dbDataLoader.update( channel, tableName, nKeyDF[ !isNewDateInds, ], c( "Rate_Id", "Date" ) )
					if ( retCode != 1 ){
						stop("ERROR in dbDataLoader.updateTable(): sqlUpdate() did not succeed...")
					}
				}
			}
		}
	}
	
	return( ret )
}

dbDataLoader.save <- function( channel, tableName, dataFrame ){
	#
	# NOTE: for debugging set verbose to T
	#
	if( is.element( "debug.dbDataLoader.save", ls(.GlobalEnv))){
		verbose <- debug.dbDataLoader.save
	} else {
		verbose <- F
	}

	return( sqlSave( channel, dataFrame, tablename=tableName, append=T, rownames=F, safer=T ,verbose = verbose) )
}

dbDataLoader.update <- function( channel, tableName, dataFrame, primaryKeys ){
	return( sqlUpdate( channel, dataFrame, tablename = tableName, index = primaryKeys ) )
}

dbDataLoader.close <- function( channel ){
	odbcClose(channel)
}