uploadToRateHeaderTableExec.main <- function(){
	pathToMe <- dirname( sys.frame( 1)$ofile )
	pathToRSource <- paste( pathToMe, "/../", sep = "")
	
	source(paste(pathToRSource, "settings/GlobalVariables.R", sep=""))
	source(paste(pathToRSource, "utils/dbDataLoader.R", sep="" ))
	
	library(RODBC)
	
	settings <- global.uploadToRateHeaderTableExecSettings()
	dbSettings <- settings[["dbSettings"]]
	inputDir <- settings[["urht.inputDir"]]
	dataNamesFile <- settings[["urht.dataNamesFile"]]

	dataNamesFile <- paste(inputDir, dataNamesFile, sep="")
	
	newDataNames <- as.character( read.csv( dataNamesFile )[, 1 ] )
	channel <- dbDataLoader.getConnection(dbSettings[["ld.uid"]], dbSettings[["ld.dsnName"]] )	
	
	rate2Key <- dbDataLoader.getTable(channel, dbSettings[["ld.rateDescrTable"]] )
		
	alreadyInTable <- is.element( newDataNames, rate2Key[["descr"]] )
	if( sum( alreadyInTable ) != 0 ){
		print(paste("WARNING in uploadToRateHeaderTableExec.main(): the following data names are already in", dbSettings[["ld.rateDescrTable"]], ":", paste(newDataNames[alreadyInTable], collapse = ", ")))
	}
	
	numNew <- sum( !alreadyInTable )
	if( numNew != 0){
		
		print(paste("INFO from uploadToRateHeaderTableExec.main(): adding", numNew,"items to header table. they are:",paste(newDataNames[!alreadyInTable], collapse = ", ")))
		
		newItems <- paste("'",newDataNames[ !alreadyInTable ],"'",sep="")
		
		for (i in 1:length(newItems)){
			# NOTE: the Rate_Id column is assumed (and is in the case of tbl_rate_header) to be an IDENTITY column, therefore we only specify the descr, 
			insertStmt <- paste("INSERT INTO", dbSettings[["ld.rateDescrTable"]], "(descr) VALUES(", newItems[i] ,")")
			
			retCode <- sqlQuery(channel, insertStmt, stringsAsFactors=FALSE, max=0, rows_at_time=1024 )
		}
	
		if( length( retCode ) != 0 ){
			print(retCode)
		}
	} else {
		print("INFO from uploadToRateHeaderTableExec.main(): no new items to add.")
	}
	
	rate2Key <- dbDataLoader.getTable(channel, dbSettings[["ld.rateDescrTable"]] )
	allRecords <- rate2Key[, "descr"]
	uniqueRecords <- unique( allRecords )
	
	# inds are the indices of the first occurrences 
	inds <- match(uniqueRecords, allRecords)
	# whatever element is left after we have removed its first occurrence must be a duplicate
	duplicates <- unique( allRecords[!is.element( 1:length(allRecords ), inds)] )
	if( length(duplicates) == 0 ){
		print(paste("INFO from uploadToRateHeaderTableExec.main(): there are",length(allRecords),"records in",dbSettings[["ld.rateDescrTable"]],"and there are no duplicates."))
	} else {
		print(paste("WARNING from uploadToRateHeaderTableExec.main(): duplicates were found. these are duplicated:"))
		print(matrix(duplicates, ncol=1) )
		print("do you want to remove duplications? (type yes to remove duplications and keep one instance of each")
		response <- scan( n = 1, what=character() )
		if( response == "yes"){
			for (i in 1:length(duplicates)){
				inds <- grep( duplicates[i], allRecords )
				rateIds <- rate2Key[inds, 1]
				# we're only here if length(rateIds) > 1
				for( j in 1:(length(rateIds) - 1)){
					deleteStmt <- paste("DELETE FROM", dbSettings[["ld.rateDescrTable"]], "WHERE Rate_Id=",rateIds[j])
					sqlQuery(channel, deleteStmt, stringsAsFactors=FALSE, max=0, rows_at_time=1024 )
				}
			}
		}
	}
	
	dbDataLoader.close( channel )
}

# execute
uploadToRateHeaderTableExec.main()



