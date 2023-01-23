positionsLoadExec.main <- function(){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe, "/../settings/GlobalVariables.R", sep=""))
	source(paste(pathToMe, "/../utils/dbDataLoader.R", sep=""))
	
	library(RODBC)

	settings <- global.positionsLoadExecSettings()
	inputDir <- settings[["plex.inputDir"]]
	positionsTableSettings <- settings[["plex.positionsTableSettings"]]
	primaryKeys <- settings[["plex.primaryKeys"]]
	
	print(paste("INFO from positionsLoadExec.main(): you will be loading the positions file to",positionsTableSettings[["positionsTableName"]],"in",positionsTableSettings[["dsnName"]]))
	print(paste("REQUEST from positionsLoadExec.main(): if the positions file is in", inputDir,"enter 1 otherwise enter anything other than 1"	))
	response <- scan(what=character(),n=1)
	if(response == 1){
		print("REQUEST from positionsLoadExec.main(): enter positions file name")
		fileName <- scan(what=character(),n=1)
		fileName <- paste(inputDir, fileName, sep="")
	} else {
		print("REQUEST from positionsLoadExec.main(): enter the FULL path to the positions file name")
		fileName <- scan(what=character(),n=1)
	}
	
	if( !file.exists(fileName) ){
		stop(paste("ERROR in positionsLoadExec.main():",fileName,"does not exist."))
	}
	
	positionsToLoad <- read.csv(fileName, stringsAsFactors=F)
	
	print(paste("INFO from positionsLoadExec.main(): Here are the positions that will be loaded."))
	print(positionsToLoad)
	print(paste("QUESTION from positionsLoadExec.main(): Type 1 if you are loading NEW positions (with NO possibility to override existing ones)"))
	print(paste("                                        Type 2 if you are loading NEW and/or updating OLD positions (with override)"))
	print(paste("                                        Type 3 if you want to cancel"))
	response <- scan(what=character(),n=1) 
	channel <- dbDataLoader.getConnection( positionsTableSettings[["uid"]], positionsTableSettings[["dsnName"]])	
	if(response == 1){
		retCode <- dbDataLoader.save( channel, positionsTableSettings[["positionsTableName"]], positionsToLoad )
	} else if( response == 2){
		retCode <- dbDataLoader.update( channel, positionsTableSettings[["positionsTableName"]], positionsToLoad, primaryKeys )
	} else {
		if (response ==3){
			print("canceling...")
		} else {
			print(paste("you typed:", response,". exiting...."))
		}
		dbDataLoader.close(channel)
		return()
	}
	
	dbDataLoader.close(channel)
	if(retCode != 1){
		print("ERROR in positionsLoadExec.main(): dbDataLoader.save() or dbDataLoader.update() returned an error")
		print(retCode)
		return()
	}
	
	print( paste(sys.frame(1)$ofile,"done.") )
}

# execute
positionsLoadExec.main()