dbToFileExec.main<-function(){
	# source global variables
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbToFile.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.dbToFileExecSettings()
	input <- dbToFile.input( settings )
	output <- dbToFile.calculate( input, settings )
	dbToFile.output( output, settings )
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
dbToFileExec.main()