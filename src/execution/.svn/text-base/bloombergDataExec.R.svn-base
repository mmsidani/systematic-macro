bloombergDataExec.main <- function(){
	# source global variables
	pathToMe<-dirname(sys.frame(1)$ofile)
	
	pathToRSourceCode <- paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"marketData/bloombergData.R", sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R", sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R", sep=""))
	
	library(RODBC)
	library(timeSeries)
	
	settings <- global.bloombergDataExecSettings()
	
	settings[["pathToRSourceCode"]] <- pathToRSourceCode
	input <- bloombergData.input( settings )
	output <- bloombergData.calculate( input, settings)
	bloombergData.output( output, settings )
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
bloombergDataExec.main()