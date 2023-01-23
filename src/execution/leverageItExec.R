leverageItExec.main <- function(){
	pathToMe <- dirname( sys.frame(1)$ofile )
	pathToRSource <- paste(pathToMe, "/../", sep = "")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"simulation/leverageIt.R", sep = ""))
	source(paste(pathToRSource,"utils/miscUtils.R", sep = ""))
	source(paste(pathToRSource,"utils/dbDataLoader.R", sep = ""))
	
	library(timeSeries)
	library(RODBC)
	
	settings <- global.leverageItExecSettings()
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )
}

# execute
leverageItExec.main()