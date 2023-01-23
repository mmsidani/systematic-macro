wbDataExec.main <- function(){
	pathToMe <- dirname( sys.frame(1)$ofile )
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	source(paste(pathToMe,"/../marketData/wbData.R", sep=""))

	settings <- global.wbDataExecSettings()
	input <- wbData.input(settings)
	output <- wbData.calculate(input, settings)
	wbData.output(output, settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
wbDataExec.main()