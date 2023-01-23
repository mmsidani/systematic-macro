createLivePXExec.main <- function(){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	source(paste(pathToMe,"/../portfolioManagement/createLivePX.R",sep=""))
	
	settings <- global.createLivePXExecSettings()
	settings[["pathToRSourceCode"]]<- paste(pathToMe, "/../", sep="")

	input <- createLivePX.input( settings )
	output <- createLivePX.calculate( input, settings )
	createLivePX.output( output, settings)

	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
createLivePXExec.main()