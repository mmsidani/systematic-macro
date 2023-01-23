# driver for dataProcessing/totalReturn.R
totalReturnExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/totalReturn.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.totalReturnExecSettings()
	input<-totalReturn.input(settings)
	output<-totalReturn.calculate(input,settings)
	totalReturn.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
totalReturnExec.main()