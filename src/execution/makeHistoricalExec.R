# driver for dataProcessing/makeHistorical.R
makeHistoricalExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")

	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/algorithms.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/makeHistorical.R",sep=""))

	library(timeSeries)
	
	settings<-global.makeHistoricalExecSettings()
	input<-makeHistorical.input(settings)
	output<-makeHistorical.calculate(input,settings)
	makeHistorical.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
makeHistoricalExec.main()
