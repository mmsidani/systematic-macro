makeForwardBackwardExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/makeForwardBackward.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.makeForwardBackwardExecSettings()
	input<-makeForwardBackward.input(settings)
	output<-makeForwardBackward.calculate(input,settings)
	makeForwardBackward.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
makeForwardBackwardExec.main()
