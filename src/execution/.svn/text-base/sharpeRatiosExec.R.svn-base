sharpeRatiosExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/sharpeRatios.R",sep=""))
	
	library(timeSeries)

	settings<-global.sharpeRatiosExecSettings()
	input<-sharpeRatios.input(settings)
	output<-sharpeRatios.calculate(input,settings)
	sharpeRatios.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
sharpeRatiosExec.main()