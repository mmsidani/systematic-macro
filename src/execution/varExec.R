varExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"riskManagement/var.R",sep=""))
	
	library(timeSeries)

	settings<-global.varExecSettings()
	input<-var.input(settings)
	output<-var.calculate(input,settings)
	var.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
varExec.main()