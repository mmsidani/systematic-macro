# driver for analysis/macroBucket.R
macroBucketExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")

	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/macroBucket.R",sep=""))
	
	library(timeSeries)
	
	settings<-global.macroBucketExecSettings()
	input<-macroBucket.input(settings)
	output<-macroBucket.calculate(input,settings)
	macroBucket.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
macroBucketExec.main()