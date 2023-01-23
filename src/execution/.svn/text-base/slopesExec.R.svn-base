#driver for dataProcessing/slopes.R
slopesExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/slopes.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.slopesExecSettings()
	input<-slopes.input(settings)
	output<-slopes.calculate(input,settings)
	slopes.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
slopesExec.main()
