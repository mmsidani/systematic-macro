eurolegacyExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/eurolegacy.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.eurolegacyExecSettings()
	input<-eurolegacy.input(settings)
	output<-eurolegacy.calculate(input,settings)
	eurolegacy.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

#execute
eurolegacyExec.main()
