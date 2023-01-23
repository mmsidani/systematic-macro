makeMacroBucketsExec.main<-function(){
	# source global variables
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/makeMacroBuckets.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.makeMacroBucketsExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	input<-makeMacroBuckets.input(settings)
	output<-makeMacroBuckets.calculate(input,settings)
	makeMacroBuckets.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
makeMacroBucketsExec.main()