# driver for dataProcessing/corps.R
corpsExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/corps.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.corpsExecSettings()
	input<-corps.input(settings)
	output<-corps.calculate(input,settings)
	corps.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
corpsExec.main()