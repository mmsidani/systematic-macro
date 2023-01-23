# driver for dataProcessing/earnFromPE.R
earnFromPE.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/earnFromPE.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.earnFromPEExecSettings()
	input<-earnFromPE.input(settings)
	output<-earnFromPE.calculate(input,settings)
	earnFromPE.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
earnFromPE.main()
