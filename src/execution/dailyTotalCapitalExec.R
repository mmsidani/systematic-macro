# driver for dataProcessing/dailyTotalCapitalRets.R
dailyTotalCapitalExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"dataProcessing/dailyTotalCapitalRets.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.dailyTotalCapitalExecSettings()
	input<-dailyTotalCapitalRets.input(settings)
	output<-dailyTotalCapitalRets.calculate(input,settings)
	dailyTotalCapitalRets.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
dailyTotalCapitalExec.main()