portfolioSimStatsExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimStats.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/draw3.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/securityClasses.R",sep=""))

	library(timeSeries)
	library(RODBC)
	
	settings<-global.portfolioSimStatsExecSettings()
	input<-portfolioSimStats.input(settings)
	output<-portfolioSimStats.calculate(input,settings)
	portfolioSimStats.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
portfolioSimStatsExec.main()
