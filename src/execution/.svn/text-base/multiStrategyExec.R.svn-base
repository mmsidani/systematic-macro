multiStrategyExec.main<-function(){

	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/multiStrategy.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/reinvesting.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/transactions.R",sep=""))
	
	library(timeSeries)
	library(RODBC)

	settings <-global.multiStrategyExecSettings()
	input<-multiStrategy.input(settings)
	output<-multiStrategy.calculate(input,settings)
	multiStrategy.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

#execute
multiStrategyExec.main()