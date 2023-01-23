buildSimulateExec.main<-function(){
	# source global variables
	pathToMe<-dirname(sys.frame(1)$ofile)

	print("now doing frontierBuilder")
	source(paste(pathToMe,"/frontierBuilderExec.R",sep=""))
		
	print("now doing portfolioSimulation")
	source(paste(pathToMe,"/portfolioSimulationExec.R",sep=""))
	
	print("now doing portfolioSimStats")
	source(paste(pathToMe,"/portfolioSimStatsExec.R",sep=""))
	
	print(paste(sys.frame(1)$ofile,"done."))
}

#execute
buildSimulateExec.main()

