portfolioMixerExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")

	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/portfolioMixer.R",sep=""))
	
	library(limSolve)
	library(timeSeries)
	library(RODBC)

	settings<-global.portfolioMixerExecSettings()
	input<-portfolioMixer.input(settings)
	output<-portfolioMixer.calculate(input,settings)
	portfolioMixer.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
portfolioMixerExec.main()