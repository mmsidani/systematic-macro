correlationsExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSource<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSource,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSource,"analysis/correlations.R",sep=""))
	source(paste(pathToRSource,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSource,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSource,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSource,"portfolioConstruction/varianceCovariance.R",sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	settings<-global.correlationsExecSettings()
	settings[["pathToRSource"]]<-pathToRSource
	input<-correlations.input(settings)
	output<-correlations.calculate(input,settings)
	correlations.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
correlationsExec.main()