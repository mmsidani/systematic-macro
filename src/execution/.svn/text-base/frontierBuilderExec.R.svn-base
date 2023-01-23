frontierBuilderExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")

	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/frontierBuilder.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/varianceCovariance.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/hmOpt.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/madOpt.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/mvOpt.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/downsideOpt.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/specialPortfolios.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/constructionMethods.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/fbUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	
	library(timeSeries)
	library(quadprog)
	library(limSolve)
	library(RODBC)

	settings<-global.frontierBuilderExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/",settings[["fbex.constraints"]],sep=""))
	portfolioNames<-constraintsFiles.portfolioNames()
	for(portfolioName in portfolioNames){
		settings[["portfolioName"]]<-portfolioName
		
		input<-frontierBuilder.input(settings)
		output<-frontierBuilder.calculate(input,settings)
		frontierBuilder.output(output,settings)
	}
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
frontierBuilderExec.main()

