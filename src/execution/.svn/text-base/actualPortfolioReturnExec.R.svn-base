actualPortfolioReturnExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/actualPortfolioReturn.R",sep=""))
	
	library(timeSeries)
	
	settings<-global.actualPortfolioReturnExecSettings()
	input<-actualPortfolioReturn.input(settings)
	output<-actualPortfolioReturn.calculate(input,settings)
	actualPortfolioReturn.output(output,settings)

	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
actualPortfolioReturnExec.main()
