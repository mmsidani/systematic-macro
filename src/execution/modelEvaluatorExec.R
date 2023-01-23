modelEvaluatorExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/modelEvaluator.R",sep=""))
	
	library(timeSeries)
	library(RODBC)

	settings<-global.modelEvaluatorExecSettings()
	input<-modelEvaluator.input(settings)
	output<-modelEvaluator.calculate(input,settings)
	modelEvaluator.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
modelEvaluatorExec.main()