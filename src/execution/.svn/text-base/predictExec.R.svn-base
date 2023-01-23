# driver for forecasting/predict.R which calculates er's/risks for bonds/equities
predictExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode, "utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode, "utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode, "utils/algorithms.R",sep=""))
	source(paste(pathToRSourceCode, "utils/kalman1d.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/equityReturnModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/equityRiskModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/bondReturnModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/bondRiskModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/tipsReturnModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/tipsRiskModels.R",sep=""))
	source(paste(pathToRSourceCode, "forecasting/predict.R",sep=""))
	
	library(MASS)
	library(timeSeries)
	library(RODBC)

	settings<-global.predictExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	
	input<-predict.input(settings)
	output<-predict.calculate(input,settings)
	predict.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
predictExec.main()

