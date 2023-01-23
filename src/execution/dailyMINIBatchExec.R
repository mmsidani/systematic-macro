dailyBatchExec.main<-function(){
	
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode <- paste( pathToMe, "/../", sep = "")
	
	source(paste(pathToRSourceCode, "settings/GlobalVariables.R", sep = ""))
	source(paste(pathToRSourceCode,"marketData/bloombergData.R", sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R", sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R", sep=""))
	source(paste(pathToRSourceCode,"utils/algorithms.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"utils/draw3.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/securityClasses.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/totalReturn.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/earnFromPE.R",sep=""))	
	source(paste(pathToRSourceCode,"dataProcessing/slopes.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/predict.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/eurolegacy.R",sep=""))
	source(paste(pathToRSourceCode,"dataProcessing/dailyTotalCapitalRets.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/frontierBuilder.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/varianceCovariance.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/specialPortfolios.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/constructionMethods.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/fbUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/mvOpt.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimulation.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimStats.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/leverageIt.R", sep = ""))
	source(paste(pathToRSourceCode,"portfolioManagement/rebalancing.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/reinvesting.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/transactions.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/multiStrategy.R",sep=""))
	source(paste(pathToRSourceCode,"riskManagement/var.R",sep=""))
	
	library(MASS)
	library(RODBC)
	library(timeSeries)
	library(quadprog)
	library(limSolve)
	library(RODBC)
	
	settings <- global.dailyBatchExecSettings()
	
	filesDir <- settings[["dabaex.outputFilesDir"]]
	sourceName <-  settings[["dabaex.sourceName"]]
	override <- settings[["dabaex.override"]]
	dbSettings <- settings[["dbSettings"]]

	#frontier builder
	settings <- global.frontierBuilderExecSettings()
	settings[["fbex.methods"]] <- c("MV","MVU")
	settings[["fbex.parametersList"]] <- list(MVU=c(50,200,350,450))
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	portfolioNames<-"Extended"
	for(portfolioName in portfolioNames){
		settings[["portfolioName"]]<-portfolioName
		
		input<-frontierBuilder.input(settings)
		output<-frontierBuilder.calculate(input,settings)
		frontierBuilder.output(output,settings)
	}

	#simulate
	settings <- global.portfolioSimulationExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	settings[["reb.euclidHurdle"]] <- .1
	settings[["reb.erRiskRatioHurdle"]]<-0.0025
	allReallocDecisions<-c("erRiskRatio", "euclidean", "euclidean", "euclidean", "euclidean", "euclidean")
	allDesignators <-c("M","M","MVU.50","MVU.200","MVU.350","MVU.450")		
	for ( i in 1:length( allReallocDecisions)){
		settings[["psex.portfolioDesignators"]] <- allDesignators[i]
		settings[["reb.reallocDecision"]] <- allReallocDecisions[i]
		for(portfolioName in portfolioNames){
			settings[["portfolioName"]]<-portfolioName
			input<-portfolioSimulation.input(settings)
			output<-portfolioSimulation.calculate(input,settings)
			portfolioSimulation.output(output,settings)
		}
	}
	
	#frontier builder
	settings <- global.frontierBuilderExecSettings()
	settings[["fbex.methods"]] <- c("MV","MVU")
	settings[["fbex.parametersList"]] <- list(MVU=c(450))
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/",settings[["fbex.constraints"]],sep=""))
	portfolioNames<-"Base"
	for(portfolioName in portfolioNames){
		settings[["portfolioName"]]<-portfolioName
		
		input<-frontierBuilder.input(settings)
		output<-frontierBuilder.calculate(input,settings)
		frontierBuilder.output(output,settings)
	}
	
	#simulate
	settings <- global.portfolioSimulationExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	settings[["reb.euclidHurdle"]] <- .1
	settings[["reb.erRiskRatioHurdle"]]<-0.0025
	allReallocDecisions<-c("erRiskRatio", "euclidean")
	allDesignators <-c("M","MVU.450")		
	for ( i in 1:length( allReallocDecisions)){
		settings[["psex.portfolioDesignators"]] <- allDesignators[i]
		settings[["reb.reallocDecision"]] <- allReallocDecisions[i]
		for(portfolioName in portfolioNames){
			settings[["portfolioName"]]<-portfolioName
			input<-portfolioSimulation.input(settings)
			output<-portfolioSimulation.calculate(input,settings)
			portfolioSimulation.output(output,settings)
		}
	}
	
	# multiStrat's
	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("Extended-MVU.450-Monday.euclidean","Extended-MVU.50-Monday.euclidean")
	settings[["msex.policy"]]<-"msex.policyProgressiveRisk1"
	settings[["msex.progressiveThresholds1"]] <- c(.2)
	settings[["msex.controlFrequency"]] <- "weekly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.prog1.450.50.p20"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("Base-MVU.450-Monday.euclidean","Extended-MVU.50-Monday.euclidean")
	settings[["msex.policy"]]<-"msex.policyProgressiveRisk1"
	settings[["msex.progressiveThresholds1"]] <- c(.2)
	settings[["msex.controlFrequency"]] <- "weekly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.prog1.450-Base.50.p20"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("Base-M-Monday.erRiskRatio","Extended-M-Monday.erRiskRatio")
	settings[["msex.policy"]]<-"msex.policyProgressiveRisk1"
	settings[["msex.progressiveThresholds1"]] <- c(.1)
	settings[["msex.controlFrequency"]] <- "weekly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.prog1.son.of.taro"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )	

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("Extended-MVU.450-Monday.euclidean","Extended-MVU.350-Monday.euclidean","Extended-MVU.200-Monday.euclidean","Extended-MVU.50-Monday.euclidean")
	settings[["msex.policy"]]<-"msex.policyProgressiveRisk1"
	settings[["msex.progressiveThresholds1"]] <- c(.075,.15,.2)
	settings[["msex.controlFrequency"]] <- "weekly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.prog1.450.350.p075.200.p15.50.p20"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("Base-MVU.450-Monday.euclidean","Extended-MVU.350-Monday.euclidean","Extended-MVU.200-Monday.euclidean","Extended-MVU.50-Monday.euclidean")
	settings[["msex.policy"]]<-"msex.policyProgressiveRisk1"
	settings[["msex.progressiveThresholds1"]] <- c(.075,.15,.2)
	settings[["msex.controlFrequency"]] <- "weekly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.prog1.450-Base.350.p075.200.p15.50.p20"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	
	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450.50.p20.policyProgressiveRisk1","Extended-M-Monday.erRiskRatio")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -100
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.TARO.JIRO"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450-Base.50.p20.policyProgressiveRisk1","production.prog1.son.of.taro.policyProgressiveRisk1")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -100
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.SON.OF.TARO.JIRO-Base"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )
	
	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450.50.p20.policyProgressiveRisk1")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -.06
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.JIRO.STOPLOSS-6"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450.350.p075.200.p15.50.p20.policyProgressiveRisk1")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -.06
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.SON.of.JIRO.STOPLOSS-6"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )
	
	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.TARO.JIRO.policyDownAndSuspend")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -0.06
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.mixed.taro.jiro"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )
	

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450.350.p075.200.p15.50.p20.policyProgressiveRisk1","Extended-M-Monday.erRiskRatio")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -100
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.TARO.SON.of.JIRO"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.prog1.450-Base.350.p075.200.p15.50.p20.policyProgressiveRisk1","production.prog1.son.of.taro.policyProgressiveRisk1")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -100
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.SON.OF.TARO.SON.of.JIRO-Base"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )
	
	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.TARO.SON.of.JIRO.policyDownAndSuspend")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -0.06
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.mixed.taro.son.of.jiro"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )

	settings <- global.multiStrategyExecSettings()
	settings[["msex.portfolios"]]<-c("production.SON.OF.TARO.SON.of.JIRO-Base.policyDownAndSuspend")
	settings[["msex.policy"]]<-"msex.policyDownAndSuspend"
	settings[["msex.downThreshold"]]  <- -0.06
	settings[["msex.controlFrequency"]] <- "monthly"
	settings[["msex.reallocationTimeUnit"]] <-"year"
	settings[["msex.outputFileName"]] <-"production.mixed.son.of.taro.son.of.jiro-Base"
	input <- multiStrategy.input(settings)
	output <- multiStrategy.calculate(input, settings)
	multiStrategy.output(output, settings )
	
	settings<- global.leverageItExecSettings()
	settings[["lit.portfolioSimFile"]] <- "production.mixed.taro.son.of.jiro.policyDownAndSuspend.sim.csv"
	settings[["lit.leverage"]] <- 2.0
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )
	
	# leverage it
	settings<- global.leverageItExecSettings()
	settings[["lit.portfolioSimFile"]] <- "production.JIRO.STOPLOSS-6.policyDownAndSuspend.sim.csv"
	settings[["lit.leverage"]] <- 2.0
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )
	
	# leverage it
	settings<- global.leverageItExecSettings()
	settings[["lit.portfolioSimFile"]] <- "production.mixed.taro.jiro.policyDownAndSuspend.sim.csv"
	settings[["lit.leverage"]] <- 2.0
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )

	# leverage it
	settings<- global.leverageItExecSettings()
	settings[["lit.portfolioSimFile"]] <- "production.SON.of.JIRO.STOPLOSS-6.policyDownAndSuspend.sim.csv"
	settings[["lit.leverage"]] <- 2.0
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )

	# leverage it
	settings<- global.leverageItExecSettings()
	settings[["lit.portfolioSimFile"]] <- "production.mixed.son.of.taro.son.of.jiro-Base.policyDownAndSuspend.sim.csv"
	settings[["lit.leverage"]] <- 2.0
	input <- leverageIt.input( settings)	
	output <- leverageIt.calculate(input, settings )
	leverageIt.output( output, settings )
	
	# sim stats
	settings <- global.portfolioSimStatsExecSettings()
	settings[["psstats.portfolioNames"]]<-c("production.mixed.taro.jiro.policyDownAndSuspend.leveraged.2",
	"production.mixed.taro.jiro.policyDownAndSuspend",
	"production.TARO.JIRO.policyDownAndSuspend",
	"production.prog1.450.50.p20.policyProgressiveRisk1",
	"Base-M-Monday.erRiskRatio",
	"Extended-M-Monday.erRiskRatio",
	"Extended-M-Monday.euclidean",
	"Base-MVU.450-Monday.euclidean",
	"Extended-MVU.450-Monday.euclidean",
	"Extended-MVU.350-Monday.euclidean",
	"Extended-MVU.200-Monday.euclidean",
	"Extended-MVU.50-Monday.euclidean",
	"production.JIRO.STOPLOSS-6.policyDownAndSuspend",
	"production.JIRO.STOPLOSS-6.policyDownAndSuspend.leveraged.2",
	"production.prog1.450.350.p075.200.p15.50.p20.policyProgressiveRisk1",
	"production.SON.of.JIRO.STOPLOSS-6.policyDownAndSuspend",
	"production.SON.of.JIRO.STOPLOSS-6.policyDownAndSuspend.leveraged.2",
	"production.mixed.taro.son.of.jiro.policyDownAndSuspend.leveraged.2",
	"production.mixed.taro.son.of.jiro.policyDownAndSuspend",
	"production.TARO.SON.of.JIRO.policyDownAndSuspend",
	"production.prog1.son.of.taro.policyProgressiveRisk1",
	"production.prog1.450-Base.350.p075.200.p15.50.p20.policyProgressiveRisk1",
	"production.SON.OF.TARO.JIRO-Base.policyDownAndSuspend",
	"production.mixed.son.of.taro.son.of.jiro-Base.policyDownAndSuspend",
	"production.mixed.son.of.taro.son.of.jiro-Base.policyDownAndSuspend.leveraged.2")
	settings[["psstats.numPortfoliosToPlot"]]<-length(settings[["psstats.portfolioNames"]])
	settings[["psstats.doAssetClassStats"]]<-FALSE
	settings[["psstats.plotPeriods"]]<-c(-1,5)
	settings[["psstats.areaPlotTitle"]]<-"SA2 Bellwether Asset Allocation"
	settings[["psstats.barPlotTitle"]]<-"SA2 Bellwether Asset Contribution"
	settings[["psstats.graphicsDevice"]]<-"png"
	input <- portfolioSimStats.input(settings)
	output <- portfolioSimStats.calculate(input, settings)
	portfolioSimStats.output(output, settings)
	
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
dailyBatchExec.main()