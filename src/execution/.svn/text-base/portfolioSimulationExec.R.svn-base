portfolioSimulationExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"simulation/portfolioSimulation.R",sep=""))
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/transactions.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/varianceCovariance.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/rebalancing.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/reinvesting.R",sep=""))

	library(timeSeries)
	library(RODBC)

	settings<-global.portfolioSimulationExecSettings()
	settings[["pathToRSourceCode"]]<-pathToRSourceCode
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/",settings[["psex.constraints"]],sep=""))
	portfolioNames<-constraintsFiles.portfolioNames()
	allDesignators <- settings[["psex.portfolioDesignators"]]
	allReallocDecisions <- settings[["reb.reallocDecision"]]
	if( length( allReallocDecisions ) > 1 && length( allReallocDecisions ) != length( allDesignators ) && length( allDesignators ) !=0 ){
		stop(paste("ERROR in portfolioSimulationExec.main(): number of elements of reb.reallocDecision must be 1 or equal to number of elements of psex.portfolioDesignators in GlobalVariables if not 0. I have", length( allReallocDecisions ),"and",length( allDesignators ) ))
	} else if ( length( allReallocDecisions ) > 1 && length( allDesignators ) == 0 ) {
		print("WARNING in portfolioSimulationExec.main(): more than 1 realloc decision and 0 designator. Only first realloc decision will be used.")
		settings[["reb.reallocDecision"]] <- allReallocDecisions[1]
	} else if ( length( allReallocDecisions ) == 0 ){
		stop(paste("ERROR in portfolioSimulationExec.main(): no realloc decision was specified in 'reb.reallocDecision'."))
	}
	
	if( length( allReallocDecisions ) > 1 ) {
		# TODO we're taking a performance hit here since we have to go to the database for every combination of reallocDecision and portfolio designator. move the handling of multiple reallocDecisions inside portfolioSimualtion.R
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
	} else {
		for(portfolioName in portfolioNames){
			settings[["portfolioName"]]<-portfolioName
			input<-portfolioSimulation.input(settings)
			output<-portfolioSimulation.calculate(input,settings)
			portfolioSimulation.output(output,settings)
		}
	}
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execution statements. psex.main() is in portfolioSimulationExecImpl.R.
portfolioSimulationExec.main()
