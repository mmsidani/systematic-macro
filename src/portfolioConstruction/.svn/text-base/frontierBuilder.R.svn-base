
frontierBuilder.input<-function(settings){
# input function
	
	portfolioName<-settings[["portfolioName"]]
	pathToRSourceCode<-settings[["pathToRSourceCode"]]
	namesOfInList<-settings[["fbex.namesOfInList"]]
	cashNameSuffix<-settings[["frnt.cashNameSuffix"]]
	conversionRateNameSuffix<-settings[["frnt.conversionRateNameSuffix"]]
	ersRisksFile<-settings[["fbex.ersRisksFile"]]
	marketDataFile<-settings[["fbex.marketDataFile"]]
	useDB<-settings[["fbex.useDB"]]
	inputDir<-settings[["fbex.inputDir"]]
	dateFormat<-settings[["fbex.dateFormat"]]
	cashBenchmark <- settings[["fbex.cashBenchmark"]]
	dbSettings<-settings[["dbSettings"]]
	
	portfolioData<-fbUtils.getPortfolioData(portfolioName,namesOfInList,pathToRSourceCode)
	correlationDataFreq<-portfolioData[["correlationDataFreq"]]
	constraintsCSVFile<-portfolioData[["constraintsNAME"]]
	baseCc<-portfolioData[["baseCc"]]
	desiredDates<-portfolioData[["desiredDates"]]
	erMapping<-portfolioData[["erMapping"]]
	riskMapping<-portfolioData[["riskMapping"]]
	dayOfWeek<-portfolioData[["dayOfWeek"]]
	
	if( baseCc != substr(cashBenchmark[["name"]], 1, 2 ) ){
		# Note: the following restriction saved us from a complication when it comes to FX hedge
		stop(paste("ERROR in frontierBuilder.input(): cash benchmark must be in base currency. you passed: base cc = ",baseCc,", and cash benchmark = ", cashBenchmark["name"]))
	}
	
	# prepare to call efficient frontier
	constraintsFileName<-paste("inputFiles/constraints-",constraintsCSVFile,".csv",sep="")
	# the assets that get included in the efficient frontier calculations are those we read from the constraints file
	constraints<-utils.getConstraints(pathToRSourceCode,constraintsFileName)
	portfolioData[["constraints"]]<-constraints
	
	assetNames<-names(constraints)
	# find countries for assets
	ccs<-unique(substr(assetNames,1,2))
	# determine cash names for those
	rfNames <- utils.generateCrossIds(union(baseCc,ccs),union(cashNameSuffix,conversionRateNameSuffix))
	baseCashName<-paste(baseCc,cashNameSuffix,sep="")
	
	assetData<-fbUtils.getData(correlationDataFreq,assetNames,baseCashName,rfNames,ersRisksFile,marketDataFile,desiredDates,erMapping,riskMapping,useDB,inputDir,dayOfWeek,dateFormat,dbSettings, cashBenchmark)
	
	return(append(portfolioData,assetData))
}

frontierBuilder.output<-function(output,settings){
# print the output to a csv file; stack all efficient frontiers in a single data frame and print that
	
	stateObjects <- output[["stateObjects"]]
	outDesName <- output[["desName"]]
	output <- output[["outputs"]]
	
	outputDir<-settings[["fbex.outputDir"]]
	portfolioName<-settings[["portfolioName"]]
	
	dayOfWeek<- outDesName[["dayOfWeek"]]
	freq<-outDesName[["freq"]]
	# one more global variable used in output
	if(freq =="weekly"){
		desname<-dayOfWeek
	}else{
		desname<-freq
	}   
	
	methods <- setdiff(names(output),"desName")
	for(method in methods){
		outp <- output[[method]]
		
		isMV <- method == "MV"
		portp <-  ifelse(isMV, portfolioName, paste(portfolioName, ".",method,sep="" ))
		if(is.element("frontier", names(outp))){
			effF<-outp[["frontier"]]
			write.csv(effF,paste(outputDir,portp,"-", desname,"-Opt-FULL.csv",sep=""),row.names=FALSE)
			save(effF,file=paste(outputDir,portp,"-", desname,"-Opt-FULL.RData",sep=""))
		} 
		
		if (is.element("summary", names(outp))) {
			labeledPoints<-outp[["summary"]]
			write.csv(labeledPoints,paste(outputDir,portp,"-", desname,"-points-labels.csv",sep=""),row.names=FALSE)
			save(labeledPoints,file=paste(outputDir,portp,"-", desname,"-points-labels.RData",sep=""))
		}
		
		if (is.element("riskAversionFactors", names(outp))) {
			riskAversions <- outp[["riskAversionFactors"]]
			if(any(!is.null(riskAversions[, 2]))){
				write.csv(riskAversions,paste(outputDir, portp,"-riskaversion.csv",sep=""),row.names=FALSE)
				save(riskAversions,file=paste(outputDir, portp,"-riskaversion.RData",sep=""))
			}
		}
		
		for (n in setdiff(names(outp), c("frontier", "summary", "riskAversionFactors"))){
			portp <-  paste(portfolioName, ifelse(isMV, "-", paste("-",method,".",sep="")), sep="" )
			
			port<-outp[[n]]
			write.csv(port,paste(outputDir, portp,n,"-", desname,".csv",sep=""),row.names=FALSE)
			save(port,file=paste(outputDir, portp,n,"-", desname,".RData",sep=""))
		}
		
		for (n in names(stateObjects[[method]])){
			port <- stateObjects[[method]][[n]]
			if( is.null( port ) ) next
			
			portp <-  paste(portfolioName, ifelse(isMV, "-", paste("-",method,".extended.",sep="")), sep="" )
			
			write.csv(port,paste(outputDir, portp,n,"-", desname,".csv",sep=""),row.names=FALSE)
			save(port,file=paste(outputDir, portp,n,"-", desname,".RData",sep=""))
		}
	}
}

fbex.reformatFrontiers <- function(allFrontiers, numFrontierPoints){
# allFrontiers is matrix with all frontiers for all dates in the run; numFrontierPoints obvious
# we just change the format of the dates in the "Date" column of allFrontiers to include hyphens and we add a "Port" column that assigns an integer, 1:numFrontierPoints, to each point on each frontier and return the data.fram()
	
	orgDates<-as.numeric(allFrontiers[,"Date"])
	numFrontiers<-length(unique(orgDates))
	columnNames<-colnames(allFrontiers)
	# add Date and Port (just an enumeration into each frontier) columns to allFrontiers
	nonDateColumnNames <- setdiff(columnNames, "Date")
	allFrontiers<-cbind(data.frame(Date=utils.ymdToHyphens(orgDates),Port=rep(1:numFrontierPoints,numFrontiers)),data.frame(allFrontiers[,nonDateColumnNames]))
	names(allFrontiers)<-c("Date","Port", nonDateColumnNames)
	
	return(allFrontiers)
}

fbex.reformatPortfolios <- function(portfolios, riskAversionFactors){
	uniqueRAs <- unique(riskAversionFactors)
	portfolios[, "Date"] <- utils.ymdToHyphens( as.numeric(portfolios[, "Date"]))
	ret <- list()
	for(ra in uniqueRAs){
		ret <- append(ret, list(portfolios[riskAversionFactors == ra,]))
		names(ret)[length(ret)] <- ra
	}
	
	return(ret)
}

fbex.getFrontier<-function(method,solverObject,numAssets,expectedRets,assetNames,numFrontierPoints,constraintsLhs,constraintsRhs,constraintsNumEqs, stateObject, thisDate){
# this builds the efficient frontier for 1 date.
# rateData is a timeSeries, the input; numAssets is a scalar, the number of assets; rollingWindow is the number of past values of ER's we use to calculate the co-variances; numFrontierPoints is a scalar, the number of efficient frontier points we want, baseCc is the code of the base country (e.g., "us", "uk", etc.), rfRates are the risk free rates indexed by country code (e.g., "us.gg.3m", "us.2y.junk", etc.), halfLife controls exponential weighting when calculating the correlations; if halfLife = 0, we get equal weights
# returns a data frame with portRet, portRisk, and asset names headings
	
	portCharc<-constructionMethods.getWeights(method, solverObject, NULL,expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs)	
	# the minimum return we want is that of the minimum variance portfolio
	minRet<-portCharc[["portRet"]]
	# get the maximum possible return. Note: this is why we couldn't put contraints* in solverObject; fbUtils functions should not be specialized per solver
	lpResults<-fbUtils.getMaxTargetReturn(expectedRets, constraintsLhs,constraintsRhs,constraintsNumEqs)
	maxRet<-lpResults[["maxRet"]]
	maxRetWeights<-lpResults[["portfolio"]]
	if(minRet<0 || minRet >= maxRet){
		stop(paste("ERROR in mvFrontier.getFrontier(): degenerate frontier on",rownames(expectedRets), ", minRet=",minRet,"maxRet=",maxRet,". expected returns are:",paste(names(expectedRets),"=",expectedRets,collapse=", ")))
	}
	
	# we want numFrontierPoints points on the efficient frontier; now calculate the target returns for those
	retStep<-(maxRet-minRet)/(numFrontierPoints-1)
	targetRets<-minRet+cumsum(rep(retStep,numFrontierPoints-1))
	# effFrontier is the matrix that will contain the output frontier
	effFrontier<-matrix(nrow=numFrontierPoints,ncol=numAssets+2)
	# raFactors will contain the risk aversion factors
	raFactors <- rep(NA,numFrontierPoints)
	effFrontier[1,]<-c(minRet,portCharc[["portRisk"]],portCharc[["portWeights"]])
	raFactors[1] <- portCharc[["riskAversionFactor"]]
	# now for each target return, get the point on the efficient frontier for that target return
	for(i in 1:(length(targetRets)-1)){
		# solve for the weights of the optimal portfolio with this target return
		portCharc<-constructionMethods.getWeights(method, solverObject,targetRets[i],expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs)
		# now save another row to the matrix; each row corresponds to a point on the efficient frontier
		effFrontier[i+1,]<-c(portCharc[["portRet"]],portCharc[["portRisk"]],portCharc[["portWeights"]])
		raFactors[i+1] <- portCharc[["riskAversionFactor"]]
	}	
	
	# special treatment for the max return portfolio since we know the solution here
	effFrontier[nrow(effFrontier),]<-c(maxRet,constructionMethods.getRisk(method, solverObject, maxRetWeights),maxRetWeights)
	
	# the columns of effFrontier in which we stored the weights of the different portfolios  have meaningless names, change them now to the asset names
	colnames(effFrontier)<-c("portRet","portRisk",assetNames)
	return(list(effFrontier=effFrontier,riskAversionFactors=raFactors))
}

fbex.getPortfolios <- function(method,solverObject, parameters,numAssets,expectedRets,assetNames,constraintsLhs,constraintsRhs,constraintsNumEqs, stateObject, thisDate){
	
	numParameters <- length(parameters)
	# effFrontier is the matrix that will contain the output frontier
	portfolios<-matrix(nrow=numParameters,ncol=numAssets+2)
	# raFactors will contain the risk aversion factors
	raFactors <- rep(NA,numParameters)
	for(i in 1:numParameters){
		# solve for the weights of the optimal portfolio with this target return
		portCharc<-constructionMethods.getWeights(method, solverObject,parameters[i],expectedRets,constraintsLhs,constraintsRhs,constraintsNumEqs)
		# now save another row to the matrix; each row corresponds to a point on the efficient frontier
		portfolios[i,]<-c(portCharc[["portRet"]],portCharc[["portRisk"]],portCharc[["portWeights"]])
		raFactors[i] <- ifelse(is.element("label", names(portCharc)), portCharc[["label"]], paste(parameters[i], collapse=".") )
		
		stateObject <- constructionMethods.updateStateObject(method, thisDate, stateObject, portCharc )
	}	
	
	# the columns of effFrontier in which we stored the weights of the different portfolios  have meaningless names, change them now to the asset names
	colnames(portfolios)<-c("portRet","portRisk",assetNames)
	return(list(portfolios=portfolios,riskAversionFactors=raFactors, stateObject=stateObject))
}

fbex.extractOutput <- function( dOutput ){
	
	if ( is.element("effFrontier", names(dOutput))){
		outputType <- "frontier"
	} else if (is.element("portfolios", names(dOutput))) {
		outputType <- "portfolios"
	} else {
		stop("ERROR in fbex.extractOutput(): output of constructionMethods.doOneDate() not recognized")
	}
	
	ret <- append(dOutput, list(outputType = outputType) )
	index <- match("effFrontier", names( dOutput) )
	if( is.finite( index )){
		names(ret)[ index ] <- "portfolios"
	}
	
	return( ret )
}

fbex.doAllDates <-function(methods,cashNameSuffix,conversionRateNameSuffix,rfCouponsPerYear,constraintsFileInfo,varScaleFactor,doRisks,rateData,rfRates,dates,rollingWindow,baseCc,numFrontierPoints,leverage,halfLife,constraints,riskMeasure,parametersList,optimParams,cashBenchmark,cashBenchmarkData,shortReturnThreshold){
# numDates is a scalar that tells it for how many dates we want the eff frontiers; numAssets is a scalar, the number of assets; rollingWindow is a scalar, the number of historical values of ER's we want to use in calculating var-covar; and numFrontierPoints is the number of eff frontier points we want on each efficient frontier
	
	ncols<-ncol(rateData)
	fbUtils.assertNumColsIsEven(ncols)
	
	numAssets<-ncols/2
	assetNames<-names(rateData)[1:numAssets]
	# this is TRUE if cash is NOT being included in the optimization; this is now controlled from the constraints file, because that's what controls the assets that go into the optimization
	cashPositionsNotIncluded <- length(grep(cashNameSuffix,assetNames)) == 0
	
	# convert returns and risks to base currency
	isConversionRatesName<-grepl(paste(conversionRateNameSuffix,"$",sep=""),colnames(rfRates))
	rateData<-erRiskDomesticEquivalent.convertErsRisks(rateData,baseCc,rfRates[,isConversionRatesName],rfCouponsPerYear,conversionRateNameSuffix,doRisks)
	# no need for conversion rates anymore
	if(cashNameSuffix != conversionRateNameSuffix){
		rfRates<-rfRates[,!isConversionRatesName]
	}
	# those assets whose ret/risk on a certain date is lower than a hurdle rate get NA as their return in fbUtils.filterForHurdles(). they will then be removed from the mix in fbUtils.rmNAs()
	rateData<-fbUtils.filterForHurdles(rateData,constraints,constraintsFileInfo[["hurdleRowName"]], varScaleFactor)
	
	rateDataDates<-utils.hyphensToYmd(row.names(rateData))
	dates<-utils.hyphensToYmd(dates)
	numRows<-nrow(rateData)
	rateDataIndices<-(1:numRows)[is.element(rateDataDates,dates)]
	riskFreeIndices<-(1:numRows)[is.element(utils.hyphensToYmd(row.names(rfRates)),dates)]
	outputs <- list()
	stateObjects <- list()
	for( method in methods){
		stateObject <- constructionMethods.getStateObject( method, parametersList[[method]], length(rateDataIndices), assetNames )
		
		numRowsPerDate <- ifelse( is.element(method, names(parametersList)), length( parametersList[[method]]), numFrontierPoints)
		outputType <- ""
		# initialize the output. we add 2 columns, portRet and portRisk to the assets weights columns
		output<-matrix(nrow=length(dates)*numRowsPerDate,ncol=numAssets+3)
		colnames(output)<-c("Date","portRet","portRisk",assetNames)
		# matrix to hold risk aversion factors: "Date"
		riskAversionFactors <- matrix(nrow=length(dates)*numRowsPerDate,ncol=2)
		colnames(riskAversionFactors) <- c("Date","riskAversion")
		# now get the eff frontier for each date
		counter<-1
		outputType <- ""
		for (i in 1:length(rateDataIndices)){

			# Note: riskFreeIndices and rateDataIndices have the same length
			dRfData<-rfRates[riskFreeIndices[i],]
			if(numRows >= rollingWindow+rateDataIndices[i] ){
				dData<-rateData[rateDataIndices[i]:(rateDataIndices[i]+rollingWindow), ]
				# now weed out the NA's
				dData<-fbUtils.rmNAs(dData)
				if(ncol(dData)>0){
					numdAssets <- ncol(dData)/2
					namesDDataAssets<-names(dData)[1:numdAssets]
					
					assetsToShort <- c()
					if( shortReturnThreshold[["doShort"]] == "threshold") {
						assetClasses <- data.frame( n=names(constraints ) , c=as.character(constraints[ match(constraintsFileInfo[["assetClassRowName"]],row.names(constraints)), ] ) )

						equityAssets <- assetClasses[[ 1 ]][ is.element(assetClasses[[ 1 ]], namesDDataAssets) & assetClasses[[2]] == "equity" ]
						bondAssets <- assetClasses[[ 1 ]][ is.element(assetClasses[[ 1 ]], namesDDataAssets) & assetClasses[[2]] == "bond" ]
						assetsToShort <- equityAssets[ dData[ 1 , match(equityAssets, names(dData) ) ] < shortReturnThreshold[["threshold"]][["equityThreshold"]] ]
						assetsToShort <- c( assetsToShort, bondAssets[ dData[ 1 , match(bondAssets, names(dData) ) ] < shortReturnThreshold[["threshold"]][["bondThreshold"]] ] )
					} else if ( shortReturnThreshold[["doShort"]] == "securityER") {
						indRet <-  match(shortReturnThreshold[["securityER"]][["security"]], names(dData))
						if( !is.finite( indRet )){
							stop(paste("ERROR in fbex.doAllDates(): ", shortReturnThreshold[["securityER"]][["security"]],"is not in the investable universe"))
						}
						retThreshold <- as.numeric( dData[ 1, indRet ])
						assetsToShort <- namesDDataAssets[ dData[ 1 , match( namesDDataAssets, names(dData) ) ] < retThreshold ]
					} else if ( shortReturnThreshold[["doShort"]] != "none" ){
						stop(paste("ERROR in fbex.doAllDates(): ", shortReturnThreshold[["doShort"]]," is not valid for doShort"))
					}

					weightConstraints<-fbUtils.getWeightConstraints(constraints,assetNames, assetsToShort,TRUE,leverage,baseCc,cashNameSuffix,constraintsFileInfo)

					constraintsLhs<-weightConstraints[["lhs"]]
					constraintsRhs<-weightConstraints[["rhs"]]
					constraintsNumEqs<-weightConstraints[["numEqs"]]
					
					# now calculate the efficient frontier but first trim the constraints matrix to remove assets for which no data exists on this date
					is0 <- rowSums( constraintsLhs[, namesDDataAssets, drop=F] ) == 0
					dConstraintsLhs <- constraintsLhs[ !is0, namesDDataAssets]
					dConstraintsRhs <- constraintsRhs[ !is0 ]
					solverObject <- constructionMethods.getSolverObject(method, riskMeasure,dData,numdAssets,rollingWindow,halfLife,varScaleFactor,dConstraintsLhs,dConstraintsRhs,constraintsNumEqs,parametersList[[method]],optimParams,rfRates,cashBenchmark, rateDataDates[rateDataIndices[i]] )
					dOutput<-fbex.extractOutput(constructionMethods.doOneDate(method,solverObject,parametersList[[method]],dData,rollingWindow,halfLife,varScaleFactor,numRowsPerDate,dConstraintsLhs,dConstraintsRhs,constraintsNumEqs, stateObject, rateDataDates[rateDataIndices[i]] )  )
					rAs <- dOutput[["riskAversionFactors"]]
					outputType <- dOutput[["outputType"]]
					stateObject <- dOutput[["stateObject"]]
					# now copy the new eff frontier to the output
					output[counter:(counter+numRowsPerDate-1),colnames(dOutput[["portfolios"]])] <- dOutput[["portfolios"]]
					riskAversionFactors[counter:(counter+numRowsPerDate-1),2] <- rAs
					output[counter:(counter+numRowsPerDate-1),"Date"]<- riskAversionFactors[counter:(counter+numRowsPerDate-1),1] <- rateDataDates[rateDataIndices[i]]
					
					counter<-counter+numRowsPerDate
				}
			}
		}
		
		# remove extraneous rows
		output<-output[1:(counter-1),]
		riskAversionFactors<-riskAversionFactors[1:(counter-1),]
	
		output <- fbUtils.addCashBenchmark( output, cashBenchmark, cashBenchmarkData )
		
		# add unit fx hedge if "cash" (i.e., funding rate) was not included in the optimization)
		if(cashPositionsNotIncluded){
			output<-fbUtils.addFxUnitHedge(baseCc,cashNameSuffix,output)
		}
		
		if(outputType == "frontier"){
			portfolios <- specialPortfolios.VCEMA(output,riskAversionFactors,cashNameSuffix,numFrontierPoints)
			output <- fbex.reformatFrontiers(output, numFrontierPoints)
			outputs <- append(outputs, list(append(list(frontier=output,riskAversionFactors=riskAversionFactors),portfolios)))

		} else if (outputType =="portfolios"){
			output <- fbex.reformatPortfolios(output, riskAversionFactors[,2])
			outputs <- append(outputs, list(output))
		}
		names(outputs)[length(outputs)] <- method
		stateObjects <- append(stateObjects, list(stateObject))
		names(stateObjects)[length(stateObjects)] <- method
	}
	
	return(list(outputs = outputs, stateObjects=stateObjects ) )
}

frontierBuilder.calculate<-function(input,settings){
# main entry point
	
	cashNameSuffix<-settings[["frnt.cashNameSuffix"]]
	conversionRateNameSuffix<-settings[["frnt.conversionRateNameSuffix"]]
	rfCouponsPerYear<-settings[["frnt.couponsPerYear"]]
	varScaleFactor<-settings[["frnt.varScaleFactor"]]
	doRisks<-settings[["frnt.doRisks"]]
	dateFormat<-settings[["fbex.dateFormat"]]
	portfolioName<-settings[["portfolioName"]]
	methods <- settings[["fbex.methods"]]
	parametersList <- settings[["fbex.parametersList"]]
	constraintsFileInfo <- settings[["fbex.constraintsFileInfo"]]
	optimParams <- settings[["fbex.optimParams"]]
	cashBenchmark <- settings[["fbex.cashBenchmark"]]
	shortReturnThreshold <- settings[["fbex.shortingControl"]]
	
	rateData<-input[["effFrontierData"]]
	freq <- input[["effFrontierFreq"]]
	dayOfWeek <- input[["dayOfWeek"]]
	rfRates<-input[["rfRates"]]
	startDate <- input[["startDateForBackTest"]]
	endDate <- input[["endDateForBackTest"]]
	rollingWindow<-input[["rollingWindow"]]
	baseCc<-input[["baseCc"]]
	numFrontierPoints<-input[["numFrontierPoints"]]
	leverage<-input[["leverage"]]
	halfLife<-input[["halfLife"]]
	constraints<-input[["constraints"]]
	riskMeasure<-input[["riskMeasure"]]
	cashBenchmarkData <- input[["cashBenchmarkData"]]
	
	if( endDate == "today" ){
		endDate <- utils.today()
	}
	
	# set up input for frontier calculate()
	dates<-utils.truncateDates(utils.extractDates(row.names(rateData), freq,dayOfWeek,TRUE),as.Date(startDate,format=dateFormat),as.Date(endDate,format=dateFormat))
	
	if(length(dates) == 0){
		stop("ERROR in frontierBuilder.R: no date satisfies the different criteria")
	}
	
	print(paste("INFO from frontierBuilder.calculate(): now doing",portfolioName))
	# build frontiers for all dates
	execTime<-system.time(output<-fbex.doAllDates(methods,cashNameSuffix,conversionRateNameSuffix,rfCouponsPerYear,constraintsFileInfo,varScaleFactor,doRisks,rateData,rfRates,dates,rollingWindow,baseCc,numFrontierPoints,leverage,halfLife,constraints,riskMeasure,parametersList,optimParams,cashBenchmark[["name"]],cashBenchmarkData,shortReturnThreshold))[["elapsed"]]
	print(paste("INFO from frontierBuilder.calculate(): mvFrontier.calculate() took",execTime,"secs."))
	
	return(append(output,list(desName=list(dayOfWeek=dayOfWeek,freq=freq))) )
}
