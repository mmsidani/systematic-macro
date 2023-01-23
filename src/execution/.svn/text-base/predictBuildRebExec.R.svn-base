pbreex.prd<-function(today,liveData,prdSettings){
	assets<-unique(setdiff(sub("\\.earnings","",sub("\\.dvd.yld","",sub("\\.pos","",names(liveData)))),"Date"))
	assets<-utils.assetsByClass(assets,prdSettings[["prd.assetIdentifiers"]])
	assets[["equity"]] <- utils.dependencies(assets[["equity"]], list(prdSettings[["eretm.seedSource"]],prdSettings[["eretm.growthBenchmark"]]))
	
	# modify the assets in prdSettings
	prdSettings[["prd.assets"]]<-c(assets[["equity"]],assets[["bond"]])
	
	equityNames<-utils.generateCrossIds(assets[["equity"]],prdSettings[["prd.listOfSuffixes"]][["equity"]])
	bondNames<-utils.generateCrossIds(assets[["bond"]],prdSettings[["prd.listOfSuffixes"]][["bond"]])
	input<-predict.input(prdSettings) 

	missingNames <- setdiff(c(equityNames,bondNames),names(liveData))
	# add live data to existing data
	if(rownames(input[["assetData"]][["equity"]])[1] >= today){
		stop(paste("ERROR in pbreex.prd(): data in DB/data file is as/more recent than data in LIVE file. Latest DB/ data file date is:", rownames(input[["assetData"]][["equity"]])[1]))
	}else if(length(missingNames)!=0){
		stop(paste("ERROR in pbreex.prd(): the following are missing from the live data file:",paste(missingNames, collapse=", "), ". values will be set to 0."))
	}
	input[["assetData"]][["equity"]]<-rbind(timeSeries(liveData[1,equityNames],charvec=today),input[["assetData"]][["equity"]][,equityNames])
	names(input[["assetData"]][["equity"]])<-equityNames

	input[["assetData"]][["bond"]]<-rbind(timeSeries(liveData[1,bondNames],charvec=today),input[["assetData"]][["bond"]][,bondNames])
	names(input[["assetData"]][["bond"]])<-bondNames

	# calculate
	output<-predict.calculate(input,prdSettings)
	predict.output(output,prdSettings)
	
	ret <- output[["forecasts"]]
	# TODO .ber hack
	names(ret) <- gsub("\\.ner$", ".ber", names(ret))
	
	return(ret)
}

pbreex.fb<-function(today,liveData,fbSettings,portfolioSettings,predictOutput,outputPrefix, mvuPortfolios){

	original.fbUtils.getPortfolioData <- fbUtils.getPortfolioData
	lastDay <- today
	if ( portfolioSettings[["isNextDay"]] ){
		# only happens for 'weekly' frequency
		lastDay <- utils.dayClosest(today, portfolioSettings[["dayOfWeek"]], F)
	}
	
	fbUtils.getPortfolioData <<- function(portfolioName,namesOfInList,pathToRSourceCode){
		portfolioSettings[["startDateForBackTest"]]<- lastDay
		portfolioSettings[["endDateForBackTest"]]<- lastDay
		return(portfolioSettings)
	}
	
	input<-frontierBuilder.input(fbSettings)

	if(rownames(input[["effFrontierData"]])[1] >= today || rownames(input[["rfRates"]])[1] >= today){
		# this is unnecessary because we would have checked for the latest date in the DB in pbreex.prd() by now
		stop(paste("ERROR in pbreex.fb(): data in DB/data file is as/more recent than data in LIVE file. Latest DB/ data file date is:", rownames(input[["effFrontierData"]])[1]))
	} else if(length(setdiff(names(input[["rfRates"]]),names(liveData))) != 0){
		stop(paste("ERROR in pbreex.fb(): data in DB/data file is as/more recent than data in LIVE file. Latest DB/ data file date is:", rownames(input[["assetData"]][["equity"]])[1]))
	}

	# first half of names() contains the asset names
	assetNames <- names(input[["effFrontierData"]])[1:(ncol(input[["effFrontierData"]])/2)]

	forecastNames <- c(utils.generateIdsFromMapping(assetNames,portfolioSettings[["erMapping"]]),utils.generateIdsFromMapping(assetNames,portfolioSettings[["riskMapping"]]))
	effFrontierNames <- c(assetNames, paste(assetNames, ".risk", sep=""))
	rfRatesNames <- names(input[["rfRates"]])
	
	# add forecasts obtained with live data to existing data
	input[["effFrontierData"]]<-rbind(timeSeries(predictOutput[1, forecastNames],charvec=lastDay),input[["effFrontierData"]][,effFrontierNames])
	names(input[["effFrontierData"]]) <- effFrontierNames
	input[["rfRates"]]<-rbind(timeSeries(liveData[rfRatesNames],charvec=lastDay),input[["rfRates"]])
	names(input[["rfRates"]]) <- rfRatesNames

	# calculate
	output<-frontierBuilder.calculate(input,fbSettings)
	# PATCH: when we have "MVU" we do not return data.frame()'s and this causes problems; we get data.drame()'s in the case of "MV" because of logic in specialPortfolios.R
	for(m in names(output[["outputs"]])){
		if( m == "MV"){
			for (p in names(output[["outputs"]][[m]])){
				for(r in 1:nrow(output[["outputs"]][[m]][[p]])){
					output[["outputs"]][[m]][[p]][, "Date"] <- today
				}
			}
		} else {
			for (p in names(output[["outputs"]][[m]])){
				if( is.element(paste(m,".",p,sep="") , mvuPortfolios) ){
					output[["outputs"]][[m]][[p]] <- data.frame(t(output[["outputs"]][[m]][[p]]))
					outNames <- names(output[["outputs"]][[m]][[p]])
					for(r in 1:nrow(output[["outputs"]][[m]][[p]])){
						output[["outputs"]][[m]][[p]][,setdiff(outNames,"Date")] <- as.numeric(output[["outputs"]][[m]][[p]][,setdiff(outNames,"Date")])
						output[["outputs"]][[m]][[p]][, "Date"] <- today
					}
				}
			}
		}
	}
	
	# NOTE: a hack! in frontierBuilder.output(), fbSettings[["portfolioName"]] is ONLY used to name the output files. we modify it here to prevent clashes with frontierBuilderExec
	fbSettings[["portfolioName"]] <- paste(outputPrefix, fbSettings[["portfolioName"]], sep="")
	frontierBuilder.output(output,fbSettings)
	
	# predictOutput is a data.frame()
	ersRisks <- timeSeries(predictOutput[, c(forecastNames, utils.generateIdsFromMapping(rfRatesNames,portfolioSettings[["erMapping"]]),utils.generateIdsFromMapping(rfRatesNames,portfolioSettings[["riskMapping"]]))],charvec=predictOutput[["Date"]])
	names(ersRisks) <- c(effFrontierNames, rfRatesNames, paste(rfRatesNames,".risk",sep=""))
	
	corrFreq <- portfolioSettings[["correlationDataFreq"]]
	dayOfWeek <- portfolioSettings[["dayOfWeek"]]
	if(corrFreq == "weekly"){	
		ersRisks <- utils.getWeeklyData(ersRisks, dayOfWeek)
	} else if(corrFreq == "monthly"){
		ersRisks <- utils.getEOMData(ersRisks, TRUE)
	} else if(corrFreq == "semi-monthly"){
		ersRisks <- utils.getSemiMonthlyData(ersRisks, TRUE)
	} else {
		stop(paste("ERROR in pbreex.fb(): the specified frequency",corrFreq,"is not supported"))
	}
	
	# restore
	fbUtils.getPortfolioData <<- original.fbUtils.getPortfolioData
	
	return(list(output=output,ersRisks=ersRisks,rfRates=input[["rfRates"]],riskMeasure=portfolioSettings[["riskMeasure"]],baseCc=portfolioSettings[["baseCc"]],rollingWindow=portfolioSettings[["rollingWindow"]],halfLife=portfolioSettings[["halfLife"]],leverage=portfolioSettings[["leverage"]]))
}

pbreex.getTargetPortfolio <- function(whichPortfolio, trader, fbOutput, firstColumn){
	if(!is.element(trader, names(whichPortfolio))){
		stop(paste("ERROR in pbreex.getTargetPortfolio(): no portfolio was associated with trader", trader))
	}
	
	if( length(whichPortfolio[[trader]]) != 1 ){
		stop(paste("ERROR in pbreex.getTargetPortfolio(): a trader should only on portfolio associated with it.", trader,"had these:", paste(whichPortfolio[[trader]], collapse=",")))
	}
	fbOutput <- fbOutput[["outputs"]]
	for (m in names(fbOutput)){
		temp <- fbOutput[[m]]
		for ( p in names(temp)){
			if( m != "MV" ){
				portName <- paste(m,".",p,sep="")
			} else {
				portName <- p
			}
			if(whichPortfolio[[trader]] == portName){
				ret <- fbOutput[[m]][[p]][1,]
				return(as.matrix(ret[1, firstColumn:ncol(ret)]))
			}
		}
	}
	
	return(NULL)
}

pbreex.setCurrentCashPositions <- function(sa2Names, weights, cashNameSuffix, baseCc){
	countryCodes <- unique( substr( sa2Names, 1, 2) )
	if(!is.element( baseCc, countryCodes)){
		countryCodes <- c(baseCc, countryCodes)
	}
	cashNames <- paste( countryCodes, cashNameSuffix, sep="")
	
	cashWeights <- rep(0, length(countryCodes))
	names(cashWeights) <- paste(countryCodes, cashNameSuffix, sep="")
	baseCashWeight <-0
	for ( i in 1:length(countryCodes) ){
		if(countryCodes[i] != baseCc){
			ccInds <- grepl( paste("^", countryCodes[i], sep=""), sa2Names )
			cashWeights[i] <- cashWeights[i] - sum( weights[ccInds])
			baseCashWeight <- baseCashWeight - cashWeights[i]
		} else {
			baseInd <- i
		}
	}
	
	cashWeights[baseInd] <- baseCashWeight
	
	return( cashWeights )
}

pbreex.getCurrentPortfolio <- function(assetNames, liveData, positionSuffix){
	positionNames <- paste(assetNames, positionSuffix,sep="")
	livePositionNames <- names(liveData)[ grep(positionSuffix, names(liveData))]
	if(!setequal(positionNames, livePositionNames)){
print(positionNames)
print(livePositionNames)
		stop("ERROR in predictBuilRebExec.R: current portfolio names do not match target portfolio names. we're not set up to handle that...")
	}
	
	ret <- as.matrix(liveData[1,positionNames])
	colnames(ret) <- assetNames
	return(ret)
}

pbreex.rebalance <- function(thisDate, reallocDecision, trader, curPort, targetPort, ersRisks, leverage, leverageThreshold,  erRiskRatioHurdle,euclidHurdle , riskMeasure, rollingWindow, halfLife, varScaleFactor, riskyAssets){
	
	if(!is.element(trader, names(reallocDecision))){
		stop(paste("ERROR in pbreex.rebalance(): no rebalancing decision was associated with trader", trader))
	}
	
	return(rebalancing.reallocHurdle(thisDate,curPort,targetPort,ersRisks,reallocDecision[[trader]], NULL , NULL, erRiskRatioHurdle, NULL ,euclidHurdle, leverage ,leverageThreshold,riskMeasure,rollingWindow,halfLife,varScaleFactor,riskyAssets) )
}

pbreex.getPortfolioSettings <- function(today, fbSettings){
	portfolio.settings <- NULL
	source(paste(fbSettings[["pathToRSourceCode"]],"inputFiles/constraints-",fbSettings[["portfolioName"]],".r",sep=""))
	portfolioSettings <- portfolio.settings()
	portfolioSettings[["isModifiedDayOfWeek"]] <- FALSE
	
	todayDate <- as.Date(today)
	
	portfolioSettings[["isNextDay"]] <- FALSE
	if(portfolioSettings[["correlationDataFreq"]] == "weekly" || portfolioSettings[["correlationDataFreq"]] == "monthly"  ){
		dayOfWeek <- weekdays(todayDate)
		eomDate <- utils.dayClosestEOM( today )
		if( portfolioSettings[["correlationDataFreq"]] == "weekly" && dayOfWeek != portfolioSettings[["dayOfWeek"]]){
			print(paste("WARNING in predictBuildRebExec.R: the running date is",today,"which falls on a",dayOfWeek,"which does not match dayOfWeek for the portfolio which is",portfolioSettings[["dayOfWeek"]],"."))
			print(paste("Request from predictBuildRebExec.R: type 1 if you want correlations to use",portfolioSettings[["dayOfWeek"]],"; type 2 to use",dayOfWeek))
			response <- scan(what=character(),n=1)
			if(response == "1"){
				print(paste("INFO from predictBuildRebExec.R: proceeding then:",portfolioSettings[["dayOfWeek"]],"will be used for correlations..."))
				portfolioSettings[["isNextDay"]] <- TRUE
			} else if(response == "2"){
				print(paste("REQUEST from predictBuildRebExec.R: enter the day of the week",dayOfWeek,"to confirm."))
				response <- scan(what=character(),n=1)
				if(response == dayOfWeek){
					portfolioSettings[["dayOfWeek"]] <- dayOfWeek
					print(paste("INFO from predictBuildRebExec.R: proceeding then:", dayOfWeek, "will be used for correlations..."))
				} else {
					stop(paste("ERROR in predictBuildRebExec.R: You did not type",dayOfWeek,". exiting...") )
				}
			} else {
				stop(paste("ERROR in predictBuildRebExec.R: You did not type '1' or '2'. exiting...") )
			}
		} else if ( portfolioSettings[["correlationDataFreq"]] == "monthly" && today != eomDate ) {
			stop(paste("ERROR in predictBuildRebExec.R: 'correlationDataFreq' in the portfolio settings is 'monthly' but", today,"is not the EOM. can't do that. the data spacing is wrong."))
		}
	}

	return(list(today=as.character(today), portfolioSettings=portfolioSettings))
}

pbreex.createPOSTable <- function( pathToRSourceCode ){
	
	clpxSettings <- global.createLivePXExecSettings()
	clpxSettings[["pathToRSourceCode"]] <- pathToRSourceCode
	clpxInput <- createLivePX.input( clpxSettings)
	ret <- data.frame(createLivePX.calculate(clpxInput, clpxSettings)[[1]])
	for (i in 2:ncol(ret)){
		ret[[i]] <- as.numeric(ret[[i]])
	}
	ret[1, 2:ncol(ret)] <- 0
	
	return( ret )
}

pbreex.getBBDataIndex <- function(bbTicker, sa2Name, bbData, dirtyPriceSuffix, securityRow){
# little factored out code
	
	indBB <- match( bbTicker, bbData[,2])
	if(is.na(indBB)){
		# now bbTicker is an instrument used for positions AND prices (example, EOJ3) and so we expect to find its price to be mapped to an sa2 name
		indBB <- match( ifelse(securityRow[["priceType"]] =="bond", paste(sa2Name, dirtyPriceSuffix, sep=""), sa2Name),  bbData[,2] )
		if(is.na(indBB)){
			stop(paste("ERROR in pbreex.calculateWeights():", bbTicker,"has no BB price."))
		}
	}
	
	return(indBB)
}

pbreex.augmentHoldings <- function(univ2Ticker, holdings ){
	
	inds <- match(univ2Ticker[["price"]], holdings[["name"]])
	if( any( is.na(inds))){
		holdings <- rbind(holdings, data.frame(name=univ2Ticker[is.na(inds), "price"], qty=0, sa2code=univ2Ticker[is.na(inds), "SA2Name"], type= NA))
	}
	
	otherHoldings <- univ2Ticker[ !is.na(univ2Ticker[["otherHolding"]] ) ,  ]
	inds2 <- match( otherHoldings[["otherHolding"]], holdings[["name"]] )
	if( any( is.na(inds2))){
		holdings <- rbind(holdings, data.frame(name=otherHoldings[is.na(inds2),"otherHolding"], qty=0, sa2code=otherHoldings[is.na(inds2), "SA2Name"], type= NA))
	}

	return(holdings)
}

pbreex.calculateWeights <- function(holdingsList, bbData, univ2Ticker, futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix, baseCc, miniContracts ){
	
	if( any( !is.element( c("SA2Name","price", "priceType","otherHolding","otherHoldingType", "pv01Eqv"), names(univ2Ticker)))){
		stop("ERROR in pbreex.calculateWeights(): universe to ticker mapping is supposed to have columns 'price' and 'securityType' ")
	}
	
	ret <- list()
	for (trader in names(holdingsList)){
		holdings <- holdingsList[[trader]]
		
		if( any( !is.element( c("qty", "name"), names(holdings))) ){
			stop(paste("ERROR in pbreex.calculateWeights(): holdings is expected to have a column named 'qty'. instead we have:", paste(names(holdings), collapse=", ")))
		}
		
		activeHoldings <- holdings[ holdings[["qty"]]!=0, ]

		holdings <- pbreex.augmentHoldings( univ2Ticker, activeHoldings )
		refData <- univ2Ticker[ is.element( univ2Ticker[["SA2Name"]], holdings[["sa2code"]]), ]
		
		sa2Names <- unique(holdings[["sa2code"]])
		holdingValues <- rep(0, length(sa2Names))
		names(holdingValues) <- sa2Names
		valuePerLot <- rep(0, nrow(holdings))
		for (i in 1:nrow(holdings) ){
			
			n <- holdings[i, "name"]
			sa2Name <- holdings[i, "sa2code"]
			ind <- match(sa2Name, refData[["SA2Name"]])
			if ( !is.finite( ind )){
				stop(paste("ERROR in pbreex.calculateWeights():",sa2Name,"has no reference data in univ2Ticker."))
			}
			securityRow <- refData[ ind, ]
			ccyPair <- paste(securityRow[["currency"]], "USD", sep="")
			indBB <- pbreex.getBBDataIndex(n, sa2Name, bbData, dirtyPriceSuffix, securityRow )
			nType <- ifelse( !is.na(holdings[i, "type"] ), holdings[i, "type"], ifelse( n== securityRow[["price"]], securityRow[["priceType"]], ifelse(n==securityRow[["otherHolding"]], securityRow[["otherHoldingType"]],NA)))
			if( is.na(nType)){
				stop(paste("ERROR in predictBuildReb: don't know the type of",n))
			}
			if( nType == "equityIndexFuture"){
				# remove "MY INDEX", where M is the expiration month, Y is the expiration year
				futurePrefix <- substr(n,1,nchar(n)-8)
				if( !is.element(futurePrefix, names(futureContractMultiplier)) ){
					stop(paste("ERROR in pbreex.calculateWeights(): the multiplier for future contract",futurePrefix,"was not specified"))
				}
				valuePerLot[i] <- bbData[ indBB, 3] * futureContractMultiplier[[futurePrefix]] * ifelse(ccyPair!="USDUSD", bbData[ bbData[, 2]==ccyPair, 3], 1)
				holdingValues[[sa2Name]] <- holdingValues[[sa2Name]] + holdings[ i, "qty" ] * valuePerLot[i]
			} else if( nType == "bond") {
				if( ccyPair != "USDUSD"){
					# NOTE: only worry for non-US is whether we multiply by factor of 10
					stop("ERROR in pbreex.calculateWeights(): logic for non-US bonds not implemented yet.")
				}
				valuePerLot[i] <- bbData[ indBB, 3] * 10
				holdingValues[[sa2Name]] <- holdingValues[[sa2Name]] + holdings[ i, "qty" ] * valuePerLot[i]
			} else if ( nType == "bondFuture" ){
				# remove "MY INDEX", where M is the expiration month, Y is the expiration year
				futurePrefix <- substr(n,1,nchar(n)-9)
				if( !is.element(futurePrefix, names(futureContractMultiplier)) ){
					stop(paste("ERROR in pbreex.calculateWeights(): the multiplier for future contract",futurePrefix,"was not specified"))
				}
				
				pvInd <- match( n, univ2Ticker[["otherHolding"]])
				if( !is.finite( pvInd)){
					stop(paste("ERROR in predictBuildReb: no pv01 equivalent factor specified for", n))
				}
				pv01Eqv <- univ2Ticker[ pvInd, "pv01Eqv" ]
				if (!is.finite( pv01Eqv )){
					stop(paste("ERROR in predictBuildReb: no pv01 equivalent factor specified for", n))
				}
				# the indBB returned is that of the underlying equivalent -- NOT the bond future. Note: we use 100 in the conversion of notionals below
				valuePerLot[i] <- bbData[ indBB, 3] * futureContractMultiplier[[futurePrefix]] * ifelse(ccyPair!="USDUSD", bbData[ bbData[, 2]==ccyPair, 3], 1)
				indSA2Name <- pbreex.getBBDataIndex(securityRow[["price"]], sa2Name, bbData, dirtyPriceSuffix, securityRow )
				if(securityRow[["priceType"]] != "bond"){
					stop(paste("ERROR in pbreex.calculateWeights(): can't handle bond futures in price column of universe-to-tickers mapping file"))
				}
				temp <- bbData[ indSA2Name, 3] * 10
				holdingValues[[sa2Name]] <- holdingValues[[sa2Name]] + holdings[ i, "qty" ] * 100 * pv01Eqv * temp
			} else {
				stop(paste("ERROR in pbreex.calculateWeights(): security type", securityRow[["securityType"]], "not implemented."))
			}
		}
		
		totalValue <- sum(holdingValues )
		newPositions <- holdingValues / totalValue
		cashWeights <- pbreex.setCurrentCashPositions( sa2Names, newPositions, cashNameSuffix, baseCc)
		newPositions <- data.frame( c(newPositions , cashWeights))
		rownames(newPositions) <- paste(c(names(holdingValues), names(cashWeights)), ".pos", sep="")

		currentInvestment <- holdings[["qty"]]*valuePerLot
		bondFuturesInd <-  match("bondFuture", holdings[["type"]] )
		bondFutures <- holdings[ bondFuturesInd , "name"]

		currentInvestment[ bondFuturesInd ] <- currentInvestment[ bondFuturesInd ] / as.numeric(univ2Ticker[ match(bondFutures, univ2Ticker[["otherHolding"]] ), "pv01Eqv" ])
		holdings <- cbind(holdings, data.frame(valuePerLot=valuePerLot, currentInvestment=currentInvestment ))

		ret <- append(ret, list(list(weights=newPositions,value=totalValue, holdings=holdings)))
		names(ret)[length(ret)] <- trader
	}

	return( ret )
}

pbreex.buildPositionData <- function(today, universeToSecsFileName, livePricesFileName, futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix, baseCc, positionsDBSettings, pathToRSourceCode, positionSuffix, miniContracts ){
	
	bbLiveData <- read.csv(livePricesFileName, stringsAsFactors = FALSE)
	univ2Ticker <- read.csv( universeToSecsFileName, stringsAsFactors=FALSE )
	
	print("INFO from predictBuildRebExec.R: Here are the market instruments that will be used:")
	print(univ2Ticker)
	print("QUESTION from predictBuildRebExec.R: OK with market instruments? (type yes or no )")
	response <- scan( what=character(), n= 1)
	if(response == "yes"){
		print("proceeding then...")
	} else {
		stop("you did not type yes. exiting...")
	}
	
	# aggregated by ticker
	holdings <- pmUtils.getCurrentPositions( positionsDBSettings )
	weightsList <- pbreex.calculateWeights( holdings, bbLiveData, univ2Ticker, futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix, baseCc, miniContracts)
	posTable <- pbreex.createPOSTable(pathToRSourceCode)
	
	universeLiveData <- posTable[1, !grepl( paste("\\",positionSuffix,"$",sep=""), colnames(posTable)),drop=F]
	# universeLiveData will possibly have instruments for which we didn't get live data, namely the instruments needed for seeding. This is why we take the intersection here
	commonBBNames <- intersect(colnames(universeLiveData), bbLiveData[, 2])
	universeLiveData[1, commonBBNames] <- as.numeric(bbLiveData[ match( commonBBNames, bbLiveData[, 2]), 3])
	universeLiveData[1, "Date"] <- today

	retList <- list()
	for( trader in names(weightsList)){
		weights <- weightsList[[trader]][["weights"]]
		
		if( any( !is.element(rownames(weights), colnames(posTable)))){
			stop(paste("ERROR in pbreex.buildPositionData():", trader,"holds positions in instruments that are no longer in the investable universe:",paste(rownames(weights)[ !is.element(rownames(weights), colnames(posTable))], collapse=", ")))
		}
		
		traderWeights <- as.data.frame(posTable[, grepl( paste("\\",positionSuffix,"$",sep=""), colnames(posTable)), drop = F] )
		traderWeights[, rownames(weights)] <- weights[ , 1]
		retList <- append(retList, list( traderWeights ))
		names(retList)[length(retList)] <- trader
	}

	return( append(list(liveData = universeLiveData), append(list(positions=retList), list(holdings=weightsList) ) ))
}

pbreex.getLiveDataAndPortfolioSettings<-function(today, fbSettings, universeToSecsFileName, livePricesFileName, futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix,positionsDBSettings, pathToRSourceCode, positionSuffix, miniContracts){
	
	portfolioSettings <- pbreex.getPortfolioSettings(today, fbSettings)
	livePosTables <- pbreex.buildPositionData(today, universeToSecsFileName, livePricesFileName, futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix, portfolioSettings[["portfolioSettings"]][["baseCc"]], positionsDBSettings, pathToRSourceCode, positionSuffix, miniContracts)	
	
	return( append(livePosTables, list( today=portfolioSettings[["today"]], portfolioSettings = portfolioSettings[["portfolioSettings"]]) ) )
}

pbreex.confirmLiveData <- function( today, liveData, positions, traders, outputDir ){
	
	posTables <- list()
	for( trader in traders){
		if( !is.element( trader, names(positions))){
			print(paste("ERROR in pbreex.confirmLiveData(): there is no information on the holdings of", trader))
		}
		
		print(paste("QUESTION from pbreex.confirmLiveData(): Here is the live data that will be used for trader:",trader))
		temp <-  cbind(liveData, positions[[trader]]) 
		posTables <- append(posTables, list(temp))
		names(posTables)[length(posTables)] <- trader
		print( temp )
		print(paste("OK with live data for",trader,"? (type yes if OK)"))
		response <- scan(what=character(),n=1)
		if(response == "yes"){
			print("proceeding then...")
		} else {
			print("you did not type yes. exiting...")
			return(NULL)
		}

	}
	
	# now save posTables to files
	for( trader in names(posTables)){
		write.table(t(posTables[[trader]]), file=paste(outputDir, "PBRExec.POS-",trader,"-",today,".csv", sep=""),sep=",",quote=F,row.names=T, col.names=F)
	}

	return( 1 )
}

pbreex.getMVUPortfolios <- function( whichPortfolio ){
	inds <- grep("MVU", whichPortfolio)
	
	ret <- NULL
	if(length(inds) != 0 ){
		for (i in inds){
			ret <- c(ret, whichPortfolio[[i]])
		}
	}
	
	return( ret )
}

pbreex.setFBSettings <- function( today, pathToRSourceCode ){
	
	fbSettings <- global.frontierBuilderExecSettings()
	
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/",fbSettings[["fbex.constraints"]],sep=""))
	portfolioName<-constraintsFiles.portfolioNames()
	if(length(portfolioName) != 1){
		stop(paste("ERROR in predictBuildRebExec.main(): this implementation allows for 1 portfolio only. in",paste(pathToRSourceCode,"inputFiles/",fbSettings[["fbex.constraints"]],sep=""),"the following were specified instead:",paste(portfolioName,collapse=", ")))
	}
	fbSettings[["pathToRSourceCode"]]<-pathToRSourceCode
	fbSettings[["portfolioName"]]<-portfolioName
	fbSettings[["fbex.outputDir"]]<-utils.createSubDir(today, fbSettings[["fbex.outputDir"]])
	fbSettings[["fbex.methods"]] <- c("MV", "MVU")
	# TODO a bug in frontierBuilder prevents us from specifying one parameter only here (i.e., 450). same below
	fbSettings[["fbex.parametersList"]] <- list(MVU=c(200,450))
	fbSettings[["fbex.shortingControl"]] <- list( doShort="none", threshold=list(), securityER=list() )
	
	return( fbSettings)
}

pbreex.setPRDSettings <- function( today, outputPrefix, pathToRSourceCode){
	
	prdSettings <- global.predictExecSettings()	
	
	prdSettings[["pathToRSourceCode"]]<-pathToRSourceCode
	prdSettings[["prd.outputFile"]] <- paste(outputPrefix,prdSettings[["prd.outputFile"]],sep="")
	prdSettings[["prd.outputDir"]] <- utils.createSubDir(today, prdSettings[["prd.outputDir"]] )
	
	return( prdSettings )
}

pbreex.write <- function( today, trader, textToWrite, outputDir, append, nColumns, separator, isTable ){
	
	print( textToWrite )
	
	outputFileName <- paste(outputDir,"PBRExec.DECISION-",trader,"-", today, ".csv", sep="")
	if(isTable){
		write( colnames(textToWrite), file= outputFileName, append=append, ncolumns= nColumns, sep= separator)
		for(i in 1:nrow(textToWrite)){
			write( as.character(textToWrite[i,]), file= outputFileName, append=append, ncolumns= nColumns, sep= separator)
		}
	} else {	
		write( textToWrite, file= outputFileName, append=append, ncolumns= nColumns, sep= separator)
	}
}

pbreex.pushToZero <- function( v, tolerance){
	
	v[ abs(v) < tolerance ] <- 0
	v <- v / sum(v)
	
	return(v)
}

pbreex.targetHoldings <- function(holdings, targetPort,  totalValue){

	inds <- match( holdings[["sa2code"]], colnames(targetPort))
	targetInvestmentValues <- as.numeric( targetPort[1, inds]) * totalValue
	targetNumberLots <- targetInvestmentValues / as.numeric(holdings[["valuePerLot"]])
	
	return( cbind(holdings, data.frame(targetInvestmentValues=targetInvestmentValues, targetNumberLots=targetNumberLots)))
}

predictBuildRebExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")
	
	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	settings <- global.predictBuildRebExecSettings()
	
	# this is the leverage creep threshold
	leverageThreshold <- settings[["pbreex.leverageThreshold"]]
	# what hurdle to use when rebalancing using erRiskRatio?
	erRiskRatioHurdle <- settings[["pbreex.erRiskRatioHurdle"]]
	# what hurdle to use when rebalancing using euclidean policy?
	euclideanHurdle <- settings[["pbreex.euclideanHurdle"]]
	# read live data from this file
	liveDataFile <- settings[["pbreex.liveDataFile"]]
	# in a portfolio file, the first asset weight is in this column
	firstColumn <- settings[["pbreex.firstColumn"]]
	# use this to prefix all files output by this program
	outputPrefix<- settings[["pbreex.outputPrefix"]]
	positionSuffix <- settings[["pbreex.positionSuffix"]]
	# this one is not really used
	varScaleFactor <- settings[["pbreex.varScaleFactor"]]
	# where to send output from this file
	outputDir <- settings[["pbreex.outputDir"]]
	inputDir <- settings[["pbreex.inputDir"]]
	traders <- settings[["pbreex.traders"]]
	whichPortfolio <- settings[["pbreex.whichPortfolio"]]
	reallocDecision <- settings[["pbreex.reallocDecision"]]
	universeToSecsFileName <- settings[["pbreex.universeToSecuritiesMappingFile"]]
	livePricesFileNamePrefix <- settings[["pbreex.livePricesFileNamePrefix"]]
	futureContractMultiplier <- settings[["pbreex.futureContractMultiplier"]]
	dirtyPriceSuffix <- settings[["pbreex.dirtyPriceSuffix"]]
	cashNameSuffix <- settings[["pbreex.cashNameSuffix"]]
	positionsDBSettings <- settings[["pbreex.positionsDBSettings"]]
	tolerance <- settings[["pbreex.weightTolerance"]]
	miniContracts <- settings[["pbreex.minis"]]
	
	print("QUESTION from predictBuildRebExec.main(): in addition to the settings in predictExecSettings() and frontierBuilderExecSettings() (NOTE: some of those settings get ignored or modified here), here we use these settings:")
	print(t(data.frame(leverageThreshold=leverageThreshold)))
	print("OK with additional settings? (type yes if OK or type something else and modify the settings in global.predictBuildRebExecSettings())")
	
	source(paste(pathToRSourceCode,"utils/dbDataLoader.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/frontierBuilder.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/varianceCovariance.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/specialPortfolios.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/fbUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/mvOpt.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioConstruction/constructionMethods.R",sep=""))
	source(paste(pathToRSourceCode,"utils/erRiskDomesticEquivalent.R",sep=""))
	source(paste(pathToRSourceCode,"utils/miscUtils.R",sep=""))
	source(paste(pathToRSourceCode,"utils/algorithms.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/equityRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/bondRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsReturnModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/tipsRiskModels.R",sep=""))
	source(paste(pathToRSourceCode,"forecasting/predict.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/rebalancing.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/reinvesting.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/pmUtils.R",sep=""))
	source(paste(pathToRSourceCode,"portfolioManagement/createLivePX.R",sep=""))
		
	library(timeSeries)
	library(quadprog)
	library(limSolve)
	library(RODBC)

	# what is today's date?
	today <- utils.today()	
	
	print(paste("INFO from predictBuildRebExec.main(): today is",today))
	
	# prd settings
	prdSettings <- pbreex.setPRDSettings( today, outputPrefix, pathToRSourceCode )
	# TODO .ber hack
	prdSettings[["prd.gdpSuffix"]]<-c(".gdprate")
	prdSettings[["eretm.numGrowthYears"]]<-10
	prdSettings[["eretm.doAverageGrowthOverHistory"]] <- FALSE
	prdSettings[["prd.useMacroDB"]]<- TRUE
	prdSettings[["eretm.nonLocalMacroGrowth"]]<-list()
	prdSettings[["eretm.nonLocalInflation"]]<-list()
	prdSettings[["eretm.seedSource"]]<-list()
	prdSettings[["eretm.growthBenchmark"]]<-list()
	# fb settings
	fbSettings <- pbreex.setFBSettings( today, pathToRSourceCode )
		
	# latest data and positions
	liveDataAndPortfolio<-pbreex.getLiveDataAndPortfolioSettings(today, fbSettings, paste(inputDir, universeToSecsFileName,sep=""), paste(inputDir, livePricesFileNamePrefix,today,".csv",sep=""), futureContractMultiplier, dirtyPriceSuffix, cashNameSuffix, positionsDBSettings, pathToRSourceCode, positionSuffix, miniContracts)
	liveData <- liveDataAndPortfolio[["liveData"]]
	positions <- liveDataAndPortfolio[["positions"]]
	today <- liveDataAndPortfolio[["today"]]
	portfolioSettings <- liveDataAndPortfolio[["portfolioSettings"]]
	holdings <- liveDataAndPortfolio[["holdings"]]

	outputDir <- utils.createSubDir(today, outputDir )
	if( is.null(pbreex.confirmLiveData( today, liveData, positions, traders, outputDir )) ) return()
	
	# prd
	pbOutput<-pbreex.prd(today,liveData,prdSettings)
	# fb
	mvuPortfolios <- pbreex.getMVUPortfolios( whichPortfolio )
	fbOutput<-pbreex.fb( today,liveData,fbSettings,portfolioSettings,pbOutput,outputPrefix, mvuPortfolios )

	for ( trader in traders){
		# target and current portfolios
		targetPort <- pbreex.getTargetPortfolio(whichPortfolio, trader, fbOutput[["output"]], firstColumn)
		curPort <- pbreex.getCurrentPortfolio(colnames(targetPort),positions[[trader]], positionSuffix)
		
		alignedData <- utils.alignSeries(list(fbOutput[["ersRisks"]],fbOutput[["rfRates"]]),TRUE)
		fbOutput[["ersRisks"]] <- erRiskDomesticEquivalent.convertErsRisks(alignedData[,names(fbOutput[["ersRisks"]])], fbOutput[["baseCc"]], alignedData[,names(fbOutput[["rfRates"]])], fbSettings[["frnt.couponsPerYear"]], fbSettings[["frnt.conversionRateNameSuffix"]], fbSettings[["frnt.doRisks"]])
		
		# calculate rebalancing decision
		if ( all( is.finite(curPort ))){
			doRebalance <- pbreex.rebalance(today, reallocDecision, trader, curPort, targetPort, fbOutput[["ersRisks"]], fbOutput[["leverage"]], leverageThreshold, erRiskRatioHurdle, euclideanHurdle, fbOutput[["riskMeasure"]], fbOutput[["rollingWindow"]], fbOutput[["halfLife"]], varScaleFactor, colnames(curPort)[!grepl(fbSettings[["frnt.cashNameSuffix"]],colnames(curPort)) ] )
		} else {
			doRebalance <- TRUE
		}
			
		pbreex.write(today, trader, paste("INFO from predictBuildReb.R: ",trader,"'s total portfolio value is,", holdings[[trader]][["value"]]), outputDir, FALSE, 1, " ", FALSE)
		if(doRebalance){
			pbreex.write(today, trader, paste("INFO from predictBuildReb.R: ",trader,"'s rebalancing decision is: DO rebalance from:"), outputDir, TRUE, 1, " ", FALSE)
			curPort[1,] <- curPort[1,]
			pbreex.write(today,trader, pbreex.pushToZero(curPort, tolerance), outputDir, TRUE, ncol(curPort), ", ", TRUE)
			pbreex.write(today,trader, "\n\n", outputDir, TRUE, 1, ", ",FALSE)
			pbreex.write(today,trader, "to:", outputDir, TRUE, 1, " ", FALSE)
			targetPort[1,] <-  pbreex.pushToZero(targetPort, tolerance)
			pbreex.write(today,trader, targetPort, outputDir, TRUE, ncol(targetPort), ", ", TRUE)
			pbreex.write(today,trader, "\n\n", outputDir, TRUE, 1, ", ", FALSE)
			holdings[[trader]][["holdings"]] <- pbreex.targetHoldings(holdings[[trader]][["holdings"]], targetPort,  holdings[[trader]][["value"]])
			pbreex.write(today,trader, holdings[[trader]][["holdings"]], outputDir, TRUE, ncol(holdings[[trader]][["holdings"]]), ", ", TRUE)
		} else {
			pbreex.write(today,trader, paste("INFO from predictBuildReb.R: ",trader,"'s rebalancing decision is: DO NOT rebalance. continue to use:"), outputDir, TRUE, 1, " ", FALSE)
			curPort[1,] <- curPort[1,]
			pbreex.write(today,trader, pbreex.pushToZero(curPort, tolerance), outputDir, TRUE, ncol(curPort), ", ", TRUE)
			pbreex.write(today,trader, "\n\n", outputDir, TRUE, 1, ", ",FALSE)
			pbreex.write(today,trader, "DO NOT use this target:", outputDir, TRUE, 1, " ", FALSE)
			targetPort[1,] <-  pbreex.pushToZero(targetPort, tolerance)
			pbreex.write(today,trader, targetPort, outputDir, TRUE, ncol(targetPort), ", ", TRUE)
			pbreex.write(today,trader, "\n\n", outputDir, TRUE, 1, ", ",FALSE)
			holdings[[trader]][["holdings"]] <- pbreex.targetHoldings(holdings[[trader]][["holdings"]], targetPort,  holdings[[trader]][["value"]] )
			pbreex.write(today,trader, holdings[[trader]][["holdings"]], outputDir, TRUE, ncol(holdings[[trader]][["holdings"]]), ", ", TRUE)
		}
		
	}
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
predictBuildRebExec.main()
