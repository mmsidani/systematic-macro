pmex.readPortfolios<-function(inputDir, portfolioFiles){
# this requires portfolioFileNames to be a vector of file names; inputDir is just a string
# returns a list with one item for each portfolio file containing the history of the portfolio
	
	ports<-NULL
	for(p in portfolioFiles){		
		ports<-append(ports,list(read.csv(paste(inputDir,p,sep=""),header=TRUE,stringsAsFactors=FALSE)))
	}

	return(ports)
}

pmex.createOneSecPortfolios<-function(assetsForOneSecPortfolios,ers,risks,useDB,inputDATADir,erRisksFile,dateFormat, mixedPortfolioFrequency,dayOfWeek,dbSettings){
# return a list() of portfolio, one for each asset in assetsForOneSecPortfolios, at the frequency specified in mixedPortfolioFrequency, and with weight 1.0 in that asset. portRet and portRisk are the forecast er and risk
	
	# which data do we need, this is specifically er/risk data
	idsToGet<-c()
	for(a in assetsForOneSecPortfolios){
		indEr<-grep(a,ers)
		indRisk<-grep(a,risks)
		if(length(indEr)!=1 || length(indRisk)!=1){
			stop(paste("error in pmex.createOneSecPortfolios(): no or more than 1 er or 1 risk were specified for asset",a))
		}
		# union() just in case we duplicated some asset in assetsForOnSecPortfolios
		idsToGet<-union(idsToGet,c(ers[indEr],risks[indRisk]))
	}
	
	# we have id's, now get data
	if(useDB){
		ersRisks<-utils.getDataFromDB(idsToGet,NULL,dbSettings)
	}else{
		ersRisks<-utils.getDataFromFile(idsToGet,NULL,paste(inputDATADir,erRisksFile,sep=""),dateFormat)
	}
	
	# filter data at desired frequency
	if(mixedPortfolioFrequency=="monthly"){
		# filter EOM data and exclude stubs
		inds<-utils.getEOMDatesIndices(row.names(ersRisks),TRUE)
	}else if(mixedPortfolioFrequency=="weekly"){
		inds<-utils.getWeeklyDatesIndices(row.names(ersRisks), dayOfWeek)
	}else if(mixedPortfolioFrequency!="daily"){
		# nothing to do if "daily"
		stop(paste("error in pmex.createOneSecPortfolios(): the mixed portfolio frequency is not implemented. you passed:", mixedPortfolioFrequency))
	}
	
	ersRisks<-ersRisks[inds,]
	
	# now form portfolios
	ports<-NULL
	for(a in assetsForOneSecPortfolios){
		indEr<-grep(a,ers)
		indRisk<-grep(a,risks)
		
		isNotNA<-is.finite(ersRisks[,ers[indEr]]) & is.finite(ersRisks[,risks[indRisk]])
		weights<-rep(NA,nrow(ersRisks))
		weights[isNotNA]<-1
		# the portfolio history for this asset; portRet and portRisk for each portfolio in the history are the er/risk of the corresponding asset
		newPortfolio<-data.frame(Date=row.names(ersRisks),portRet=as.numeric(ersRisks[,ers[indEr]]),portRisk=as.numeric(ersRisks[,risks[indRisk]]),newAsset=weights)
		names(newPortfolio)[ncol(newPortfolio)]<-a
		
		ports <-append(ports,list(newPortfolio))
	}

	return(ports)
}

pmex.getRFRates<-function(baseCashName,ports,cashNameSuffix,riskFreeSuffix,useDB,inputDATADir,dataFileName,dateFormat,dbSettings){
	# get all country codes
	ccs<-c()
	for(i in 1:length(ports)){
		# get it off the first asset in the first portfolio in the history
		ccs<-union(ccs,substr(names(ports[[i]])[4],1,2))
	}
	
	ratesToGet<-union(union(paste(ccs, cashNameSuffix,sep=""),paste(ccs,riskFreeSuffix,sep="")),baseCashName)
	
	if(useDB){
		return(utils.getDataFromDB(ratesToGet,NULL,dbSettings))
	} else {
		return(utils.getDataFromFile(ratesToGet,NULL,paste(inputDATADir,dataFileName,sep=""),dateFormat))
	}
}

pmex.addCashIfMissing<-function(ports, cashNameSuffix){
# this requires ports to be a list()
	
	for(i in 1:length(ports)){
		port<-ports[[i]]
		# each portfolio that is passed here would have assets from one country
		countryCode <- substr(names(port)[4],1,2)
		cashName <- paste(countryCode, cashNameSuffix, sep="")
		cashInd <- match(cashName, names(port))
		# add cash if port doesn't have it
		if(!is.finite(cashInd)){
			port<-cbind(port, cashCol=0)
			names(port)[ncol(port)]<-cashName
			ports[[i]] <- port
		}
	}
	
	return(ports)
}

portfolioMixer.input<-function(settings){
	doOneSecPortfolios<-settings[["pmex.doOneSecPortfolios"]]
	baseCashName<-settings[["pmex.baseCashName"]]
	inputDir<-settings[["pmex.inputDir"]]
	inputPortfolioFiles<-settings[["pmex.inputPortfolioFiles"]]
	assetsForOneSecPortfolios<-settings[["pmex.assetsForOneSecPortfolios"]]
	ers<-settings[["pmex.ers"]]
	risks<-settings[["pmex.risks"]]
	useDB<-settings[["pmex.useDB"]]
	inputDATADir<-settings[["pmex.inputDATADir"]]
	erRisksFile<-settings[["pmex.erRisksFile"]]
	dateFormat<-settings[["pmex.dateFormat"]]
	mixedPortfolioFrequency<-settings[["pmex.mixedPortfolioFrequency"]]
	dayOfWeek<-settings[["pmex.dayOfWeek"]]
	cashNameSuffix<-settings[["pmex.cashNameSuffix"]]
	riskFreeSuffix<-settings[["pmex.riskFreeSuffix"]]
	dataFileName<-settings[["pmex.dataFileName"]]
	dbSettings<-settings[["dbSettings"]]
	
	if(doOneSecPortfolios){
		portfolios<-pmex.createOneSecPortfolios(assetsForOneSecPortfolios,ers,risks,useDB,inputDATADir,erRisksFile,dateFormat, mixedPortfolioFrequency,dayOfWeek,dbSettings)
	}else{
		portfolios<-pmex.readPortfolios(inputDir,inputPortfolioFiles)
	}
	portfolios<-pmex.addCashIfMissing(portfolios, cashNameSuffix)
	
	rfRates<-pmex.getRFRates(baseCashName,portfolios,cashNameSuffix,riskFreeSuffix,useDB,inputDATADir,dataFileName,dateFormat,dbSettings)
	
	return(list(portfolios=portfolios,rfRates=rfRates))
}

portfolioMixer.output<-function(output,settings){
	outputDir<-settings[["pmex.outputDir"]]
	outputFile<-settings[["pmex.outputFile"]]
	
	write.csv(output[["mixedPort"]], paste(outputDir, outputFile, sep=""), row.names=FALSE)
	rootFileName <- sub("\\.csv","",outputFile)
	for( n in setdiff( names(output), "mixedPort")){
		write.csv(output[[n]], paste(outputDir, rootFileName, ".", n, ".csv", sep=""), row.names=FALSE)
	}
}

pmex.policyOptimalERWeights <-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# portfolios is a list of portfolios; constraints is a list with 2 items "lowerBounds" and "upperBounds"; numPortsToKeep is ignored -- it's here for compatibility with other policy functions
	
	if(length(portfolios)!=length(constraints[[1]]) || length(portfolios)!=length(constraints[[2]])){
		stop("error in pmex.policyOptimalERWeights(): every portfolio must have a lower and an upper bound specified for its weight in the mixed portfolio")
	}
	thisDate<-as.Date(portfolios[[1]]$Date)
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	
	portRets<-c()
	counter<-1
	keepInds<-c()
	for(p in portfolios){
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		if(is.finite(p$portRet) && is.finite(cashRate) && is.finite(foreignCash)){
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			portRets<-c(portRets, domEqRet)
			keepInds <-c(keepInds,counter)
		}
		counter<-counter+1
	}

	numPorts<-length(keepInds)
	if(numPorts==1){
		return(as.numeric((1:length(portfolios))==keepInds)*constraints[["upperBounds"]][keepInds])
	}
	lowerBounds<-constraints[["lowerBounds"]][keepInds]
	upperBounds<-constraints[["upperBounds"]][keepInds]

	lpResults<-linp(E=matrix(rep(1,numPorts),nrow=1),F=c(1),G=rbind(diag(rep(1, numPorts)),diag(rep(-1, numPorts)),deparse.level=0),H=c(lowerBounds,-upperBounds), -portRets,ispos=TRUE,verbose=TRUE)

	if(lpResults[["IsError"]]){
		stop("error in pmex.policyOptimalERWeights(): linear programming solver failed")
	}
	
	ret<-rep(0,length(portfolios))
	ret[keepInds]<-lpResults[["X"]]
	return(ret)
}

pmex.policyBestERSpread<-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# rank portfolios by the spread of their ER's to risk free rates; constraints is not used; it's there so that all policy functions have the same arguments and since constraints are required in pmex.policyOptimalERWeights()
	
	thisDate<-as.Date(portfolios[[1]]$Date)
	portERSpreads<-c()
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	rfRate<-rateData[thisDate,paste(baseCc,riskFreeSuffix,sep="")]
	for(p in portfolios){
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		# domestic equivalent return
		if(is.finite(p$portRet) && is.finite(cashRate) && is.finite(foreignCash)&& is.finite(rfRate)){
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			portERSpreads <-c(portERSpreads, domEqRet-rfRate)
		} else {
			portERSpreads <-c(portERSpreads, NA)
		}
	}
	
	rankings<-order(portERSpreads, na.last=NA, decreasing=TRUE)
	return(rankings[1:min(length(rankings),numPortsToKeep)])
}

pmex.policySharpeRatio<-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# constraints is not used
	
	thisDate<-as.Date(portfolios[[1]]$Date)
	portERSpreads<-c()
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	rfRate<-rateData[thisDate,paste(baseCc,riskFreeSuffix,sep="")]
	portSharpes<-c()
	for(p in portfolios){
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		if(is.finite(p$portRet) && is.finite(p$portRisk) && is.finite(cashRate) && is.finite(foreignCash)&& is.finite(rfRate)){
			# domestic equivalent return
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			# domestic equivalent risk
			domEqRisk<-p$portRisk * (1+ foreignCash) /(1+ cashRate)
			portSharpes <-c(portSharpes, (domEqRet-rfRate) / domEqRisk)
		} else {
			portSharpes <-c(portSharpes, NA)
		}
	}
	
	rankings<-order(portSharpes, na.last=NA, decreasing=TRUE)
	return(rankings[1:min(length(rankings),numPortsToKeep)])
}

pmex.policyProgressiveRisk <-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# constraints, numPortsToKeep, riskFreeSuffix not used
	
	if(length( portfolios) == 1){
		# nothing to do 
		
		return( 1 )
	}
	
	thisDate<-as.Date(portfolios[[1]]$Date)
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	
	# Note: we assume the first portfolio in the array is the base portfolio -- i.e., the one we want to be in unless the gain in expected return from some other portfolio in the array is larger than the increase in risk
	basePortfolio <- portfolios[[1]]
	# Note: we rely on the first asset appearing in names() to be in the home currency. it's typically the base bash position as output from frontierBuilder
	cc<-substr(names(basePortfolio)[4],1,2)
	if(cc!=baseCc){
		foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
	} else{
		foreignCash<-cashRate
	}
	if(is.finite(basePortfolio$portRet) && is.finite(basePortfolio$portRisk) && is.finite(cashRate) && is.finite(foreignCash)){
		# domestic equivalent return
		baseDomEqRet<-(1+basePortfolio$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
		# domestic equivalent risk
		baseDomEqRisk<-basePortfolio$portRisk * (1+ foreignCash) /(1+ cashRate)
	} else {
		stop("ERROR in pmex.policyProgressiveRisk(): the base portfolio cannot have a shorter history than the other portfolios.")
	}
		
	portRetRisks <-c()
	for(i in 2:length(portfolios) ){
		
		p <- portfolios[[i]]
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		if(is.finite(p$portRet) && is.finite(p$portRisk) && is.finite(cashRate) && is.finite(foreignCash)){
			# domestic equivalent return
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			# domestic equivalent risk
			domEqRisk<-p$portRisk * (1+ foreignCash) /(1+ cashRate)
			
			# The formula: the ratio of the increase in return over the increase in risk
			portRetRisks<-c(portRetRisks,  (domEqRet - baseDomEqRet ) / ( domEqRisk - baseDomEqRisk ) )
		} else{
			portRetRisks<-c(portRetRisks, NA)
		}
	}

	rankings<-order(portRetRisks, na.last=NA, decreasing=TRUE)
	# TODO to be revisited. the next clause is rarely true
	if(portRetRisks[ rankings[1 ] ] > 1 ){
		# return the portfolio that gives the biggest win of return over risk only if the gain in return is larger than the increase in risk. Note: we add one since the for loop started at 2
		return( rankings[ 1 ] + 1 )
	} else {
		# return the base portfolio
		return( 1 )
	}
}

pmex.policyBestEROverRisk<-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# constraints is not used
	
	thisDate<-as.Date(portfolios[[1]]$Date)
	portERSpreads<-c()
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	portRetRisks <-c()
	for(p in portfolios){
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		if(is.finite(p$portRet) && is.finite(p$portRisk) && is.finite(cashRate) && is.finite(foreignCash)){
			# domestic equivalent return
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			# domestic equivalent risk
			domEqRisk<-p$portRisk * (1+ foreignCash) /(1+ cashRate)
			portRetRisks<-c(portRetRisks,  domEqRet/domEqRisk)
		} else{
			portRetRisks<-c(portRetRisks, NA)
		}
	}
	
	rankings<-order(portRetRisks, na.last=NA, decreasing=TRUE)
	return(rankings[1:min(length(rankings),numPortsToKeep)])
}

pmex.policyBestER<-function(portfolios, numPortsToKeep, baseCashName, rateData, constraints,riskFreeSuffix,cashNameSuffix){
# constraints is not used
	
	thisDate<-as.Date(portfolios[[1]]$Date)
	portERSpreads<-c()
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[thisDate,baseCashName]
	portRets<-c()
	for(p in portfolios){
		cc<-substr(names(p)[4],1,2)
		if(cc!=baseCc){
			foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
		} else{
			foreignCash<-cashRate
		}
		if(is.finite(p$portRet) && is.finite(p$portRisk) && is.finite(cashRate) && is.finite(foreignCash)){
			# domestic equivalent return
			domEqRet<-(1+p$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
			portRets<-c(portRets, domEqRet)
		} else {
			portRets<-c(portRets, NA)
		}
	}
	
	rankings<-order(portRets, na.last=NA, decreasing=TRUE)
	return(rankings[1:min(length(rankings),numPortsToKeep)])
}

pmex.unionDates<-function(ports,dateFormat){
	dates<-c()
	for(p in ports){
		dates<-union(dates,p$Date)
	}

	return(as.character(sort(as.Date(dates, format=dateFormat),decreasing=TRUE)))
}

pmex.domesticPerspective<-function(portfolio,baseCashName,thisDate,rateData,cashNameSuffix){
# convert portfolio$portRet and portfolio$portRisk to baseCashName perspective
# portfolio is 1-row data.frame(); baseCashName is a string, "us.gg.3m", say; thisDate is a string and rateData is a timeSeries()
# returns a list() of 2 items, domEqRet and domEqRisk
	
	cc<-substr(names(portfolio)[4],1,2)
	baseCc<-substr(baseCashName,1,2)
	cashRate<-rateData[as.Date(thisDate),baseCashName]
	if(cc!=baseCc){
		foreignCash<-rateData[thisDate,paste(cc,cashNameSuffix,sep="")]
	} else{
		foreignCash<-cashRate
	}
	if(is.finite(portfolio$portRet) && is.finite(portfolio$portRisk) && is.finite(cashRate) && is.finite(foreignCash)){
		# domestic equivalent return
		domEqRet<-(1+portfolio$portRet)*(1+ cashRate)/(1+ foreignCash)-1.
		# domestic equivalent risk
		domEqRisk<-portfolio$portRisk * (1+ foreignCash) /(1+ cashRate)
	}else{
		domEqRet<-0
		domEqRisk<-0
	}
	
	return(list(domEqRet=domEqRet,domEqRisk=domEqRisk))
}

pmex.mix<-function(portfolios, policyFunction, policyFunctionName, portfolioWeights, constraints, rfRates,baseCashName,dateFormat,riskFreeSuffix,cashNameSuffix){
	if(sum(portfolioWeights)!=1){
		stop(paste("error in pmex.mix(): portfolioWeights sum to ",sum(portfolioWeights),", not 1"))
	}
	
	dates<-pmex.unionDates(portfolios,dateFormat)
	rankingsMatrix <- matrix(nrow=length(dates),ncol=length(portfolioWeights))
	ret<-NULL
	for(d in 1:length(dates)){
		ports<-NULL
		for(p in portfolios){
			dateInd<-match(dates[d], p$Date)
			if(is.finite(dateInd)){
				ports<-append(ports, list(p[dateInd,]))
			} else {
				temp<-data.frame(matrix(rep(NA, ncol(p)),nrow=1))
				names(temp)<-names(p)
				temp$Date<-dates[d]
				ports<-append(ports, list(temp))
			}
		}
		
		if(policyFunctionName != "pmex.policyOptimalERWeights"){
			# rankings in descending order
			rankings<- policyFunction(ports,length(portfolioWeights), baseCashName, rfRates, constraints,riskFreeSuffix,cashNameSuffix)
		} else {
			# this one returns weights as opposed to rankings; TODO: confusing?
			portfolioWeights<- pmex.policyOptimalERWeights(ports,length(portfolioWeights), baseCashName, rfRates, constraints,riskFreeSuffix,cashNameSuffix)
			rankings<-1:length(ports)
		}

		rankingsMatrix[ d, ] <- rankings
		# initialize newPort including a baseCashName column
		newPort<-data.frame(Date=dates[d],portRet=0,portRisk=0,baseCashName=0)
		names(newPort)[4]<-baseCashName
		for(i in 1:length(ports)){
			port<-ports[[i]]
			portInd <- match(i, rankings)
			if(is.finite(portInd) && is.finite(port$portRet)){
				nonNAs <- as.logical(is.finite(as.numeric(port[1,4:ncol(port)])))
				cashInd<-grep(cashNameSuffix,names(port))
				# the first cash column in the portfolio determines the currency of the portfolio
				cashName<-names(port)[cashInd[1]]

				port[1,4:ncol(port)][nonNAs] <- port[1,4:ncol(port)][nonNAs] * portfolioWeights[portInd]
				domEqs<-pmex.domesticPerspective(port,baseCashName,port[1,"Date"], rfRates,cashNameSuffix)
				port[1,2] <- domEqs[["domEqRet"]] * portfolioWeights[portInd] # scale portRet
				port[1,3] <- (domEqs[["domEqRisk"]] * portfolioWeights[portInd])^2 # scale portRisk; we take the square root later below
				if(cashName!=baseCashName){
					# fx unit hedge
					port[[cashName]][1] <- port[[cashName]][1]-portfolioWeights[portInd]
					newPort[[baseCashName]][1]<-newPort[[baseCashName]][1] + portfolioWeights[portInd]
				}
			} else {
				port[1,2:ncol(port)]<-0
			}			

			newAssets<-setdiff(names(port)[2:ncol(port)], names(newPort))
			for(a in names(port[2:ncol(port)])){
				if(is.element(a,newAssets)){
					temp<-data.frame(newCol=port[[a]][1])
					newPort<-cbind(newPort,temp)
					names(newPort)[ncol(newPort)]<-a
				} else {
					# if a is portRet or portRisk then we get here also
					newPort[[a]][1]<-newPort[[a]][1]+port[[a]][1]
				}
			}
		}

		# we had accumulated variances in newPort[1,3], so take sqrt to get risk
		newPort[1,3]<-sqrt(newPort[1,3])
		finiteNewPort<-is.finite(as.numeric(newPort[1,4:ncol(newPort)]))
		newPort[[baseCashName]][1]<-newPort[[baseCashName]][1] + (1 - sum(newPort[1,4:ncol(newPort)][finiteNewPort]))

		ret<-rbind(ret,newPort)
	}
	
	rankingsDF <- cbind( data.frame(Date=dates), data.frame(rankingsMatrix))
	names( rankingsDF ) <- c("Date", paste("active.", 1:ncol(rankingsMatrix), sep=""))
	
	return(list(mixedPort = ret, activePortfolios = rankingsDF ))
}

portfolioMixer.calculate<-function(input,settings){
	policyFunction<-settings[["pmex.policyFunction"]]
	mixedPortfolioWeights <-settings[["pmex.portfolioWeights"]]
	constraints<-settings[["pmex.portfolioConstraints"]]
	policyFunction<-settings[["pmex.policyFunction"]]
	baseCashName<-settings[["pmex.baseCashName"]]
	riskFreeSuffix<-settings[["pmex.riskFreeSuffix"]]
	cashNameSuffix<-settings[["pmex.cashNameSuffix"]]
	dateFormat<-settings[["pmex.dateFormat"]]
	annualizeRisk <- settings[["pmex.annualizeRisk"]]
	
	rfRates<-input[["rfRates"]]
	portfolios<-input[["portfolios"]]
	
	if (policyFunction == "pmex.policyProgressiveRisk"){
		if( length( mixedPortfolioWeights ) != 1 || mixedPortfolioWeights[ 1 ] != 1){
			print("WARNING in portfolioMixer.calculate(): pmex.policyProgressiveRisk requires pmex.portfolioWeights in the settings to be 1. making the change and proceeding.")
		}
		# yes, whether set the proper way or not we do this.
		mixedPortfolioWeights <- 1
		# annulize risk. why? because portfolio risk as calculated in frontierBuilder is not annualized and results for pmex.policyProgressiveRisk would be distorted without annualization
		for (i in 1:length(portfolios)){
			portfolios[[i]][, "portRisk"] <-  portfolios[[i]][, "portRisk"] * annualizeRisk
		}
	}
	
	policyFunctionName<-policyFunction
	if(policyFunction=="pmex.policyOptimalERWeights"){
		policyFunction<-pmex.policyOptimalERWeights
	}else if(policyFunction=="pmex.policyBestERSpread"){
		policyFunction<-pmex.policyBestERSpread
	}else if(policyFunction=="pmex.policySharpeRatio"){
		policyFunction<-pmex.policySharpeRatio
	}else if(policyFunction=="pmex.policyBestEROverRisk"){
		policyFunction<-pmex.policyBestEROverRisk
	}else if(policyFunction=="pmex.policyBestER"){
		policyFunction<-pmex.policyBestER
	}else if (policyFunction== "pmex.policyProgressiveRisk") {
		policyFunction<- pmex.policyProgressiveRisk
	}else{
		stop(paste("error in portfolioMixerExec.R: the policy function ",policyFunction,"does not exist"))
	}

	return( pmex.mix(portfolios, policyFunction, policyFunctionName, mixedPortfolioWeights,constraints, rfRates, baseCashName,dateFormat,riskFreeSuffix,cashNameSuffix) )
}


