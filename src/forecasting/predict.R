prd.getMacroCcs<-function(assets, nonLocalMacroGrowth, nonLocalInflation,useGNI){
# determine cc's for which macro data is needed
# assets is a list of strings, typically equity asset names
# returns list() indexed by "inflationCcs", "gdpCcs", "gniCcs"
	
	# although macroCoeffs is not used here, we check correctness of input here instead of waiting until we are in equityReturnModels
	badSpecs<-c()
	for(a in intersect(assets,names(nonLocalMacroGrowth))){
		if((length(nonLocalMacroGrowth[[a]][["macroCcs"]])!=length(nonLocalMacroGrowth[[a]][["macroCoeffs"]])) || (sum(nonLocalMacroGrowth[[a]][["macroCoeffs"]])!=1)){
			badSpecs <-c(badSpecs,a)
		}
	}
	for(a in intersect(assets,names(nonLocalInflation))){
		if((length(nonLocalInflation[[a]][["macroCcs"]])!=length(nonLocalInflation[[a]][["macroCoeffs"]])) || (sum(nonLocalInflation[[a]][["macroCoeffs"]])!=1)){
			badSpecs <-c(badSpecs,a)
		}
	}
	if(length(badSpecs)!=0){
		stop(c("error in prd.getMacroCcs(): nonLocalMacroGrowth and/or nonLocalInflation is not specified properly. ",paste(badSpecs,collapse=",")," have macroCcs that are of different length to macroCoeffs or macroCoeffs that don't sum to 1"))
	}
	
	gdpCcs<-c()
	gniCcs<-c()
	# logic is: nonLocalMacro determines where the macro data comes from and useGNI determines whether we use gni or gdp 
	for(a in intersect(assets,names(nonLocalMacroGrowth))){
		if(!is.element(substr(a,1,2),useGNI)){
			gdpCcs<-union(gdpCcs, nonLocalMacroGrowth[[a]][["macroCcs"]])
		}else{
			gniCcs<-union(gniCcs, nonLocalMacroGrowth[[a]][["macroCcs"]])
		}
	}
	localGrowthCcs<-unique(substr(setdiff(assets,names(nonLocalMacroGrowth)),1,2))
	otherGdpCcs<-setdiff(localGrowthCcs,useGNI)
	otherGniCcs<-intersect(localGrowthCcs,useGNI)
	inflationCcs<-c()
	for(a in intersect(assets,names(nonLocalInflation))){
		inflationCcs <-union(inflationCcs, nonLocalInflation[[a]][["macroCcs"]])
	}
	otherInflCcs<-unique(substr(setdiff(assets,names(nonLocalInflation)),1,2))

	return(list(inflationCcs=union(inflationCcs,otherInflCcs),gdpCcs=union(gdpCcs,otherGdpCcs),gniCcs=union(gniCcs,otherGniCcs)))
}

prd.getMacroData<-function(ccsInflation,ccsGDP,ccsGNI,useMacroDB,inflationSuffix,inflationFile,gdpSuffix,gdpFile,gniSuffix,gniFile,dateFormat, macroDbSettings, inputDir ){
	if(length(gdpSuffix)!=length(gdpFile)){
		stop("error in prd.getMacroData(): logic for gdp data requires a data suffix be given for every file")
	}
	
	# we might want gdpData, gniData or both; we initialize with NULL in case one type of data is not needed
	gdpData<-NULL
	gniData<-NULL
	if(useMacroDB){
		gdpNames <- c()
		gniNames <- c()
		if(length(ccsGDP) != 0 ) {
			gdpNames <- utils.generateCrossIds(ccsGDP, gdpSuffix)
		} 
		if(length(ccsGNI) != 0){
			gniNames <- utils.generateCrossIds(ccsGNI, gniSuffix )
		}
		cpiNames <- utils.generateCrossIds(ccsInflation, inflationSuffix)
		if(length(ccsGDP) != 0 ){
			gdpData <- utils.getDataFromDB( gdpNames, NULL, macroDbSettings )
		}
		if(length(ccsGNI) != 0){
			gniData <- utils.getDataFromDB( gniNames, NULL, macroDbSettings )
		}
		inflationData <- utils.getDataFromDB( cpiNames, NULL, macroDbSettings )
	}else{
		inflationData<-utils.getDataFromFile(ccsInflation,inflationSuffix,paste(inputDir,inflationFile,sep=""),dateFormat)
		if(length(ccsGDP)!=0){
			gdpData<-utils.getDataFromFile(ccsGDP,gdpSuffix,paste(inputDir,gdpFile,sep=""),dateFormat)
		}
		
		if(length(ccsGNI)!=0){
			gniData<-utils.getDataFromFile(ccsGNI,gniSuffix,paste(inputDir,gniFile,sep=""),dateFormat)
		}
	}
	
	return(list(inflation=inflationData,gdp=gdpData,gni=gniData))
}

prd.buildInflationSeries<-function(inflationData, numberOfMonths){
# build timeSeries of annualized inflation rates over period of numberOfMonths
# inflationData is a timeSeries; numberOfMonths is an integer
# returns list() of timeSeries() of inflation rates indexed by country corresponding

	if(numberOfMonths %% 12 !=0){
		stop(paste("ERROR in prd.buildInflationSeries(): the number of months has to be a multiple of 12 instead this was passed:", numberOfMonths ))
	}
	
	ret<-NULL
	if(is.null(inflationData)){
		return(ret)
	}

	# inflation data is updated monthly so get end of month data only and discard stubs
	inflationEOMData<- utils.getEOMData(inflationData,TRUE)

	for(j in 1:ncol(inflationEOMData)){
		# fill gaps one column at a time because if whole series is passed, dates get chopped
		temp <- utils.fillGapsSeries( inflationEOMData[, j], TRUE )
		# convert back to levels
		nrows <- nrow(temp)
		for (i in 0:11){
			subrows <- (1:nrows) %% 12 == i
			temp[ subrows, 1] <- utils.cumprodMatrixPlus1( temp[ subrows, 1 ] )
		}
		
		# check for NA's and make sure all NA's appear at the end of the vector only
		nonNAs<-utils.extractTrue(as.logical(is.finite(temp[,1])))
		# get annualized inflation rate over period of numberOfMonths
		inflationRates<-(as.numeric(temp[1:(sum(nonNAs)-numberOfMonths),1])/as.numeric(temp[(numberOfMonths+1):sum(nonNAs),1,]))^(12/numberOfMonths)-1
		ret<-append(ret,list(timeSeries(inflationRates,charvec=rownames(temp)[1:(sum(nonNAs)-numberOfMonths)])))
		names(ret)[length(ret)]<-colnames(inflationEOMData)[j]
	}
	
	return(ret)
}

prd.buildGDPSeries<-function(gdpData, gfdGDPData, gdpExtendedSuffix, extrapolateGDP, extrapolateNumYears){
# build gdp series from quarterly and GFD data
# gdpData and gfdGDPData are timeSeries
# returns a list() of timeSeries() of gdp growth indexed by country; Note: we don't return a simple timeSeries() here because of complications due to some countries having GFD data and others not
	
	ccs<-substr(names(gdpData),1,2)
	ret<-NULL
	if(is.null(gdpData) || ncol(gdpData)==0){
		return(ret)
	}
	for(i in 1:ncol(gdpData)){
		cc<-ccs[i]
		nonNAs<-utils.extractTrue(as.logical(is.finite(gdpData[,i])))
		gdpGrowthYoY<-gdpData[nonNAs,i]
		gdpGrowthYoYDates<-rownames(gdpGrowthYoY)
		gdpGrowthYoY<-as.numeric( gdpGrowthYoY )
		
		# now check to see if we have GFD data for country "cc"
		gdpGrowthYoY2<-c()
		gdpGrowthYoYDates2<-c()
		gfdGDPName<-paste(cc,gdpExtendedSuffix,sep="")
		extrapolate<-FALSE
		if(!is.null(gfdGDPData) && is.element(gfdGDPName,names(gfdGDPData))){
			# we have GFD data for country "cc"
			ccGFDData<-gfdGDPData[,gfdGDPName]
			ccGFDData<-ccGFDData[utils.extractTrue(as.logical(is.finite(ccGFDData[,1]))),1]
			# we use GFD to fill gaps anterior to the first date for which we have GDP data for country cc in the main source
			ind<-match(TRUE,as.Date(row.names(ccGFDData))<as.Date(gdpGrowthYoYDates[length(gdpGrowthYoYDates)]))
			if(is.finite(ind) && ind < nrow(ccGFDData)){
				gdpGrowthYoY2<-as.numeric(ccGFDData[ind:(nrow(ccGFDData)-1),])/as.numeric(ccGFDData[(ind+1):nrow(ccGFDData),])-1
				gdpGrowthYoYDates2<-row.names(ccGFDData)[ind:(nrow(ccGFDData)-1)]
			} else{
				# we found no GFD data that predates the earliest date we had data for so we extrapolate
				extrapolate<-TRUE
			}
		} else {
			# we did not find GFD data for country "cc", so extrapolate
			extrapolate<-TRUE
		}
		
		# extrapolate only if we need to, as set in extrapolate, and if allowed to, as set in global variable extrapolateGDP
		if(extrapolateGDP && extrapolate){
			firstDate<-gdpGrowthYoYDates[length(gdpGrowthYoYDates)]
			# first year we have data for
			year<-utils.getYear(firstDate)-1
			# take the first value and replicate it backwards and set the date to the last date in the year
			gdpGrowthYoYDates2 <-paste(year:(year-extrapolateNumYears),"-12-31",sep="")
			gdpGrowthYoY2<-rep(gdpGrowthYoY[length(gdpGrowthYoY)],length(gdpGrowthYoYDates2))
		}
	
		gdpGrowthYoY<-c(gdpGrowthYoY, gdpGrowthYoY2)
		gdpGrowthYoYDates<-c(gdpGrowthYoYDates, gdpGrowthYoYDates2)
		ret<-append(ret,list(timeSeries(gdpGrowthYoY,charvec= gdpGrowthYoYDates)))
		names(ret)[length(ret)]<-colnames(gdpData)[i]
	}
	
	return(ret)
}

prd.buildGNISeries<-function(gniData, extrapolateGNI, extrapolateNumYears){
# build gni series from quarterly and GFD data
# gniData is a timeSeries
# returns a list() of timeSeries() of gni growth indexed by country; Note: we don't return a simple timeSeries() here because of complications due to some countries having GFD data and others not

	ccs<-substr(colnames(gniData),1,2)
	ret<-NULL
	if(is.null(gniData) || ncol(gniData)==0){
		return(ret)
	}
	for(i in 1:ncol(gniData)){
		cc<-ccs[i]
		nonNAs<-utils.extractTrue(as.logical(is.finite(gniData[,i])))
		gniGrowthYoY<-gniData[nonNAs,i]
		gniGrowthYoYDates<-rownames(gniGrowthYoY)
		gniGrowthYoY<- as.numeric( gniGrowthYoY )
		
		gniGrowthYoY2<-c()
		gniGrowthYoYDates2<-c()
		if(extrapolateGNI){
			firstDate<-gniGrowthYoYDates[length(gniGrowthYoYDates)]
			# first year we have data for
			year<-utils.getYear(firstDate)-1
			# take the first value and replicate it backwards and set the date to the last date in the year
			gniGrowthYoYDates2 <-paste(year:(year-extrapolateNumYears),"-12-31",sep="")
			gniGrowthYoY2<-rep(gniGrowthYoY[length(gniGrowthYoY)],length(gniGrowthYoYDates))
		}
	
		gniGrowthYoY<-c(gniGrowthYoY, gniGrowthYoY2)
		gniGrowthYoYDates <-c(gniGrowthYoYDates, gniGrowthYoYDates2)
		ret<-append(ret,list(timeSeries(gniGrowthYoY,charvec= gniGrowthYoYDates)))
		names(ret)[length(ret)]<-colnames(gniData)[i]
	}
	
	return(ret)
}

prd.getAssetData<-function(equity,bonds,reits,tips,listOfSuffixes,useDB,inputDir,inputDataFile,dateFormat,dbSettings){
# Important NOTE: reits are ignored until we have models for them
	
	equityIds<-utils.generateCrossIds(equity,listOfSuffixes[["equity"]])
	bondIds<-utils.generateCrossIds(bonds,listOfSuffixes[["bond"]])
	reitIds<-c() # we don't do these now
	tipsIds<-utils.generateCrossIds(tips,listOfSuffixes[["tips"]])
	if(useDB){
		secData<-utils.getDataFromDB(c(equityIds,bondIds,reitIds,tipsIds),NULL,dbSettings)
	}else{
		secData<-utils.getDataFromFile(c(equityIds,bondIds,reitIds,tipsIds),NULL,paste(inputDir,inputDataFile,sep=""),dateFormat)
	}

	return(list(equity=secData[,equityIds],bond=secData[,bondIds],reit=secData[,reitIds],tips=secData[,tipsIds]))
}

predict.input<-function(settings){
# input function for equityRetModels, equityRiskModels, bondRetModels, bondRiskModels
	
	inputDir<-settings[["prd.inputDir"]]
	outputDir<-settings[["prd.outputDir"]]
	useDB<-settings[["prd.useDB"]]
	useMacroDB<-settings[["prd.useMacroDB"]]		
	inputDataFile<-settings[["prd.inputDataFile"]]
	assets<-settings[["prd.assets"]]
	dateFormat<-settings[["prd.dateFormat"]]
	listOfSuffixes<-settings[["prd.listOfSuffixes"]]
	inflationSuffix<-settings[["prd.inflationSuffix"]]	
	assetIdentifiers<-settings[["prd.assetIdentifiers"]]	
	inflationFile<-settings[["prd.inflationFile"]]	
	gdpFile<-settings[["prd.gdpFile"]]
	gdpSuffix<-settings[["prd.gdpSuffix"]]
	gdpExtendedFile<-settings[["prd.gdpExtendedFile"]]
	gdpExtendedSuffix<-settings[["prd.gdpExtendedSuffix"]]
	gniFile<-settings[["prd.gniFile"]]
	gniSuffix<-settings[["prd.gniSuffix"]]
	gdpFrequency<-settings[["prd.gdpFrequency"]]
	gdpExtendedSuffix<-settings[["prd.gdpExtendedSuffix"]]
	gniFrequency<-settings[["prd.gniFrequency"]]
	extrapolateGDP<-settings[["prd.extrapolateGDP"]]
	extrapolateGNI<-settings[["prd.extrapolateGNI"]]
	extrapolateNumYears<-settings[["prd.extrapolateNumYears"]]
	dbSettings<-settings[["dbSettings"]]
	macroDbSettings <- settings[["macroDbSettings"]]
	
	useGNI<-settings[["eretm.useGNI"]]
	nonLocalMacroGrowth<-settings[["eretm.nonLocalMacroGrowth"]]
	nonLocalInflation<-settings[["eretm.nonLocalInflation"]]
	numInflationMonths<-settings[["eretm.numInflationMonths"]]

	# main input logic here
	assets<-utils.assetsByClass(assets,assetIdentifiers)
	if(length(assets[["reit"]])!=0){
		print(c("Warning from predictExecImpl.R: we do not model property assets. the following assets will be ignored", assets[propInds]))
	}
	assetData<-prd.getAssetData(assets[["equity"]],assets[["bond"]],assets[["reit"]],assets[["tips"]],listOfSuffixes,useDB,inputDir,inputDataFile,dateFormat,dbSettings)
	macroCcs<-prd.getMacroCcs(c(assets[["equity"]],assets[["reit"]]), nonLocalMacroGrowth, nonLocalInflation, useGNI)
	macroData<-prd.getMacroData(macroCcs[["inflationCcs"]],macroCcs[["gdpCcs"]],macroCcs[["gniCcs"]],useMacroDB,inflationSuffix,inflationFile,gdpSuffix,gdpFile,gniSuffix,gniFile,dateFormat,macroDbSettings, inputDir)
	# Note: we read.csv() because utils.getDataFromFile() requires a list of data items to get whereas here we just need the whole file
	gfdGDPData<-read.csv(paste(inputDir,gdpExtendedFile,sep=""),header=TRUE,stringsAsFactors=FALSE)
	gfdGDPData<-timeSeries(gfdGDPData[setdiff(colnames(gfdGDPData),c("Date"))],charvec=gfdGDPData$Date)
	
	gdpSeries<-prd.buildGDPSeries(macroData[["gdp"]],gfdGDPData, gdpExtendedSuffix, extrapolateGDP, extrapolateNumYears)
	inflationSeries<-prd.buildInflationSeries(macroData[["inflation"]],numInflationMonths)
	inflationSeriesYoY<-prd.buildInflationSeries(macroData[["inflation"]], 12)
	gniSeries<-prd.buildGNISeries(macroData[["gni"]], extrapolateGNI, extrapolateNumYears)
	
	return(list(assetData=assetData,gdpSeries=gdpSeries,gfdData=gfdGDPData,gniSeries=gniSeries,inflationSeries=inflationSeries,inflationSeriesYoY=inflationSeriesYoY))
}

predict.output<-function(output,settings){
# output function
	
	growth <- output[["growth"]]
	inflation <- output[["inflation"]]
	output <- output[["forecasts"]]
	
	if(!is.null(output)){
		outputDir<-settings[["prd.outputDir"]]
		outputFile<-settings[["prd.outputFile"]]
		numLast<-settings[["prd.numLast"]]
		
		write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
		write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
		save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
	}
	
	write.csv(growth,paste(outputDir,sub("\\.csv",".growth.csv",outputFile),sep=""),row.names= TRUE)
	write.csv(inflation,paste(outputDir,sub("\\.csv",".inflation.csv",outputFile),sep=""),row.names= TRUE)
}

predict.calculate<-function(input,settings){
# main logic
	
	frequency<-settings[["prd.frequency"]]
	dayOfWeek<-settings[["prd.dayOfWeek"]]	
	inflationSuffix<-settings[["prd.inflationSuffix"]]
	
	assetData<-input[["assetData"]]
	gdpSeries<-input[["gdpSeries"]]
	gfdGDPData<-input[["gfdData"]]
	gniSeries<-input[["gniSeries"]]
	inflationSeries<-input[["inflationSeries"]]
	inflationSeriesYoY<-input[["inflationSeriesYoY"]]
	
	equityRetsInput<-list(equityData=assetData[["equity"]],inflationSeries=inflationSeries,gdpSeries=gdpSeries,gniSeries=gniSeries,freq=frequency,dayOfWeek=dayOfWeek)
	systime<-system.time(equityRets<-equityReturnModels.calculate(equityRetsInput,settings))
	print(paste("equityReturnModels time=",systime[["elapsed"]]))

	if(!is.null(equityRets)){
		equityRisksInput<-list(equityData=assetData[["equity"]],pes=equityRets[["pes"]],inflationPeriod=equityRets[["inflation"]],inflationYoY=inflationSeriesYoY,growth=equityRets[["growth"]],freq=frequency,dayOfWeek=dayOfWeek,inflationSuffix=inflationSuffix)
		systime<-system.time(equityRisks<-equityRiskModels.calculate(equityRisksInput,settings))
		print(paste("equityRiskModels time=",systime[["elapsed"]]))
	} else {
		systime<-system.time(equityRisks<-NULL)
		print(paste("equityRiskModels time=",systime[["elapsed"]]))
	}
	
	bondRetInput<-list(bondYields=assetData[["bond"]],freq=frequency,dayOfWeek=dayOfWeek)
	systime<-system.time(bondRets<-bondReturnModels.calculate(bondRetInput,settings))
	print(paste("bondReturnModels time=",systime[["elapsed"]]))
	
	# bondRiskInput is identical for now
	bondRiskInput<-bondRetInput
	systime<-system.time(bondRisks<-bondRiskModels.calculate(bondRiskInput,settings))
	print(paste("bondRiskModels time=",systime[["elapsed"]]))

	tipsRetInput <- list(tipsYields=assetData[["tips"]],inflationSeries=inflationSeries,freq=frequency,dayOfWeek=dayOfWeek)
	systime<-system.time(tipsRets<-tipsReturnModels.calculate(tipsRetInput,settings))
	print(paste("tipsReturnModels time=",systime[["elapsed"]]))

	if(!is.null(tipsRets)){
		tipsRetsNamesNoDate <- setdiff(colnames(tipsRets),"Date")
		tipsRetsSeries <- timeSeries(tipsRets[,tipsRetsNamesNoDate],charvec=tipsRets[["Date"]])
		colnames(tipsRetsSeries) <- tipsRetsNamesNoDate
		tipsRisksInput<-list(tipsYields=tipsRetsSeries,freq=frequency,dayOfWeek=dayOfWeek)
		systime<-system.time(tipsRisks<-tipsRiskModels.calculate(tipsRisksInput,settings))
		print(paste("tipsRiskModels time=",systime[["elapsed"]]))
	} else {
		systime<-system.time(tipsRisks<-NULL)
		print(paste("tipsRiskModels time=",systime[["elapsed"]]))
	}
	
	systime<-system.time(output<-list(forecasts=utils.align(list(equityRets=equityRets[["rets"]],bondRets=bondRets,tipsRets=tipsRets,equityRisks=equityRisks,bondRisks=bondRisks,tipsRisks=tipsRisks),TRUE),growth=utils.alignSeries(equityRets[["growth"]],TRUE),inflation=utils.alignSeries(equityRets[["inflation"]], TRUE)))
	print(paste("utils.align() time=",systime[["elapsed"]]))
			
	return(output)
}

