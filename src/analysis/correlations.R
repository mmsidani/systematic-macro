corrs.getData<-function(assets,baseCc,erMapping,riskMapping,cashNameSuffix,useDB,dateFormat,inputDir,mktDataFile,forecastsFile,dbSettings){
	erIds<-utils.generateIdsFromMapping(assets, erMapping)
	riskIds<-utils.generateIdsFromMapping(assets,riskMapping)
	ccs<-union(baseCc,substr(assets,1,2))
	cashIds<-paste(ccs,cashNameSuffix,sep="")
	
	if(useDB){
		allData<-utils.getDataMatrixFromDB(c(erIds,riskIds,cashIds),NULL,dbSettings)
		forecastData<-allData[,c(erIds,riskIds)]
		cashData<-allData[,cashIds]
	}else{
		forecastData<-utils.getDataMatrixFromFile(c(erIds,riskIds),NULL,paste(inputDir,forecastsFile,sep=""),dateFormat)
		cashData<-utils.getDataMatrixFromFile(cashIds,NULL,paste(inputDir,mktDataFile,sep=""),dateFormat)
	}
	
	colnames(forecastData)<-c(assets,paste(assets,".risk",sep=""))
	return(list(forecasts=forecastData,cash=cashData))
}

correlations.input<-function(settings){
	inputDir<-settings[["corrs.inputDir"]]
	portfolioSettingsFile<-settings[["corrs.portfolioSettingsFile"]]
	mktDataFile<-settings[["corrs.mktDataFile"]]
	forecastsFile<-settings[["corrs.forecastsFile"]]
	cashNameSuffix<-settings[["corrs.cashNameSuffix"]]
	useDB<-settings[["corrs.useDB"]]
	dbSettings<-settings[["dbSettings"]]
	dateFormat<-settings[["corrs.dateFormat"]]
	
	pathToRSource<-settings[["pathToRSource"]]
	
	# get portfolio settings. set it to NULL first as a precaution because all portfolio settings files have the same one function, namely, portfolio.settings()
	portfolio.settings<-NULL
	source(paste(pathToRSource,"inputFiles/",portfolioSettingsFile,sep=""))
	portfolioSettings<-portfolio.settings()
	
	baseCc<-portfolioSettings[["baseCc"]]
	erMapping<-portfolioSettings[["erMapping"]]
	riskMapping<-portfolioSettings[["riskMapping"]]
	
	assets<-names(utils.getConstraints(paste(pathToRSource,"inputFiles/",sep=""),paste("constraints-",portfolioSettings[["constraintsNAME"]],".csv",sep="")))
	
	return(append(corrs.getData(assets,baseCc,erMapping,riskMapping,cashNameSuffix,useDB,dateFormat,inputDir,mktDataFile,forecastsFile,dbSettings),list(portfolioSettings =portfolioSettings,assets=assets)))
	
}

correlations.output<-function(output,settings){
	outputDir<-settings[["corrs.outputDir"]]
	filesPrefix<-settings[["corrs.portfolioSettingsFile"]]
	
	if(!is.null(output)){
		filesPrefix <- sub("\\.r$","",sub("\\.R$","",sub("^constraints-","",filesPrefix)))
		temp<-cbind(data.frame(Date=utils.ymdToHyphens(rownames(output[["correlations.table"]]))),data.frame(output[["correlations.table"]]))
		write.csv(temp,paste(outputDir,filesPrefix,".corr.csv",sep=""),row.names=FALSE)
		save(temp,file=paste(outputDir,filesPrefix,".corr.RData",sep=""))
		temp<-cbind(data.frame(Date=utils.ymdToHyphens(rownames(output[["covariances.table"]]))),data.frame(output[["covariances.table"]]))
		write.csv(temp,paste(outputDir,filesPrefix,".cov.csv",sep=""),row.names=FALSE)
		save(temp,file=paste(outputDir,filesPrefix,".cov.RData",sep=""))
		temp<-cbind(data.frame(Date=utils.ymdToHyphens(rownames(output[["durations"]]))),data.frame(output[["durations"]]))
		write.csv(temp,paste(outputDir,filesPrefix,".durations.csv",sep=""),row.names=FALSE)
		save(temp,file=paste(outputDir,filesPrefix,".durations.RData",sep=""))
		
		temp<-output[["correlations.matrices"]]
		save(temp,file=paste(outputDir,filesPrefix,".corrMats.RData",sep=""))
		temp<-output[["covariances.matrices"]]
		save(temp,file=paste(outputDir,filesPrefix,".covMats.RData",sep=""))
	}
}

corrs.filterDates<-function(dates,corrFreq,dayOfWeek){
	if(corrFreq=="monthly"){
		inds<-utils.getEOMDatesIndices(utils.ymdToHyphens(dates),TRUE)
	}else if(corrFreq=="weekly"){
		inds<-utils.getWeeklyDatesIndices(utils.ymdToHyphens(dates),dayOfWeek)
	}else if(corrFreq=="daily"){
		inds<-1:length(dates)
	}else{
		stop(paste("error in corrs.filterDates(): unrecognized frequency",corrFreq))
	}
	
	return(inds)
}

corrs.calculateCorrelations<-function(assets,calculationDates,forecasts,cash,couponsPerYear,cashNameSuffix,rollingWindow,riskMeasure,halfLife,varScaleFactor,numAssets,baseCc,doRisks){
	if(numAssets==1){
		print("WARNING in corrs.calculateCorrelations(): you passed 1 and only 1 asset?!")
		return(NULL)
	}
	
	# convert forecasts to base currency
	forecasts<-erRiskDomesticEquivalent.convertErsRisks(forecasts,baseCc,cash,couponsPerYear,cashNameSuffix,doRisks)
	# make sure contents are in the expected order
	forecasts<-forecasts[,c(assets,paste(assets,".risk",sep=""))]

	# this gives us a matrix with TRUE below the diagonal. we use it to extract unique correlations from the var-covar matrix
	lowerTriangle <- outer(1:numAssets,1:numAssets,FUN=">")
	# now repeat for covariances. keep the diagonal
	lowerTriangleDiag <- outer(1:numAssets,1:numAssets,FUN=">=")
	# now we want to make the labels for the correlations: we want asset1.asset2
	labels<-outer(assets,assets,FUN="paste")[lowerTriangle]
	# paste() by default has sep=" ". let's replace the white space with "."
	labels<-gsub(" ",".",labels)
	# repeat for labels of covariances. we don't ignore diagonal here
	labelsDiag <- outer(assets,assets,FUN="paste")[lowerTriangleDiag]
	labelsDiag <- gsub(" ",".",labelsDiag)
	
	forecastsDates<-as.numeric(rownames(forecasts))
	numRows<-nrow(forecasts)
	numDates<-length(calculationDates)
	riskInds <- (numAssets + 1 ):(2 * numAssets)
	ret1<-matrix(rep(NA,numAssets*(numAssets-1)/2* numDates),nrow=numDates)
	ret2<-NULL
	ret3<-NULL
	ret4<-matrix(rep(NA,numAssets*(numAssets+1)/2* numDates),nrow=numDates)
	ret5 <- matrix(nrow=numDates,ncol=numAssets)
	for(d in 1:numDates){
		ind<-match(calculationDates[d],forecastsDates)
		if(numRows-ind+1 < rollingWindow+1){
			break
		}

		varCovarMat<-varianceCovariance.riskMatrix(riskMeasure,forecasts[ind:numRows,],numAssets,rollingWindow,halfLife,varScaleFactor)
		iStds<-1/sqrt(diag(varCovarMat))
		corrMat<-as.matrix(iStds)[,rep(1,numAssets)] * varCovarMat * t(iStds)[rep(1,numAssets),]
		colnames(corrMat) <- names(iStds)
		# when the lower triangle is extracted from a matrix as in the next line it's returned as a vector
		ret1[d,]<-corrMat[lowerTriangle]
		ret2<- append(ret2, list(varCovarMat))
		ret3<-append(ret3,list(corrMat))
		ret4[d, ]<- varCovarMat[lowerTriangleDiag]
		ret5[d, ]<- forecasts[ind, riskInds]
	}
	
	if(d<numDates){
		d <- d-1
		ret1<-ret1[1:d,]
		ret4<-ret4[1:d,]
		ret5<-ret5[1:d,]
	}
	
	rownames(ret1) <- rownames(ret4) <- rownames(ret5) <- names(ret2) <- names(ret3) <- calculationDates[1:d]
	colnames(ret1) <- labels
	colnames(ret4) <- labelsDiag
	colnames(ret5) <- paste(assets,".risk",sep="")
	
	return(list(correlations.table=ret1,covariances.table=ret4,correlations.matrices=ret3,covariances.matrices=ret2, durations=ret5))
}

correlations.calculate<-function(input,settings){
	couponsPerYear<-settings[["corrs.couponsPerYear"]]
	cashNameSuffix<-settings[["corrs.cashNameSuffix"]]
	varScaleFactor <-settings[["corrs.varScaleFactor"]]
	doRisks <- settings[["corrs.doRisks"]]
	
	portfolioSettings<-input[["portfolioSettings"]]
	forecasts<-input[["forecasts"]]
	cash<-input[["cash"]]
	assets<-input[["assets"]]
	
	startDate<-portfolioSettings[["startDateForBackTest"]]
	endDate<-portfolioSettings[["endDateForBackTest"]]
	effFrontierFreq<-portfolioSettings[["effFrontierFreq"]]
	correlationDataFreq<-portfolioSettings[["correlationDataFreq"]]
	dayOfWeek<-portfolioSettings[["dayOfWeek"]]
	rollingWindow<-portfolioSettings[["rollingWindow"]]
	riskMeasure<-portfolioSettings[["riskMeasure"]]
	halfLife<-portfolioSettings[["halfLife"]]
	baseCc<-portfolioSettings[["baseCc"]]
	
	if( endDate == "today" ){
		endDate <- utils.today()
	}
	startDate<-as.Date(startDate)
	endDate<-as.Date(endDate)
	
	freq<-sub("ly","",effFrontierFreq)
	if(freq=="week" && (weekdays(startDate)!=dayOfWeek || weekdays(endDate)!=dayOfWeek)){
		if( weekdays(startDate)!=dayOfWeek ){
			modStartDate <- utils.dayClosest( startDate, dayOfWeek, F)
			print(paste("WARNING from correlations.calculate(): the start date",as.character(startDate),"does not fall on the portfolio's day of week", dayOfWeek, "and was changed to",modStartDate))
			startDate <- as.Date(modStartDate)
		}
		
		if(weekdays(endDate) != dayOfWeek){
			modEndDate <- utils.dayClosest( endDate, dayOfWeek, T )
			print(paste("WARNING from correlations.calculate(): the end date",as.character(endDate),"does not fall on the portfolio's day of week", dayOfWeek, "and was changed to",modEndDate))
			endDate <- as.Date(modEndDate)
		}
	}
	
	calculationDates<-rev(as.numeric(gsub("-","",as.character(seq.Date(from= startDate,to= endDate,by=paste("1",freq,sep=" "))))))
	
	startDate<-as.numeric(gsub("-","",startDate))
	endDate<-as.numeric(gsub("-","",endDate))

	forecasts<-forecasts[ corrs.filterDates(rownames(forecasts), correlationDataFreq,dayOfWeek), ]
	forecastsDates<-rownames(forecasts)
	cashDates<-rownames(cash)
	commonDates <- intersect(cashDates, forecastsDates)
	forecasts <- forecasts[is.element(forecastsDates, commonDates), ]
	cash <- cash[ is.element(cashDates, commonDates), ]
	
	commonDates <- intersect( commonDates, calculationDates )
	if(length(calculationDates) != length(commonDates)){
		print(paste("INFO from correlations.calculate(): some test dates have no forecasts. skipping those."))
	}
	
	calculationDates<- commonDates	
	numAssets<-length(assets)
	
	return(corrs.calculateCorrelations(assets,calculationDates,forecasts,cash,couponsPerYear,cashNameSuffix,rollingWindow,riskMeasure,halfLife,varScaleFactor,numAssets,baseCc,doRisks))
}
