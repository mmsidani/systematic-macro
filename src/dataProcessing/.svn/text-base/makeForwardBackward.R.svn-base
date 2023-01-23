makeForwardBackward.input<-function(settings){
	inputDir<-settings[["frr.inputDir"]]
	assetRetFile<-settings[["frr.assetRetFile"]]
	useDB<-settings[["frr.useDB"]]
	assets<-settings[["frr.assets"]]
	dtrSuffix<-settings[["frr.dtrSuffix"]]
	dateFormat<-settings[["frr.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	bondIdentifiers<-settings[["frr.bondIdentifiers"]]
	bondForecastsSuffixes<-settings[["frr.bondForecastsSuffixes"]]
	bondForecastsFile<-settings[["frr.bondForecastsFile"]]
	
	if(length(dtrSuffix)!=1){
		stop(paste("error in makeForwardBackward.input(): only one suffix should be passed. you passed",length(dtrSuffix)))
	}
	
	isBond<-rep(FALSE,length(assets))
	for(i in bondIdentifiers){
		isBond <-isBond  | grepl(i,assets)
	}

	equitiesIds<-paste(assets[!isBond], dtrSuffix,sep="")
	bondsIds <-utils.generateCrossIds(assets[isBond], bondForecastsSuffixes)

	if(useDB){
		allData<-utils.getDataMatrixFromDB(c(equitiesIds,bondsIds),NULL,dbSettings)
		return(list(equities= allData[,equitiesIds],bonds= allData[,bondsIds]))
	}else{
		equitiesData<-utils.getDataMatrixFromFile(equitiesIds,NULL,paste(inputDir, assetRetFile,sep=""),dateFormat)
		bondsData<-utils.getDataMatrixFromFile(bondsIds,NULL,paste(inputDir, bondForecastsFile,sep=""),dateFormat)
		return(list(equities= equitiesData,bonds= bondsData))
	}
}

makeForwardBackward.output<-function(output,settings){
	outputDir<-settings[["frr.outputDir"]]
	
	outputFile<-settings[["frr.outputFile"]]
	
	for(i in 1:length(output)){
		temp<-cbind(data.frame(Date=utils.ymdToHyphens(rownames(output[[i]]))),data.frame(output[[i]]))
		dir<-names(output)[i] # typically "forward" or "backward"
		write.csv(temp,paste(outputDir,dir,".",outputFile,sep=""),row.names=FALSE)
		save(temp,file=paste(outputDir,dir,".",sub("\\.csv",".RData",outputFile),sep=""))
	}
}

makeForwardBackward.calculate<-function(input,settings){
	numYears<-settings[["frr.numberOfYears"]]
	assets<-settings[["frr.assets"]]
	busDaysPerYear<-settings[["frr.busDaysPerYear"]]
	bondIdentifiers<-settings[["frr.bondIdentifiers"]]
	dtrSuffix <-settings[["frr.dtrSuffix"]]
	retSuffix<-settings[["frr.retSuffix"]]
	riskSuffix<-settings[["frr.riskSuffix"]]
	whichWay<-settings[["frr.whichWay"]]
	
	assetRets<-input
	
	# do equities
	equities<-assetRets[["equities"]]
	colnames(equities)<-sub(dtrSuffix,"",colnames(equities))
	equitiesNames<-colnames(equities)
	equitiesRetRisk<-utils.annualizedRetRiskMatrix(equities,numYears, sqrt(busDaysPerYear), whichWay)# bonds forecasts were calculated elsewhere (predict.R, as of Apr 2012), we read them in makeForwardBackward.input() and now we just bind to the equities rets/risks
	bonds<-assetRets[["bonds"]]
	if(!is.null(bonds)){
		bondsDates<-as.numeric(rownames(bonds))
	}

	ret<-NULL
	for(i in 1:length(equitiesRetRisk)){
		colnames(equitiesRetRisk[[i]])<-c(paste(equitiesNames, retSuffix,sep=""),paste(equitiesNames, riskSuffix,sep=""))
		equitiesDates <-as.numeric(rownames(equitiesRetRisk[[i]]))
		tempBonds<-NULL
		if(!is.null(bonds)){
			tempBonds<-bonds[bondsDates>=min(equitiesDates) & bondsDates<=max(equitiesDates),]
			if(nrow(tempBonds)!=length(equitiesDates)){
				stop("error in makeForwardBackward.calculate(): bonds and equities data don't match")
			}
		}
		ret<-append(ret,list(cbind(equitiesRetRisk[[i]], tempBonds,deparse.level=0)))
		# the name is either "forward" or "backward"
		names(ret)[length(ret)]<-names(equitiesRetRisk)[i]
	}
		
	return(ret)
}
