makeHistorical.input<-function(settings){
# input function

	assets<-settings[["hrr.assets"]]
	useDB<-settings[["hrr.useDB"]]
	dateFormat<-settings[["hrr.dateFormat"]]
	inputDir<-settings[["hrr.inputDir"]]
	mktDataFile<-settings[["hrr.mktDataFile"]]
	forecastsFile <- settings[["hrr.forecastsFile"]]
	bondIdentifiers <- settings[["hrr.bondIndentifiers"]]
	durationRiskSuffix <- settings[["hrr.durationRiskSuffix"]]
	dbSettings<-settings[["dbSettings"]]
	
	bonds <- c()
	for(i in 1:length(bondIdentifiers)){
		bonds <- union(bonds, assets[grepl(bondIdentifiers[i], assets)])
	}
	
	bondDurations <- c()
	if(length(bonds) != 0){
		bondDurations <- paste(bonds, durationRiskSuffix, sep="")
	}
	if(useDB){
		return(list(assetData=utils.getDataFromDB(c(assets, bondDurations),NULL,dbSettings),bonds=bonds))
	}else{
		assetData <- utils.getDataFromFile(assets,NULL,paste(inputDir,mktDataFile,sep=""),dateFormat)
		if(length(bonds) !=0){
			bondDurationData <- utils.getDataFromFile(bondDurations,NULL,paste(inputDir,forecastsFile,sep=""),dateFormat)
			return(list(assetData=utils.alignSeries(list(assetData, bondDurationData), TRUE),bonds=bonds))
		} else {
			return(list(assetData=assetData, bonds=bonds))
		}
	}
}

makeHistorical.output<-function(output,settings){
# output function

	outputDir<-settings[["hrr.outputDir"]]
	outputFile<-settings[["hrr.outputFile"]]
	
	temp<-output[["hErRisks"]]
	write.csv(temp,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	save(temp,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
	
	temp <- output[["returns"]]
	temp <- cbind(data.frame(Date=rownames(temp)),data.frame(temp))
	write.csv(temp,paste(outputDir,sub("\\.csv",".rets.csv",outputFile),sep=""),row.names=FALSE)
	save(temp,file=paste(outputDir,sub("\\.csv",".rets.RData",outputFile),sep=""))
}

makeHistorical.calculate<-function(input,settings){
# main logic

	period<-settings[["hrr.period"]]
	dayOfWeek<-settings[["hrr.dayOfWeek"]]
	retSuffix<-settings[["hrr.retSuffix"]]
	riskSuffix<-settings[["hrr.riskSuffix"]]
	assets<-settings[["hrr.assets"]]
	durationRiskSuffix <- settings[["hrr.durationRiskSuffix"]]

	secDataSeries<-input[["assetData"]]
	bonds <- input[["bonds"]]
	
	if(period=="month"){
		secDataSeries<-utils.getEOMData(secDataSeries,TRUE)
		annFactor<-12
	}else if(period=="week"){
		secDataSeries<-utils.getWeeklyData(secDataSeries,dayOfWeek)
		annFactor<-52
	}else{
		stop(paste("error in hrr.calculate(): period",period,"is not supported."))
	}
	
	ret<-data.frame(Date=rownames(secDataSeries))
	ret2<-matrix(nrow=nrow(secDataSeries),ncol=length(assets))
	rownames(ret2) <- rownames(secDataSeries)
	colnames(ret2) <- assets
	for(i in 1:length(assets)){
		isNotNA<-as.logical(is.finite(secDataSeries[,assets[i]]))
		numPoints<-sum(isNotNA)
		indFalse<-match(FALSE,isNotNA)
		if(is.finite(indFalse)&& (indFalse!= numPoints+1)){
			stop(paste("error in hrr.calculateRets(): NA's are intermixed with non-NA's."))
		}
		
		newRets<-rep(NA,nrow(secDataSeries))
		if(!is.element(assets[i], bonds)){
			temp<-as.numeric(secDataSeries[1:(numPoints-1),assets[i]])/as.numeric(secDataSeries[2:numPoints,assets[i]])-1
		} else {
			durs <- as.numeric(secDataSeries[2:numPoints, paste(assets[i],durationRiskSuffix,sep="")])
			temp <- -durs * (as.numeric(secDataSeries[1:(numPoints-1),assets[i]]) - as.numeric(secDataSeries[2:numPoints, assets[i]]) )
		}
		
		newRets[1:(numPoints-2)]<-algos.cumSampleMean(temp,FALSE)[1:(numPoints-2)]*annFactor
		
		newRisks<-rep(NA,nrow(secDataSeries))
		newRisks[1:(numPoints-2)]<-algos.cumSampleSD(temp,FALSE)[1:(numPoints-2)]*sqrt(annFactor)
		ret<-cbind(ret,data.frame(newRets=newRets,newRisks=newRisks))
		names(ret)[(length(ret)-1):length(ret)]<-c(paste(assets[i],retSuffix,sep=""),paste(assets[i],riskSuffix,sep=""))
		
		ret2[1:(numPoints-1), i] <- temp
	}
	
	return(list(hErRisks=ret, returns=ret2))
}
