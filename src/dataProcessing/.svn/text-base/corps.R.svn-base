# input function
corps.input<-function(settings){
	currencies<-settings[["corpex.currencies"]]
	sourceData<-settings[["corpex.sourceData"]]
	cashNameSuffix<-settings[["corpex.cashNameSuffix"]]
	useDB<-settings[["corpex.useDB"]]
	dataFileName<-settings[["corpex.dataFileName"]]
	dateFormat<-settings[["corpex.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	
	# get data; countryCodes is a vector that determines the interest rates to get from the DB; example: countryCodes=c("us","uk","jp")
	countryCodes<-union("us",currencies)
	# we want to get the us corp series and rf rates for all countries
	idsToGet<-c(sourceData,paste(countryCodes,cashNameSuffix,sep=""))
	if(useDB){
		return(utils.getDataFromDB(idsToGet,NULL,dbSettings))
	} else {
		if(is.null(dataFileName)){
			stop("error in corps.input(): useDB is FALSE and the data file name is NULL")
		}
		return(utils.getDataFromFile(idsToGet,NULL,dataFileName,dateFormat))
	}
}

# output function
corps.output<-function(output,settings){
	outputDir<-settings[["corpex.outputDir"]]
	outputFile<-settings[["corpex.outputFile"]]
	numLast<-settings[["corpex.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

# corpData is the input
corpex.calculateUs<-function(corpData,sourceData,sourceDataWeights,usDataSeriesName){
	# initial output with dates
	output<-data.frame(Date=row.names(corpData),stringsAsFactors=FALSE)
	nonNAs<-rep(TRUE,nrow(output))
	corpDataNames<-names(corpData)
	for(i in 1:length(sourceData)){
		nonNAs<-nonNAs & as.logical(is.finite(corpData[,match(sourceData[i], corpDataNames)]))
	}

	newCol<-rep(NA,nrow(output))
	# this gives the gcorp series as a weighted sum of the series specified above in sourceData; we do it efficiently as a matrix multiplication
	newCol[nonNAs]<-matrix(corpData[nonNAs,is.element(corpDataNames,sourceData)],ncol=length(sourceData))%*%matrix(sourceDataWeights,ncol=1)
	output<-cbind(output,data.frame(newCol=newCol))
	names(output)[2]<-usDataSeriesName

	# and return it
	return(output)
}

# do non-US; countryCodes is, for example, c("us","uk","jp")
corpex.calculateNonUs<-function(countryCodes,usSeries,rfRates,nonUsDataSeriesNameSuffix){
	ratesDates<-as.character(row.names(rfRates))
	usSeriesDates<-as.character(row.names(usSeries))

	ratesNames<-names(rfRates)
	usRates<-as.numeric(rfRates[,grep("^us\\.", ratesNames)])
	nonNAsUS<-as.logical(is.finite(usRates))
	ret<-data.frame(Date= usSeriesDates)
	nonUsCcs<-setdiff(countryCodes,"us")
	for(cc in nonUsCcs){
		newCol<-rep(NA,nrow(ret))
		ccRates<-as.numeric(rfRates[,grep(paste("^",cc,"\\.",sep=""), ratesNames)])
		nonNAs<-as.logical(is.finite(ccRates)) & nonNAsUS
		# convert us rate into foreign rate
		newCol[nonNAs]<-(1+usSeries[nonNAs]) * (1+ccRates[nonNAs]) / (1+usRates[nonNAs]) - 1.
		ret<-cbind(ret,newCol=newCol)
		names(ret)[ncol(ret)]<-paste(cc,nonUsDataSeriesNameSuffix,sep="")
	}
	
	return(ret)
}

# main logic
corps.calculate<-function(input,settings){
	countryCodes<-settings[["corpex.currencies"]]
	usDataSeriesName<-settings[["corpex.usDataSeriesName"]]
	nonUsDataSeriesNameSuffix<-settings[["corpex.nonUsDataSeriesNameSuffix"]]
	cashNameSuffix<-settings[["corpex.cashNameSuffix"]]
	sourceData<-settings[["corpex.sourceData"]]
	sourceDataWeights<-settings[["corpex.sourceDataWeights"]]
	useForNonUs<-settings[["corpex.useForNonUs"]]
	
	usOutput<-corpex.calculateUs(input,sourceData,sourceDataWeights,usDataSeriesName)
	
	rfRates<-input[,grep(cashNameSuffix,names(input))]
	nonUSOutput<-corpex.calculateNonUs(countryCodes,input[,match(useForNonUs,names(input))],rfRates,nonUsDataSeriesNameSuffix)

	output<-cbind(usOutput,nonUSOutput[setdiff(names(nonUSOutput),"Date")])
	
	# and return it
	return(output)
}


