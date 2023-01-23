# input function
earnFromPE.input<-function(settings){
	assets<-settings[["earnFromPE.assets"]]
	priceSuffix<-settings[["earnFromPE.priceSuffix"]]
	peSuffix<-settings[["earnFromPE.peSuffix"]]
	useDB<-settings[["earnFromPE.useDB"]]
	inputDir<-settings[["earnFromPE.inputDir"]]
	mktDataFile<-settings[["earnFromPE.mktDataFile"]]
	dateFormat<-settings[["earnFromPE.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	
	if(useDB){
		assetData<-utils.getDataFromDB(assets,c(priceSuffix,peSuffix),dbSettings)
	}else{
		assetData<-utils.getDataFromFile(assets,c(priceSuffix,peSuffix),paste(inputDir,mktDataFile,sep=""),dateFormat)
	}
	
	return(assetData)
}

# output function
earnFromPE.output<-function(output,settings){
	outputDir<-settings[["earnFromPE.outputDir"]]
	outputFile<-settings[["earnFromPE.outputFile"]]
	numLast<-settings[["earnFromPE.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

# main logic
# calculate earnings of equity assets given pe and price. Note: we don't check that the assets are "equity" but the code will crash because non-equity assets don't have ".pe" data
# assetNames is a vector of strings; assetData is a timeSeries()
# returns data.frame() of ".earnings" data and a "Date" column
earnFromPE.calculate<-function(input,settings){
	assets<-settings[["earnFromPE.assets"]]
	priceSuffix<-settings[["earnFromPE.priceSuffix"]]
	peSuffix<-settings[["earnFromPE.peSuffix"]]
	earningsSuffix<-settings[["earnFromPE.earningsSuffix"]]

	
	assetData<-input
	# reverse so we can calculate cumprod()
	assetData<-assetData[nrow(assetData):1,]
	dates<-as.Date(row.names(assetData))
	# initialize return data.frame()
	ret<-data.frame(Date=dates)
	for(a in assets){
		ids<-paste(a,c(priceSuffix,peSuffix),sep="")
		aData<-assetData[,ids]
		isNotNA<-as.logical(is.finite(aData[,1])) & as.logical(is.finite(aData[,2]))
		aData<-aData[isNotNA,]
		aDates<-dates[isNotNA]
		
		# earnings formula: earnings = price / (price/earnings)

		aTemp<-aData[1:nrow(aData),1]/aData[1:nrow(aData),2]
		
		newCol<-rep(NA,nrow(assetData))
		newCol[isNotNA]<-aTemp
		ret<-cbind(ret,newCol)
		names(ret)[ncol(ret)]<-paste(a,earningsSuffix,sep="")
	}
	
	return(ret[nrow(ret):1,])
}
