# input function
totalReturn.input<-function(settings){
	assets<-settings[["totret.assets"]]
	priceSuffix<-settings[["totret.priceSuffix"]]
	yldSuffix<-settings[["totret.yldSuffix"]]
	useDB<-settings[["totret.useDB"]]
	inputDir<-settings[["totret.inputDir"]]
	mktDataFile<-settings[["totret.mktDataFile"]]
	dateFormat<-settings[["totret.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	
	if(useDB){
		assetData<-utils.getDataFromDB(assets,c(priceSuffix,yldSuffix),dbSettings)
	}else{
		assetData<-utils.getDataFromFile(assets,c(priceSuffix,yldSuffix),paste(inputDir,mktDataFile,sep=""),dateFormat)
	}
	
	return(assetData)
}

# output function
totalReturn.output<-function(output,settings){
	outputDir<-settings[["totret.outputDir"]]
	outputFile<-settings[["totret.outputFile"]]
	numLast<-settings[["totret.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

# main logic
# calculate total returns of equity assets. Note: we don't check that the assets are "equity" but the code will crash because non-equity assets don't have ".dvd.yld" data
# assetNames is a vector of strings; assetData is a timeSeries()
# returns data.frame() of ".tr" data and a "Date" column
totalReturn.calculate<-function(input,settings){
	assets<-settings[["totret.assets"]]
	priceSuffix<-settings[["totret.priceSuffix"]]
	yldSuffix<-settings[["totret.yldSuffix"]]
	totRetSuffix<-settings[["totret.totRetSuffix"]]
	daysPerYear<-settings[["totret.daysPerYear"]]
	
	assetData<-input
	# reverse so we can calculate cumprod()
	assetData<-assetData[nrow(assetData):1,]
	dates<-as.Date(row.names(assetData))
	# initialize return data.frame()
	ret<-data.frame(Date=dates)
	for(a in assets){
		ids<-paste(a,c(priceSuffix,yldSuffix),sep="")
		aData<-assetData[,ids]
		isNotNA<-as.logical(is.finite(aData[,1])) & as.logical(is.finite(aData[,2]))
		aData<-aData[isNotNA,]
		aDates<-dates[isNotNA]
		
		# total return formula: v_t = v_(t-1) * p_t/p_(t-1) * (dvd_yld_t * (t-(t-1))/days_per_year + 1). Note: t and t-1 are not necessarily two consecutive days. Note 2: we start the series with 100
		aTemp<-cumprod((as.numeric(aData[2:nrow(aData),2])*as.numeric(aDates[2:length(aDates)]-aDates[1:(length(aDates)-1)])/daysPerYear+1)*as.numeric(aData[2:nrow(aData),1])/as.numeric(aData[1:(nrow(aData)-1),1]))
		aTemp<-c(100,100*aTemp)
		
		newCol<-rep(NA,nrow(assetData))
		newCol[isNotNA]<-aTemp
		ret<-cbind(ret,newCol)
		names(ret)[ncol(ret)]<-paste(a,totRetSuffix,sep="")
	}
	
	return(ret[nrow(ret):1,])
}