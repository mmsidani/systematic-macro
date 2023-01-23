# input function
slopes.input<-function(settings){
	currencies<-settings[["slex.currencies"]]
	listOfSuffixes<-settings[["slex.listOfSuffixes"]]
	useDB<-settings[["slex.useDB"]]
	inputDir<-settings[["slex.inputDir"]]
	inputFile<-settings[["slex.inputFile"]]
	dateFormat<-settings[["slex.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	
	if(useDB){
		return(utils.getDataFromDB(currencies,listOfSuffixes,dbSettings))
	}else{
		return(utils.getDataFromFile(currencies,listOfSuffixes,paste(inputDir,inputFile,sep=""),dateFormat))
	}
}

# output function
slopes.output<-function(output,settings){
	outputDir<-settings[["slex.outputDir"]]
	outputFile<-settings[["slex.outputFile"]]
	numLast<-settings[["slex.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

# main logic
slopes.calculate<-function(input,settings){
	currencies<-settings[["slex.currencies"]]
	
	TreasData<-input
	numberOfCurrencies<-length(currencies)
	output<-data.frame(Date=row.names(TreasData))
	for(i in 1:numberOfCurrencies){
		nonNAs<-as.logical(is.finite(TreasData[,i])) & as.logical(is.finite(TreasData[,numberOfCurrencies+i]))
		newCol<-rep(NA,nrow(output))
		newCol[nonNAs]<-TreasData[nonNAs,i]-TreasData[nonNAs,numberOfCurrencies+i]

		output<-cbind(output,data.frame(newCol))
		names(output)[i+1]<-paste(currencies[i],".gg.210curve",sep="")
		
	}

	return(output)
}

