eurolegacy.input<-function(settings){
	inputcurrencies<-settings[["euroleg.inputcurrencies"]]
	useDB<-settings[["euroleg.useDB"]]
	inputDir<-settings[["euroleg.inputDir"]]
	inputFile<-settings[["euroleg.inputFile"]]
	dateFormat<-settings[["euroleg.dateFormat"]]
	dbSettings<-settings[["dbSettings"]]
	
	if(useDB){
		return(utils.getDataFromDB(inputcurrencies,"",dbSettings))
	}else{
		return(utils.getDataFromFile(inputcurrencies,"",paste(inputDir,inputFile,sep=""),dateFormat))
	}
}

eurolegacy.output<-function(output,settings){
	outputDir<-settings[["euroleg.outputDir"]]
	outputFile<-settings[["euroleg.outputFile"]]
	numLast<-settings[["euroleg.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

eurolegacy.calculate<-function(input,settings){
	inputcurrencies<-settings[["euroleg.inputcurrencies"]]
	outputcurrencies<-settings[["euroleg.outputcurrencies"]]
	
	TreasData<-input
	numberOfCurrencies<-length(inputcurrencies)
	output<-data.frame(Date=row.names(TreasData))
	for(i in 1:numberOfCurrencies){
		nonNAs<-as.logical(is.finite(TreasData[,i]))
		newCol<-rep(NA,nrow(output))
		newCol[nonNAs]<-1/TreasData[nonNAs,i]

		output<-cbind(output,data.frame(newCol))
		names(output)[i+1]<-outputcurrencies[i]
		
	}

	return(output)
}
