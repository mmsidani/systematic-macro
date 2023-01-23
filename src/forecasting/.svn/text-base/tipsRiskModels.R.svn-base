tipsRiskModels.calculate<-function(input,settings){
	durations<-settings[["tretm.durations"]]
	couponsPerYear<-settings[["tretm.couponsPerYear"]]
	tipsERSuffix <- settings[["triskm.whichTipsERForRisk"]]
	
	secYields<-input[["tipsYields"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	
	if(length(tipsERSuffix)!=1){
		stop(paste("ERROR in tipsRiskModels.calculate(): only on tipsERSuffix allowed for now. You passed:",paste(tipsERSuffix,collapse=", ")))
	}
	
	# now reduce secYields to just the "er"'s we want in case we have more than 1
	secYields <- secYields[,grep(paste("\\",tipsERSuffix,"$",sep=""),colnames(secYields))]
	
	if(length(secYields)==0){
		return(NULL)
	}
	# now... we want to use briskm.oModel() for the risk (since it's there and we don't want to duplicate code) so we change the names to fit what briskm.oModel() expects
	colnames(secYields) <- sub(tipsERSuffix,"",colnames(secYields))
	
	# yes! we call briskm.omodel() because our tips looks like a nominal bond with yield y_0 + i, the quoted yield + the inflation rate. See tipsReturnModels.R. defaultPremia and illiquiditySpreads arguments set to NULL
	oOutput<-briskm.oModel(secYields,NULL,NULL,durations,couponsPerYear)
	
	if(freq=="monthly"){
		return(oOutput[utils.getEOMDatesIndices(oOutput$Date,TRUE),])
	}else if(freq=="weekly"){
		return(oOutput[utils.getWeeklyDatesIndices(oOutput$Date,dayOfWeek),])
	}else if(freq=="daily"){
		return(oOutput)
	}else{
		stop(paste("error in triskm.calculate(): the specified frequency",freq,"is not implemented"))
	}
}
