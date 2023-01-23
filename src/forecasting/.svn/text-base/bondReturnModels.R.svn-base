bretm.oModel<-function(secPrices,defaults,CPY){
	numAssets<-ncol(secPrices)
	assetNames<-names(secPrices)	
	ret<-data.frame(Date=row.names(secPrices))
	for(i in 1:numAssets){
		assetName<-assetNames[i]
		newCol<-rep(NA,nrow(secPrices))
		nonNAs<-as.logical(is.finite(secPrices[,i]))
		defaultPremium<-0
		if(is.element(assetName,names(defaults))){
			defaultPremium<-defaults[[assetName]]
		}
		
		if(!is.element(assetName,names(CPY))){
			stop(paste("error in bretm.oModel(): no coupon frequency specified for bond",assetName))
		}
		
		newCol[nonNAs]<-(((secPrices[nonNAs,i]-defaultPremium)/CPY[[assetName]]+1)^CPY[[assetName]]-1)
		ret<-cbind(ret,data.frame(newCol))
	}
	
	names(ret)[2:ncol(ret)]<-paste(assetNames,".er",sep="")
	return(ret)
}

bondReturnModels.calculate<-function(input,settings){
	defaultPremia<-settings[["bretm.defaultPremia"]]
	couponsPerYear<-settings[["bretm.couponsPerYear"]]
	
	secPrices<-input[["bondYields"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	
	if(length(secPrices)==0){
		return(NULL)
	}
	
	oOutput<-bretm.oModel(secPrices,defaultPremia,couponsPerYear)
	if(freq=="monthly"){
		return(oOutput[utils.getEOMDatesIndices(oOutput$Date,TRUE),])
	}else if(freq=="weekly"){
		return(oOutput[utils.getWeeklyDatesIndices(oOutput$Date,dayOfWeek),])
	}else if(freq=="daily"){
		return(oOutput)
	}else{
		stop(paste("error in bretm.calculate(): the specified frequency",freq,"is not implemented"))
	}

}