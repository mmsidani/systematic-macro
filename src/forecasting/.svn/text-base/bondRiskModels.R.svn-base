#modified duration of annual coupon par bond, with maturity an integer years
briskm.duration<-function(y,maturity){
	ret<-rep(0,length(y))
	for(i in 1:maturity){
		ret<-ret+i/(1+y)^i
	}
	ret<-(y*ret+maturity/(1+y)^maturity)/(1+y)
	
	return(ret)
}

briskm.oModel<-function(secPrices,defaults,spreads,durations,CPY){
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
		
		spread<-0
		if(is.element(assetName,names(spreads))){
			spread <-spreads[[assetName]]
		}
		
		if(!is.element(assetName,names(CPY))){
			stop(paste("error in briskm.oModel(): no coupon frequency specified for bond",assetName))
		}
		
		if(!is.element(assetName,names(durations))){
			stop(paste("error in briskm.oModel(): no duration specified for bond",assetName))
		}
		
		adjYld<-((secPrices[nonNAs,i]-defaultPremium-spread)/CPY[[assetName]]+1)^CPY[[assetName]]-1
		newCol[nonNAs]<-briskm.duration(adjYld,durations[[assetName]])
		ret<-cbind(ret,data.frame(newCol))
	}
	
	names(ret)[2:ncol(ret)]<-paste(assetNames,".orisk",sep="")
	return(ret)
}

bondRiskModels.calculate<-function(input,settings){
	defaultPremia<-settings[["bretm.defaultPremia"]]
	illiquiditySpreads<-settings[["bretm.illiquiditySpreads"]]
	durations<-settings[["bretm.durations"]]
	couponsPerYear<-settings[["bretm.couponsPerYear"]]
	
	secPrices<-input[["bondYields"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	
	if(length(secPrices)==0){
		return(NULL)
	}
	
	oOutput<-briskm.oModel(secPrices,defaultPremia,illiquiditySpreads,durations,couponsPerYear)
	if(freq=="monthly"){
		return(oOutput[utils.getEOMDatesIndices(oOutput$Date,TRUE),])
	}else if(freq=="weekly"){
		return(oOutput[utils.getWeeklyDatesIndices(oOutput$Date,dayOfWeek),])
	}else if(freq=="daily"){
		return(oOutput)
	}else{
		stop(paste("error in briskm.calculate(): the specified frequency",freq,"is not implemented"))
	}
}
	

