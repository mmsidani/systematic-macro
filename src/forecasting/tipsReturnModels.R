tretm.oModel<-function(secYields,inflationSeries,CPY,maturities,inflationSuffix){
	numAssets<-ncol(secYields)
	assetNames<-names(secYields)	
	ret<-data.frame(Date=row.names(secYields))
	for(i in 1:numAssets){
		assetName<-assetNames[i]
		newCol<-rep(NA,nrow(secYields))
		nonNAs<-as.logical(is.finite(secYields[,i]))
		
		if(!is.element(assetName,names(CPY)) || !is.element(assetName, names(maturities))){
			stop(paste("ERROR in tretm.oModel(): no coupon frequency or no maturity specified for tips", assetName))
		}
		
		ccInflation <- inflationSeries[[paste(substr(assetName, 1, 2),inflationSuffix,sep="")]]
		# recall inflationRates are already annualized
		inflationRates <- utils.getDataForDates(rownames(secYields)[nonNAs],ccInflation,rownames(ccInflation))
		# the yield we have excludes inflation, i.e., does not take into account the fact that the notional will inflate/deflate over time. We assume then that today's yield is that of a nominal par bond and we ask what that yield becomes if the notional grew at a constant rate equal to the annual inflation rate we passed in inflationSeries
		# the equations are: 1 = Sigma_{t=1}^{t=T} ( y_0 / (1 + y_0)^t ) + 1 / (1 + y_0)^T and 1 = Sigma_{t=1}^{t=T} ( y_0 * (1 + i)^t / (1 + y_1)^t ) + (1 + i)^T / (1 + y_1)^T
		# comparing the 2 gives 1 + y_0 = (1 + i) / (1 + y_1) and upon ignoring second order terms we get the following
		newCol[nonNAs]<-(secYields[nonNAs,i]/CPY[[assetName]]+1)^CPY[[assetName]]-1 + inflationRates
		ret<-cbind(ret,data.frame(newCol))
	}
	
	names(ret)[2:ncol(ret)]<-paste(assetNames,".er",sep="")
	return(ret)
}

tipsReturnModels.calculate<-function(input,settings){
	couponsPerYear<-settings[["tretm.couponsPerYear"]]
	maturities <- settings[["tretm.durations"]]
	inflationSuffix <- settings[["prd.inflationSuffix"]]
	
	secYields<-input[["tipsYields"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	inflationSeries<-input[["inflationSeries"]]
	
	if(length(secYields)==0){
		return(NULL)
	}
	
	oOutput<-tretm.oModel(secYields,inflationSeries,couponsPerYear,maturities,inflationSuffix)
	if(freq=="monthly"){
		return(oOutput[utils.getEOMDatesIndices(oOutput$Date,TRUE),])
	}else if(freq=="weekly"){
		return(oOutput[utils.getWeeklyDatesIndices(oOutput$Date,dayOfWeek),])
	}else if(freq=="daily"){
		return(oOutput)
	}else{
		stop(paste("ERROR in tipsReturnModels.calculate(): the specified frequency",freq,"is not implemented"))
	}
	
}