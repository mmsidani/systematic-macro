# use annualized inflation number for past fixed-length (typically 10y) period
# dates is a vector of date strings. pToEPeaks is a list() indexed by asset name of timeSeries() of price to peak-earnings ratios. inflationSeries is a list() indexed by asset name of timeSeries() of annualized inflation rates over the period specified in GlobalVariables.
# returns data.frame() whith $Date and one column per asset
eriskm.oModel<-function(dates,pToEPeaks,inflationSeries){
	ret<-NULL
	for(a in names(pToEPeaks)){
		# pToEPeaks is a list of one-column timeSeries()
		secPToEPeak<-pToEPeaks[[a]]
		aDates<-row.names(secPToEPeak)
		
		# find the inflation timeSeries() for asset a
		j<-match(a,names(inflationSeries))
		newCol<-rep(NA,length(dates))
		nonNAs<-as.logical(is.finite(secPToEPeak[,1]))
		inflation<-utils.getDataForDates(aDates[nonNAs],as.numeric(inflationSeries[[j]]),row.names(inflationSeries[[j]]))
		nonNAInfls<- is.finite(inflation)
		newCol[is.element(dates,aDates)][nonNAs][nonNAInfls]<-1/(inflation[nonNAInfls]+1/as.numeric(secPToEPeak[nonNAs,1][nonNAInfls]))
		ret<-cbind(ret,newCol,deparse.level=0)
	}
	
	ret<-data.frame(ret)
	names(ret)<-names(pToEPeaks)
	return(cbind(data.frame(Date=dates),ret))
}

# use avg of yoy inflation numbers to the beginning of time; unlike the .oModel case, inflationSeries here is not a series indexed by asset name
# same input/output as eriskm.oModel()
eriskm.aModel<-function(dates,pToEPeaks,inflationSeries,inflationSuffix,nonLocalInflation){
	ret<-NULL
	for(a in names(pToEPeaks)){
		# pToEPeaks is a list of one-column timeSeries()
		secPToEPeak<-pToEPeaks[[a]]
		aDates<-row.names(secPToEPeak)
		
		# unlike oModel, what we don't get here an inflation series that takes nonLocalInflation into account, since we pass into aModel the yoy inflation series, and that is not used in equityReturnModels. so we have to implement nonLocalInflation requirements here.
		if(is.element(a,names(nonLocalInflation))){
			inflList<-NULL
			for(cc in nonLocalInflation[[a]][["macroCcs"]]){
				inflList<-append(inflList,list(inflationSeries[[match(paste(cc,inflationSuffix,sep=""),names(inflationSeries))]]))
			}
			inflList<-utils.alignSeries(inflList,TRUE)
			inflationToUse<-timeSeries(as.matrix(inflList)%*%matrix(nonLocalInflation[[a]][["macroCoeffs"]],ncol=1),charvec=row.names(inflList))
		}else{
			j<-match(paste(substr(a,1,2),inflationSuffix,sep=""),names(inflationSeries))
			inflationToUse<-inflationSeries[[j]]
		}
		
		newCol<-rep(NA,length(dates))
		nonNAs<-as.logical(is.finite(secPToEPeak[,1]))
		# algos.cumSampleMean() would return all NA's if the first component of the vector is NA. handle that next, by passing non NA inflation numbers only and augmenting the output from algos.cumSampleMean() with NA's to bring its length back to nrow(inflationToUse)
		nonNAInfls<- as.logical(is.finite(inflationToUse))
		cumInflMeans<-c(algos.cumSampleMean(inflationToUse[nonNAInfls],FALSE),rep(NA,nrow(inflationToUse)-sum(nonNAInfls)))
		# now proceed as in oModel
		inflation<-utils.getDataForDates(aDates[nonNAs],cumInflMeans,row.names(inflationToUse))
		nonNAInfls<- is.finite(inflation)
		newCol[is.element(dates,aDates)][nonNAs][nonNAInfls]<-1/(inflation[nonNAInfls]+1/as.numeric(secPToEPeak[nonNAs,1][nonNAInfls]))
		ret<-cbind(ret,newCol,deparse.level=0)
	}
	
	ret<-data.frame(ret)
	names(ret)<-names(pToEPeaks)
	return(cbind(data.frame(Date= dates),ret))
}

# do not use inflation. duration as follows from DDM, i.e., inverse dividend yields
# equityData is a timeSeries() in the "",".earnings",".dvd.yld" format. actually, all we get from it is the ".dvd.yld" columns which we expect to be the last third of its columns
# returns data.frame() with $Date and and one column per asset
eriskm.dModel<-function(equityData){
# TODO what we calculate here, P/D, is the sensitivity of the price to the DDR; we need a method in varianceCovariance calcs that applies drisk to the changes in levels of DDR's to get covariances
	
	# ACHTUNG: hard-coded 3
	numAssets<-ncol(equityData)/3
	divYlds<-equityData[,(2*numAssets+1):ncol(equityData)]
	ret<-NULL
	for(i in 1:ncol(divYlds)){
		newCol<-rep(NA,nrow(divYlds))
		nonNAsNon0s<-as.logical(is.finite(divYlds[,i])) & as.logical(divYlds[,i]!=0)
		newCol[nonNAsNon0s]<-1/as.numeric(divYlds[nonNAsNon0s,i])
		ret<-cbind(ret,newCol,deparse.level=0)
	}
	
	ret<-data.frame(ret)
	names(ret)<-names(equityData)[1:numAssets]
	return(cbind(data.frame(Date=row.names(equityData),ret)))
}

# instead of inverse dividend yields, i.e., P/D, as in dModel, use P/E_peak
# pToEPeaks same as in "o"/"a" models
# same return as elsewhere
eriskm.eModel<-function(pToEPeaks){
	# just align it. nothing more. but... return data.frame() like the other models
	ret<-utils.alignSeries(pToEPeaks,TRUE)
	return(cbind(data.frame(Date=row.names(ret)),data.frame(ret)))
}

# use growth in risk; this only uses expected return j model growth
# first 3 args are as in "o"/"a" models; growthSeries is a list() indexed by asset name of the growthSeries as it was built in eretm.getMacroSeries().
# same return as above
eriskm.gModel<-function(dates,pToEPeaks,inflationSeries,growthSeries){
	ret<-NULL
	for(a in names(pToEPeaks)){
		# pToEPeaks is a list of one-column timeSeries()
		secPToEPeak<-pToEPeaks[[a]]
		aDates<-row.names(secPToEPeak)
		
		j<-match(a,names(growthSeries))
		k<-match(a,names(inflationSeries))
		
		newCol<-rep(NA,length(dates))
		nonNAs<-as.logical(is.finite(secPToEPeak[,1]))
		nonNAGrowths<- is.finite(growthSeries[[j]])
		cumGrowthMeans<-c(algos.cumSampleMean(growthSeries[[j]][nonNAGrowths],FALSE),rep(NA,nrow(growthSeries[[j]])-sum(nonNAGrowths)))
		growth<-utils.getDataForDates(aDates[nonNAs],cumGrowthMeans,row.names(growthSeries[[j]])[nonNAGrowths])
		inflation<-utils.getDataForDates(aDates[nonNAs],as.numeric(inflationSeries[[k]]),row.names(inflationSeries[[k]]))
		
		# Note: this calculates the long-term growth number as in, for example, the "j" and "n" models, but is different to the long-term growth of, for example, the "t" model which would require the "growth offset" that we calculate using regressions
		newCol[is.element(dates,aDates)][nonNAs][nonNAGrowths]<-as.numeric(secPToEPeak[nonNAs,1][nonNAGrowths])/(1+ growth+inflation)
		ret<-cbind(ret,newCol,deparse.level=0)
	}
	
	ret<-data.frame(ret)
	names(ret)<-names(pToEPeaks)
	return(cbind(data.frame(Date= dates),ret))
}

equityRiskModels.calculate<-function(input,settings){
	nonLocalInflation<-settings[["nonLocalInflation"]]
	
	# pToEPeaks, inflationPeriod, growth are list()'s indexed by asset name; inflationYoY is a list indexed by country code "." inflationSuffix; equityData is a timeSeries() with typically, price, .dvd.yld and .earnings data
	pToEPeaks<-input[["pes"]]
	equityData<-input[["equityData"]]
	inflationPeriod<-input[["inflationPeriod"]]
	inflationYoY<-input[["inflationYoY"]]
	growth<-input[["growth"]]
	freq<-input[["freq"]]
	dayOfWeek<-input[["dayOfWeek"]]
	inflationSuffix<-input[["inflationSuffix"]]
	
	if(length(pToEPeaks)==0){
		return(NULL)
	}
	
	dates<-c()
	for(a in names(pToEPeaks)){
		dates<-union(dates,row.names(pToEPeaks[[a]]))
	}
	dates<-sort(dates,decreasing=TRUE)

	oOutput<-eriskm.oModel(dates,pToEPeaks, inflationPeriod)
	isNotDateCol<-!is.element(names(oOutput),"Date")
	names(oOutput)[isNotDateCol]<-paste(names(oOutput)[isNotDateCol],".orisk",sep="")
	aOutput<-eriskm.aModel(dates,pToEPeaks,inflationYoY,inflationSuffix,nonLocalInflation)
	isNotDateCol<-!is.element(names(aOutput),"Date")
	names(aOutput)[isNotDateCol]<-paste(names(aOutput)[isNotDateCol],".arisk",sep="")
	eOutput<-eriskm.eModel(pToEPeaks)
	isNotDateCol<-!is.element(names(eOutput),"Date")
	names(eOutput)[isNotDateCol]<-paste(names(eOutput)[isNotDateCol],".erisk",sep="")
	dOutput<-eriskm.dModel(equityData)
	isNotDateCol<-!is.element(names(dOutput),"Date")
	names(dOutput)[isNotDateCol]<-paste(names(dOutput)[isNotDateCol],".drisk",sep="")
	gOutput<-eriskm.gModel(dates,pToEPeaks, inflationPeriod, growth)
	isNotDateCol<-!is.element(names(gOutput),"Date")
	names(gOutput)[isNotDateCol]<-paste(names(gOutput)[isNotDateCol],".grisk",sep="")

	ret<-utils.align(list(aOutput=aOutput,oOutput=oOutput,eOutput=eOutput,dOutput=dOutput,gOutput=gOutput),TRUE)
	if(freq=="monthly"){
		return(ret[utils.getEOMDatesIndices(ret$Date,TRUE),])
	}else if(freq=="weekly"){
		return(ret[utils.getWeeklyDatesIndices(ret$Date,dayOfWeek),])
	}else if(freq=="daily"){
		return(ret)
	}else{
		stop(paste("error in bretm.calculate(): the specified frequency",freq,"is not implemented"))
	}
}
