grth.earningsMovingAverage<-12
grth.inflationMovingAverage<-1
grth.growthMovingAverage<-1
grth.addIntercept<-TRUE

grth.getData<-function(){
	
}

# remove NAs and reverse
grth.revRmNAs<-function(tSeries){
	ret<-NULL
	for(i in 1:ncol(tSeries)){
		nonNAs<-is.finite(tSeries[,i])
		newCol<-rep(NA,nrow(tSeries))
		newCol[nonNAs]<-tSeries[nonNAs,i]
		ret<-cbind(ret,data.frame(newCol=rev(newCol)))
		names(ret)[i]<-names(tSeries)[i]
	}
	
	return(tSeries(ret,charvec=rev(row.names(tSeries))))
}

grth.alignData<-function(earningsSeries,inflationSeries,growthSeries){
	eomEarn<-utils.getEOMData(earningsSeries,TRUE)
	eomInfl<-utils.getEOMData(inflationSeries,TRUE)
	eomGrowth<-utils.getEOMData(growthSeries,TRUE)
	
	allDates<-as.Date(sort(union(row.names(eomEarn),union(row.names(eomInfl),row.names(eomGrowth))),decreasing=TRUE))
	earnDates<-as.Date(row.names(eomEarn))
	inflDates<-as.Date(row.names(eomInfl))
	growthDates<-as.Date(row.names(eomGrowth))
	
	ret<-NULL
	for(d in allDates){
		earnInd<-match(TRUE,earnDates<=d)
		inflInd<-match(TRUE,inflDates<=d)
		grthInd<-match(TRUE,growthDates<=d)
		
		newRec<-data.frame(Date=as.character(d),earnings=eomEarn[earnInd,1],inflation=eomInfl[inflInd,1],growth=eomGrowth[grthInd,1])
		ret<-rbind(ret,newRec)
	}
	
	return(ret)
}

grth.regress<-function(earningsSeries,inflationSeries,growthSeries){
	earnChanges<-earningsSeries[1:(nrow(earningsSeries)-1),]/earningsSeries[2:nrow(earningsSeries),]-1
	earnChangesMA<-grth.revRmNAs(filter(earnChanges[nrow(earnChanges):1,],rep(1/grth.earningsMovingAverage,grth.earningsMovingAverage),sides=1,circular=FALSE,method="convolution"))
	
	inflChangesYOY<-inflationSeries[1:(nrow(inflationSeries)-13),]/inflationSeries[13:nrow(inflationSeries),]-1
	grthChangesYOY<-growthSeries[1:(nrow(growthSeries)-13),]/growthSeries[13:nrow(growthSeries),]-1

	numPoints<-min(nrow(earnChangesMA[,i]),min(nrow(inflChangesMA[, inflInd]),nrow(grthChangesMA[,grthInd])))
		
	y<-earnChangesMA[1:(numPoints-1),i]
	x<-data.frame(inflChangesMA[2:numPoints,inflInd],grthChangesMA[2:numPoints,grthInd])
	if(grth.addIntercept){
		x<-cbind(x,data.frame(rep(1,numPoints)))
	}
		
	coeff<-lm.fit(as.numeric(x),y)
	
	return(coeff)
}

grth.main<-function(){
	
	
}

