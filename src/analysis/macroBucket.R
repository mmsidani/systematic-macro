# input function
macroBucket.input<-function(settings){
	inputDir<-settings[["mbex.inputDir"]]
	cumulativeReturnDataFile<-settings[["mbex.cumulativeReturnDataFile"]]
	macroBucketDataFile<-settings[["mbex.macroBucketDataFile"]]
	
	# returns timeSeries() with cumulative returns of different portfolios. Note: fileName is, for example, the "FULL-sim.allPortsdaily-CUMM.csv" file output from the simulator
	getCumulativeReturnData<-function(fileName){		
		cumRets<-read.csv(fileName,header=TRUE,stringsAsFactors=FALSE)
		ret<-timeSeries(cumRets[,setdiff(names(cumRets),"Date")],charvec= cumRets$Date)
		names(ret)<-setdiff(names(cumRets),"Date")
		
		return(ret)
	}
	
	return(list(cumRetData= getCumulativeReturnData(paste(inputDir,cumulativeReturnDataFile,sep="")),macroBucketData=read.csv(paste(inputDir,macroBucketDataFile,sep=""),header=TRUE,stringsAsFactors=FALSE)))
}

# output function
macroBucket.output<-function(output,settings){
	outputDir<-settings[["mbex.outputDir"]]
	outputFile<-settings[["mbex.outputFile"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}
	
# given cumulative returns of a portfolio calculate the average return and average risk in quarters of a given macro label
# macroBuckets is a 2-column data.frame, with a column called "Date" containing quarter designations of the form "YYYY-Qx", where "x" is 1,...,4 (indeed, "Date" column does NOT contain dates proper); second column contains the macro labels of the quarters. cumRetSeries is a 1-column cumulative return timSeries()
# returns data.frame of 4 columns: "Date", containing macro labels (NOTE: why the misnaming? I want to use utils.align() later and that requires a column called "Date" along which the alignment is done); 2 columns "portName.qRet" and "portName.qRisk" with average quarterly returns and quarterly risk (using daily returns) for quarters of a given macro label; a 4th column called "portName.number" giving the number of quarters with a given label that were used in the averages
mbex.bucketCumRets<-function(cc,macroBuckets,cumRetSeries){
	if(ncol(macroBuckets)!=2 || !is.finite(match("Date",names(macroBuckets)))){
		stop("error in mbex.bucketCumRets(): macroBuckets is in the wrong format.")
	}
	
	if(ncol(cumRetSeries)!=1 || !is.timeSeries(cumRetSeries)){
		stop("error in mbex.bucketCumRets(): cumRetSeries is in the wrong format.")
	}

	# remove NA's
	macroBuckets<-macroBuckets[!is.na(macroBuckets[,2]),]
	# we discover the macroeconomic labels on the fly in the following statement
	macroBucketVals<-unique(macroBuckets[,2])
	# prepend country code
	macroBucketVals <-paste(cc,".", macroBucketVals,sep="")
	# initialize the space to accumulate returns and risks in different macro regimes
	retList<-list()
	for(mb in macroBucketVals){
		retList<-append(retList,list(list(qRet=0,qRisk=0,number=0)))
		names(retList)[length(retList)]<-mb
	}	
	cumRetSeries<-cumRetSeries[as.logical(is.finite(cumRetSeries[,1])),]
	dates<-as.character(row.names(cumRetSeries))
	if(dates[1]>dates[2]){
		stop("error in mbex.bucketCumRets(): cumRetSeries is in the wrong format. dates must be increasing")
	}

	dateBuckets<-utils.getYearQuarterPairs(dates,TRUE)
	# dateBucketNames is indexed by "year-quarter", to which macroBuckets assigns a macroeconomic label
	dateBucketNames<-names(dateBuckets)
	for(i in 1:length(dateBuckets)){
		# dateBuckets[i] gives the dates corresponding to a year-quarter pair
		retSeries<-cumRetSeries[as.Date(dateBuckets[[i]]),1]
		# get period return
		periodRet<-retSeries[nrow(retSeries)]/retSeries[1]-1
		periodDailyRets<-retSeries[2:nrow(retSeries)]/retSeries[1:(nrow(retSeries)-1)]-1
		# get vol
		periodRisk<-sd(periodDailyRets)
		# get the macroeconomic label for quarter dateBucketNames[i]
		bInd<-match(dateBucketNames[i],macroBuckets[,1])
		if(is.finite(bInd)){
			thisMacroBucket<-paste(cc,".",macroBuckets[bInd,2],sep="")
			retList[[thisMacroBucket]][["qRet"]]<-retList[[thisMacroBucket]][["qRet"]]+periodRet
			retList[[thisMacroBucket]][["qRisk"]]<-retList[[thisMacroBucket]][["qRisk"]]+periodRisk
			retList[[thisMacroBucket]][["number"]]<-retList[[thisMacroBucket]][["number"]]+1
		}
	}
	
	# calculate average return and risk in each macro bucket and store in a data.frame()
	ret<-NULL
	retListNames<-names(retList)
	for(i in 1:length(retList)){
		number<-retList[[i]][["number"]]
		if(number!=0){
			ret<-rbind(ret,data.frame(Date=retListNames[i],qRet=retList[[i]][["qRet"]]/number,qRisk=retList[[i]][["qRisk"]]/number,number=number))
		}else{
			ret<-rbind(ret,data.frame(Date=retListNames[i],qRet=NA,qRisk=NA,number=0))
		}
	}
	
	# prepend portfolio name to the columns
	names(ret)[2:ncol(ret)]<-paste(names(cumRetSeries),".",names(ret)[2:ncol(ret)],sep="")
	return(ret)
}

# main logic
macroBucket.calculate<-function(input,settings){
	countryPortfolioMapping<-settings[["mbex.countryPortfolioMapping"]]

	cumRetData<-input[["cumRetData"]]
	cumRetData<-rev(cumRetData)
	portNames<-names(cumRetData)
	macroBucketData<-input[["macroBucketData"]]
	mbNames<-names(macroBucketData)
	
	retList<-NULL
	for(cc in names(countryPortfolioMapping)){
		ccInds<-grep(paste("^",cc,"\\.", sep=""),mbNames)
		if(length(ccInds)==0){
			stop(paste("error in macroBucket.calculate(): country",cc,"has no macro data."))
		}
		
		# Important NOTE: only bucket those portfolios for which we have full data, i.e., the country mapping and and the cumulative returns
		portfolios<-intersect(countryPortfolioMapping[[cc]],portNames)
		
		for(p in portfolios){
			pInd<-match(p,portNames)
			if(is.finite(pInd)){
				pBuckets<-NULL
				for(i in ccInds){
					pBuckets<-rbind(pBuckets,mbex.bucketCumRets(cc,macroBucketData[,c("Date", mbNames[i])],cumRetData[,pInd]))
				}
			}else{
				stop(paste("error in macroBucket.calculate(): there's no cumulative return data for portfolio",p))
			}

			# we want to use utils.align() to merge into 1 data.frame() so we order by pBuckets$Date. Note: the "Date" column does NOT contain dates. we just call it that because this is what utils.align() expects
			pBuckets <- pBuckets[order(pBuckets[["Date"]], decreasing = TRUE ), ]
			retList<-append(retList,list(pBuckets))
		}
	}
	
	allPortfolioRetsPerBucket<-utils.align(retList,TRUE)
	names(allPortfolioRetsPerBucket)[match("Date",names(allPortfolioRetsPerBucket))]<-"macroBucket"
	
	return(allPortfolioRetsPerBucket)
}


