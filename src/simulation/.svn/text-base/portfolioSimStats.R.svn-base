psstats.readInputFiles<-function(portfolio,portfolioReturnHistFileSuffix,portfolioReallocationHistFileSuffix,inputDir){
	retHistFile<-paste(portfolio,portfolioReturnHistFileSuffix,sep="")
	reallocHistFile<-paste(portfolio,portfolioReallocationHistFileSuffix,sep="")
	
	portfolioReturnHistory<-read.csv(paste(inputDir, retHistFile,sep=""),header=TRUE,stringsAsFactors=FALSE)
	portfolioReallocationHistory<-read.csv(paste(inputDir, reallocHistFile,sep=""),header=TRUE,stringsAsFactors=FALSE)
	
	return(list(portRetHist= portfolioReturnHistory,reallocHist= portfolioReallocationHistory))
}

psstats.getAssetData<-function(idsToGet,useDB,inputDir,retHistFile,dateFormat,dbSettings){
	if(useDB){
		return(utils.getDataFromDB(idsToGet,NULL,dbSettings))
	}else{
		return(utils.getDataFromFile(idsToGet,NULL,paste(inputDir,retHistFile,sep=""),dateFormat))
	}
}

portfolioSimStats.input<-function(settings){
	useDB<-settings[["psstats.useDB"]]
	inputDir<-settings[["psstats.inputDir"]]
	inputDirER<-settings[["psstats.inputDirER"]]
	retHistFile<-settings[["psstats.retHistFile"]]
	dateFormat<-settings[["psstats.dateFormat"]]
	portfolioReturnHistFileSuffix<-settings[["psstats.portfolioReturnHistFileSuffix"]]
	portfolioReallocationHistFileSuffix<-settings[["psstats.portfolioReallocationHistFileSuffix"]]
	portfolioNames<-settings[["psstats.portfolioNames"]]
	cashNameSuffix<-settings[["psstats.cashNameSuffix"]]
	totalRetSuffix<-settings[["psstats.totalRetSuffix"]]
	doAssetClassStats<-settings[["psstats.doAssetClassStats"]]
	dbSettings<-settings[["dbSettings"]]
	
	# get portfolio simulation data
	allResults<-NULL
	reallocHists<-NULL
	baseCashRetHist <-list()
	baseCashRetNames <-c()	
	allSecs<-c()
	allWeightsHists<-NULL
	for (p in portfolioNames){
		portSimHist<-psstats.readInputFiles(p,portfolioReturnHistFileSuffix,portfolioReallocationHistFileSuffix,inputDir)
		portRetHist<-portSimHist[["portRetHist"]]
		reallocHist<-portSimHist[["reallocHist"]]
			
		reallocHists<-append(reallocHists,list(reallocHist[nrow(reallocHist):1,]))
		names(reallocHists)[length(reallocHists)]<-p

		newRets<-list(portRetHist[nrow(portRetHist):1,c("Date","ret")])
		names(newRets)<-p
		allResults<-append(allResults,newRets)
		
		portSecs<-setdiff(names(portRetHist),c("Date","ret","realloc"))
		allSecs<-union(allSecs,portSecs)
		allWeightsHists<-append(allWeightsHists,list(portRetHist[c("Date",portSecs)]))
		names(allWeightsHists)[length(allWeightsHists)]<-p
			
		# first cash position in portfolio is base cash by construction in frontierBuilder
		baseCashRetNames<-c(baseCashRetNames,paste(portSecs[grep(cashNameSuffix,portSecs)][1],totalRetSuffix,sep=""))
	}
	
	# get cash rates and possibly assetDtrs
	idsToGet<-baseCashRetNames
	if(doAssetClassStats){
		idsToGet<-union(idsToGet,utils.generateCrossIds(allSecs,totalRetSuffix))
	}
	assetData<-psstats.getAssetData(idsToGet,useDB,inputDirER,retHistFile,dateFormat,dbSettings)
	baseCashRetHist<-assetData[nrow(assetData):1,baseCashRetNames]
	
	return(list(baseCashRetHist=baseCashRetHist,allResults=allResults,reallocHists=reallocHists,allSecs=allSecs,allWeightsHists= allWeightsHists,secDtrs=assetData))
}

portfolioSimStats.output<-function(output,settings){
	outputDir<-settings[["psstats.outputDir"]]

	yearlyRets<-output[["yearlyRets"]]
	monthlyRets<-output[["monthlyRets"]]
	if(!setequal(names(yearlyRets),names(monthlyRets))){
		stop("error in portfolioSimStats.output(): not all portfolios have both yearly and monthly returns")
	}
	for(portName in names(yearlyRets)){
		temp<-yearlyRets[[portName]]
		write.csv(temp[nrow(temp):1,],paste(outputDir,portName,".sim.yearlyRets.csv",sep=""),row.names=FALSE)
		temp<-monthlyRets[[portName]]
		write.csv(temp[nrow(temp):1,],paste(outputDir,portName,".sim.monthlyRets.csv",sep=""),row.names=FALSE)
	}
	
	write.csv(output[["portStatsFirsts"]],paste(outputDir,"portStats.csv",sep=""),row.names=TRUE)
	write.csv(output[["portStats"]],paste(outputDir,"portStatsAll.csv",sep=""),row.names=TRUE)
	write.csv(output[["yearlyComparisons1"]],paste(outputDir,"FULL-sim.allPortsYearly.csv",sep=""),row.names=FALSE)  

	write.csv(output[["MOMCUMComparisons1"]],paste(outputDir,"FULL-sim.allPortsMOM-CUMM.csv",sep=""),row.names=FALSE)
	write.csv(output[["MOMComparisons1"]],paste(outputDir,"FULL-sim.allPortsMOM.csv",sep=""),row.names=FALSE)	
	
	write.csv(output[["DODCUMComparisons1"]],paste(outputDir,"FULL-sim.allPortsdaily-CUMM.csv",sep=""),row.names=FALSE)
	write.csv(output[["DODComparisons1"]],paste(outputDir,"FULL-sim.allPortsdaily.csv",sep=""),row.names=FALSE)
	
	acWeights<-output[["assetClassWeights"]]
	if(!is.null(acWeights)){
		for(portName in names(acWeights)){
			temp<-acWeights[[portName]]
			temp2<-NULL
			for(i in 1:length(temp)){
				dates<-row.names(temp[[i]])
				thisDF<-cbind(data.frame(start=rep(dates[length(dates)],nrow(temp[[i]])),end=rep(dates[1],nrow(temp[[i]])),Date=dates),data.frame(temp[[i]]))
				temp2<-rbind(temp2,thisDF)
			}
			write.csv(temp2,paste(outputDir,portName,"-assetClassWeights.csv",sep=""),row.names=FALSE)
		}
	}
	acRets<-output[["assetClassRets"]]
	if(!is.null(acRets)){
		for(portName in names(acRets)){
			write.csv(acRets[[portName]],paste(outputDir,portName,"-assetClassRets.csv",sep=""),row.names=FALSE)
		}
	}
}

psstats.getMaxDrawdown<-function(dates,cumRets){
# find the maximum draw down over a period
# cumRets are the cumulative returns over the period; dates are the associated dates
# returns data frame containing the max draw-down and the date on which it occurred
	
	# all high water marks
	cumMaxes <- cummax(cumRets)
	# for every date, find the return between the previous max return and this date
	fromPeaks<-cumRets/cumMaxes-1
	# find the index of the minimum return starting from a peak
	ind<-order(fromPeaks)[1]
	# find the previous date on which the peak was reached
	rleLengths <- rle(cumMaxes[1:ind])$lengths
	lastPeakFirstInd <- ind - rleLengths[length(rleLengths)] + 1
	
	return(data.frame(Date=as.character(dates[ind]),maxDrawdown=as.numeric(fromPeaks[ind]),highWaterMarkDate=dates[lastPeakFirstInd]))
}

psstats.getVol<-function(ret){
# ret is a vector
# calculate the vol of returns and return in a data frame
	
	# return standard deviation
	return(data.frame(vol=sd(ret[1:length(ret)])))
}

psstats.getEOMData<-function(tSeries,excludeStubs){
# NOTE: the time series, tSeries is assumed to be in chronological order
# for all the months for which tSeries has data, this returns the data for the latest date in those months
# the return object is also a timeSeries
	
	# tSeries that we pass here is in chronological order; reverse the order to be able to use datesUtils utilities; why not just write one to handle chronological order and save ourselves from reversing and then reversing again? because of the existing stubs checking logic. it should quite simple to handle chronological order though. TODO
	tempSeries<-tSeries[nrow(tSeries):1,]
	ret<-utils.getEOMData(tempSeries,excludeStubs)
	# now reverse again and return
	return(ret[nrow(ret):1,])
}

psstats.getYearlyRets<-function(cumRets){
# cumRets is a data frame with Date and associated cumulative returns
# returns data frame with one row per year and return during year.
	
	# find all the years that appear in Date
	yearsBlocks<-rle(utils.getYear(cumRets$Date))$lengths
	
	#  TODO: if the first or last (or both) years are truncated, we still report returns for them 
	inds<-union(1,cumsum(yearsBlocks))
	yearCumRets<-cumRets$cumRet[inds]
	eoYear<-cumRets$Date[inds]
	# cumRets is in chronological order
	yearRets<-yearCumRets[2:length(yearCumRets)]/yearCumRets[1:(length(yearCumRets)-1)]-1
	yearRets<-c(NA,yearRets)
	
	return(data.frame(Date=eoYear,ret=yearRets))
}

psstats.calcPeriodSummaryStats<-function(periodDF,periodPerYear){
# periodDF is a timeSeries with ret and riskFreeCum columns, containing cumulative portfolio returns and cumulative cash returns; periodPerYear is frequency per year
# returns data frame with many entries
	
	lastInd<-nrow(periodDF)
	if(lastInd < 2){
		# can't calculate any of the quantities of interest here
		return(data.frame(Full=0,AverageYear=NA,AverageYearlyAlpha=NA,AnnualizedVol=NA,AverageSHARPE=NA,Calmar=NA,InformationRatioCashplus5=NA,UPpercentage=NA,DOWNpercertage=NA,AverageRet=NA,AverageCashRet=NA,AvgUP=NA,AvgDOWN=NA,up25percentilemean=NA,down25percentilemean=NA,MaxDrawdown=NA,DD=NA))
	}
	
	maxDrawdownPeriod<-psstats.getMaxDrawdown(row.names(periodDF),periodDF$ret)
	periodRets<-(periodDF$ret[2:lastInd]/periodDF$ret[1:(lastInd-1)])-1
	numWholePeriods<-length(periodRets)
	CashRets<-(periodDF$riskFreeCum[2:lastInd]/periodDF$riskFreeCum[1:(lastInd-1)])-1
	Periodvols<-psstats.getVol(periodRets) * sqrt(periodPerYear)

	AverageRet<-mean(periodRets)
	AverageCashRet<-mean(CashRets)
	AverageYear<-((1+AverageRet)^periodPerYear)-1
	AverageCashYear<-((1+AverageCashRet)^periodPerYear)-1
	AverageYearlyAlpha<-AverageYear-AverageCashYear
	AverageSHARPE<-AverageYearlyAlpha/Periodvols$vol[1]
	
	UpPeriodRets<-periodRets[periodRets>0]
	DownPeriodRets<-periodRets[periodRets <0]
	up<-length(UpPeriodRets)/numWholePeriods
	down<-length(DownPeriodRets)/numWholePeriods
	AvgUP<-mean(UpPeriodRets)
	AvgDOWN<-mean(DownPeriodRets)

	up25PercentileMean<-mean(periodRets[periodRets>=quantile(periodRets,.75)])
	down25PercentileMean<-mean(periodRets[periodRets <=quantile(periodRets,.25)])
	
	calmar<--AverageYear/maxDrawdownPeriod$maxDrawdown[1]
	
	infoseries<-(periodRets-(CashRets+(.05/periodPerYear)))
	informationratio<-(AverageYearlyAlpha-.05)/(sd(infoseries)* sqrt(periodPerYear))
		
	ret<-data.frame(Full= numWholePeriods,AverageYear=AverageYear,AverageYearlyAlpha=AverageYearlyAlpha,AnnualizedVol=Periodvols$vol[1],AverageSHARPE=AverageSHARPE,Calmar=calmar,InformationRatioCashplus5=informationratio,UPpercentage=up,DOWNpercertage=down,AverageRet=AverageRet,AverageCashRet=AverageCashRet,AvgUP=AvgUP,AvgDOWN=AvgDOWN,up25percentilemean=up25PercentileMean,down25percentilemean=down25PercentileMean,MaxDrawdown=maxDrawdownPeriod$maxDrawdown[1],DD=maxDrawdownPeriod$Date[1],HighWaterMark.DD=maxDrawdownPeriod$highWaterMarkDate[1])
	
	return(ret)
}

psstats.getPeriodOnPeriodComparisons<-function(dates,rets){
# multiple portfolio comparisons
# dates is a vector of dates for which the rets of multiple portfolios should be mapped; rets is a list of portfolio returns histories indexed by portfolio name
# returns single data frame with the returns of all portfolios in the rets list
	
	dates <-sort(dates) 
	comparisons<-data.frame(Date=dates)
	for(i in 1:length(rets)){
		portName<-names(rets)[i]
		newCol<-rep(NA,nrow(comparisons))
		newCol[is.element(comparisons$Date,rets[[i]]$Date)]<-rets[[i]]$ret
		comparisons<-cbind(comparisons,data.frame(newCol))
		names(comparisons)[i+1]<-portName
	}
	
	return(comparisons[order(comparisons[,1],decreasing=TRUE),])
}

psstats.doPortStats<-function(portName,cumPortRetSeries,reallocHist,bDaysPerYear,cashNameSuffix){
# do portfolio stats (avg return, vol, etc.) over entire period, and over every decade and half-decade in the history
# cumPortSeries are the cumulative returns of one portfolio over entire history; bDaysPerYear is 365.25, for example
# returns data frame for portfolio with stats over relevant time periods
	
	dates<-as.character(row.names(cumPortRetSeries))
	portStats<-NULL
	# entire period
	startEndDatePairs<-data.frame(start=dates[1],end=dates[length(dates)])
	# add decades in data
	startEndDatePairs<-rbind(startEndDatePairs,utils.getPeriodsInYears(dates,10,FALSE))
	# add half-decades
	startEndDatePairs<-rbind(startEndDatePairs,utils.getPeriodsInYears(dates,5,FALSE))
	for (i in 1:nrow(startEndDatePairs)){
		start<-startEndDatePairs$start[i]
		end<-startEndDatePairs$end[i]
		# extract return series for period between start and end
		returnSeries<-cumPortRetSeries[dates>= start & dates<=end,]
		# ... and rebase it ; note we're working with cumulative returns so we divide by just first element, not 1+first element
		returnSeries$ret<-returnSeries$ret/returnSeries$ret[1]
		returnSeries$riskFreeCum<-returnSeries$riskFreeCum/returnSeries$riskFreeCum[1]
		horizon<-(as.numeric(as.Date(end))-as.numeric(as.Date(start)))/365.25		
		# do the stats at various frequencies
		daily<-psstats.calcPeriodSummaryStats(returnSeries, bDaysPerYear)
		weekly<-psstats.calcPeriodSummaryStats(utils.getWeeklyData(returnSeries,"Friday"),52)
		monthly<-psstats.calcPeriodSummaryStats(psstats.getEOMData(returnSeries,FALSE),12)
		quarterly<-psstats.calcPeriodSummaryStats(utils.getQuarterData(returnSeries),4)
		yearly<-psstats.calcPeriodSummaryStats(utils.getYearData(returnSeries),1)

		list1<-list(Start=start,End=end,Years=horizon,IRR=returnSeries$ret[nrow(returnSeries)]^(1/horizon)-1)
		
		# do reallocation stats
		reallocStats<-psstats.getReallocStats(portName,reallocHist[reallocHist$Date>=start & reallocHist$Date<=end,],cashNameSuffix)
		list7<-list(totalTrades=reallocStats[[portName]][["totalTrades"]],totalTradeDays=reallocStats[[portName]][["totalTradeDays"]],avTradesYear=reallocStats[[portName]][["totalTrades"]]/horizon,avTradeDays=reallocStats[[portName]][["totalTradeDays"]]/horizon,AvgAnnPortTurnover=reallocStats[[portName]][["volumeTraded"]]/horizon,AnnTransactionCosts=reallocStats[[portName]][["transactionCosts"]]/horizon)
		
		bigList<-c(list1,yearly=yearly,quarterly=quarterly,monthly=monthly,weekly=weekly,daily=daily,list7)

		newColumn<-t(data.frame(row.names=paste(portName,ifelse(i==1,"",paste(".",i-1,sep="")),sep=""), bigList))	
		portStats<-cbind(portStats,newColumn)
	}
	
	return(portStats)
}

psstats.doReports<-function(baseCashRetHist,allResults,reallocHists, bDaysPerYear,dateFormat,cashNameSuffix){
	eomCumRets<-NULL
	yearlyRets<-NULL
	maxDrawdowns<-NULL
	vols<-NULL
	allYearlyRets<-NULL
	allYearlyDates<-NULL
	
	allcumMOMRets<-NULL
	allMOMRets<-NULL
	allMOMDates<-NULL
	allCumRets<-NULL
	
	allcumDODRets<-NULL
	allDODRets<-NULL
	allDODDates<-NULL
	
	baseCashNames<-names(baseCashRetHist)
	
	portStats<-NULL
	avgPortTurnover<-0
	# loop over the different portfolio simulations that we want stats for
	for(i in 1:length(allResults)){
		portName<-names(allResults)[i]	
		portHist<-allResults[[i]]
		dates<-as.Date(portHist$Date,format=dateFormat)
		
		thisBaseCashRetHist<-baseCashRetHist[,baseCashNames[i]]
		startIndex<-match(as.character(dates[1]),as.character(row.names(thisBaseCashRetHist)))
		endIndex<-match(as.character(dates[length(dates)]),as.character(row.names(thisBaseCashRetHist)))
		cashCumRetRange<-thisBaseCashRetHist[startIndex:endIndex]
		# calculate cumulative returns of cash and portfolio and rebase to beginning of period
		cashCumReturn<-cumprod(1+cashCumRetRange)/(1+cashCumRetRange[1])
		cumPortRet<-cumprod(1+portHist$ret)/(1+portHist$ret[1])

		cumPortRetSeries<-timeSeries(data.frame(ret=cumPortRet,riskFreeCum= cashCumReturn),charvec=dates)
		
		# DOD return summary
		allcumDODRets<-append(allcumDODRets,list(data.frame(Date=dates,ret=cumPortRet)))
		names(allcumDODRets)[i]<-portName
		allDODDates<-as.Date(union(allDODDates,as.Date(portHist$Date,format=dateFormat)),format=dateFormat, origin="1970-01-01")
		allDODRets<-append(allDODRets,list(data.frame(Date=dates,ret=portHist$ret)))
		names(allDODRets)[i]<-portName
		
		monthsBlocks<-rle(months(dates))$lengths
		indsforMonthly<-union(1,cumsum(monthsBlocks))

		monthlyCumRets<-cumPortRet[indsforMonthly]
		monthlyRets<-c(NA,monthlyCumRets[2:length(monthlyCumRets)]/monthlyCumRets[1:(length(monthlyCumRets)-1)]-1)
		
		cumRets<-data.frame(Date=dates[indsforMonthly],cumRet=monthlyCumRets,ret=monthlyRets)
		cummom<-data.frame(Date=dates[indsforMonthly],ret=monthlyCumRets)
		mom<-data.frame(Date=dates[indsforMonthly],ret=monthlyRets)
		
		# MOM CUM returns summary
		allCumRets<-append(allCumRets,list(cumRets))
		names(allCumRets)[length(allCumRets)]<-portName
		allcumMOMRets<-append(allcumMOMRets,list(cummom))
		names(allcumMOMRets)[i]<-portName
		allMOMDates<-as.Date(union(allMOMDates,as.Date(cumRets$Date,format=dateFormat)),format=dateFormat, origin="1970-01-01")
		# MOM RETURN summary
		allMOMRets<-append(allMOMRets,list(mom))
		names(allMOMRets)[i]<-portName
		
		# YEARly Numbers
		yearlyRets<-psstats.getYearlyRets(cumRets)

		# YOY returns summary
		allYearlyRets<-append(allYearlyRets,list(yearlyRets))
		names(allYearlyRets)[i]<-portName
		allYearlyDates<-as.Date(union(allYearlyDates,as.Date(yearlyRets$Date,format=dateFormat)),format=dateFormat, origin="1970-01-01")

		portStats<-cbind(portStats,psstats.doPortStats(portName,cumPortRetSeries,reallocHists[[portName]], bDaysPerYear,cashNameSuffix))
	}

	# firsts are the columns that contain stats for the entire history. we leave out decade and half-decade stats to portStatsAll.csv
	firsts<-match(names(allResults),colnames(portStats))
	portStatsFirsts<-data.frame(portStats[,firsts])
	names(portStatsFirsts)<-names(allResults)
	# yoy summary
	yearlyComparisons1<-psstats.getPeriodOnPeriodComparisons(allYearlyDates,allYearlyRets)
	# MOM cum and MOM summaries
	MOMCUMComparisons1<-psstats.getPeriodOnPeriodComparisons(allMOMDates, allcumMOMRets)
	MOMComparisons1<-psstats.getPeriodOnPeriodComparisons(allMOMDates, allMOMRets)
	# DOD cum and DOD summaries
	DODCUMComparisons1<-psstats.getPeriodOnPeriodComparisons(allDODDates, allcumDODRets)
	DODComparisons1<-psstats.getPeriodOnPeriodComparisons(allDODDates, allDODRets)
	
	return(list(yearlyRets=allYearlyRets,monthlyRets=allCumRets,portStats=portStats,portStatsFirsts=portStatsFirsts, yearlyComparisons1=yearlyComparisons1,MOMCUMComparisons1=MOMCUMComparisons1,MOMComparisons1=MOMComparisons1,DODCUMComparisons1=DODCUMComparisons1,DODComparisons1=DODComparisons1))
}

psstats.getReallocStats<-function(portName,reallocHist,cashNameSuffix){
	if(is.null(reallocHist) || nrow(reallocHist) == 0){
		ret<-list(newPort=list(totalTrades=0, totalTradeDays = 0, volumeTraded = 0, transactionCosts=0))
    	names(ret)[length(ret)]<-portName
    	return(ret)
    }
    
	# exclude Date and cash columns from the stats
	reallocHistNames<-names(reallocHist)
	nonCashAssetInds<-setdiff(1:ncol(reallocHist),union(grep(cashNameSuffix, reallocHistNames),union(grep("Date", reallocHistNames),grep("tc",reallocHistNames))))
	tempDF<-reallocHist[,nonCashAssetInds]
	
	# where there's a non-zero in tempDF, a trade had to have been done
	tempDF1<-as.numeric(tempDF!=0)

	totalTrades<-sum(tempDF1)

	# if a trade was done on a given day, then the corresponding row would have at least one non-zero 

	#totalTradeDays <-sum(as.numeric(rowSums(tempDF,na.rm=TRUE)!=0))
	totalTradeDays <- nrow(tempDF)
	
	volumeTraded <- sum(abs(tempDF))
	
	# transaction costs
	tc<-sum(reallocHist[["tc"]])
	
    ret<-list(newPort=list(totalTrades=totalTrades, totalTradeDays = totalTradeDays, volumeTraded = volumeTraded, transactionCosts=tc))
    names(ret)[length(ret)]<-portName
	
	return(ret)
}

psstats.plotAssetWeights<-function(allWeightsHists,numPortfoliosToPlot,securityAssetClasses,dateFormat, ourPalette,plotPeriods,plotTitle,graphicsDevice, outputDir,pngSettings){
# allWeightsHists is a list() indexed by portfolio name of the portfolio's weights history
# no return value
	
	ret<-NULL
	plotCounter<-1
	for(p in names(allWeightsHists)){
		# find asset class (as defined in names(securityAssetClasses) and then plot. Note: if no asset in p belongs to a certain asset class, then that asset class is just omitted instead of being added with weight 0
		assetClassesHist<-securityClasses.assetClassPeriodsWeights(timeSeries(allWeightsHists[[p]][setdiff(names(allWeightsHists[[p]]),"Date")],charvec=allWeightsHists[[p]][["Date"]]),securityAssetClasses,plotPeriods)
		ret<-append(ret,list(assetClassesHist))
		names(ret)[length(ret)]<-p
		if(plotCounter<= numPortfoliosToPlot){
			for(i in 1:length(assetClassesHist)){
				# new plot window
				if(graphicsDevice=="screen"){
					dev.new()
				}

				dates<-row.names(assetClassesHist[[i]])
				fileName<-paste(outputDir,p,".",dates[length(dates)],".",dates[1],".area.",graphicsDevice,sep="")
				draw.areaPlot(graphicsDevice,fileName,cbind(data.frame(Date=dates),data.frame(assetClassesHist[[i]])),plotTitle,"Date","Weight",dateFormat, ourPalette,pngSettings)
			}
			plotCounter<-plotCounter+1
		}
	}
	
	return(ret)
}

psstats.plotAssetContributions<-function(allResults,allWeightsHists,numPortfoliosToPlot,secDtrs,securityAssetClasses,dtrSuffix, ourPalette,plotPeriods,barPlotTitle,pieTitle,graphicsDevice, outputDir,pngSettings){
# allWeightsHists as for psstats.areaPlots(); allResults is a list() indexed by portfolio giving the simulated returns of the portfolio with a "ret" field; numPortfoliosToPlot is the number of plots we want to produce; secDtrs is a timeSeries() of dtr's; securityAssetClasses is a list() indexed by custom asset classes of assets in those asset classes; dtrSuffix is a string; ourPalette is typically a vector of strings as returned by global.palette(); plotPeriods is an array of the lengths of periods over which we want graphs
# no return value
	
	ret<-NULL
	plotCounter<-1
	for(p in names(allWeightsHists)){
		allResults[[p]]<-allResults[[p]][nrow(allResults[[p]]):1,]
		# calculate asset class returns for the entire history of the portfolio and for every decade and half-decade
		assetClassReturns<-securityClasses.assetClassPeriodsReturns(timeSeries(allResults[[p]][setdiff(names(allResults[[p]]),"Date")],charvec=allResults[[p]][["Date"]]),timeSeries(allWeightsHists[[p]][setdiff(names(allWeightsHists[[p]]),"Date")],charvec=allWeightsHists[[p]][["Date"]]),securityAssetClasses,secDtrs, plotPeriods,dtrSuffix)
		ret<-append(ret,list(assetClassReturns))
		names(ret)[length(ret)]<-p
		if(plotCounter<= numPortfoliosToPlot){
			for(i in 1:nrow(assetClassReturns)){
				# new plot window
				if(graphicsDevice=="screen"){
					dev.new()
				}
			
				subTitle<-paste(assetClassReturns[["start"]][i],"  to  ",assetClassReturns[["end"]][i],sep="")
				fileName<-paste(outputDir,p,".",assetClassReturns[["start"]][i],".",assetClassReturns[["end"]][i],sep="")
				draw.barPlot(graphicsDevice,paste(fileName,".bar.",graphicsDevice,sep=""),as.numeric(assetClassReturns[i,4:ncol(assetClassReturns)]), barPlotTitle, subTitle,names(assetClassReturns)[4:ncol(assetClassReturns)],"","Return", ourPalette,pngSettings, list(x="topright"))
			}
			plotCounter<-plotCounter+1
		}
	}
	
	return(ret)
}

psstats.doPlots<-function(doAssetClassStats,allResults,allWeightsHists,secDtrs,securityAssetClasses,numPortfoliosToPlot,dateFormat,dtrSuffix, ourPalette,plotPeriods,areaPlotTitle,barPlotTitle,pieTitle,graphicsDevice,outputDir,pngSettings){
	acWeights<-NULL
	acRets<-NULL
	if(doAssetClassStats){
		acWeights<-psstats.plotAssetWeights(allWeightsHists,numPortfoliosToPlot,securityAssetClasses,dateFormat, ourPalette,plotPeriods,areaPlotTitle,graphicsDevice, outputDir,pngSettings)
		acRets<-psstats.plotAssetContributions(allResults,allWeightsHists,numPortfoliosToPlot,secDtrs,securityAssetClasses,dtrSuffix, ourPalette,plotPeriods,barPlotTitle,pieTitle,graphicsDevice, outputDir,pngSettings)
	}
	
	return(list(assetClassWeights= acWeights, assetClassRets= acRets))
}

portfolioSimStats.calculate<-function(input,settings){
	dateFormat<-settings[["psstats.dateFormat"]]
	outputDir<-settings[["psstats.outputDir"]]
	cashNameSuffix<-settings[["psstats.cashNameSuffix"]]
	doAssetClassStats<-settings[["psstats.doAssetClassStats"]]
	numPortfoliosToPlot<-settings[["psstats.numPortfoliosToPlot"]]
	securityAssetClasses<-settings[["psstats.securityAssetClasses"]]
	dateFormat<-settings[["psstats.dateFormat"]]
	dtrSuffix<-settings[["psstats.totalRetSuffix"]]
	bDaysPerYear<-settings[["psstats.bDaysPerYear"]]
	ourPalette<-settings[["psstats.palette"]]
	plotPeriods<-settings[["psstats.plotPeriods"]]
	areaPlotTitle<-settings[["psstats.areaPlotTitle"]]
	barPlotTitle<-settings[["psstats.barPlotTitle"]]
	graphicsDevice<-settings[["psstats.graphicsDevice"]]
	pngSettings<-settings[["psstats.pngSettings"]]
	
	baseCashRetHist<-input[["baseCashRetHist"]]
	allResults<-input[["allResults"]]
	reallocHists<-input[["reallocHists"]]
	allSecs<-input[["allSecs"]]
	allWeightsHists<-input[["allWeightsHists"]]
	secDtrs<-input[["secDtrs"]]
	
	# should we move psstats.doPlots() call to portfolioSimStats.output() for consistency?
	assetClassesHist<-psstats.doPlots(doAssetClassStats,allResults,allWeightsHists,secDtrs,securityAssetClasses,numPortfoliosToPlot,dateFormat,dtrSuffix, ourPalette,plotPeriods,areaPlotTitle,barPlotTitle,pieTitle,graphicsDevice,outputDir,pngSettings)

	return(append(assetClassesHist,psstats.doReports(baseCashRetHist[nrow(baseCashRetHist):1,],allResults,reallocHists,bDaysPerYear,dateFormat,cashNameSuffix)))
}

