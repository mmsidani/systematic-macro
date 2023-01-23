bcsec.findClass<-function(sec, secClasses){
# sec is a string; secClasses is a list(), indexed by asset class name of arrays of security to assigned that class
# returns integer that is the index of the asset class name that is to be given to sec
	
	# grep() actually works on list()'s
	ind<-grep(sec, secClasses)
	if(length(ind)!=1){
		stop(paste("error in securityClasses.R: 0 or more than 1 class was specified for security",sec))
	}
	
	return(ind)
}

bcsec.matchSecs<-function(secs,secClasses){
# find the asset classes of secs
# secs is an array of strings; secClasses is as in bcsec.findClass()
# returns list() indexed by asset class of assets in secs that belong to that class
	
	ret<-NULL
	for(ac in names(secClasses)){
		inds<-match(secClasses[[ac]],secs)
		if(sum(is.finite(inds))!=0){
			ret<-append(ret,list(newClass=secClasses[[ac]][is.finite(inds)]))
			names(ret)[length(ret)]<-ac
		}
	}
	
	return(ret)
}

securityClasses.assetClassWeights<-function(weightsHistory,secClasses){
# weightsHistory is a timeSeries() -- the history of weights of a portfolio; secClasses is as above
# returns a timeSeries() with weights of each asset class
	
	# initialize return structure
	ret<-data.frame(Date=row.names(weightsHistory))
	for(i in 1:ncol(weightsHistory)){
		sec<-names(weightsHistory)[i]
		# determine the asset class to which sec belongs
		secClass<-names(secClasses)[bcsec.findClass(sec, secClasses)]
		if(!is.element(secClass,names(ret))){
			# we're here, therefore sec is the first in asset class secClass
			ret<-cbind(ret,data.frame(newCol=rep(0,nrow(weightsHistory))))
			names(ret)[ncol(ret)]<-secClass
		}

		# accumulate the weights of assets in asset class secClass
		ret[[secClass]]<-ret[[secClass]]+as.numeric(weightsHistory[,i])
	}

	secClassesNames<-names(secClasses)
	retSecClasses<-secClassesNames[is.element(secClassesNames,names(ret))]
	return(timeSeries(data.frame(ret[, retSecClasses]),charvec=ret$Date))
}

securityClasses.assetClassPeriodsWeights<-function(weightsHistory,secClasses,periodsInYears){
# calculate assets classes weights over periods of specific lengths. a negative number means over entire period.
# first 2 args as in securityClasses.assetClassWeights(); periodsInYears is an array of numbers
# returns list() NOT indexed by anything meaningful of timeSeries() of weights for the period between start and end dates
	
	# utils.getPeriodsInYears() requires dates in chronological order
	dates<-row.names(weightsHistory)
	revDates<-rev(dates)
	ret<-NULL
	for(p in periodsInYears){
		if(p<0){
			startEndDatePairs<-data.frame(start= revDates[1],end= revDates[length(revDates)])
		}else{
			startEndDatePairs<-utils.getPeriodsInYears(revDates,p,FALSE)
		}
		if (!is.null(startEndDatePairs)){
			for(i in 1:nrow(startEndDatePairs)){
				startDate<-startEndDatePairs[["start"]][i]
				endDate<-startEndDatePairs[["end"]][i]
				# we include both start and end dates here
				ret<-append(ret,list(securityClasses.assetClassWeights(weightsHistory[dates>= startDate & dates<= endDate,],secClasses)))
			}
		}
	}
	
	return(ret)
}

securityClasses.assetClassReturns<-function(weightsHistory,secClasses,secDtrs,dtrSuffix){
# weightsHistory is a timeSeries() -- the history of weights of a portfolio; secClasses is a list() indexed by asset class name of securities belong to the class; secDtrs is a timeSeries() of dtrs (and by its very name, daily); dtrSuffix is a string, typically ".dtr"
# returns timeSeries() of asset class returns
	
	weightsHistoryDates<-row.names(weightsHistory)
	secDtrsDates<-row.names(secDtrs)

	# align the two. we want to apply the dtr dated t in secDtrs to the weights dated t-1 in weightsHistory
	maxDate<-weightsHistoryDates[1]
	minDate<-weightsHistoryDates[length(weightsHistoryDates)]
	if(maxDate<minDate){
		stop("error in securityClasses.assetClassReturns(): weightsHistory and secDtrs are expected to be in reverse chronological order")
	}
	secDtrs<-secDtrs[secDtrsDates>minDate & secDtrsDates<=maxDate,]
	if(nrow(secDtrs)!=nrow(weightsHistory)-1){
		stop("error in securityClasses.assetClassReturns(): something off. portfolio weights history dates and return dates do not match.")
	}
	weightsHistory<-weightsHistory[2:nrow(weightsHistory),]
	
	secRetHist<-NULL
	assetClassSecs<-bcsec.matchSecs(setdiff(names(weightsHistory),"Date"),secClasses)
	# date the data by the date of the dtr not the date of the weights
	ret<-data.frame(Date=row.names(secDtrs))
	for(ac in names(assetClassSecs)){
		secs<-assetClassSecs[[ac]]
		secsDtr<-paste(secs,dtrSuffix,sep="")

		matxDtrs<-as.matrix(secDtrs[,secsDtr])
		matxDtrs[!is.finite(matxDtrs)]<-0.
		newCol<-rowSums(as.matrix(weightsHistory[,secs])* matxDtrs)
		ret<-cbind(ret,newCol=newCol)
		names(ret)[ncol(ret)]<-ac
	}
	
	secClassesNames<-names(secClasses)
	retSecClasses<-secClassesNames[is.element(secClassesNames,names(ret))]
	return(timeSeries(data.frame(ret[, retSecClasses]),charvec=ret[[1]]))
}

securityClasses.assetClassPeriodsReturns<-function(portRetHistory,weightsHistory,secClasses,secDtrs,periodsInYears,dtrSuffix){
# portRetHistory is a timeSeries() -- the history of returns of the portfolio; weightsHistory is a timeSeries() -- the history of weights of a portfolio; secClasses is a list() indexed by asset class name of securities belong to the class; secDtrs is a timeSeries() of dtrs; periodsInYears is an array of numbers (integers)
# Note: a negative number in periodsInYears is interpreted as a desire to get returns over the maximum period (between min and max dates of weightsHistory)
# returns data.frame() with columns start, end, and one column per asset class for the portfolio in weightsHistory
	
	portRetDates<-row.names(portRetHistory)
	if(sum(portRetDates!=row.names(weightsHistory))!=0){
		stop("error in securityClasses.assetClassPeriodsReturns(): port return history and weights history must have the same dates")
	}
	
	if(portRetDates[1]<portRetDates[length(portRetDates)]){
		stop("error in securityClasses.assetClassPeriodsReturns(): port return history must be in reverse chronological order")
	}
	# put in chronological order
	portRetHistory<-portRetHistory[nrow(portRetHistory):1,]
	# reset portfolio dates
	portRetDates<-row.names(portRetHistory)
	cumPortRets<-cumprod(1+as.numeric(portRetHistory[,"ret"]))

	# weightsHistory and secDtrs should be in reverse chronological order. it's checked in the following function
	secRets<-securityClasses.assetClassReturns(weightsHistory,secClasses,secDtrs,dtrSuffix)
	# must have dates in chronological order to use some utils and cumprod
	secRets<-secRets[nrow(secRets):1,]
	dates<-rownames(secRets)
	# calculate boundaries of all desired time periods
	startEndDatePairs<-NULL
	for(p in periodsInYears){
		if(p<0){
			startEndDatePairs<-data.frame(start=dates[1],end=dates[length(dates)])
		}else{
			startEndDatePairs<-rbind(startEndDatePairs,utils.getPeriodsInYears(dates,p,FALSE))
		}
	}
	
	# for every period, calculate the returns of every asset class over that period
	ret<-NULL
	ret2<-NULL
	if(!is.null(startEndDatePairs)){
		for(i in 1:nrow(startEndDatePairs)){
			startDate<-startEndDatePairs[["start"]][i]
			endDate<-startEndDatePairs[["end"]][i]
			acDateIncluded<-dates> startDate & dates <= endDate
			portDateIncluded<-portRetDates>= startDate & portRetDates < endDate
			
			periodCumRets<-as.numeric(cumPortRets[portDateIncluded])
			# so portfolio is worth 1 at time 1 (the origin of time) so return over period is also gain/loss
			periodCumRets<-periodCumRets/periodCumRets[1]
			# new row and first component is portfolio gain/loss in period
			newRow <-c(periodCumRets[length(periodCumRets)]-1)
			for(ac in names(secRets)){
				# at time t, the portfolio is worth periodCumRets[t] and the gain from asset class ac between t and t+1 is periodCumRets[t]*secRets[t+1,ac]. we sum all such gains during the period of interest.
				acContribution<-sum(periodCumRets*secRets[acDateIncluded,ac])
				newRow<-c(newRow,acContribution)
			}
	
			newDFRow<-data.frame(startDate , endDate ,matrix(newRow,nrow=1))
			names(newDFRow)<-c("start","end","portChange",names(secRets))
			ret<-rbind(ret, newDFRow)
		}
	}
	
	# return after reordering
	secClassesNames<-names(secClasses)
	retSecClasses<-secClassesNames[is.element(secClassesNames,names(ret))]
	return(ret[,c("start","end","portChange", retSecClasses)])
}
