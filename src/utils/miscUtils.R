utils.getConstraints<-function(inputDir,constraintFileName){
# this gets additional constraints we want to put on the portfolio optimization
	
	return(read.table(paste(inputDir,constraintFileName,sep=""),header=TRUE,sep=",",stringsAsFactors=FALSE,row.names=1))
}

utils.generateCrossIds<-function(listOfIds,listOfSuffixes){
# returns cartesian product of ids with suffixes appended
	
	if(length(listOfIds)==0){
		return(c())
	}
	
	if(length(listOfSuffixes)==0){
		stop("error in dataUtils: listOfSuffixes cannot be of length 0. If there are no suffixes, pass empty string or omit listOfSuffixes")
	}
	
	idsToGet<-c()
	for(i in 1:length(listOfSuffixes)){
		idsToGet<-c(idsToGet,paste(listOfIds,listOfSuffixes[i],sep=""))
	}
	
	return(idsToGet)
}

utils.generateIdsFromMapping<-function(listOfIds,suffixIdMapping){
# listOfIds is a vector of strings representing asset names; suffixIdMapping is a list() indexed by suffix, such as ".jer", etc., each item being a vector of strings that are asset names that are to take this suffix
# returns vector of strings that are asset names with the right suffixes
	
	# create expected return ids
	idsToGet<-c()
	for(suffix in names(suffixIdMapping)){
		takesThisSuffix<-is.element(listOfIds, suffixIdMapping[[suffix]])
		if(is.finite(match(TRUE,takesThisSuffix))){
			idsToGet <-c(idsToGet,paste(listOfIds[takesThisSuffix],suffix,sep=""))
		}
	}
	
	# now reorder all ids in the order of listOfIds
	ids<-c()
	for(a in listOfIds){
		inds<-grep(paste("^",a,"\\.",sep=""), idsToGet)
		if(length(inds)!=1){
			stop(paste("error in utils.generateIdsFromMapping(): no or more than one suffix was specified for asset",a))
		}
		ids<-c(ids, idsToGet[inds])
	}
	
	return(ids)
}

utils.getDataFromFile<-function(listOfIds,listOfSuffixes,dataFileName,dateFormat){
# this reads the data from a file; the file should contain a column called Date; if listOfSuffixes is NULL, it's assumed that listOfIds is the name of data to be retrieved; otherwise the data names are generated from listOfIds and listOfSuffixes
	
	# if listOfSuffixes is not NULL then we have to generate ids first
	if(!is.null(listOfSuffixes)){
		idsToGet<-utils.generateCrossIds(listOfIds,listOfSuffixes)
	} else {
		idsToGet<-listOfIds
	}
	
	if(length(idsToGet)==0){
		return(NULL)
	}
	
	if(grepl("\\.csv$",dataFileName)){
		# read the csv data file 
		allRateData<-read.csv(dataFileName,header=TRUE,stringsAsFactors=FALSE)
	}else if(grepl("\\.RData$",dataFileName)){
		# we load a saved data object
		allRateData<-as.data.frame(utils.load(dataFileName))
	}else{
		stop(paste("error in utils.getDataFromFile(): the data file format is not recognized. it has to be a .csv file or a .RData file. you passed",dataFileName))
	}
	
	requiredColumns<-c("Date",idsToGet)
	if(sum(is.element(requiredColumns,names(allRateData)))!=length(requiredColumns)){
		stop(paste("error in utils.getDataFromFile():", paste(requiredColumns[!is.element(requiredColumns,names(allRateData))],collapse=", "),"was not found in file",dataFileName))
	}else{
		rateData<-allRateData[requiredColumns]
	}
	
	rateData$Date<-as.Date(rateData$Date,format=dateFormat)
	return(timeSeries(rateData[setdiff(names(rateData),"Date")],charvec=rateData$Date))
}

utils.getDataMatrixFromFile<-function(listOfIds,listOfSuffixes,dataFileName,dateFormat){
# same as before but returns named matrix
	
	series<-utils.getDataFromFile(listOfIds,listOfSuffixes,dataFileName,dateFormat)
	if(is.null(series)){
		return(NULL)
	}
	ret<-as.matrix(series)
	rownames(ret)<-gsub("-","",row.names(series))
	colnames(ret)<-names(series)
	
	return(ret)
}

utils.getDataFromDB<-function(listOfIds,listOfSuffixes,dbSettings){
# get data from db; listOfIds is a vector of asset names; we append a suffix to each asset name and then submit the db request; if listOfSuffixes is NULL, it's assumed that listOfIds is the name of data to be retrieved; otherwise the data names are generated from listOfIds and listOfSuffixes
# returns a timeSeries object in reverse chronological order
	
	# if listOfSuffixes is not NULL then we have to generate ids first
	if(!is.null(listOfSuffixes)){
		idsToGet<-utils.generateCrossIds(listOfIds,listOfSuffixes)
	} else {
		idsToGet<-listOfIds
	}
	return(dbDataLoader.getDataList(idsToGet,dbSettings))
}

utils.getDataMatrixFromDB<-function(listOfIds,listOfSuffixes,dbSettings){
# same as utils.getDataFromDB() but returns named matrix
	
	series<-utils.getDataFromDB(listOfIds,listOfSuffixes,dbSettings)
	ret<-as.matrix(series)
	rownames(ret)<-gsub("-","",row.names(series))
	colnames(ret)<-names(series)
	
	return(ret)
}

utils.getYear<-function(dates){
# input: dates is a vector of dates as R Date objects or date strings, for example, c("2010-01-31", "2011-02-28")
# output: returns the years in dates as date numbers, for example, c(2010, 2011)
	
	dates<-as.character(dates)
	return(as.numeric(substr(dates,1,4)))
}

utils.isWeekend<-function(charDate){
# input: charDate is expected to be a date string, for example, "2011-07-04"; dateFormat is the format of the date but we give it a default value so it's optional
# output: returns TRUE if charDate falls on a weekend; FALSE otherwise
	
	# which day of the week does it fall on? convert it to a proper R date object first
	dayOfWeek<-weekdays(as.Date(charDate,format="%Y-%m-%d"))
	if(is.element(dayOfWeek,c("Saturday","Sunday"))){
		return(TRUE)
	}
	
	return(FALSE)
}

utils.checkForStartStub<-function(firstDateInMonth){
# input: firstDateInMonth is either a date string in the format "2011-07-04" or a R date object
# output: returns TRUE if we have a start stub; FALSE if we do not have a start stub
	
	firstDate<-as.character(firstDateInMonth)
	
	month<-as.numeric(strsplit(firstDate,"-")[[1]][2])
	year<-as.numeric(utils.getYear(firstDate))
	allDatesInMonth<-as.Date(timeCalendar(y=year,m=month,d=1:31))
	# if the month only had 30 or fewer days we remove the NA's next
	allDatesInMonth<-as.character(allDatesInMonth[is.finite(allDatesInMonth)])
	
	index<-match(firstDate,allDatesInMonth)
	if(!is.finite(index)){
		stop("error in utils.checkForStartStub(): can't find the date")
	} else if( index != 1 ){
		for (d in 1:(index-1)){
			if(!utils.isWeekend(allDatesInMonth[d])){
				return(TRUE)
			}
		}
	}
	
	return(FALSE);
}

utils.checkForEndStub<-function(lastDateInMonth){
# input: lastDateInMonth is either a date string in the format "2011-07-04" or a R date object
# returns TRUE if we have a start stub; FALSE if we do not have a start stub
	
	lastDate<-as.character(lastDateInMonth)
	
	month<-as.numeric(strsplit(lastDate,"-")[[1]][2])
	year<-as.numeric(utils.getYear(lastDate))
	allDatesInMonth<-as.Date(timeCalendar(y=year,m=month,d=1:31))
	# if the month only had 30 or fewer days we remove the NA's next
	allDatesInMonth<-as.character(allDatesInMonth[is.finite(allDatesInMonth)])
	
	index<-match(lastDate,allDatesInMonth)
	if(!is.finite(index)){
		stop("error in utils.checkForEndStub(): can't find the date")
	} else if( index != length(allDatesInMonth) ){
		for (d in (index+1):length(allDatesInMonth)){
			if(!utils.isWeekend(allDatesInMonth[d])){
				return(TRUE)
			}
		}
	}
	
	return(FALSE);
}

utils.getEOMDatesIndices<-function(dates,excludeStubs){
# input: dates is a vector of strings, dates, in REVERSE chronological order; excludeStubs determines if we exclude stubs
# output: returns the indices in the vector dates that correspond to end of month dates or NA if no such dates were left after stubs were excluded; excludeStubs specifies if stub months are to be removed from the return
	# get the months corresponding to those dates
	allMonthsInSeries<-months(as.Date(dates))
	
	# now, for each month, get the first date in the time series that fell in that month; yes, the first date, because the data is in reverse order
	monthsBlocks<-rle(allMonthsInSeries)$lengths
	# now some manipulations to get the dates we want
	numMonths<-length(monthsBlocks)
	#check if the last month is a stub
	endStub<-utils.checkForEndStub(dates[1])
	#check if the first month is a stub
	startStub<-utils.checkForStartStub(dates[length(dates)])
	if(numMonths>1){
		indices<-c(1,cumsum(monthsBlocks[1:(length(monthsBlocks)-1)])+1)
		if(excludeStubs){
			if(startStub){
				indices<-indices[1:(length(indices)-1)]
			}
			if(endStub && length(indices)>1){
				indices<-indices[2:length(indices)]
			} else if( endStub ){
				return(NA)
			}
		}
	} else {
		indices<-c(1)
		if(excludeStubs && ( endStub || startStub)){
			return(NA)
		}
	}
	
	return(indices)
}

utils.getSemiMonthlyDatesIndices<-function(dates, excludeStubs){
# input: dates is a vector of R Date objects or coercible in REVERSE chronological order; excludeStubs is passed to the EOM dates function
# output: the indices (NOT the dates) of the desired dates, or NA if no such dates
	
	dates<-as.Date(dates)
	# get the end of month dates
	eomIndices<-utils.getEOMDatesIndices(dates, excludeStubs)
	if(sum(is.finite(eomIndices))!=length(eomIndices)){
		eomIndices<-c()
	}
	
	splitFirstDate<-strsplit(as.character(dates[length(dates)]),"-",fixed=TRUE)[[1]]
	splitLastDate<-strsplit(as.character(dates[1]),"-",fixed=TRUE)[[1]]
	# generate a sequence of dates that fall on the 15th of the month, within the range of dates
	datesOn15th<-seq(as.Date(paste(splitLastDate[1],"-",splitLastDate[2],"-15",sep="")),as.Date(paste(splitFirstDate[1],"-",splitFirstDate[2],"-15",sep="")),by="-1 month")
	# make sure the last date in the sequence is not beyond the end date
	if(datesOn15th[1]>dates[1]){
		if(length(datesOn15th)<2){
			return(NA)
		}
		datesOn15th<-datesOn15th[2:length(datesOn15th)]
	}
	# make sure the first date in the sequence is not earlier than the first date
	if(datesOn15th[length(datesOn15th)]<dates[length(dates)]){
		if(length(datesOn15th)<2){
			return(NA)
		}
		datesOn15th<-datesOn15th[1:(length(datesOn15th)-1)]
	}
	
	# pick the indices of those dates that fall on the 15th
	datesInDataIndices<-(1:length(dates))[is.element(dates,datesOn15th)]
	datesNotInDataIndices<-(1:length(datesOn15th))[!is.element(datesOn15th,dates)]
	ret<-c(eomIndices,datesInDataIndices)
	# now for every 15th that is not in dates, find the date closest to that
	for(i in datesNotInDataIndices){
		lastEarlierDateInd<-match(FALSE,dates > datesOn15th[i])
		ret<-c(ret,lastEarlierDateInd)
	}
	
	# sort and return
	return(sort(unique(ret)))
}

utils.getWeeklyDatesIndices<-function(dates,weekDay){
# input: dates are strings; weekDay is a string, for example, "Friday"
# output: the indices of components of dates that fall on weekDay
	
	dates<-as.Date(dates)
	return((1:length(dates))[is.element(weekdays(dates),c(weekDay))])
}

utils.getWeeklyDates<-function(dates,weekDay){
# input: dates are strings; weekDay is a string, for example, "Friday"
# output: the subset of dates that fall on weekDay
	
	return(dates[utils.getWeeklyDatesIndices(dates,weekDay)])
}

utils.getWeeklyData<-function(tSeries,weekDay){
# input: tSeries is a timeSeries object; weekDay is one of ("Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday", "Sunday")
# output: returns the rows in tSeries that fall on the weekDay day of the week
	
	# get all the dates in the time series and convert to R date object
	allDatesInSeries<-as.Date(row.names(tSeries))
	# get the days on which those days fall
	allDaysInDates<-weekdays(allDatesInSeries)
	# now select those days that are equal to weekDay
	datesForWeekDay<-is.element(allDaysInDates,c(weekDay))
	
	# finally select the rows we want and return
	return(tSeries[datesForWeekDay,])
}

utils.getEOMData<-function(tSeries, excludeStubs){
# NOTE: the time series, tSeries is assumed to be in REVERSE chronological order
# for all the months for which tSeries has data, this returns the data for the latest date in those months
# the return object is also a timeSeries
	
	# get all the dates in the time series
	allDatesInSeries<-as.Date(row.names(tSeries))	
	# a simply simplistically simplistic test to make sure the series is in the right order
	if(allDatesInSeries[1] < allDatesInSeries[2]){
		stop("error in utils.getEOMData(): tSeries must be in REVERSE chronological order")
	}
	
	# get the indices of the dates falling at the end of the month
	eomIndices<-utils.getEOMDatesIndices(allDatesInSeries,excludeStubs= excludeStubs)
	if(sum(is.finite(eomIndices))==length(eomIndices)){
		return(tSeries[eomIndices,])
	} else {
		stop("error in utils.getEOMData(): no dates were left")
	}
}

utils.getSemiMonthlyData<-function(tSeries,excludeStubs){
# NOTE: the time series, tSeries is assumed to be in REVERSE chronological order
# for every month in the series find the EOM date and the date that is closest, but <=, the 15th of the month; excludeStubs is passed to the EOM function internally
	
	allDatesInSeries<-as.Date(row.names(tSeries))
	
	# a simply simplistically simplistic test to make sure the series is in the right order
	if(allDatesInSeries[1] < allDatesInSeries[2]){
		stop("error in utils.getEOMData(): tSeries must be in REVERSE chronological order")
	}
	
	# get the indices of the dates falling on the 15th (or the closest to that) or at the end of the month
	smIndices<-utils.getSemiMonthlyDatesIndices(allDatesInSeries,excludeStubs)
	if(sum(is.finite(smIndices))==length(smIndices)){
		return(tSeries[smIndices,])
	} else {
		stop("error in utils.getSemiMonthlyData(): didn't find any date that falls on the 15th of any month in the given range")
	}
	
}

utils.extractDates<-function(dates,freq,weekDay,excludeStubs){
# input: dates must R Date objects, freq see below, and weekDay is set to Friday by default
# output: returns the subset of dates at the frequency freq
	
	if(freq=="monthly"){
		return(dates[utils.getEOMDatesIndices(dates,excludeStubs)])
	} else if( freq=="weekly"){
		return(utils.getWeeklyDates(dates,weekDay))
	} else if( freq=="daily"){
		return(dates)
	} else if( freq=="semi-monthly"){
		return(dates[utils.getSemiMonthlyDatesIndices(dates,excludeStubs)])
	} else {
		stop(paste("error in utils.extractDates(): the frequency ",freq,"is not valid"))
	}
}

utils.truncateDates<-function(dates,startDate,endDate){
# input: dates is an array of date strings or R Date objects; startDate and endDate are expected in the same format as dates
# output: returns a sub-array of dates with those dates falling between startDate and endDate
	
	rDates<-as.Date(dates)
	sDate<-as.Date(startDate)
	eDate<-as.Date(endDate)
	
	return(dates[(rDates>=sDate) & (rDates <= endDate)])
}

utils.shiftToEndOfQuarter <- function ( objDates ){
	
	qs <- quarters(objDates)
	years <- utils.getYear(objDates)
	
	ret<-rep(NA, length(objDates))
	ret[ qs == "Q1"] <- paste(years[ qs=="Q1"],"-03-31",sep="")
	ret[ qs == "Q2"] <- paste(years[ qs=="Q2"],"-06-30",sep="")
	ret[ qs == "Q3"] <- paste(years[ qs=="Q3"],"-09-30",sep="")
	ret[ qs == "Q4"] <- paste(years[ qs=="Q4"],"-12-31",sep="")
	
	return( ret )
	
}

utils.getEOQDatesIndices <- function(dates){
# TODO utils.getQuarterData() should be made to call this for its date calculations. And: why do we have utils.getQuarterData() and utils.getEOQData()?
	
	# we need to convert to be able to call quarters()
	objDates <- as.Date(dates)
	
	# get the quarters corresponding to those dates
	allQuarters<-quarters(objDates)
	# now, for each quarter, get the last date in the time series that fell in that quarter
	lastDatesInQuarters<-rle(allQuarters)$lengths
	# now some manipulations to get the dates we want
	numQuarters<-length(lastDatesInQuarters)
	if(numQuarters>1){
		indices<-cumsum(lastDatesInQuarters[1:length(lastDatesInQuarters)])
	} else {
		indices<-c(1)
	}
	
	return(indices)
}

utils.getQuarterData<-function(tSeries){
# NOTE: the time series, tSeries is assumed to be in chronological order
# for all the Quarters for which tSeries has data, this returns the data for the latest date in those Quarters
# the return object is also a timeSeries
	
	# get all the dates in the time series
	allDatesInSeries<-as.Date(row.names(tSeries))
	
	# a simply simplistically simplistic test to make sure the series is in the right order
	if(allDatesInSeries[1] > allDatesInSeries[2]){
		stop("error in utils.getQuarterData(): tSeries must be in chronological order not reverse order")
	}
	
	# get the quarters corresponding to those dates
	allQuartersInSeries<-quarters(allDatesInSeries)
	# now, for each quarter, get the last date in the time series that fell in that quarter
	lastDatesInQuarters<-rle(allQuartersInSeries)$lengths
	# now some manipulations to get the dates we want
	numQuarters<-length(lastDatesInQuarters)
	if(numQuarters>1){
		indices<-cumsum(lastDatesInQuarters[1:length(lastDatesInQuarters)])
	} else {
		indices<-c(1)
	}
	
	# now select the rows we want and return
	return(tSeries[indices,])
}

utils.getEOQData<-function(tSeries){
# for all the Quarters for which tSeries has data, this returns the data for the latest date in those Quarters; Note: if the data is not in chronological order, this reverses it first and again before returning (might be costly)
# the return object is also a timeSeries
	
	# get all the dates in the time series
	allDatesInSeries<-as.Date(row.names(tSeries))
	
	# a simply simplistically simplistic test to test the series is in reverse order
	reverseIt<-FALSE
	if(allDatesInSeries[1] > allDatesInSeries[2]){
		reverseIt<-TRUE
	}
	
	# get the quarters corresponding to those dates
	allQuartersInSeries<-quarters(allDatesInSeries)
	# now, for each quarter, get the last date in the time series that fell in that quarter
	quarterBlocks<-rle(allQuartersInSeries)$lengths
	if(!reverseIt){
		indices<-cumsum(quarterBlocks)
	}else{
		indices<-c(1,cumsum(quarterBlocks[1:(length(quaterBlocks)-1)])+1)
	}
	
	# now select the rows we want and return
	return(tSeries[indices,])
}

utils.getEOYDatesIndices <- function(dates, excludeStubs){
# TODO this duplicates logic in utils.getYearData(). we should modify utils.getYearData() to call this function to get the indices and then return the sub-series. didn't do it on 6/9/12 because didn't want to test all the dependencies
	
	# a simply simplistically simplistic test to make sure the series is in the right order
	if(dates[1] > dates[2]){
		stop("error in utils.getYearData(): dates must be in chronological order not reverse order")
	}
	
	# get the years corresponding to those dates
	allYearsInDates<-utils.getYear(dates)
	# now, for each year, get the last date in the time series that fell in that year
	yearBlocks<-rle(allYearsInDates)$lengths
	# now some manipulations to get the dates we want
	numYears<-length(yearBlocks)
	indices<-cumsum(yearBlocks)
	
	# Note: has this arbitrary 26 whose presence marks a complete month. saves us from calculating holidays/week-ends
	isWholeLastYear<-months(as.Date(dates[length(dates)])) =="December" && as.numeric(substr(dates[length(dates)],9,10)) >= 26
	if(!isWholeLastYear && excludeStubs ){
		if(length(indices)>=2){
			indices<-indices[1:(length(indices)-1)]
		}else{
			return(NULL)
		}
	}
	
	return(indices)
}

utils.getYearData<-function(tSeries){
# NOTE: the time series, tSeries is assumed to be in chronological order
# for all the Quarters for which tSeries has data, this returns the data for the latest date in those Quarters
# the return object is also a timeSeries
	
	# get all the dates in the time series
	allDatesInSeries<-as.Date(row.names(tSeries))
	
	# a simply simplistically simplistic test to make sure the series is in the right order
	if(allDatesInSeries[1] > allDatesInSeries[2]){
		stop("error in utils.getYearData(): tSeries must be in chronological order not reverse order")
	}
	
	# get the years corresponding to those dates
	allYearsInSeries<-utils.getYear(allDatesInSeries)
	# now, for each year, get the last date in the time series that fell in that year
	yearBlocks<-rle(allYearsInSeries)$lengths
	# now some manipulations to get the dates we want
	numYears<-length(yearBlocks)
	indices<-cumsum(yearBlocks)
	
	isWholeLastYear<-months(allDatesInSeries[length(allDatesInSeries)])=="December" && as.numeric(substr(row.names(tSeries)[nrow(tSeries)],9,10)) >= 26
	if(!isWholeLastYear){
		if(length(indices)>=2){
			indices<-indices[1:(length(indices)-1)]
		}else{
			return(NULL)
		}
	} 
	
	# now select the rows we want and return
	return(tSeries[indices,])
}

utils.ccyMap<-function(countryCodes){
# countryCodes is a vector of country codes; read code below for countries currently recognized
# returns a vector with corresponding currency codes
	
	ret <- c()
	for(i in 1:length(countryCodes)){
		if(countryCodes[i]=="us"){
			ret<-c(ret,"usd")
		} else if (countryCodes[i] == "uk"){
			ret<-c(ret, "gbp")
		} else if (countryCodes[i] == "ca") {
			ret<-c(ret, "cad")
		} else if (countryCodes[i] == "jp") {
			ret<-c(ret, "jpy")
		} else if (countryCodes[i] == "eu") {
			ret<-c(ret, "eur")
		} else if (countryCodes[i] == "au" ) {
			ret<-c(ret, "aud")		
		} else if (countryCodes[i] == "sw" ) {
			ret<-c(ret, "sek")
		} else if (countryCodes[i] == "sz" ) {
			ret<-c(ret, "chf")
		} else if (countryCodes[i] == "ge" ) {
			ret<-c(ret, "dem")
		} else if (countryCodes[i] == "fr" ) {
			ret<-c(ret, "frf")
		} else if (countryCodes[i] == "it" ) {
			ret<-c(ret, "itl")
		} else if (countryCodes[i] == "sp" ) {
			ret<-c(ret, "esp")
		} else if (countryCodes[i] == "nl" ) {
			ret<-c(ret, "nlg")
		} else if (countryCodes[i] == "sg" ) {
			ret<-c(ret, "sgd")
		} else if (countryCodes[i] == "hk" ) {
			ret<-c(ret, "hkd")
		} else if (countryCodes[i] == "no" ) {
			ret<-c(ret, "nok")
		} else if (countryCodes[i] == "dk" ) {
			ret<-c(ret, "dkk")
		} else if (countryCodes[i] == "fn" ) {
			ret<-c(ret, "fim")
		} else if (countryCodes[i] == "sa" ) {
			ret<-c(ret, "zar")
		} else if (countryCodes[i] == "tw" ) {
			ret<-c(ret, "twd")
		} else if (countryCodes[i] == "ko" ) {
			ret<-c(ret, "krw")
		} else if (countryCodes[i] == "nz" ) {
			ret<-c(ret, "nzd")			
		} else if (countryCodes[i] == "mx" ) {
			ret<-c(ret, "mxn")	
		} else if (countryCodes[i] == "id" ) {
			ret<-c(ret, "idr")	
		} else if (countryCodes[i] == "my" ) {
			ret<-c(ret, "myr")	
		} else if (countryCodes[i] == "th" ) {
			ret<-c(ret, "thb")	
		} else if (countryCodes[i] == "in" ) {
			ret<-c(ret, "inr")	
		} else if (countryCodes[i] == "ch" ) {
			ret<-c(ret, "cny")	
		} else if (countryCodes[i] == "pl" ) {
			ret<-c(ret, "pln")				
		} else if (countryCodes[i] == "bz" ) {
			ret<-c(ret, "brl")		
		} else if (countryCodes[i] == "co" ) {
			ret<-c(ret, "cop")		
		} else if (countryCodes[i] == "cl" ) {
			ret<-c(ret, "clp")		
		} else if (countryCodes[i] == "tk" ) {
			ret<-c(ret, "trl")		
		} else if (countryCodes[i] == "ph" ) {
			ret<-c(ret, "php")		
		} else if (countryCodes[i] == "ch" ) {
			ret<-c(ret, "cny")		
		} else if (countryCodes[i] == "pk" ) {
			ret<-c(ret, "pkr")		
		} else if (countryCodes[i] == "ru" ) {
			ret<-c(ret, "rub")		
		} else if (countryCodes[i] == "vn" ) {
			ret<-c(ret, "vnd")		
		} else if (countryCodes[i] == "il" ) {
			ret<-c(ret, "ils")	
		} else if (countryCodes[i] == "sr" ) {
			ret<-c(ret, "sar")	
		} else if (countryCodes[i] == "kw" ) {
			ret<-c(ret, "kwd")	
		} else if (countryCodes[i] == "ae" ) {
			ret<-c(ret, "aed")	
		} else if (countryCodes[i] == "bh" ) {
			ret<-c(ret, "bhd")	
		} else if (countryCodes[i] == "qa" ) {
			ret<-c(ret, "qar")	
		} else if (countryCodes[i] == "om" ) {
			ret<-c(ret, "omr")	
		} else if (countryCodes[i] == "hu" ) {
			ret<-c(ret, "huf")	
		} else if (countryCodes[i] == "bg" ) {
			ret<-c(ret, "bgn")	
		} else if (countryCodes[i] == "cz" ) {
			ret<-c(ret, "czk")	
		} else if (countryCodes[i] == "ro" ) {
			ret<-c(ret, "ron")	
			} else {
			stop(paste("error in utils.ccyMap(): don't have currency for country code ",countryCodes[i]))
		}
	}
	
	return( ret )
}

utils.align<-function(dfList,decreasing){
# dfList is a list of data frames; each must have a column named "Date". decreasing specifies if we want increasing (most recent last) or decreasing dates (most recent first)
# returns a data frame with a column named Date that is the union of all Date columns and columns containing the other data in the data frames mapped to the right date
	
	# find all the dates that appear in any of the data frames in the list
	dates<-NULL
	first<-TRUE
	numCols<-0
	for(i in 1:length(dfList)){
		if(!is.null(dfList[[i]])){
			thisDF<-as.data.frame(dfList[[i]])
			dateInd<-match("Date",names(thisDF))
			if(!is.finite(dateInd)){
				stop(c("error in utils.align(): the ",i,"th item in the list doesn't have a column called Date. the columns are ",paste(names(thisDF),collapse=", ")))
			}
			# now check if this data.frame() is decreasing or increasing
			if(!first){
				argDatesAgree<- ifelse(decreasing, thisDF$Date[1] > thisDF$Date[2], thisDF$Date[1] < thisDF$Date[2])
				error<- (argDatesAgree + argDatesAgree1) %% 2
				if(error==1){
					stop("error in utils.align(): data.frame's in the list should all have increasing dates or decreasing dates. no mixing the two kinds.")
				}
			}else{
				first<-FALSE
				argDatesAgree1<- ifelse(decreasing, thisDF$Date[1] > thisDF$Date[2], thisDF$Date[1] < thisDF$Date[2])
			}
			
			dates<-union(dates,as.character(thisDF$Date))
			# we subtract one to avoid counting the "Date" column
			numCols<-numCols+ncol(thisDF)-1
		}
	}
	
	if(is.null(dates)){
		return(NULL)
	}
	
	# argDatesAgree1 is FALSE if decreasing is TRUE and the data.frame()'s in the list are in chronological order, or decreasing is FALSE and the data.frame()'s in the list are in reverse order
	dates<-sort(dates,decreasing=decreasing)
	# initialize the return data frame
	collapsedDF<-matrix(nrow=length(dates),ncol=numCols)
	colnames(collapsedDF)<-rep("",ncol(collapsedDF))
	colCounter<-1
	for(i in 1:length(dfList)){
		if(!is.null(dfList[[i]])){
			dfDates<-dfList[[i]][["Date"]]
			dfNames<-setdiff(names(dfList[[i]]),"Date")
			thisDF<-as.matrix(dfList[[i]][, dfNames,drop=FALSE])
			if(!argDatesAgree1){
				thisDF<-thisDF[nrow(thisDF):1,,drop=FALSE]
			}			
			collapsedDF[is.element(dates,dfDates),colCounter:(colCounter+ncol(thisDF)-1)]<-thisDF
			colnames(collapsedDF)[colCounter:(colCounter+ncol(thisDF)-1)]<-dfNames
			colCounter<-colCounter+ncol(thisDF)
		}
	}
	
	return(cbind(data.frame(Date=dates),data.frame(collapsedDF)))
}

utils.alignSeries<-function(seriesList,decreasing){
# same as utils.align() except that seriesList is a list of series -- not data.frame()'s, and...
# returns timeSeries() not data.frame()
	
	# first create list of data frames from seriesList so we can reuse code in utils.align()
	dfList<-NULL
	for(i in 1:length(seriesList)){
		ss<-seriesList[[i]]
		if(ncol(ss)*nrow(ss)!=0){
			df<-data.frame(Date=row.names(ss),data.frame(ss,row.names=NULL))
			dfList<-append(dfList,list(df))
			names(dfList)[length(dfList)]<-names(seriesList)[i]
		}
	}
	
	# utils.align() now can handle it
	alignedDF<-utils.align(dfList,decreasing)
	# NOTE: recall that output from utils.align() can have different columns with same names; so setdiff(names(alignedDF),"Date") wouldn't work in the next statement because it would return unique names that are different to "Date"
	dateInd<-match("Date",names(alignedDF))
	return(timeSeries(alignedDF[setdiff(1:ncol(alignedDF),dateInd)],charvec=alignedDF$Date))
}

utils.alignMatrices<-function(matrixList, decreasing){
# assumes matrices in list() matrixList have dates as their rownames()
# returns matrix()
	
	# first create list of data frames from seriesList so we can reuse code in utils.align()
	dfList<-NULL
	for(i in 1:length(matrixList)){
		mat<-matrixList[[i]]
		if(ncol(mat)*nrow(mat)!=0){
			df<-data.frame(Date=rownames(mat),data.frame(mat,row.names=NULL))
			dfList<-append(dfList,list(df))
			names(dfList)[length(dfList)]<-names(matrixList)[i]
		}
	}
	
	# utils.align() now can handle it
	alignedDF<-utils.align(dfList,decreasing)
	# NOTE: recall that output from utils.align() can have different columns with same names; so setdiff(names(alignedDF),"Date") wouldn't work in the next statement because it would return unique names that are different to "Date"
	ret<-as.matrix(alignedDF[,setdiff(names(alignedDF),"Date")])
	rownames(ret)<-alignedDF[,"Date"]
	
	return(ret)
}

utils.fillGapsSeries <- function(series,decreasing){
# if there's a gap in 1 column but not in some other column corresponding to the same period, fill it
# series is a timeSeries(); decreasing is logical, TRUE if series is in reverse chronological order
# returns timeSeries() with the gaps filled. NOTE: returned series has dates only for the shortest span over which some column has data. in other words, returned timeSeries() doesn't necessarily have the same dates as input series
	
	# if the series is not in reverse chronological order, reverse it
	if(!decreasing){
		series<-series[nrow(series):1,]
	}
	
	dates<-row.names(series)
	isNotNA0<-rep(TRUE,nrow(series))
	for(i in 1:ncol(series)){
		isNotNA<-as.logical(is.finite(series[,i]))
		allTrues<-grep(TRUE,isNotNA)
		
		logicalBlocks<-rle(isNotNA)
		# indices to the beginning of blocks
		blockInds<-c(1,1+cumsum(logicalBlocks$lengths))
		# the last index should be beyond the end
		blockInds<-blockInds[1:(length(blockInds)-1)]
		# blocks of TRUE start from these indices
		trueInds<-blockInds[logicalBlocks$values==TRUE]
		# blocks of FALSE start from these indices
		falseInds<-blockInds[logicalBlocks$values==FALSE]
		# no sense keeping bottom NA values. remove them
		falseInds<-falseInds[falseInds<trueInds[length(trueInds)]]
		
		# now fill gaps
		for(j in falseInds){
			# the higher the index, the earlier the date. search here for the start of the first block of data after the gap
			ind<-match(TRUE,trueInds>j)
			# it should never happen that ind=NA because we start from the earliest date for which ALL columns have data. trueInds[ind] is also necessarily greater than 1 since it's larger than j which is at least 1. now fill gap with the latest, but prior available data point
			series[j:(trueInds[ind]-1),i]<-series[trueInds[ind],i]
		}
		
		# keep track of non-NA's across the series because we want to truncate it later
		isNotNA0<-isNotNA0 & as.logical(is.finite(series[,i]))
	}
	
	# truncate series
	series<-series[isNotNA0,]
	
	if(!decreasing){
		series<-series[nrow(series):1,]
	}
	
	return(series)
}

utils.getPeriodsInYears<-function(dates,numberOfYearsInPeriods,endAdjusted){
# dates is a vector of sorted dates in chronological order; numberOfYearsInPeriods an integer, must be multiple of 5; endAdjusted is logical, TRUE means the last complete year in dates that is a multiple of 5 must be included
# returns data frame with 2 columns, start and end, for the beginnings and ends of the periods of numberOfYearsInPeriod in dates; or NULL if dates runs over less than numberOfYearsInPeriods; the years that start a period are multiples of numberOfYearsInPeriods
# NOTE: if we don't have the full first year in a period, for example a decade, or the full last year, we don't throw that period out; but, we do report the start and end dates of the period so we would know it when this happens
	
	if(as.Date(dates[1]) > as.Date(dates[2])){
		stop("ERROR in utils.getPeriodsInYears(): dates must be in chronological order not reverse order")
	}
	
	if(numberOfYearsInPeriods %% 5 !=0){
		stop(paste("ERROR in utils.getPeriodsInYears(): numberOfYearsInPeriods must be multiple of 5. you passed",numberOfYearsInPeriods))
	}
	
	years<-utils.getYear(dates)
	ratio5<-numberOfYearsInPeriods/5
	# get those years that mark the start of periods
	periodStarts<-unique(years[years%%5 ==0])
	if(numberOfYearsInPeriods != 5){
		if(!endAdjusted){
			inds<-(1:length(periodStarts)) %% ratio5 == 1
		}else{
			inds<-(1:length(periodStarts)) %% ratio5 == length(periodStarts) %% ratio5
		}
		periodStarts<-periodStarts[inds]
	}
	if(length(periodStarts)<2){
		# can't do anything if we have less than a period-worth of dates
		return(NULL)
	} else{
		ret<-NULL
		for(i in 1:(length(periodStarts)-1)){
			# I am safely assuming here that all years between period starts are in the data; i.e., the history represented by dates doesn't have gaps in years; that's why for the end date I just subtract 1 from the next period's start without further checks
			firstDateIndex<-match(periodStarts[i],years)
			firstDateIndex<-ifelse(firstDateIndex>1,firstDateIndex-1,firstDateIndex)
			ret<-rbind(ret,data.frame(start=dates[firstDateIndex],end=dates[match(periodStarts[i+1],years)-1]))
		}
	}
	return(ret)
}

utils.getYearQuarterPairs<-function(dates,excludeStubs){
# dates is a vector of dates; excludeStubs tells us whether to include the first quarter and the last quarter if parts of any is missing from dates
# NOTE: we use a very CRUDE way to check if we have the whole quarter; see code below
# bucket the dates in dates into a list indexed by "year-quarter"
	
	if(length(dates)<2){
		if(excludeStubs) return(list()) # quarter certainly not full here so exclude it, so we're left with nothing
		datesInReverse<-FALSE
	}else{
		# we want dates in ascending order. reverse them if not and reverse again when returning below. we need to know we have at least 2 dates before we make the next comparison, this is why we are in the else block
		datesInReverse<-as.Date(dates[1])>as.Date(dates[2])
	}
	if(datesInReverse) dates<-rev(dates)
	
	years<-utils.getYear(dates)
	qs<-quarters(as.Date(dates))
	# form years-quarters pairs
	yqPairNames<-paste(years,"-",qs,sep="")
	yqPairs<-unique(yqPairNames)
	
	if(excludeStubs){
		startingMonth<-(as.numeric(substr(qs[1],2,2))-1)*3+1
		# if the first date in the first quarter falls after the 5th of the first month in the quarter, throw the quarter out
		if(as.Date(dates[1])>as.Date(paste(years[1],"-",ifelse(startingMonth < 10,"0",""),startingMonth,"-05",sep=""))){
			if(length(yqPairs)>1){
				yqPairs<-yqPairs[2:length(yqPairs)]
			} else {
				return(list())
			}
		}
		
		endingMonth<-(as.numeric(substr(qs[length(qs)],2,2))-1)*3+2
		# if the last date in the last quarter falls before the 26th of the last month in the quarter, throw the quarter out
		if(as.Date(dates[length(dates)])<as.Date(paste(years[length(years)],"-",ifelse(endingMonth < 10,"0",""), endingMonth,"-26",sep=""))){
			if(length(yqPairs)>1){
				yqPairs<-yqPairs[1:(length(yqPairs)-1)]
			} else {
				return(list())
			}
		}
	}
	
	ret<-NULL
	yqPairNamesRuns<-rle(yqPairNames)
	yqPairEndOfBlocks<-cumsum(yqPairNamesRuns$lengths)
	# now for every unique year-quarter pair, assign the dates that fall in that quarter
	for(yq in yqPairs){
		ind<-match(yq, yqPairNamesRuns$values)
		datesStartInd<-ifelse(ind==1,1, yqPairEndOfBlocks[ind-1]+1)
		datesEndInd<-yqPairEndOfBlocks[ind]
		yqDates<-dates[datesStartInd:datesEndInd]
		if(datesInReverse){
			yqDates<-rev(yqDates)
		}
		ret<-append(ret,list(newItem= yqDates))
		names(ret)[length(ret)]<-yq
	}
	
	if(datesInReverse){
		# reverse the order of the quarters to make them descending
		ret<-rev(ret)
	}
	
	return(ret)
}

utils.robustXPlusAlpha<-function(x,y,maximumNumberOfIterations){
# this function does robust linear regression of y on x; maximumNumberOfIterations is the maximum we allow rlm() to do
# returns alpha such that ||y-(x+alpha)||_2 is minimal
	
	##return(as.numeric(rlm(matrix(rep(1,length(x)),ncol=1),y-x,maxit=maximumNumberOfIterations)$coefficients))
	return(lm.fit(matrix(rep(1,length(x)),ncol=1),y-x)$coefficients)
}

utils.zscore<-function(indicators){
# for each column in indicators, calculate the zscore for each date relative to the mean and sd up to date-1
# indicators is a data frame with one column called Date and columns with indicator values whose zscores are sought
# returns data frame with columns: $Date and columns containing the zscores for each date
	
	dateColInd<-match("Date",names(indicators))
	if(!is.finite(dateColInd)){
		stop("error in utils.zscore(): indicators data frame must have a Date column")
	}
	
	if(indicators$Date[1]>indicators$Date[2]){
		stop("error in utils.zscore(): indicators should in chronological order not reverse chronological")
	}
	
	# initialize return data frame
	ret<-data.frame(Date=indicators$Date)
	for(j in setdiff(1:ncol(indicators),dateColInd)){
		# initialize new column
		newCol<-rep(NA,nrow(indicators))
		nonNAs<-is.finite(indicators[,j])
		nonNAIndicators<-indicators[nonNAs,j]
		# calculate a cumulative sample mean
		means<-algos.cumSampleMean(nonNAIndicators,TRUE)
		# we can't calculate with fewer than 3 values
		if(length(nonNAIndicators)>2){
			zs<-c(NA,NA)
			for(i in 3:length(nonNAIndicators)){
				# use the mean and sd up to i-1; this uses sample (not population) mean and sd
				temp<-nonNAIndicators[1:(i-1)]-means[i-1]
				# ACHTUNG: if sd is small or indicators[,j] is plain constant up to i-1 the z-score blows up
				zs<-c(zs, (nonNAIndicators[i]-means[i-1])/sqrt(sum(temp^2)/(i-1)))
			}
			newCol[nonNAs]<-zs
		}
		# bind to return data frame
		ret<-cbind(ret,data.frame(newCol=newCol))
		names(ret)[ncol(ret)]<-names(indicators)[j]
	}
	
	return(ret)
}

utils.getDataForDates<-function(dates,tSeries,tSeriesDates){
# for each date in dates, extract from tSeries the last data available before that date
# dates is a vector of dates as strings in the format "%Y-%m-%d"; tSeries is a vector of numerical values; tSeriesDates is a vector of Date objects of length equal to rows in tSeries. Note: we require tSeriesDates separately for performance reasons; the alternative is to require tSeries to have date info and to convert those dates to Date objects at every call, but this gets quite expensive after repeated calls. Another Note: the output can duplicate data if the same data was the latest available for more than 1 date. This would be problematic if the intention is to take means of data returned, say.
# returns vector containing the required data
	
	return(tSeries[algos.mesh(dates,tSeriesDates)])
}

utils.is.finite<-function(v,naAtEnd){
# check that NA's form one block at the beginning or end of the vector
# v is a vector; naAtEnd is logical: if TRUE, the NA's, if any, should form one block at the end of the vector
# returns a logical vector like is.finite() or stop()'s if NA's are intermixed with non-NA's
	
	nonNAs<-as.logical(is.finite(v))
	if(naAtEnd){
		naInd<-match(FALSE,nonNAs)
		if(is.finite(naInd)){
			if(naInd!=sum(nonNAs)+1){
				stop("error in utils.is.finite(): NA's are not at the end of the vector only")
			}
		}
	}else{
		naInd<-match(TRUE, nonNAs)
		if(is.finite(naInd)){
			if(length(v)-sum(nonNAs)+1!=naInd){
				stop("error in utils.is.finite(): NA's are not at the beginning of the vector only")
			}
		}
	}
	
	return(nonNAs)
}

utils.extractTrue<-function(nonNAs){
# we use this when the a vector has NA's at the beginning and the end.
# nonNAs is a vector of TRUE/FALSE
# returns vector of TRUE/FALSE of same length, where all values, before the first TRUE, and after the first FALSE that comes after the first TRUE, in nonNAs, are FALSE
	
	# look for the very first TRUE
	firstTrueInd<-match(TRUE,nonNAs)
	if(is.finite(firstTrueInd)){
		# now look for the first FALSE that comes after that
		firstFalseAfterTrueInd<-match(FALSE,nonNAs[firstTrueInd:length(nonNAs)])
		if(is.finite(firstFalseAfterTrueInd)){
			if(firstTrueInd!=1){
				nonNAs<-c(rep(FALSE,firstTrueInd-1),nonNAs[firstTrueInd:(firstTrueInd+firstFalseAfterTrueInd-1)],rep(FALSE,length(nonNAs)-firstTrueInd-firstFalseAfterTrueInd+1))
			}else{
				nonNAs<-c(nonNAs[firstTrueInd:(firstTrueInd+firstFalseAfterTrueInd-1)],rep(FALSE,length(nonNAs)-firstTrueInd-firstFalseAfterTrueInd+1))
			}
		}else{
			if(firstTrueInd!=1){
				nonNAs<-c(rep(FALSE,firstTrueInd-1),nonNAs[firstTrueInd:length(nonNAs)])
			}
		}
	}
	
	return(nonNAs)
}

utils.assetsByClass<-function(assets,identifiersList){
# assets is a vector; identifiers is a list() indexed by asset class and whose elements are vectors of strings identifying classes, for example, reit=".property"; Note: "equity" is not expected to be identified by any patterns
# returns list indexed by 4 asset classes: equity, bond, reit, commodity
	
	if(is.finite(match("equity",names(identifiersList)))){
		stop("error in utils.assetsByClass(): the equity asset class is not expected to have any identifiers.")
	}
	
	ret<-NULL
	allInds<-c()
	# loop on index classes
	for(i in 1:length(identifiersList)){
		assetClassIdentifiers<-identifiersList[[i]]
		inds<-c()
		# loop on the identifying strings for this asset class
		for(j in 1:length(assetClassIdentifiers)){
			inds<-c(inds,grep(assetClassIdentifiers[j],assets))
		}
		ret<-append(ret,list(newClass=assets[inds]))
		names(ret)[length(ret)]<-names(identifiersList)[i]
		allInds<-c(allInds,inds)
	}
	
	# what wasn't identified is equity
	return(append(list(equity=assets[setdiff(1:length(assets),allInds)]),ret))
}

utils.cumprodMatrixPlus1<-function(mat){
# cumulative product of 1+column, for all columns of a matrix()
# mat is a matrix()
# returns matrix()

	dates<-rownames(mat)
	
	# check to see if we have to reverse mat to make in chronological order
	reversed<-FALSE
	if(dates[1]> dates[length(dates)]){
		#  mat is in reverse order
		mat<-mat[nrow(mat):1,]
		reversed<-TRUE
	}
	
	mat[!is.finite(mat)]<-0
	# get cumprod() for each column in mat
	for(i in 1:ncol(mat)){
		mat[,i]<-cumprod(1+mat[,i])
	}
	
	# reverse again if we had reversed earlier
	if(reversed){
		mat <- mat[nrow(mat):1,]
	}
	
	return(mat)
}

utils.offsetYears<-function(dates,targetDates,numYears){
# dates is an array of sorted integers representing dates in the "%Y%m%d" format; same thing for targetDates; numYears is a numbers
# returns vector of integers representing INDICES, such that, for all i, targetDates[ret[i]] == dates[i]+numYears. if dates[i]+numYears > max(targetDates), then ret[i] is NA
	
	reversed<-FALSE
	if(dates[1]> dates[length(dates)]){
		if(targetDates[length(targetDates)] > targetDates[1]){
			stop("error in utils.offsetYears(): dates and targetDates must be in the same order.")
		}
		dates<-rev(dates)
		# we could call this function with dates=targetDates . this doesn't hurt the following since R calls by value
		targetDates<-rev(targetDates)
		reversed<-TRUE
	}else if((dates[1] < dates[length(dates)]) && (targetDates[1] > targetDates[length(dates)])){
		stop("error in utils.offsetYears(): dates and targetDates must be in the same order.")
	}
	
	# dates and targetDates are in chronological order from this point
	offsetDates<-dates+numYears*10000
	ret<-rep(NA,length(dates))
	if(numYears>0){
		ind<-1
		for(i in 1:length(offsetDates)){
			# ind will hold the index of the first date >= offsetDates[i-1]. the first date >= offsetDates[i] should have an index of at least ind+1
			ind<-match(TRUE, targetDates[ind:length(targetDates)]>=offsetDates[i]) + ind -1
			if(!is.finite(ind)){
				break
			}
			ret[i]<-ind
		}
	}else if(numYears<0){
		ind<-length(targetDates)
		for(i in length(offsetDates):1){
			# ind will hold the index of the first date >= offsetDates[i-1]. the first date >= offsetDates[i] should have an index of at least ind+1
			newInd<-match(FALSE, targetDates[1:ind]<=offsetDates[i]) -1
			if(is.finite(newInd) && newInd >=1){
				ret[i]<-newInd
				ind<-newInd
			}else if(!is.finite(newInd) && targetDates[ind]<=offsetDates[i]){
				ret[i]<-ind
			}else{
				break
			}
		}
	}
	
	if(reversed){
		ret<-rev(ret)
	}
	
	return(ret)
}	

utils.annualizedRetRiskMatrix<-function(mat,numYears,sdScale,whichWay){
# mat is a named matrix of returns (not levels) with rownames() of dates in the "%Y%m%d" format; numYears is a number, the length of the period over which we want to calculate the annualized rate; sdScale is a number used to annualize vol, for example, sqrt(business_days_per_year); whichWay is a string, "forward", "backward" or "both". "forward" means stamp the returned annualized rates with the dates of the beginnings of the relevant period, "backward" means stamp with the dates of the ends. "both" means do "forward" and "backward"
# returns a list() indexed by one or both of "forward" and "backward" of named matrices of annualized rates with dates in rownames()
	
	if(numYears<0){
		stop(paste("ERROR in utils.annualizedRetRiskMatrix(): numYears must be positive. if you want to go backward set whichWay to backward or both. you passed",numYears))
	}
	
	charDates<-rownames(mat)
	reversed<-FALSE
	if(charDates[1]>charDates[length(charDates)]){
		mat<-mat[nrow(mat):1,]
		reversed<-TRUE
	}
	
	# calculate cumulative gross returns for all securities
	cumRets<-utils.cumprodMatrixPlus1(mat)		
	matDates<-as.numeric(rownames(mat))
	numAssets<-ncol(mat)
	assetNames<-colnames(mat)
	columnNames<-c(paste(assetNames,".irr",sep=""),paste(assetNames,".sdev",sep=""))
	
	ret<-NULL
	
	if(whichWay=="both" || whichWay=="forward"){
		# forwardOffsetInds is such that: matDates[forwardOffsetInds[i]]=matDates[i]+numYears, or NA, if no such date exists
		forwardOffsetInds<-utils.offsetYears(matDates,matDates,numYears)
		# keep the non-NA's only
		forwardOffsetInds <-forwardOffsetInds[is.finite(forwardOffsetInds)]
		numOffsetInds<-length(forwardOffsetInds)
		forwardRet <-matrix(data=NA,nrow= numOffsetInds,ncol=2*numAssets)
		
		# because we replace NA's with 0's utils.cumprodMatrixPlus1(), cumRets will have 1 where mat had NA
		forwardRet[,1:numAssets]<-(cumRets[forwardOffsetInds,]/cumRets[1:numOffsetInds,])^(1/numYears)-1	
		riskColumnInds<-(numAssets+1):ncol(forwardRet)
		for(i in 1:numOffsetInds){
			# annualized volatility of (typically, daily) returns
			forwardRet[i, riskColumnInds]<-apply(mat[i:forwardOffsetInds[i],],2,sd)* sdScale
		}
		
		# annualized return/risk should be stamped with the date of the beginning of the period
		rownames(forwardRet)<-matDates[1:numOffsetInds]
		colnames(forwardRet)<-columnNames
		if(reversed){
			forwardRet <-forwardRet[nrow(forwardRet):1,]
		}
		ret<-append(ret,list(forward=forwardRet))
	}
	
	if(whichWay=="both" || whichWay=="backward"){
		# backwardOffsetInds is such that: matDates[backwardOffsetInds[i]]=matDates[nrow(matDates)-length(backwardOffsetInds)+i]-numYears, or NA, if no such date exists
		backwardOffsetInds<-utils.offsetYears(matDates,matDates,-numYears)
		# keep the non-NA's only
		backwardOffsetInds <-backwardOffsetInds[is.finite(backwardOffsetInds)]
		numOffsetInds<-length(backwardOffsetInds)
		numRows<-nrow(mat) # == nrow(cumRets)
		backwardRet <-matrix(data=NA,nrow= numOffsetInds,ncol=2*numAssets)
		
		# because we replace NA's with 0's utils.cumprodMatrixPlus1(), cumRets will have 1 where mat had NA
		backwardRet[,1:numAssets]<-(cumRets[(numRows-numOffsetInds +1):numRows,]/cumRets[backwardOffsetInds,])^(1/numYears)-1	
		riskColumnInds<-(numAssets+1):ncol(backwardRet)
		for(i in 1:numOffsetInds){
			# annualized volatility of (typically, daily) returns
			backwardRet[i, riskColumnInds]<-apply(mat[backwardOffsetInds[i]:(numRows-numOffsetInds+i),],2,sd)* sdScale
		}
		
		# annualized return/risk should be stamped with the date of the beginning of the period
		rownames(backwardRet)<-matDates[(numRows-numOffsetInds +1):numRows]
		colnames(backwardRet)<-columnNames
		if(reversed){
			backwardRet <-backwardRet[nrow(backwardRet):1,]
		}
		ret<-append(ret,list(backward= backwardRet))
	}
	
	return(ret)
}

utils.ymdToHyphens<-function(dates){
# takes dates in the %Y%m%d format and returns dates in the %Y-%m-%d format. dates is an array of integers and return value is array of characters
	
	return(paste(substr(dates,1,4), substr(dates,5,6), substr(dates,7,8), sep="-"))
}

utils.hyphensToYmd<-function(dates){
# takes dates in the %Y-%m-%d format and returns dates in the %Y%m%d format. dates is an array of strings and return value is array of integers
	
	return(as.numeric(paste(substr(dates,1,4),substr(dates,6,7),substr(dates,9,10),sep="")))
}

utils.matrixToTimeSeries<-function(mat){
# takes a matrix, mat, with dates in rownames(mat) and returns timeSeries()
	
	return(timeSeries(mat,charvec=utils.ymdToHyphens(rownames(mat)),row.names=NULL))
}

utils.timeSeriesToMatrix<-function(series){
# takes timeSeries() and returns matrix with dates in rownames() stripped of "-"
	
	series<-as.matrix(series)
	# strip "-" only because this is the format for dates in matrices that we have chosen
	rownames(series)<-gsub("-","",rownames(series))
	
	return(series)
}

utils.load<-function(fileName){
# a naked call to load() causes problems because it overwrites variables in the environment with the same name as the loaded variable. a call to utils.load() protects the calling environment
	
	savedDataNames<-load(file=fileName)
	if(length(savedDataNames)!=1){
		stop(paste("ERROR in utils.load(): the file should have had one variable saved to it only. instead all these variables were saved to it:",paste(savedDataNames,collapse=", ")))
	}
	
	# get() retrieves the value of a variable specified by its name as a string.
	return(get(savedDataNames))
}

utils.read <- function(fileName){
# single interface to read a .csv or .RData file
	
	if(length(grep("\\.RData$", fileName)) != 0) return(utils.load(fileName))
	if(length(grep("\\.csv$", fileName)) == 0){
		stop(paste("ERROR in utils.read(): only .csv or .RData files are recognized. you passed:",fileName))
	}
	
	# if we get here it must be a .csv file
	return(read.csv(fileName, header=TRUE, stringsAsFactors=FALSE))
}

utils.dependencies <- function(assets, depends){
# recursive. assets is a vector of strings; depends is a list() of list()'s of strings
# returns a vector containing assets and such that for every asset, items in the list()'s contained in depends appear in the return vector before that asset. What's that for? it can take assets that we're interested in and add to them assets that seed or provide the growth benchmarks for those assets in our forecasting 
	
	ret <- c()
	for (a in assets){
		# look for a in the sub-lists. Note: the inds returned are those of the sub-lists that have a in their names() or their content
		inds <- grep(a, depends)
		# if length(inds) == 0, this means that a has no dependencies and no other asset depends on it either. in forecasting, this means a is not being seeded by/is not seeding another asset and is not using/being used by another asset's growth as benchmark
		if(length(inds) != 0){
			for(i in inds){
				# only if a is in the names() of the sub-list does it have dependencies. if it's only in the contents of the sub-list, then that means that it might be the seeder for another asset, for example
				if(is.element(a, names(depends[[i]]))){
					# a could depend on b which depends on c . hence the following recursion. Note: we do things so that b appears before a in ret
					ret <- c(ret, utils.dependencies(depends[[i]][[a]],depends))
				}
			}
		}
		ret <- c(ret,a)
	}
	
	# if an asset is duplicated, unique() will pick the first occurrence and so will appear in ret before any other asset that depends on it -- as desired
	return(unique(ret))
}

utils.semiVariance <- function(assetReturns, weights, targetReturn){
# semi-variance of a portfolio given a history of asset returns and a vector of weights
# assetReturns is a matrix with rows containing asset returns corresponding to a given date; weights is a vector of length() equal to number of assets; targetReturn is a double -- the return below which we don't want to go
# returns semi-variance
	
	if(length(weights) != ncol(assetReturns)){
		stop("ERROR in utils.semiVariance(): asset returns and weights do not match.")
	}
	
	if(length(targetReturn) != 1 && length(targetReturn) != nrow(assetReturns)) {
		stop(paste("ERROR in utils.semiVariance(): targetReturn must be a scalar or a vector of length = nrow(assetReturns). you passed:",length(targetReturn), nrow(assetReturns)))
	}
	
	# calculate the history of portfolio returns.
	portfolioReturns <- assetReturns %*% matrix(weights, ncol=1) - targetReturn
	# if in a period we outperformed targetReturn we do not penalize the semi-variance
	portfolioReturns[ portfolioReturns >= 0 ] <- 0
	
	return( 1/length(portfolioReturns) * sum(portfolioReturns^2))
}

utils.3Moment <- function(x, na.rm ){
# calculate the 3rd central co-moments; x is matrix and na.rm is logical
# returns 3-dimensional matrix
	
	numCols <- ncol( x )
	numRows <- nrow( x) 
	# initialize 3-dim return array
	ret <- array( dim = c( numCols, numCols, numCols ))
	# de-mean
	meanCols <- colMeans( x, na.rm = na.rm )
	x <- x - matrix(meanCols, nrow=1 )[rep(1, numRows ),]
	
	# calculate 3rd central moments
	for (i in 1:numCols){
		tempi <- x[, rep(i, ncol(x))] * x
		for (j in 1:numCols ){
			# we want: ret[k , j, i ] <-  sum( x[, i] * x[, j] * x[, k] , na.rm = na.rm)
			ret[, j , i ] <-  colSums(  x[, rep(j, ncol(x))] * tempi, na.rm = na.rm)
		}
	}
	
	ret <-  1 / numRows * ret 
	
	return( ret )
}

utils.4Moment <- function(x, na.rm) {
# calculate the 4th central co-moments; x is matrix and na.rm is logical
# returns 4-dimensional matrix
	
	numCols <- ncol( x )
	numRows <- nrow( x) 
	# initialize 4-dimensional return array
	ret <- array( dim = c( numCols, numCols, numCols, numCols ))
	# de-mean
	meanCols <- colMeans( x, na.rm = na.rm )
	x <- x - matrix(meanCols, nrow=1 )[rep(1, numRows ),]
	
	# calculate 4th central moments
	for (i in 1:numCols){
		tempi <- x[, rep(i, ncol(x))] * x
		for (j in i:numCols){
			tempij <- x[, rep(j, ncol(x))] * tempi
			for(k in i:numCols){
				# what we're looking for is: ret[l, k , j, i ] <-  sum( x[, i] * x[, j] * x[, k] * x[, l], na.rm = na.rm)
				ret[, i , j, k]  <- ret[, k , j, i]  <- ret[, k , i, j]  <-  colSums(  x[, rep(k, ncol(x)) ] * tempij, na.rm = na.rm)
			}
		}
	}
	
	ret <-  1 / numRows * ret 
	
	return( ret )
}

utils.4DimTo2 <- function( x ){
# change a 4-dimensional array into a 2-dim one. 

	dims <- dim(x)
	ret <- matrix(0.0, nrow=dims[1]*dims[4], ncol=dims[2] * dims[3] )
	for(i in 1:dims[4]){
		irow <- (i-1) * dims[1] +1
		for(j in 1:dims[3]){
			jcol <- (j-1) * dims[2] +1
			ret[irow:(irow+dims[1]-1), jcol:(jcol +dims[2]-1)] <- x[,,j,i]
		}
	}
	
	return(ret)
}

utils.3DimTo2 <- function( x ){
# change a 3-dimensional array into a 2-dim one. stack the 2-dim sub-arrays "horizontally"
	
	dims <- dim( x )
	ret <- matrix(0.0, nrow= dims[1], ncol= dims[2] * dims[3])
	for(i in 1:dims[3]){
		icol <- (i-1) * dims[2] +1
		ret[,icol:(icol+dims[2]-1)] <- x[,,i]
	}
	
	return( ret )
}

utils.today <- function(){
# returns today's date as a string
	
	return(  strsplit(as.character(Sys.time())," ")[[1]][1] )
}

utils.now <- function(){
# returns the current time (hh:mm:ss) as a string
	
	return(  strsplit(as.character(Sys.time())," ")[[1]][2] )
}

utils.addMinutes <- function( thisTime, minutesToAdd){
# add or subtract (then pass negative minutesToAdd); thisTime is a string in hh:mm:ss format
	
	if( abs( minutesToAdd) >= 60 ){
		stop(paste("ERROR in utils.addMinutes(): minutesToAdd should be between -59 and +59. you passed:", minutesToAdd))
	}
	
	breakTime <- as.numeric(strsplit( thisTime, ":")[[1]])
	breakTime[2] <- breakTime[2] + minutesToAdd
	if (breakTime[2] < 0 ){
		breakTime[2] <- breakTime[2] + 60
		breakTime[1] <- breakTime[1] - 1
	} else if (breakTime[2] > 59 ){
		breakTime[2] <- breakTime[2] - 60
		breakTime[1] <- breakTime[1] + 1
	}
	
	if(breakTime[1] < 0 ){
		breakTime[1] <- breakTime[1] + 24
	} else if ( breakTime[1] > 23){
		breakTime[1] <- breakTime[1] - 24
	}
	
	return( paste( ifelse( breakTime[1] < 10, paste("0", breakTime[1], sep=""), breakTime[1]),":", ifelse( breakTime[2] < 10, paste("0", breakTime[2], sep=""), breakTime[2]), ":", ifelse(breakTime[3] < 10, paste("0", breakTime[3], sep=""), breakTime[3]), sep=""))
}

utils.getPreviousWeekDay <- function( thisDate ){
# return the last week day strictly before thisDate

	pastWeek <- as.Date( thisDate ) - 1:7
	return( as.character( pastWeek[ !is.element( weekdays( pastWeek ), c( "Saturday", "Sunday")) ][1]) )
}

utils.subtractWeekDays <- function( endDate, numberOfDays ){
# return startDate such that [startDate, endDate] has exactly numberOfDays
	
	if( numberOfDays == 1){
		return( endDate )
	}
	
	# get a whole range of previous dates and be generous -- multiply by 2
	pastRange <- as.Date( endDate ) - (1:( 2 * numberOfDays + 5))
	
	return( as.character( pastRange[ !is.element( weekdays(pastRange), c( "Saturday", "Sunday" ))][numberOfDays-1]) )
}

utils.changeToPrevWeekday <- function( dates ){
# if a date in dates falls on a Saturday or Sunday return the date of the previous friday
	
	dates <- as.Date( dates )
	
	days <- weekdays(dates)
	isSaturday <- days == "Saturday"
	isSunday <- days == "Sunday"
	
	dates[ isSaturday ]	 <- dates[isSaturday] - 1
	dates[ isSunday ] <- dates[ isSunday ] - 2
	
	return(as.character(dates))
}

utils.beta <- function(v1, v2){
# v1 and v2 are vectors
# returns beta of v1 relative to v2
	
	# do the calculation only for when v1 and v2 were defined. so we can't rely on the 'use' argument in cov() and the na.rm() argument in var() because we might end up using entries in v2 in var() that were not used in the cov() calculation
	isNotNA <- is.finite(v1) & is.finite(v2)
	
	return( cov(v1[isNotNA], v2[isNotNA]) / var( v2[isNotNA] ))
}

utils.conformRevisable <- function( dataDF, isRevisable, dbLabel ){
	
	if( isRevisable ){
		if( !is.element("Label", names(dataDF)) && is.null(dbLabel)){
			stop("ERROR in utils.conformRevisable(): the data frame doesn't have 'Label' and dbLabel is NULL.")
		} else if( !is.element("Label", names(dataDF))){
			dataDF <- dbDataLoader.addLabels( dataDF, dbLabel )
		}
		if( !is.element( "Date_Entered", names(dataDF))){
			# add today by default
			dataDF <- cbind( dataDF, Date_Entered = utils.today() )
		}
		
		if(!is.element( "Is_Revisable", names(dataDF))){
			# make it revisable by default
			dataDF <- cbind( dataDF, Is_Revisable = 1)
		}
	}
	
	return( dataDF )
}

utils.uploadDataFromFile <- function( fileName, sourceName, override, dbSettings, dbLabel ){
# load file into database table. its job is to read the file and call the next function
	
	dataDF <- read.csv( fileName, header=T, stringsAsFactors = F)
	utils.uploadDataFromDataFrame( dataDF, sourceName, override, dbSettings, dbLabel )
}

utils.uploadDataFromDataFrame <- function( dataDF, sourceName, override, dbSettings, dbLabel ) {
# load data.frame into database table. its job is essentially to stack the columns in the file into a data.frame() and call dbDataLoader
	
	if( !is.element("Date", names(dataDF))){
		stop("ERROR in utils.loadDataFromDataFrame(): the data.frame() does not have a column called 'Date'. ")
	}
	
	dates <- dataDF[["Date"]]
	
	for (n in setdiff( names( dataDF), "Date")) {
		
		temp <- as.numeric( dataDF[, n] )
		isNotNA <- is.finite( temp )
		numVals <- sum(isNotNA )
		temp <- utils.conformRevisable( data.frame(Date = dates[ isNotNA ], name = rep( n, numVals), Rate= temp[isNotNA] ), dbSettings[["ld.isRevisable"]], dbLabel)
		
		dummy<-dbDataLoader.pushData( temp, dbSettings[["ld.uid"]], dbSettings[["ld.dsnName"]], dbSettings[["ld.rateDescrTable"]], dbSettings[["ld.rateDataTable"]], override, dbSettings[["ld.isRevisable"]], sourceName )
	}
}

utils.uploadStackedDataFromFile <- function(fileName, secIdentifierColumnName, valueColumnName, sourceName, override, dbSettings, dbLabel){
# assumed stacked, i.e., in the format that we get back from bloomberg java call
	
	dataDF <- read.csv( fileName, header=T, stringsAsFactors = F)
	
	utils.uploadStackedDataFromDataFrame( dataDF, secIdentifierColumnName, valueColumnName, sourceName, override, dbSettings, dbLabel )
}

utils.uploadStackedDataFromDataFrame <- function(dataDF, secIdentifierColumnName, valueColumnName, sourceName, override, dbSettings, dbLabel){
# assumed stacked, i.e., in the format that we get back from bloomberg java call
	
	inds <- match(c(secIdentifierColumnName, valueColumnName, "Date"), names(dataDF))
	if( any( !is.finite(inds)) ){
		stop(paste("ERROR in utils.uploadStackedDataFromDataFrame(): the data frame does not have column(s) named", paste(c(secIdentifierColumnName, valueColumnName, "Date")[!is.finite(inds)], collapse=", ")) )
	}
	
	names(dataDF)[inds[1]] <- "name"
	names(dataDF)[inds[2]] <- "Rate"	
	dataDF <- utils.conformRevisable( dataDF, dbSettings[["ld.isRevisable"]], dbLabel)
	dummy<-dbDataLoader.pushData( dataDF, dbSettings[["ld.uid"]], dbSettings[["ld.dsnName"]], dbSettings[["ld.rateDescrTable"]], dbSettings[["ld.rateDataTable"]], override, dbSettings[["ld.isRevisable"]], sourceName )
}

utils.stackThem <- function(dataDF){
	
	if( !is.element("Date", names(dataDF))){
		stop("ERROR in utils.stackThem(): the data.frame() does not have a column called 'Date'. ")
	}
	
	dates <- dataDF[["Date"]]
	
	ret <- NULL
	for (n in setdiff( names( dataDF), "Date")) {
		
		temp <- as.numeric( dataDF[, n] )
		isNotNA <- is.finite( temp )
		numVals <- sum(isNotNA )
		temp <- data.frame(Date = dates[ isNotNA ], name = rep( n, numVals), Rate= temp[isNotNA] )
		
		ret <- rbind(ret, temp)
	}
	
	return( ret )
}

utils.aggregateForBarplot <- function( nodes, v1, v2, functionToUse){####, graphicsDevice, outputFile, plotTitle, subTitle, labels, xLabel, yLabel, ourPalette, pngSettings ){
# use nodes (an increasing vector) to bucket the values of v1 and, for every bucket, calculate functionToUse() of the v2 components corresponding to the v1 components in that bucket (i.e., with the same indices)
	
	if( length(v1) != length(v2) ){
		stop(paste("ERROR in utils.aggregateForBarplot(): we should have length(v1) == length(v2). we have: length(v1) =",length(v1),"and length(v2) =",length(v2)))
	}
	if( !is.function( functionToUse )) {
		stop(paste("ERROR in utils.aggregateForBarplot():", functionToUse,"is not a function."))
	}
	
	byVector <- rep(NA, length(v1) )
	
	# bucket v1 into buckets determined by nodes
	for ( i in 1:length(v1) ){
		if(v1[i] <= nodes[1]){
			byVector[ i ] <- 1
		} else if ( v1[i] > nodes[length(nodes)]){
			byVector[i] <- length(nodes) + 1
		} else {
			temp <- v1[i] > nodes[1:(length(nodes)-1)] & v1[i] <= nodes[2:length(nodes)]
			byVector[ i ] <- match(TRUE,temp) + 1
		}
	}
	
	# now calculate functionToUse() of v2 after aggregating them as dtermined by byVector
	return( aggregate( v2, list(by=byVector), functionToUse, na.rm=T ) )
}

utils.dayClosest <- function( dayDate, dayOfWeek, isBefore) {
# find the date that falls on dayOfWeek closest to day, before or after
	
	dayDTObj <- as.Date(dayDate)
	if( weekdays( dayDTObj) == dayOfWeek){
		# nothing to do
		return( as.character(dayDate ) )
	}
	
	if(isBefore){
		sixDays <- dayDTObj - (1:6)
	} else {
		sixDays <- dayDTObj + (1:6)
	}
	
	return( as.character( sixDays[ weekdays(sixDays) == dayOfWeek] ) )
}

utils.eom <- function( month, year ){
	
	month <- month+1
	if (month == 13){
		month <- "01"
		year <- year +1
	}
	
	eomDay <- as.Date(paste(year,"-",month,"-01", sep="")) - 1
	if( is.element( weekdays(eomDay), c("Saturday", "Sunday"))){
		eomDay <- utils.getPreviousWeekDay( eomDay)
	}
	
	return( as.character(eomDay ))
}

utils.mapMonth <- function( monthNames ) {
	
	ret <- rep(NA, length(monthNames))
	for( i in 1:length(monthNames)){
		monthName <- monthNames[i]
		
		if( monthName == "January"){
			ret[i] <- 1
		} else if( monthName == "February"){
			ret[i] <- 2
		} else if( monthName == "March"){
			ret[i] <- 3
		} else if( monthName == "April"){
			ret[i] <- 4
		} else if( monthName == "May"){
			ret[i] <- 5
		} else if( monthName == "June"){
			ret[i] <- 6
		} else if( monthName == "July"){
			ret[i] <- 7
		} else if( monthName == "August"){
			ret[i] <- 8
		} else if( monthName == "September"){
			ret[i] <- 9
		} else if( monthName == "October"){
			ret[i] <- 10
		} else if( monthName == "November"){
			ret[i] <- 11
		} else if( monthName == "December"){
			ret[i] <- 12
		} else {
			stop(paste("ERROR in utils.mapMonth():", monthName,"is not a month name."))
		}
	}
	
	return( ret )
}

utils.dayClosestEOM <- function( dayDate) {
# find the date that falls on the last business day (as in, not Saturday or Sunday) in the same month as dayDate before or after
	
	dayDate <- as.character( dayDate )
	
	return(utils.eom( as.numeric(substr(dayDate, 6, 7)), as.numeric(substr(dayDate, 1, 4))))
}

utils.createSubDir <- function( subDir, rootDir ){
# returns dir with "/" at the end
	
	nChars <- nchar(rootDir)
	lastChar <- substr(rootDir, nChars, nChars)
	if( lastChar != "/"){
		rootDir <- paste(rootDir, "/", sep="")
	}
	
	ret <-paste( rootDir, subDir, sep="" )
	if( !file.exists( ret ) ) dir.create( ret )
	
	return( paste(ret, "/" , sep="") )
}