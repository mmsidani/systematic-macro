# get the data series specified in the global variables
makeMacroBuckets.input<-function(settings){
	pathToRSource<-settings[["pathToRSourceCode"]]
	inflationIds<-settings[["macroex.inflationIds"]]
	growthIds<-settings[["macroex.growthIds"]]
	volatilityIds<-settings[["macroex.volatilityIds"]]
	suffixesFiles<-settings[["macroex.suffixesFiles"]]
	useDB<-settings[["macroex.useDB"]]
	mktDataFile<-settings[["macroex.mktDataFile"]]
	dateFormat<-settings[["macroex.dateFormat"]]
	useMacroDB<-settings[["macroex.useMacroDB"]]
	inputDir<-settings[["macroex.inputDir"]]
	dbSettings<-settings[["dbSettings"]]
	macroDbSettings <- settings[["macroDbSettings"]]
	
	idsToGet<-union(inflationIds, union(growthIds, volatilityIds))
	# now which of those id's is a macro variable? we need this because most likely the macro data is from a different source than market data
	macroIds<-c()
	for(suffix in names(suffixesFiles)){
		# any id that has suffix in it is that of a macro variable
		macroIds <-union(macroIds,idsToGet[grep(suffix,idsToGet)])
	}
	# segregate id's
	notMacroIds<-setdiff(idsToGet,macroIds)
	
	# get market data
	if(useDB){
		notMacroData<-utils.getDataFromDB(notMacroIds,NULL,dbSettings)
	} else {
		notMacroData<- utils.getDataFromFile(notMacroIds,NULL,mktDataFile, dateFormat)
	}
	
	# get macro data. Note: controlled by a separate variable useMacroDB
	if(useMacroDB){
		macroData<-utils.getDataFromDB(macroIds,NULL,macroDbSettings)
	}else{
		# our macro data is spread over different files. handle this through the loop
		macroData<-NULL
		for(suffix in names(suffixesFiles)){
			# suffixesFiles tells us where to find data with "suffix" as a suffix. Note: we look for macro data files in trunk/data
			suffixData<-utils.getDataFromFile(macroIds[grep(paste(suffix,"$",sep=""),macroIds)],NULL, paste(pathToRSource,"../data/",suffixesFiles[[suffix]],sep=""))
			if(!is.null(suffixData)){
				macroData<-append(macroData,list(suffixData))
			}
		}
		# collapse macro data from different sources into one series
		macroData<-utils.alignSeries(macroData,TRUE)
	}

	# collapse mkt and macro data into 1 series
	return(utils.alignSeries(list(notMacroData,macroData),TRUE))
}

makeMacroBuckets.output<-function(output,settings){
	outputDir<-settings[["macroex.outputDir"]]
	outputFile<-settings[["macroex.outputFile"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

# build contingency tables
# macroLablesCcList is a list indexed by country code, each item is a data.frame() of 2 columns, YYYY-Qx and macroLabel; labels1 and labels2 are vectors of macro labels for the different dimensions
# returns list indexed by cc, each with a contingency table for cc
macroex.makeTables<-function(macroLabelsCcList,labels1,labels2){
	ret<-NULL
	for(cc in names(macroLabelsCcList)){
		macroLabels<-macroLabelsCcList[[cc]][[2]]

		# initialize
		ccTable<-matrix(rep(0,length(labels1)*length(labels2)),nrow=length(labels1))
		rownames(ccTable)<-labels1
		colnames(ccTable)<-labels2
		
		# now fill table
		for(l1 in labels1){
			for(l2 in labels2){
				label<-paste("^",l1,"-",l2,"$",sep="")
				numOccurrences<-length(grep(label,macroLabels))
				ccTable[l1,l2]<-numOccurrences
			}
		}
		
		# append to return list
		ret<-append(ret,list(ccTable))
		names(ret)[length(ret)]<-cc
	}
	
	return(ret)
}

# calculate zscore over entire period. Note: why not use utils.zscore()? the latter calculates the zscore of a value at time t based on the mean and sd calculated using data up to time t-1. macroex.zscore() uses the whole data to calculate the mean/sd
# dataFrame is a data.frame() with a column called Date 
macroex.zscore<-function(dataFrame){	
	if(!is.finite(match("Date",names(dataFrame)))){
		stop(c("error in macroex.zscore(): dataFrame does not have a column called Date. its columns are ",names(dataFrame)))
	}
	ret<-data.frame(Date=dataFrame$Date)
	if(ncol(dataFrame)<2){
		stop("error in macroex.zscore(): the data frame that was passed only has the Date column.")
	}
	
	# now calculate the zscore of each column in dataFrame based on its mean/sd values
	for(i in 2:ncol(dataFrame)){
		m<-mean(dataFrame[,i],na.rm=TRUE)
		s<-sd(dataFrame[,i],na.rm=TRUE)
		temp<-(as.numeric(dataFrame[,i])-m)/s
		ret<-cbind(ret,data.frame(temp=temp))
		names(ret)[ncol(ret)]<-names(dataFrame)[i]
	}

	return(ret)
}

# auxiliary function to calculate measures per quarter
# indicatorSeries is a timeSeries(); method at the time of first writing is either "return", in which case the percent change in the variable in each quarter is calculated, or "standardDeviation" in which we return sd in each quarter
# returns list of data.frame()'s, each data frame has Date as "YYYY-QX" values and the associated measure (depending on "method")
macroex.valuesPerQuarter<-function(indicatorSeries,method){
	ret<-NULL
	# loop through the indicators in the series (these are inflation, growth, volatility, etc.)
	for(i in 1:ncol(indicatorSeries)){
		currentSeries<-indicatorSeries[as.logical(is.finite(indicatorSeries[,i])),i]
		if(method=="returnYoY"){
			# use EOM values only
			currentSeriesEOM<-utils.getEOMData(currentSeries,TRUE)
			# get YoY returns
			currentSeries<-timeSeries(as.numeric(currentSeriesEOM[1:(nrow(currentSeriesEOM)-12),1])/as.numeric(currentSeriesEOM[13:nrow(currentSeriesEOM),1])-1,charvec=row.names(currentSeriesEOM)[1:(nrow(currentSeriesEOM)-12)])
			names(currentSeries)<-names(indicatorSeries)[i]
		}else if(method=="standardDeviation"){
			# get daily returns
			currentSeries<-timeSeries(as.numeric(currentSeries[1:(nrow(currentSeries)-1),1])/as.numeric(currentSeries[2:nrow(currentSeries),1])-1,charvec=row.names(currentSeries)[1:(nrow(currentSeries)-1)])
			names(currentSeries)<-names(indicatorSeries)[i]
		}
		
		# bucket the dates in the indicator's series into quarters
		yearQuarters<-utils.getYearQuarterPairs(row.names(currentSeries),TRUE)
		valPerQuarter<-NULL
		# now for each quarter in the history of this indicator calculate its return in the quarter or its sd, etc.
		if(length(yearQuarters)>=2){
			for(j in 2:length(yearQuarters)){
				yq <- names(yearQuarters)[j]
				# get all the dates in the indicator's series that fall in this quarter
				dates<-yearQuarters[[j]]
				prevDates<-yearQuarters[[j-1]]
				if(method=="return"){
					# calculate percent change
					lastValInPrevQuarter<-as.numeric(currentSeries[as.Date(max(prevDates))])
					lastValInQuarter<-as.numeric(currentSeries[as.Date(max(dates))])
	
					if(lastValInPrevQuarter!=0){
						valPerQuarter<-rbind(valPerQuarter,data.frame(Date=yq,value=lastValInQuarter/lastValInPrevQuarter-1))
					}else{
						valPerQuarter<-rbind(valPerQuarter,data.frame(Date=yq,value=NA))
					}
				}else if(method=="standardDeviation"){
					# calculate sd
					valPerQuarter<-rbind(valPerQuarter,data.frame(Date=yq,value=sd(as.numeric(currentSeries[as.Date(dates)]))))
				}else if(method=="averageLevel" || method=="returnYoY"){
					valuesInQuarter<-as.numeric(currentSeries[as.Date(dates)])
					valPerQuarter<-rbind(valPerQuarter,data.frame(Date=yq,value=mean(valuesInQuarter,na.rm=TRUE)))
				}else{
					stop(paste("error in macroex.valuesPerQuarter(): unknown method of calculation",method))
				}
			}
		}

		# append results for indicator i into a list
		ret<-append(ret,list(valPerQuarter))
		names(ret)[length(ret)]<-names(currentSeries)
	}
		
	return(ret)
}

# quarterValsList is the output from macroex.valuesPerQuarter; seriesName is the string to assign as name to the resulting series
# returns data.frame() with columns Date, as "YYYY-QX" values, and a column called seriesName that contains the average  of all columns in quarterValsList
macroex.averageValuePerQuarter<-function(quarterValsList, seriesName){
	if(length(quarterValsList)==1){
		# in this case, just rename the unique column (besides Date) and return
		ret<-data.frame(Date=quarterValsList[[1]]$Date,seriesName=quarterValsList[[1]]$value)
		names(ret)[2]<-seriesName
		return(ret)
	} 
	
	# find intersection of all quarters for which all columns of quarterValsList have data
	commonQuarters<-quarterValsList[[1]]$Date
	for(i in 2:length(quarterValsList)){
		commonQuarters<-intersect(commonQuarters,quarterValsList[[i]]$Date)
	}
	commonQuarters<-sort(commonQuarters,decreasing=TRUE)
	
	# now calculate the sums of the values of all columns over those common quarters
	vals<-rep(0,length(commonQuarters))
	for(i in 1:length(quarterValsList)){
		temp<-quarterValsList[[i]]
		vals<-vals+as.numeric(temp[["value"]][match(commonQuarters,temp$Date)])
	}

	# simple division to get average
	ret<-data.frame(Date=commonQuarters,value=vals/length(quarterValsList))
	names(ret)[2]<-seriesName	
	return(ret)
}

# build an inflation series out of the different components we specified in GlobalVariables.R . all components relating to a certain country make up the series for that country
# allData is a timeSeries() that should contain the components we need to build the inflation series 
# returns list() indexed by country, each item is a data.frame() with a column Date containing YYYY-QX's, and a column called "inflation" 
macroex.buildInflation<-function(allData,inflationIdsAll){
	ccs<-unique(substr(inflationIdsAll,1,2))
	ret<-NULL
	for(cc in ccs){
		# which components go into the make-up of an inflation series?
		inflationIds<-inflationIdsAll[grep(paste("^",cc,"\\.",sep=""), inflationIdsAll)]
		inflationData<-allData[,inflationIds]
		# evaluate the percent change of each component in each quarter in its history
		allIndicatorsPerQuarter<-macroex.valuesPerQuarter(inflationData,"returnYoY")
		# get average of all components and call that "inflation"
		avgOfIndicatorsPerQuarter<-macroex.averageValuePerQuarter(allIndicatorsPerQuarter,"inflation")
		ret<-append(ret,list(avgOfIndicatorsPerQuarter))
		names(ret)[length(ret)]<-cc
	}
	
	return(ret)
}

# build a growth series out of the different components we specified. see comments for macroex.inflation() and replace "inflation" with "growth"
macroex.buildGrowth<-function(allData,growthIdsAll){
	ccs<-unique(substr(growthIdsAll,1,2))
	ret<-NULL
	for(cc in ccs){
		# which components make up "growth"?
		growthIds<-growthIdsAll[grep(paste("^",cc,"\\.",sep=""), growthIdsAll)]
		growthData<-allData[,growthIds]
		# get percent changes of each component over each quarter in its history
		allIndicatorsPerQuarter<-macroex.valuesPerQuarter(growthData,"returnYoY")
		# get average of all components over quarters for which all components have values and call series "growth"
		avgOfIndicatorsPerQuarter<-macroex.averageValuePerQuarter(allIndicatorsPerQuarter,"growth")
		ret<-append(ret,list(avgOfIndicatorsPerQuarter))
		names(ret)[length(ret)]<-cc
	}
	
	return(ret)
}

# build a volatility series out of the different components we specified. see comments for macroex.inflation() and replace "inflation" with "volatility"
macroex.buildVolatility<-function(allData,volatilityIdsAll){
	ccs<-unique(substr(volatilityIdsAll,1,2))
	ret<-NULL
	for(cc in ccs){
		# which components make up volatility series?
		volIds<-volatilityIdsAll[grep(paste("^",cc,"\\.",sep=""), volatilityIdsAll)]
		volData<-allData[,volIds]
		# get standard deviation of each component over each quarter in its history. NOTE: we might have to make non-trivial changes if we included a component in the volatility series that required "return" instead of "standardDeviation". the same is true of growth and inflation if we included a component that should be characterized by other than its percent change in a quarter
		allIndicatorsPerQuarter<-macroex.valuesPerQuarter(volData,"standardDeviation")
		# average all sd's of all components over each quarter for which all components have values and call resulting series "volatility"
		avgOfIndicatorsPerQuarter<-macroex.averageValuePerQuarter(allIndicatorsPerQuarter,"volatility")
		ret<-append(ret,list(avgOfIndicatorsPerQuarter))
		names(ret)[length(ret)]<-cc
	}
	
	return(ret)
}

macroex.bucket<-function(dates,dataFrame,listOfLabels,header){
	ret<-data.frame(Date=dates)
	labels<-rep("",length(dates))
	for(i in 1:ncol(dataFrame)){
		zscores<-dataFrame[,i]
		labelsSeries<-listOfLabels[[i]]
		for(n in names(labelsSeries)){
			bounds<-labelsSeries[[n]]
			inds<-zscores>bounds[1] & zscores<=bounds[2]
			labels[inds]<-paste(labels[inds],n,ifelse(i!=ncol(dataFrame),"-",""),sep="")
		}
	}
	ret<-data.frame(Date=dates,labels)
	names(ret)[2]<-header
	
	return(ret)
}

# cc is a country code used for labeling output; inflationGrowth is a data.frame() with 3 columns, "Date", "inflation" and "growth"
# returns data.frame() with 2 columns, "Date" and "cc.macroex.inflationGrowthSuffix" containing the quarter labels
macroex.assignInflationGrowthLabels<-function(cc,inflationGrowth,inflationLabels,growthLabels,inflationGrowthSuffix){
	nonNAs<-is.finite(inflationGrowth[["inflation"]])&is.finite(inflationGrowth[["growth"]])
	# remove NA's
	finiteInflation<-as.numeric(inflationGrowth[["inflation"]])[nonNAs]
	finiteGrowth<-as.numeric(inflationGrowth[["growth"]])[nonNAs]
	
	return(macroex.bucket(inflationGrowth$Date[nonNAs],data.frame(finiteInflation,finiteGrowth),list(inflationLabels,growthLabels),paste(cc,inflationGrowthSuffix,sep="")))
	
}

macroex.assignVolatilityGrowthLabels<-function(cc,volatilityGrowth,volatilityLabels,growthLabels,volatilityGrowthSuffix){
	nonNAs<-is.finite(volatilityGrowth[["volatility"]])&is.finite(volatilityGrowth[["growth"]])
	# remove NA's
	finiteVolatility<-as.numeric(volatilityGrowth[["volatility"]])[nonNAs]
	finiteGrowth<-as.numeric(volatilityGrowth[["growth"]])[nonNAs]
	
	return(macroex.bucket(volatilityGrowth$Date[nonNAs],data.frame(finiteVolatility,finiteGrowth),list(volatilityLabels,growthLabels),paste(cc,volatilityGrowthSuffix,sep="")))

}

# returns list with 2 items: "inflationGrowth" and "volatilityGrowth" -- both are lists indexed by country containing data frames with "Date" with quarters, "YYYY-QX", and macro labels for the quarters
macroex.assignLabels<-function(allData,inflationIds,growthIds,volatilityIds,inflationLabels,growthLabels,volatilityLabels,inflationGrowthSuffix,volatilityGrowthSuffix){
	# build macro series out of the components specified in GlobalVariables.R for each country
	inflationSeries<-macroex.buildInflation(allData,inflationIds)
	growthSeries<-macroex.buildGrowth(allData,growthIds)
	volatilitySeries<-macroex.buildVolatility(allData,volatilityIds)

	growthCcs<-names(growthSeries)
	inflationCcs<-names(inflationSeries)
	volatilityCcs<-names(volatilitySeries)
	
	# now loop over countries
	inflationGrowthLabels<-NULL
	for(cc in growthCcs){
		if(is.element(cc,inflationCcs)){
			# merge data.frames into 1
			inflationGrowth<-utils.align(list(inflation=inflationSeries[[cc]],growth=growthSeries[[cc]]),TRUE)
			inflationGrowthLabels<-append(inflationGrowthLabels,list(macroex.assignInflationGrowthLabels(cc,macroex.zscore(inflationGrowth),inflationLabels,growthLabels,inflationGrowthSuffix)))
			names(inflationGrowthLabels)[length(inflationGrowthLabels)]<-cc
		}else{
			print(paste("warning in macroex.assignLabels():",cc,"has no inflation series. skipping it."))
		}
	}
	
	volatilityGrowthLabels<-NULL
	for(cc in growthCcs){
		if(is.element(cc,volatilityCcs)){
			# merge data.frames into 1
			volatilityGrowth<-utils.align(list(volatility=volatilitySeries[[cc]],growth=growthSeries[[cc]]),TRUE)
			volatilityGrowthLabels<-append(volatilityGrowthLabels,list(macroex.assignVolatilityGrowthLabels(cc,macroex.zscore(volatilityGrowth),volatilityLabels,growthLabels,volatilityGrowthSuffix)))
			names(volatilityGrowthLabels)[length(volatilityGrowthLabels)]<-cc
		}else{
			print(paste("warning in macroex.assignLabels():",cc,"has no volatility series. skipping it."))
		}
	}

	return(list(inflationGrowth=inflationGrowthLabels,volatilityGrowth=volatilityGrowthLabels))
}

makeMacroBuckets.calculate<-function(input,settings){
	suffixesFiles<-settings[["macroex.suffixesFiles"]]
	inflationIds<-settings[["macroex.inflationIds"]]
	growthIds<-settings[["macroex.growthIds"]]
	volatilityIds<-settings[["macroex.volatilityIds"]]
	growthLabels<-settings[["macroex.growthLabels"]]
	inflationLabels<-settings[["macroex.inflationLabels"]]
	volatilityLabels<-settings[["macroex.volatilityLabels"]]
	inflationGrowthSuffix<-settings[["macroex.inflationGrowthSuffix"]]
	volatilityGrowthSuffix<-settings[["macroex.volatilityGrowthSuffix"]]
	
	# get all data, market and macro
	allData<-input
	
	allSeries<-macroex.assignLabels(allData,inflationIds,growthIds,volatilityIds,inflationLabels,growthLabels,volatilityLabels,inflationGrowthSuffix,volatilityGrowthSuffix)
	
	inflationGrowth<-allSeries[["inflationGrowth"]]	
	volatilityGrowth<-allSeries[["volatilityGrowth"]]

	# calculate contingency tables
	igTable<-macroex.makeTables(inflationGrowth,names(inflationLabels),names(growthLabels))
	vgTable<-macroex.makeTables(volatilityGrowth,names(volatilityLabels),names(growthLabels))

	# print contigency tables to the screen
	print(c(igTable,vgTable))
	
	return(utils.align(c(inflationGrowth,volatilityGrowth),TRUE))
}


