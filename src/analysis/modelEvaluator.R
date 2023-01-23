modelEvaluator.input<-function(settings){
# input function

	assets<-settings[["meeex.assets"]]
	erSuffixes<-settings[["meeex.erSuffixes"]]
	dtrSuffix<-settings[["meeex.dtrSuffix"]]
	dcrSuffix<-settings[["meeex.dcrSuffix"]]
	assetIdentifiers <- settings[["meeex.assetIdentifiers"]]
	forecastsFile<-settings[["meeex.forecastsFile"]]
	dateFormat<-settings[["meeex.dateFormat"]]
	frequency<-settings[["meeex.frequency"]]
	dayOfWeek<-settings[["meeex.dayOfWeek"]]
	inputDir<-settings[["meeex.inputDir"]]
	drFile<-settings[["meeex.drFile"]]
	useDB<-settings[["meeex.useDB"]]
	dbSettings<-settings[["dbSettings"]]
	
	assetClasses <- utils.assetsByClass(assets, assetIdentifiers)
	
	erIds <- c()
	for(n in names(assetClasses)){
		# returns list() indexed by c("ers","dtrs") of timeSeries()
		erIds <- c(erIds, utils.generateCrossIds(assetClasses[[n]],erSuffixes[[n]]))
	}
	dtrIds<-paste(assets,dtrSuffix,sep="")
	dcrIds<-paste(assets,dcrSuffix,sep="")
	if(useDB){
		ret<-utils.getDataFromDB(c(erIds,dtrIds,dcrIds),NULL,dbSettings)
		erRet<-ret[,erIds]
		dtrRet<-ret[,dtrIds]
		dcrRet<-ret[,dcrIds]
	}else{
		erRet<-utils.getDataFromFile(erIds,NULL,paste(inputDir,forecastsFile,sep=""),dateFormat)
		drRet<-utils.getDataFromFile(c(dtrIds,dcrIds),NULL,paste(inputDir,drFile,sep=""),dateFormat)
		dtrRet<-drRet[,dtrIds]
		dcrRet<-drRet[,dcrIds]
	}
	
	if(frequency=="monthly"){
		return(list(ers=utils.getEOMData(erRet,TRUE),dtrs=dtrRet,dcrs=dcrRet))
	}else if(frequency=="weekly"){
		return(list(ers=utils.getWeeklyData(erRet,dayOfWeek),dtrs=dtrRet,dcrs=dcrRet))
	}else if(frequency=="daily"){
		return(list(ers=erRet,dtrs=dtrRet,dcrs=dcrRet))
	}else{
		stop(paste("error in meeex.getData(): frequency",frequency,"not implemented"))
	}
}

modelEvaluator.output<-function(output,settings){
# output function

	outputDir<-settings[["meeex.outputDir"]]
	outputFile<-settings[["meeex.outputFile"]]
	
	write.csv(output[["dailyRMSE"]],paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[["periodRMSE"]],paste(outputDir,"period-",outputFile,sep=""),row.names=FALSE)
	write.table( data.frame(t(output[["dailyRMSE"]][1,grep("\\.rmse",colnames(output[["dailyRMSE"]]))]) ), paste(outputDir,"last-",outputFile,sep=""), row.names=T, col.names=F, quote=F, sep=",")
	if(!is.null(output[["periodCapRMSE"]])){
		write.csv(output[["periodCapRMSE"]],paste(outputDir,"periodCap-",outputFile,sep=""),row.names=FALSE)
	}
}

meeex.plot<-function(assets,output,numSubPlots,rmseSuffix){
# assets is a vector of strings; output is a data.frame() with columns including $Date
# NO return value

	# identify the ".rmse" columns in the output; we obviously don't want to plot these
	rmseInds<-grep(rmseSuffix,names(output))
	# this is used to determine when we need a new "device" (graph window when running in console)
	counter<-0
	for(a in assets){
		# exclude ".rmse" columns from columns related to asset a
		aColsToPlot<-setdiff(grep(paste("^",a,sep=""),names(output)),rmseInds)
		# check if we skipped an asset earlier because of lack of enough history, for example
		if(length(aColsToPlot) == 0) next
		numberOfColumnsToPlot<-length(aColsToPlot)
		# get colors
		cols<-rainbow(numberOfColumnsToPlot)
		
		# graphing in R doesn't like NA's
		isNotNA<-rep(TRUE,nrow(output))
		for(j in aColsToPlot){
			temp<-is.finite(output[,j])
			if(sum(!temp)!=length(temp)){
				# so it's not all NA's
				isNotNA<-isNotNA & temp
			}
		}
		# do we need a new device? this is either a window when running in the console, or a PDF file when running from the command line
		if(counter%%numSubPlots==0){
			dev.new()
			layout(matrix(1:numSubPlots,nrow=2))
		}
		
		# plot
		dates<-as.Date(output$Date[isNotNA])
		plot(dates,output[isNotNA, aColsToPlot[1]],xlab="Dates",ylab="Returns",col=cols[1],main=a,ylim=c(min(output[isNotNA,aColsToPlot],na.rm=TRUE),max(output[isNotNA,aColsToPlot],na.rm=TRUE)),type="l")
		counter<-counter+1
		for(j in 2:numberOfColumnsToPlot){
			lines(dates,output[isNotNA,aColsToPlot[j]],col=cols[j])
		}
		labels <- sub("\\.","",sub(a,"",names(output)[aColsToPlot]) ) 
		labels[ labels != "er"] <- sub("er","", labels[ labels != "er"])
		legend("topright", labels,col=cols,lty=rep(1,length(cols)),bty="n")
	}
}


meeex.getCumDr<-function(dtrData,frequency,dayOfWeek,forecastPeriodInMonths,drSuffix){
# dtrData is a timeSeries()
# returns list() indexed by asset name of timeSeries() of cumulative returns for that asset

	ret<-NULL
	for(a in names(dtrData)){
		aDtr<-dtrData[,a]
		aDtr<-aDtr[as.logical(is.finite(aDtr[,1])),1]
		# calculate cumulative returns
		cumRets<-timeSeries(rev(cumprod(rev(1+aDtr[,1]))),charvec=row.names(aDtr))
		# filter at desired frequency. we always assume we're starting from daily data
		if(frequency=="monthly"){
			cumRets<-utils.getEOMData(cumRets,TRUE)
		}else if(frequency=="weekly"){
			cumRets<-utils.getWeeklyData(cumRets,dayOfWeek)
		}else if(frequency=="daily"){
			# nothing to do
		}else{
			stop(paste("ERROR in meeex.getCumDr(): frequency",frequency,"not implemented."))
		}

		periodCumRets<-(as.numeric(cumRets[1:(nrow(cumRets)-forecastPeriodInMonths),1])/as.numeric(cumRets[(1+forecastPeriodInMonths):nrow(cumRets),1]))^(12/forecastPeriodInMonths)-1
		ret<-append(ret,list(timeSeries(periodCumRets,charvec=row.names(cumRets)[(1+forecastPeriodInMonths):nrow(cumRets)])))
		# remove the dtr suffix from the name of the returned columns
		names(ret)[length(ret)]<-sub(drSuffix,"",a)
	}
	
	return(ret)
}

meeex.calculateRMSE<-function(erData,cumDrData, numberOfYearsInSubperiods,rmseSuffix,actRetSuffix){
# erData timeSeries(); cumDtrData list() indexed by asset names of timeSeries()
# returns a list() of data.frame()'s

	# reverse data because we want to calculate cumsum()'s below
	erData<-erData[nrow(erData):1,]
	ret<-NULL
	periodRMSEs <- NULL
	for(a in names(cumDrData)){
		isIncluded<-FALSE
		aDrs<-cumDrData[[a]]
		aDrs<-aDrs[nrow(aDrs):1,]
		aErs<-erData[,grep(a,names(erData))]
		for(er in names(aErs)){
			notNA<-as.logical(is.finite(aErs[,er]))
			aDates<-intersect(row.names(aErs)[notNA],row.names(aDrs))
			if(length(aDates)!=0){
				isIncluded<-TRUE
				aTemp<-data.frame(Date=aDates,ers=aErs[aDates,er])
				# calculate rmse for column "er"
				aTemp<-cbind(aTemp,rmse=sqrt(cumsum((aErs[aDates,er]-aDrs[aDates,1])^2)/(1:length(aDates))))
				names(aTemp)[2:3]<-c(er,paste(er,rmseSuffix,sep=""))
				ret<-append(ret,list(aTemp))
				periods <- utils.getPeriodsInYears(aDates, numberOfYearsInSubperiods, TRUE)
				if(!is.null(periods)){
					for(i in 1:nrow(periods)){
						periodDates <- aDates[ aDates > periods[["start"]][i] & aDates <= periods[["end"]][i] ]
						periodRMSE <- sqrt(sum((aErs[periodDates,er]-aDrs[periodDates,1])^2)/length(periodDates))
						periodRMSEs <- rbind(periodRMSEs, data.frame(start=periods[["start"]][i],end=periods[["end"]][i],model=er,rmse=periodRMSE))
					}
				}
			}else{
				print(paste("WARNING in meeex.calculateRMSE(): there's not enough data to evaluate model",er,"for asset",a))
			}
		}
		if(isIncluded){
			# we're here if asset a is included in the return and therefore we want to include its actual returns in the return structure
			aTemp<-data.frame(Date=row.names(aDrs),cumRet=aDrs)
			# "actAnnRet" as in "actual annualized return"
			names(aTemp)[2]<-paste(a,actRetSuffix,sep="")
			ret<-append(ret,list(aTemp))
		}
	}
	
	return(list(dailyRMSE=ret,periodRMSE=periodRMSEs))
}

meeex.rearrange<-function(assets,output,rmseSuffix){
# rearrange the columns in output to make convenient to plot the returns of an asset in a spreadsheet. essentially we separate the .rmse columns for an asset from the return columns for that asset
# assets is a vector of strings; output is a data.frame()
# returns a data.frame() which a column permuted version of the intput

	namesOutput <- names(output)
	rmseInds<-grep(rmseSuffix,namesOutput)
	orderedIndices <- c()
	for(a in assets){
		aInds<-grep(paste("^",a,sep=""),namesOutput)
		orderedIndices <- c(orderedIndices, aInds)
		output[aInds]<-output[c(setdiff(aInds,rmseInds),intersect(aInds,rmseInds))]
		# because we're just permuting columns the names() do not get modified. we take care of this nows
		names(output)[aInds]<-namesOutput[c(setdiff(aInds,rmseInds),intersect(aInds,rmseInds))]
	}
	
	# the "Date" column was left out
	dateIndex <- grep("Date", names(output))
	
	return(output[,c(dateIndex,orderedIndices)])
}

modelEvaluator.calculate<-function(input,settings){
# main logic

	assets<-settings[["meeex.assets"]]
	forecastPeriodInMonths<-settings[["meeex.forecastPeriodInMonths"]]
	allowGraphs<-settings[["meeex.allowGraphs"]]
	numSubPlots<-settings[["meeex.numSubPlots"]]
	frequency<-settings[["meeex.frequency"]]
	dtrSuffix<-settings[["meeex.dtrSuffix"]]
	dcrSuffix<-settings[["meeex.dcrSuffix"]]
	dayOfWeek<-settings[["meeex.dayOfWeek"]]
	doCap <- settings[["meeex.doCapitalReturns"]]
	numberOfYearsInSubperiods<-settings[["meeex.numberOfYearsInSubperiods"]]

	allData<-input
	erData<-allData[["ers"]]
	dtrData<-allData[["dtrs"]]
	dcrData<-allData[["dcrs"]]	
	cumDtrs<-meeex.getCumDr(dtrData,frequency,dayOfWeek,forecastPeriodInMonths,dtrSuffix)
	drmses <- meeex.calculateRMSE(erData,cumDtrs, numberOfYearsInSubperiods,".rmse",".act")
	dOutput<-utils.align(drmses[["dailyRMSE"]],TRUE)
	crmses <- NULL
	cOutput <- NULL
	if(doCap){
		cumDcrs<-meeex.getCumDr(dcrData,frequency,dayOfWeek,forecastPeriodInMonths,dcrSuffix)
		crmses <- meeex.calculateRMSE(erData,cumDcrs, numberOfYearsInSubperiods,".cap.rmse",".capAct")
		cOutput<-utils.align(crmses[["dailyRMSE"]],TRUE)
	}
	output <- dOutput
	if(!is.null(cOutput) && sum(dOutput[["Date"]] != cOutput[["Date"]]) !=0 ){
		stop("ERROR in modelEvaluator.calculate(): total and cap RMSE's data frames have different dates.")
	} else if(!is.null(cOutput)){
		output<-cbind(dOutput,cOutput[,setdiff(names(cOutput),names(dOutput))])
	}
	output<-meeex.rearrange(assets,output,"\\.rmse$")

	if(allowGraphs){
		meeex.plot(assets,output,numSubPlots,"\\.rmse$")
	}
	return(list(dailyRMSE=output,periodRMSE=drmses[["periodRMSE"]],periodCapRMSE=crmses[["periodRMSE"]]))
}
