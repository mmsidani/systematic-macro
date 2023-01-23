# given a portfolio calculated, for example, in our frontier builder, we can use this to calculate the actual future return and future (therefore, realized) vol of the portfolio

apret.readPortfolio<-function(inputDir,portfolioName, nonAssetWeightsColumns, portRetRiskColumns){
# inputDir and protfolioName are strings
# returns named matrix.

	portData<-read.csv(paste(inputDir,portfolioName,sep=""),stringsAsFactors=FALSE)
	portData[["Date"]]<-as.numeric(gsub("-","",portData[["Date"]]))
	# header of file expected to be "Date", "portRet", "portRisk", asset1, asset2, ... Note: since we might pass an "Opt-FULL" file here, i.e., a full frontier, dates can be duplicated. this creates problems if we set rownames() to the dates since then referencing a date returns only the row of the first occurrence -- not all rows in which it occurred
	assetWeightsAndDateColumns<-c("Date",setdiff(names(portData), nonAssetWeightsColumns))
	portfolioWeights<-as.matrix(portData[, assetWeightsAndDateColumns])
	colnames(portfolioWeights)<-assetWeightsAndDateColumns
	rownames(portfolioWeights)<-NULL
	
	# now "portRet", "portRisk" in a separate strucuture
	portfolioRetRisk<-as.matrix(portData[, c("Date",portRetRiskColumns)])
	colnames(portfolioRetRisk)<-c("Date",portRetRiskColumns)
	rownames(portfolioRetRisk)<-NULL

	return(list(portfolioWeights=portfolioWeights,portfolioRetRisk=portfolioRetRisk))
}

apret.readAssetData<-function(assets,inputDir,assetRetFile,useDB,dbSettings,dateFormat,retSuffix){
# assets is an array of strings, typically the assets in the portfolio of interest for the analysis; inputDir is a string; assetRetFile is a string, typically, "dTrCr.csv", that is only used if useDB is FALSE; useDB is logical; dbSettings is a list() of "ld." for DB access; dateFormat is, typically, "%Y-%m-%d", of the dates in assetRetFile; retSuffix is, typically, ".dtr"

	if(length(retSuffix)!=1){
		stop(paste("error in apret.readAssetData(): only one suffix should be passed. you passed",length(retSuffix)))
	}
	if(useDB){
		return(utils.getDataMatrixFromDB(paste(assets,retSuffix,sep=""),NULL,dbSettings))
	}else{
		return(utils.getDataMatrixFromFile(paste(assets,retSuffix,sep=""),NULL,paste(inputDir,assetRetFile,sep=""),dateFormat))
	}
}

actualPortfolioReturn.input<-function(settings){
# returns a list() with 3 items, "portfolioWeights", "portfolioRetRisk" and "assetRetData"

	inputDir<-settings[["apret.inputDir"]]
	portfolioName<-settings[["apret.portfolioName"]]
	assetRetFile<-settings[["apret.assetRetfile"]]
	dateFormat<-settings[["apret.dateFormat"]]
	useDB<-settings[["apret.useDB"]]
	# typically ".dtr"
	retSuffix<-settings[["apret.retSuffix"]]
	
	dbSettings<-settings[["dbSettings"]]
	
	# why here this one? I worried it would cause errors if I exposed it
	nonAssetWeightsColumns<-c("Date","Port","portRet","portRisk")
	portRetRiskColumns<-c("portRet","portRisk")
	
	portfolioData<-apret.readPortfolio(inputDir,portfolioName, nonAssetWeightsColumns, portRetRiskColumns)
	assetRetData<-apret.readAssetData(setdiff(colnames(portfolioData[["portfolioWeights"]]),"Date"),inputDir,assetRetFile,useDB,dbSettings,dateFormat,retSuffix)
	
	return(append(portfolioData,list(assetRetData=assetRetData)))
}

apret.doPlot<-function(dataFrameToPlot, maximumNumberPlotsAllowed, ourPalette){
# plot for the various frontiers/portofolios the actual return/risk and the forecast return/risk

	dates<-unique(dataFrameToPlot$Date)
	if(length(dates)> maximumNumberPlotsAllowed){
		print(paste("INFO from apret.doPlot(): too many dates. you're asking to produce more plots than is allowed in the settings, which is:", maximumNumberPlotsAllowed,". nothing will be plotted"))
	}else{
		for(d in dates){
			dev.new()
			
			subFrame<-dataFrameToPlot[dataFrameToPlot$Date==d,]
			plot(subFrame$actualSD,subFrame$actualReturn,xlim=c(min(c(subFrame$portRisk,subFrame$actualSD)),max(c(subFrame$portRisk,subFrame$actualSD))),ylim=c(min(c(subFrame$portRet,subFrame$actualReturn)),max(c(subFrame$portRet,subFrame$actualReturn))),main=d,xlab="risk",ylab="return",col= 2,type="p")
			points(subFrame$portRisk,subFrame$portRet,col= 3)
			legend("topright",legend=c("actual","predicted"),col=2:3,lty=1)
		}
	}
}

actualPortfolioReturn.output<-function(output,settings){
# output is a named matrix() and settings list() as returned from GlobalVariables

	outputDir<-settings[["apret.outputDir"]]
	portfolioName<-settings[["apret.portfolioName"]]
	doPlot<-settings[["apret.doPlot"]]
	maximumNumberPlotsAllowed<-settings[["apret.maximumNumberPlotsAllowed"]]
	ourPalette<-settings[["apret.palette"]]

	# create data.frame() to contain the predicted and actual (future) portfolio returns on the specified dates
	nonDateActCols<-setdiff(colnames(output[["actualPortfolioRetRisk"]]),"Date")
	forecastDF<-data.frame(output[["forecastPortfolioRetRisk"]])
	forecastDF$Date<-utils.ymdToHyphens(forecastDF[,"Date"])
	dataFrame<-cbind(forecastDF,data.frame(output[["actualPortfolioRetRisk"]][, nonDateActCols]))
	names(dataFrame)<-c(names(forecastDF), nonDateActCols)
	
	if(doPlot){
		apret.doPlot(dataFrame, maximumNumberPlotsAllowed, ourPalette)
	}
	
	outputFile<-paste(outputDir,sub("\\.csv","",portfolioName),".actret.csv",sep="")
	write.csv(dataFrame, outputFile,row.names=FALSE)
	save(dataFrame,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

apret.checkDates<-function(desiredDates,badDesiredDates,portfolioDates){
# bunch of verifications. all arguments are arrays. no return value

	if(sum(!is.finite(match(desiredDates,portfolioDates)))!=0){
		stop(paste("error in actualPortfolioReturn.calculate(): desiredDates must be a subset of the dates for which the frontier or portfolio was built. these dates are in violation:",paste(desiredDates[!is.finite(match(desiredDates,portfolioDates))],collapse=", ")))
	}else if(length(desiredDates)==0){
		print("INFO from actualPortfolioReturn.calculate(): not enough future data for any of the desired dates you passed. nothing to do. returning.")
		return(list(actualPortfolioRetRisk=NULL,forecastPortfolioRetRisk=NULL))
	}else if(length(badDesiredDates)!=0){
		print(paste("INFO from actualPortfolioReturn.calculate(): not enough future data for some of the desired dates you passed. can't calculate actual future returns for: ",paste(badDesiredDates,collapse=", ")))
	}
}

# main logic
actualPortfolioReturn.calculate<-function(input,settings){
	numYears<-settings[["apret.numberOfYears"]]
	busDaysPerYear<-settings[["apret.busDaysPerYear"]]
	desiredDates<-settings[["apret.desiredDates"]]
	
	portfolioWeights<-input[["portfolioWeights"]]
	assetRetData<-input[["assetRetData"]]
	portRetRisk<-input[["portfolioRetRisk"]]
	
	# convert dates to integers. so we expect desiredDates to be input in the "%Y-%m-%d" format
	desiredDates<-as.numeric(gsub("-","",desiredDates))
	desiredDates<-sort(desiredDates)
	
	# date format here is assumed: YYYYMMDD and so can be converted to number. reverse portfolio dates and assetRetDates to have them in chronological order. we take unique because we allow frontiers to be passed to here
	portfolioWeights<-portfolioWeights[nrow(portfolioWeights):1,]
	# replace NA's with 0's
	portfolioWeights[!is.finite(portfolioWeights)]<-0
	portfolioDates<-portfolioWeights[,"Date"]
	portfolioWeights<-portfolioWeights[,setdiff(colnames(portfolioWeights),"Date")]
	if(length(desiredDates)==1 && desiredDates=="all"){
		# we want to do the calculation for every date in the portfolios file
		desiredDates<-unique(portfolioDates)
	}

	assetRetData<-assetRetData[nrow(assetRetData):1,]
	assetRetData<-assetRetData[rownames(assetRetData)>= desiredDates[1],]
	# replace NA's with 0's
	assetRetData[!is.finite(assetRetData)]<-0
	assetRetDates<-as.numeric(rownames(assetRetData))
	
	# utils.offsetYears() returns indices. Recall that: assetRetDates[offsetUniquePortDatesInds[i] ] == desiredDates[i]+numYears, or the very first date in assetRetDates that is after RHS. We get NA if no such date exists
	offsetUniquePortDatesInds<-utils.offsetYears(desiredDates,assetRetDates,numYears)
	offsetUniquePortDatesInds<-offsetUniquePortDatesInds[is.finite(offsetUniquePortDatesInds)]
	# truncate the dates to those for which there is enough future data, as specified in numYears
	badDesiredDates<-NULL
	if(length(offsetUniquePortDatesInds)<length(desiredDates)){
		badDesiredDates<-desiredDates[(length(offsetUniquePortDatesInds)+1):length(desiredDates)]
	}
	desiredDates <-desiredDates[1:length(offsetUniquePortDatesInds)]
	apret.checkDates(desiredDates,badDesiredDates,portfolioDates)
	
	# initialize return with 2 columns for the actual return and standard deviation. this is more rows than we need since we have to stop numYears in the past
	ret<-matrix(data=0,nrow=length(portfolioDates),ncol=2)
	retDates<-rep(0,length(portfolioDates))
	rowCounter<-1
	for(d in 1:length(desiredDates)){
		dDatesInds<-(1:length(portfolioDates))[portfolioDates==desiredDates[d]]
		for(i in dDatesInds){
			portfolio<-portfolioWeights[i,]
			firstDateInd<-match(desiredDates[d],assetRetDates)
			if(!is.finite(firstDateInd)){
				stop("error in actualPortfolioReturn.calculate(): something off. dates in portfolio are supposed to be a subset of those in asset returns data")
			}
			lastDateInd<-match(assetRetDates[offsetUniquePortDatesInds[d]],assetRetDates)
			
			if(firstDateInd+1>nrow(assetRetData)){
				break
			}

			# assetRetData and portfolio have columns in the same order. we saw to it in the call to apret.readAssetData()
			portfolioDailyRets<-as.numeric(assetRetData[(firstDateInd+1):lastDateInd,] %*% portfolio)
			annualRet<-(prod(1+portfolioDailyRets))^(1/numYears)-1
			sdRet<-sd(portfolioDailyRets)*sqrt(busDaysPerYear)
			ret[rowCounter,1:2]<-c(annualRet,sdRet)
			retDates[rowCounter]<-desiredDates[d]
			rowCounter<-rowCounter+1
		}
	}
	
	ret<-ret[(rowCounter-1):1,]
	retDates<-retDates[(rowCounter-1):1]
	colnames(ret)<-c("actualReturn","actualSD")
	
	# here we use fact that portRetRisk has the same rownames() as portfolioWeights
	return(list(actualPortfolioRetRisk=cbind(Date=retDates,ret,deparse.level=0),forecastPortfolioRetRisk=portRetRisk[is.element(portRetRisk[,"Date"],retDates),]))
}
