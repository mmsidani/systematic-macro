erRiskDomesticEquivalent.convertErsRisks<-function(rateData,baseCc,rfRates,rfCouponsPerYear,cashNameSuffix,doRisks){
# convert returns on foreign assets to domestic equivalent returns. NOTE: this function assumes the caller has aligned rateData and rfRates on the same set of dates. no further checking is done here as of Apr. 3, 2012
# rateData is a timeSeries(); baseCc is a string; rfRates is a timeSeries(); rfCouponsPerYear is a list() indexed by rate name; cashNameSuffix is a string
# returns timeSeries() after converting er's/risks to base currency equivalent

	columnNames<-names(rateData)
	if(is.null(columnNames)){
		# it must be that we passed a matrix
		columnNames <-colnames(rateData)
	}
	# Note: the er's might not have the right suffix because of the way we do things in frontier builder; doesn't matter, we need to search for one of the suffixes only
	riskInds<-grep("\\.risk", columnNames)
	if(length(riskInds)==0){
		stop(c("error in erRiskDomesticEquivalent.convertErsRisks(): rateData has no .risk columns. the columns are ",paste(columnNames,collapse=", ")))
	}
	numCols<-length(columnNames)
	expectedRets<-rateData[,setdiff(1:numCols,riskInds)]
	risks<-rateData[,riskInds]
	
	assetNames<-columnNames[setdiff(1:numCols,riskInds)]
	if(sum(assetNames!=sub("\\.risk","",names(risks)))!=0){
		print(c("assetNames",assetNames))
		print(c("risks",names(risks)))
		stop("error in erRiskDomesticEquivalent.convertErsRisks(): data in expectedRets and risks do not seem to correspond to the same instruments")
	}
	
	ratesNames<-names(rfRates)
	if(is.null(ratesNames)){
		# a matrix was passed
		ratesNames<-colnames(rfRates)
	}
	otherCcs<-setdiff(substr(assetNames,1,2),c(baseCc))
	if(length(otherCcs)==0){
		# nothing to do
		return(rateData)
	}
	
	# find the home country risk free rate
	baseCcRateInd<-grep(paste("^",baseCc,"\\.",sep=""), ratesNames)
	if(!is.finite(baseCcRateInd)){
		stop(paste("error in erRiskDomesticEquivalent.convertErsRisks(): home country",baseCc,"does not have risk free rates"))
	}

	# if we get here, then length(otherCcs) != 0
	nonNABaseRf<-as.logical(is.finite(rfRates[,baseCcRateInd]))
	baseCouponsPerYear<-rfCouponsPerYear[[paste(baseCc,cashNameSuffix,sep="")]]
	rfRates[nonNABaseRf,baseCcRateInd]<-(rfRates[nonNABaseRf,baseCcRateInd]/baseCouponsPerYear+1)^baseCouponsPerYear-1
	for(i in 1:length(otherCcs)){
		cc<- otherCcs[i]
		ccPrefix<-paste("^",cc,"\\.",sep="")
		# find all assets with this country code
		ccIndices<-grep(ccPrefix,assetNames)
		# find the risk free rate to apply for this country code
		ccRateInd<-grep(ccPrefix, ratesNames)
		if(length(ccRateInd)==0){
			stop(paste("error in erRiskDomesticEquivalent.convertErsRisks(): risk free rate for ",cc,"is missing"))
		}
		# convert
		nonNARf<-as.logical(is.finite(rfRates[,ccRateInd])) & nonNABaseRf
		ccCouponsPerYear<-rfCouponsPerYear[[paste(cc,cashNameSuffix,sep="")]]
		rfRates[nonNARf,ccRateInd]<-(rfRates[nonNARf,ccRateInd]/ccCouponsPerYear+1)^ccCouponsPerYear-1
		for(j in ccIndices){
			isNotNA<-as.logical(is.finite(expectedRets[,j])) & nonNARf
			if(doRisks){
				isNotNA<- isNotNA & as.logical(is.finite(risks[,j]))
			}
			ratesDiff<-(1+as.numeric(rfRates[isNotNA,baseCcRateInd]))/(1+as.numeric(rfRates[isNotNA,ccRateInd]))
			expectedRets[isNotNA,j]<-(1+as.numeric(expectedRets[isNotNA,j]))*ratesDiff - 1.
			expectedRets[!isNotNA,j]<-NA
			if(doRisks){
				risks[isNotNA,j]<-as.numeric(risks[isNotNA,j])/ratesDiff
				risks[!isNotNA,j]<-NA
			}
		}		
	}
	
	rateData[,setdiff(1:numCols,riskInds)]<-expectedRets
	if(doRisks){
		rateData[,riskInds]<-risks
	}
	
	return(rateData)
}

erRiskDomesticEquivalent.convertReturnsMatrix<-function(assetData,baseCc,rfRates,rfCouponsPerYear,cashNameSuffix){
# this function assumes that assetData is made up of returns only.
# assetData is a matrix() of asset returns; baseCc is a string; rfRates is a matrix(); rfCouponsPerYear is a list() indexed by rate name; cashNameSuffix is a string. both assetData and rfRates are assumed to colnames() and rownames(), and their rownames() are dates in the "%Y%m%d" format
# returns matrix() after converting the returns to base currency equivalent

	if(!is.matrix(assetData) || !is.matrix(rfRates)){
		stop("error in erRiskDomesticEquivalent.convertReturnsMatrix(): both assetData and rfRates must be matrices.")
	}
	
	assetNames<-colnames(assetData)
	ratesNames<-colnames(rfRates)
	otherCcs<-setdiff(substr(assetNames,1,2),c(baseCc))
	if(length(otherCcs)==0){
		# nothing to do. all assets are baseCc assets
		return(assetData)
	}
	
	riskInds<-grep("\\.risk",colnames(assetData))
	if(length(riskInds)!=0){
		stop(c("error in erRiskDomesticEquivalent.convertReturnsMatrix(): assetData has .risk columns. the columns are ",paste(names(assetData),collapse=", ")," . you called the wrong function!"))
	}
	
	# %Y%m%d is an integer
	assetDates<-as.numeric(rownames(assetData))
	rfDates<-as.numeric(rownames(rfRates))
	maxDate<-max(assetDates)
	if(maxDate!=max(rfDates)){
		stop(paste("error in erRiskDomesticEquivalent.convertReturnsMatrix(): data not up-to-date. assetData is defined up to",maxDate,"and rfRates is defined up to",max(rfDates)))
	}
	
	# if assetData was obtained from one file and rfRates from another, the minimum dates might/will differ
	minDate<-max(min(assetDates),min(rfDates))
	assetData<-assetData[assetDates>=minDate & assetDates<=maxDate,]
	rfRates <-rfRates[rfDates>=minDate & rfDates <=maxDate,]
	if(nrow(assetData)!=nrow(rfRates)){
		stop("error in erRiskDomesticEquivalent.convertReturns(): assetData and rfRates are not defined for the same dates.")
	}else if(rownames(rfRates)[1]!=rownames(assetData)[1]){
		# we're here if dates in assetData and rfRates are not in the same order. reverse rfRates so it matches assetData
		rfRates<-rfRates[nrow(rfRates):1,]
	}
	
	# find the home country risk free rate
	baseCcRateInd<-grep(paste("^",baseCc,"\\.",sep=""), ratesNames)
	if(!is.finite(baseCcRateInd)){
		stop(paste("error in erRiskDomesticEquivalent.convertReturnsMatrix(): home country",baseCc,"does not have risk free rates"))
	}

	# if we get here, then length(otherCcs) != 0
	baseCouponsPerYear<-rfCouponsPerYear[[paste(baseCc,cashNameSuffix,sep="")]]
	rfRates[,baseCcRateInd]<-(rfRates[,baseCcRateInd]/baseCouponsPerYear+1)^baseCouponsPerYear-1
	for(i in 1:length(otherCcs)){
		cc<- otherCcs[i]
		ccPrefix<-paste("^",cc,"\\.",sep="")
		# find all assets with this country code
		ccIndices<-grep(ccPrefix,assetNames)
		# find the risk free rate to apply for this country code
		ccRateInd<-grep(ccPrefix, ratesNames)
		if(length(ccRateInd)==0){
			stop(paste("error in erRiskDomesticEquivalent.convertReturnsMatrix(): risk free rate for ",cc,"is missing"))
		}
		# convert
		ccCouponsPerYear<-rfCouponsPerYear[[paste(cc,cashNameSuffix,sep="")]]
		rfRates[,ccRateInd]<-(rfRates[,ccRateInd]/ccCouponsPerYear+1)^ccCouponsPerYear-1
		
		ratesDiff<-(1+as.numeric(rfRates[,baseCcRateInd]))/(1+as.numeric(rfRates[,ccRateInd]))
		assetData[,ccIndices]<-(1+assetData[,ccIndices])*ratesDiff - 1.
	}
		
	return(assetData)
}
