# read the portfolios for which we want to calculate VaR's
# returns data.frame() in the format of the input file
varex.readPortfolios<-function(inputDir, portfoliosFileName){
	return(read.csv(paste(inputDir, portfoliosFileName,sep=""),header=TRUE,stringsAsFactors=FALSE))
}

# get the return data to be used in calculating shocks
# assetNames is a vector of strings
# returns timeSeries() of data returns. the header is assetNames, regardless of the data used (.dtr or .dcr or whatever)
varex.readDailyReturns<-function(assetNames, useDB,retSuffix,inputDir,retFileName,dateFormat,dbSettings){
	if(useDB){
		retData<-utils.getDataFromDB(assetNames,retSuffix,dbSettings)
	}else{
		retData<-utils.getDataFromFile(assetNames,retSuffix,paste(inputDir,retFileName,sep=""),dateFormat)
	}
	
	names(retData)<-assetNames
	return(retData)
}

var.input<-function(settings){
	inputDir<-settings[["varex.inputDir"]]
	portfoliosFileName<-settings[["varex.portfoliosFileName"]]
	inputDir<-settings[["varex.inputDir"]]
	retFileName<-settings[["varex.retFileName"]]
	dateFormat<-settings[["varex.dateFormat"]]
	portfolioFirstAssetCol<-settings[["varex.portfolioFirstAssetCol"]]
	useDB<-settings[["varex.useDB"]]
	retSuffix<-settings[["varex.retSuffix"]]
	dbSettings<-settings[["dbSettings"]]
	
	portfolios<-varex.readPortfolios(inputDir, portfoliosFileName)
	assets<-names(portfolios)[portfolioFirstAssetCol:ncol(portfolios)]
	assetRets<-varex.readDailyReturns(assets,useDB,retSuffix,inputDir,retFileName,dateFormat,dbSettings)
	
	return(list(portfolios=portfolios,assets=assets,assetRets=assetRets))
}

var.output<-function(output,settings){
	outputDir<-settings[["varex.outputDir"]]
	portfoliosFileName<-settings[["varex.portfoliosFileName"]]
	outputFileExtension<-settings[["varex.outputFileExtension"]]
	
	write.csv(output,paste(outputDir,sub(".csv","",portfoliosFileName),outputFileExtension,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub(".csv","",portfoliosFileName),outputFileExtension,".RData",sep=""))
}

# dailyReturnData is a timeSeries() with asset returns
# returns timeSeries() with cumulative returns starting from oldest date for data was available for any asset
varex.calculateCumulativeReturns<-function(dailyReturnData){
	assetNames<-names(dailyReturnData)	
	ret<-data.frame(Date=row.names(dailyReturnData))
	for(i in 1:ncol(dailyReturnData)){
		newCol<-rep(NA,nrow(dailyReturnData))
		iRets<-dailyReturnData[,i]
		nonNAs<-as.logical(is.finite(iRets))
		# rev() because data is in reverse chronological order
		temp<-cumprod(rev(1+iRets[nonNAs]))
		# rev() again and scale to make original value 1
		newCol[nonNAs]<-rev(temp)/temp[1]
		ret<-cbind(ret,data.frame(newCol=newCol))
		names(ret)[ncol(ret)]<-assetNames[i]
	}
	
	return(timeSeries(ret[,2:ncol(ret)],charvec=ret$Date))
}

# assets is a vector of asset names; cumRetSeries is a timeSeries() of cumulative returns of those assets
# returns list() indexed by asset, each item of which is a list() indexed by horizon, each item of which is shocks for that horizon. no NA's in returned shocks
varex.calculateAssetShocks<-function(assets,cumRetSeries,horizons, numberOfShocks){
	ret<-NULL
	for(a in assets){
		temp<-cumRetSeries[,a]
		nonNA<-as.logical(is.finite(temp[,1]))
		numNonNAs<-sum(nonNA)
		if(numNonNAs!=sum(as.logical(is.finite(temp[1:numNonNAs,1])))){
			stop(paste("error in varex.calculateAssetShocks(): NA's are intermixed with non-NAs in the cumulative returns of",a,"below date",thisDate))
		}
		
		aShocks<-NULL
		# loop over VaR horizons we're interested in
		for(h in horizons){
			if(numNonNAs>=numberOfShocks+h){
				# we're here therefore we have enough data to calculate the required number of shocks for at least 1 date. h is a number of days. we calculate here all observed h day changes over the history
				hShocks<-as.numeric(temp[1:(numNonNAs-h),1])/as.numeric(temp[(1+h):numNonNAs,1])-1
				# build the list for this asset indexed by horizon
				aShocks<-append(aShocks,list(timeSeries(hShocks,charvec=row.names(temp)[1:(numNonNAs-h)])))
				names(aShocks)[length(aShocks)]<-as.character(h)
			}else{
				print(paste("warning in varex.calculateAssetShocks(): asset",a,"does not have a long enough history at horizon",h))
			}	
		}
		# build overall list indexed by asset
		ret<-append(ret,list(aShocks))
		names(ret)[length(ret)]<-a
	}
	return(ret)
}

# thisDate is a string; effectivePortfolio is the part of the portfolio that has non-zero, non-NA weights; assetShocks is a history of shocks as output by varex.calculateAssetShocks()
# returns list indexed by horizon, each item of which is the required number of shocks (specified by numberOfShocks) to the portfolio, or NULL if not enough history
varex.calculatePortfolioShocks<-function(thisDate,effectivePortfolio,assetShocks, horizons, numberOfShocks){
	ret<-NULL
	for(h in horizons){
		isNULL<-FALSE
		temp<-NULL
		for(a in names(effectivePortfolio)){
			# retrieve historical shocks to asset a at horizon h
			aShocks<-assetShocks[[a]][[as.character(h)]]
			if(is.null(aShocks)){
				isNULL<-TRUE
				break
			}else{
				ind<-match(thisDate,row.names(aShocks))
				if(is.finite(ind)){
					# look for shocks predating thisDate. but first do we have enough shocks left in the history?
					if(nrow(aShocks)-ind+1 >= numberOfShocks){
						temp<-rbind(temp,matrix(aShocks[ind:(numberOfShocks+ind-1)],nrow=1),deparse.level=0)
					}else{
						isNULL<-TRUE
						break
					}
				}else{
					isNULL<-TRUE
					break
				}		
			}
		}
		if(isNULL){
			# we get here if: a) some asset had NULL shocks; or b) not enough shocks were left in the history of shocks of some asset; or c) thisDate was missing from the history of shocks of some asset a altogether
			ret<-append(ret,list(NULL))
		}else{
			# we're good; the shocks to the portfolio can be obtained by a matrix multiplication of portfolio weights and historical shocks to individual assets. the shocks to the individual assets are in the ROWS of temp. see rbind() above
			hShocks<-matrix(as.numeric(effectivePortfolio),nrow=1)%*%temp
			ret<-append(ret,list(hShocks))
		}
		names(ret)[length(ret)]<-as.character(h)
	}
	
	return(ret)
}

# portfolioShocks is a list() indexed by horizon with historical shocks to the portfolio as output by varex.calculatePortfolioShocks()
# return list() of VaR numbers for portfolio.
varex.quantile<-function(portfolioShocks, confidenceLevels){
	ret<-NULL
	for(h in names(portfolioShocks)){
		if(!is.null(portfolioShocks[[h]])){
			vars<-quantile(portfolioShocks[[h]],probs=confidenceLevels)
		}else{
			vars<-rep(NA,length(confidenceLevels))
		}
		# assign names in the format VaR30D95. 30-day horizon and 95% confidence level
		names(vars)<-paste("VaR",h,"D",paste(round(100-confidenceLevels*100),sep=""),sep="")
		ret<-append(ret,as.list(vars))
	}
	return(ret)
}

# assets is a vector of strings; portfolios is data.frame(); assetCumRets is a timeSeries() of cumulative returns of the assets in portfolios
# returns a data.frame() containing the original portfolios with VaR columns inserted
varex.calculateVar<-function(assets,portfolios,assetCumRets, horizons, numberOfShocks, confidenceLevels,portfolioFirstAssetCol){
	dates<-unique(portfolios$Date)
	assetShocks<-varex.calculateAssetShocks(assets,assetCumRets, horizons, numberOfShocks)	
	ret<-NULL
	for(d in dates){
		# find all portfolios with this date stamp
		inds<-grep(d,portfolios$Date)
		for(i in inds){
			# the first few column are not portfolio weights
			portfolioWeights<-portfolios[i,portfolioFirstAssetCol:ncol(portfolios)]
			# only use this part of the portfolio that is non-zero, non-NA weights
			effectivePortfolio<-portfolioWeights[is.finite(as.numeric(portfolioWeights))&(as.numeric(portfolioWeights)!=0)]
			# calculate portfolio shocks at ALL horizons as specified in horizons
			portfolioShocks<-varex.calculatePortfolioShocks(d,effectivePortfolio,assetShocks, horizons, numberOfShocks)
			if(portfolioFirstAssetCol>1){
				# insert VaR numbers. the number of these is specified by confidenceLevels
				ret<-rbind(ret,data.frame(portfolios[i,1:(portfolioFirstAssetCol-1)],varex.quantile(portfolioShocks, confidenceLevels),portfolios[i,portfolioFirstAssetCol:ncol(portfolios)]))
			}else{
				ret<-rbind(ret,data.frame(var=varex.quantile(portfolioShocks, confidenceLevels),portfolios[i,]))
			}
		}
	}
	
	return(ret)
}

var.calculate<-function(input,settings){
	horizons<-settings[["varex.horizons"]]
	numberOfShocks<-settings[["varex.numberOfShocks"]]
	confidenceLevels<-settings[["varex.confidenceLevels"]]
	portfolioFirstAssetCol<-settings[["varex.portfolioFirstAssetCol"]]
	
	portfolios<-input[["portfolios"]]
	assets<-input[["assets"]]
	assetRets<-input[["assetRets"]]
	
	assetCumRets<-varex.calculateCumulativeReturns(assetRets)

	# main calculation
	portfolioVaRs<- varex.calculateVar(assets,portfolios,assetCumRets, horizons, numberOfShocks, confidenceLevels,portfolioFirstAssetCol)
	
	return(portfolioVaRs)
}


