# input function
dailyTotalCapitalRets.input<-function(settings){
	suffixes<-settings[["dtcex.listOfInSuffixes"]]
	assets<-settings[["dtcex.assets"]]
	assetIdentifiers<-settings[["dtcex.assetIdentifiers"]]
	useDB<-settings[["dtcex.useDB"]]
	inputDir<-settings[["dtcex.inputDir"]]
	inputDataFile<-settings[["dtcex.inputDataFile"]]
	dbSettings<-settings[["dbSettings"]]
	
	dateFormat<-dbSettings[["ld.dbDateFormat"]]

	allSecs<-utils.assetsByClass(assets,assetIdentifiers)
	eqOrReits<-c(allSecs[["equity"]],allSecs[["reit"]])
	bonds<-allSecs[["bond"]]
	commodities<-allSecs[["commodity"]]
	tips <- allSecs[["tips"]]
	#fxname<-fassetNames[is.element(assetClass,"fx")]

	idsToGet<-utils.generateCrossIds(eqOrReits,suffixes[["re"]])
	numREs<-length(idsToGet)
	
	idsToGet<-c(idsToGet,utils.generateCrossIds(bonds,suffixes[["bond"]]))
	numREsandBonds<-length(idsToGet)
	
	idsToGet<-c(idsToGet,utils.generateCrossIds(commodities,suffixes[["commodity"]]))
	numREsBondsCommodities <- length(idsToGet)
	
	idsToGet<-c(idsToGet,utils.generateCrossIds(tips,suffixes[["tips"]]))
	numREBCT <- length(idsToGet)
	
	if(useDB){
		secData<-utils.getDataFromDB(idsToGet, NULL,dbSettings)
	} else {
		secData<-utils.getDataFromFile(idsToGet, NULL, paste(inputDir,inputDataFile,sep=""),dateFormat)
	}
	
	if(numREs!=0){
		eqReits<-secData[,1:numREs]
	}else{
		eqReits<-NULL
	}
	if(numREsandBonds>numREs){
		bond<-secData[,(numREs+1):(numREsandBonds)]
	}else{
		bond<-NULL
	}
	
	if(numREsBondsCommodities>numREsandBonds){
		commodity<-secData[,(numREsandBonds+1):numREsBondsCommodities]
	}else{
		commodity<-NULL
	}
	
	if(numREBCT > numREsBondsCommodities){
		tipsData <- secData[,(numREsBondsCommodities+1):numREBCT]
	}else{
		tipsData<-NULL
	}

	return(list(secData=list(re=eqReits,bond=bond,commodity=commodity,tips=tipsData),secs=list(eqOrReits=eqOrReits,bonds=bonds,commodities=commodities,tips=tips)))
}

# output function
dailyTotalCapitalRets.output<-function(output,settings){
	outputDir<-settings[["dtcex.outputDir"]]
	outputFile<-settings[["dtcex.outputFile"]]
	numLast<-settings[["dtcex.numLast"]]
	
	write.csv(output,paste(outputDir,outputFile,sep=""),row.names=FALSE)
	write.csv(output[1:numLast,],paste(outputDir,"latest",outputFile,sep=""),row.names=FALSE)
	save(output,file=paste(outputDir,sub("\\.csv",".RData",outputFile),sep=""))
}

dtcex.bondsDCR<-function(ylds,maturity,CPY){
	if(maturity>=1){
		y<-ylds/CPY
		m<-maturity*CPY
		numYlds<-length(y)
		ret<-rep(0, numYlds-1)
		invY<-1/(1+y[1:(numYlds - 1)])
		for(i in 1:m){
			ret<-ret + invY^i
		}
		ret<-y[2:numYlds] * ret + invY^m
		ret<-ret-1  #dcr
	} else ret<-0
	return(ret)
}

dtcex.bondsDTR<-function(dates,ylds,dcr,defaultPremium,CPY){
	dates<-as.Date(dates)
	y<-ylds[2:length(ylds)]

	dtr<-dcr+((1+(y-defaultPremium)/CPY)^(CPY/365.25)-1)*(as.numeric(dates[1:(length(dates)-1)])-as.numeric(dates[2:length(dates)]))


	return(dtr)
}

dtcex.addSuffixesToHeader<-function(assetNames,suffixes,dataFrame){
	numSuffixes<-length(suffixes)
	
	for(i in 1:numSuffixes){
		names(dataFrame)[((i-1)*length(assetNames)+1):(i*length(assetNames))]<-paste(assetNames,suffixes[i],sep="")
	}
	
	return(dataFrame)
}

dtcex.calculateBonds<-function(assetNames,maturity,defaultPremium,ylds,couponsperyear,outSuffixes){
	nRows<-nrow(ylds)-1
	
	dcr<-NULL
	dtr<-NULL

	for(i in 1:ncol(ylds)){
		newDcrCol<-rep(NA,nRows)
		nonNAs<-as.logical(is.finite(ylds[,i]))
		numNonNAs<-sum(as.numeric(nonNAs))

		newDcrCol[1:(numNonNAs-1)]<-dtcex.bondsDCR(ylds[nonNAs,i],maturity[[assetNames[i]]],couponsperyear[[assetNames[i]]])
		dcr<-cbind(dcr,timeSeries(newDcrCol,charvec=NULL))
		
		bondDefaultPremium<-0
		if(is.element(assetNames[i],names(defaultPremium))){
			bondDefaultPremium<-defaultPremium[[assetNames[i]]]
		}
		newDtrCol<-rep(NA,nRows)
		newDtrCol[1:(numNonNAs-1)]<-dtcex.bondsDTR(row.names(ylds)[nonNAs],ylds[nonNAs,i],newDcrCol[1:(numNonNAs-1)], bondDefaultPremium,couponsperyear[[assetNames[i]]])
		dtr<-cbind(dtr,timeSeries(newDtrCol,charvec=NULL))
	}

	bondDtcr<-data.frame(data.frame(dcr),data.frame(dtr))
	bondDtcr<-dtcex.addSuffixesToHeader(assetNames,outSuffixes,bondDtcr)
	bondDtcr<-data.frame(Date=as.character(row.names(ylds)[1:nRows]),bondDtcr)
	
	return(bondDtcr)
}

dtcex.calculateReitEq<-function(assetNames,pricesAndTR,inSuffixes,outSuffixes){
# get relative changes
	
	rets<-NULL
	numAssets<-length(assetNames)
	nRows<-nrow(pricesAndTR)
	for(i in 1:numAssets){
		assetName<-assetNames[i]
		inds<-match(paste(assetName,inSuffixes,sep=""),names(pricesAndTR))
		if(sum(is.finite(inds))!=2){
			stop(paste("error in dtcex.calculateReitEq(): some data for",assetName,"is missing or found more data than expected"))
		}

		nonNAs<-rep(TRUE,nRows)
		nonNAs<- nonNAs & as.logical(is.finite(pricesAndTR[,i])) & as.logical(is.finite(pricesAndTR[,i+numAssets]))
		numNonNAs<-sum(as.numeric(nonNAs))
		tmpPricesAndTR<-data.frame(pricesAndTR[nonNAs,inds])
		row.names(tmpPricesAndTR)<-NULL

		tempRets<-as.matrix(tmpPricesAndTR[1:(numNonNAs-1),])/as.matrix(tmpPricesAndTR[2:numNonNAs,])-1
		row.names(tempRets)<-NULL
		rets<-cbind(rets,rbind(tempRets,matrix(rep(NA,(nRows-numNonNAs)*length(inds)),ncol=length(inds)),deparse.level=0),deparse.level=0)
	}	
	
	ret<-data.frame(rets)
	permutation<-c(2*(1:numAssets)-1,2*(1:numAssets))
	ret<-ret[,permutation]
	ret<-dtcex.addSuffixesToHeader(assetNames,outSuffixes,ret)
	ret<-data.frame(Date=as.character(row.names(pricesAndTR)[1:(nRows-1)]),ret)

	# and return it
	return(ret)
}

dtcex.calculateSimpleDtr<-function(assetNames,pricesonly,listOfOutSuffixes){
	#get relative changes
	rets<-NULL
	numAssets<-length(assetNames)
	nRows<-nrow(pricesonly)
	for(i in 1:numAssets){
		assetName<-assetNames[i]
		inds<-grep(assetName,names(pricesonly))
		if(sum(is.finite(inds))!=1){
			stop(paste("error in dtcex.calculatesimpledtcr(): some data for",assetName,"is missing or found more data than expected"))
		}

		nonNAs<-rep(TRUE,nRows)
		nonNAs<- nonNAs & as.logical(is.finite(pricesonly[,i])) 
		numNonNAs<-sum(as.numeric(nonNAs))
		tmpPricesonly<-data.frame(pricesonly[nonNAs,inds])
		row.names(tmpPricesonly)<-NULL

		tempRets<-as.matrix(tmpPricesonly[1:(numNonNAs-1),])/as.matrix(tmpPricesonly[2:numNonNAs,])-1
		row.names(tempRets)<-NULL
		rets<-cbind(rets,rbind(tempRets,matrix(rep(NA,(nRows-numNonNAs)*length(inds)),ncol=length(inds)),deparse.level=0),deparse.level=0)
	}	
	
	ret<-data.frame(rets)
	
	# add headers after duplicating dtr columns, so dcr = dtr for assets passed to this function
	#ret<-dtcex.addSuffixesToHeader(assetNames,".dtr",ret)
	ret<-dtcex.addSuffixesToHeader(assetNames,listOfOutSuffixes,cbind(ret,ret,row.names=NULL))
	ret<-data.frame(Date=as.character(row.names(pricesonly)[1:(nRows-1)]),ret)

	# and return it
	return(ret)
}

# main logic
dailyTotalCapitalRets.calculate<-function(input,settings){
	inSuffixes<-settings[["dtcex.listOfInSuffixes"]]
	outSuffixes<-settings[["dtcex.listOfOutSuffixes"]]
	secSpecsFile<-settings[["dtcex.securitySpecsFile"]]
	bondSettings<-settings[["dtcex.bondSettings"]]
	
	secData<-input[["secData"]]
	secs<-input[["secs"]]
	
	if(sum(is.finite(match(c("secs","secData"),names(input))))!=2){
		stop("error in dailyTotalCapitalRets(): secs and/or secData missing from input")
	}
	
	eqOrReits<-secs[["eqOrReits"]]
	bonds<-secs[["bonds"]]
	commodityName<-secs[["commodities"]]
	tips<-secs[["tips"]]
	
	pricesAndTR<-secData[["re"]]
	ylds<-secData[["bond"]]
	priceOnly<-secData[["commodity"]]
	#fxonly<-secData[["fx"]]
	trOnly <- secData[["tips"]]

	dtcrEqReit<-dtcex.calculateReitEq(eqOrReits,pricesAndTR,inSuffixes[["re"]],outSuffixes)

	dtcrBonds<-dtcex.calculateBonds(bonds,bondSettings[["durations"]],bondSettings[["defaultPremia"]],ylds,bondSettings[["couponsPerYear"]],outSuffixes)

	dtrCommodity<-dtcex.calculateSimpleDtr(commodityName,priceOnly,outSuffixes)
	#dtrFX<-dtcex.calculateSimpleDtr(fxname,fxpnly,outSuffixes)
	
	dtrTips <- dtcex.calculateSimpleDtr(tips, trOnly, outSuffixes)
	
	#ret<-cbind(dtcrEqReit,dtcrBonds[,2:ncol(dtcrBonds)],dtrCommodity[,2:ncol(dtrCommodity)],dtrFX[,2:ncol(dtrFX)])
	ret<-cbind(dtcrEqReit,dtcrBonds[,2:ncol(dtcrBonds)],dtrCommodity[,2:ncol(dtrCommodity)],dtrTips[,2:ncol(dtrTips)])
	
	return(ret)
}


