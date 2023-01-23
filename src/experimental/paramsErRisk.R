



###############################
### ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG ACHTUNG 
## This file is just the old erRisk.R used here for experimenting with params.R
###############################






library(timeSeries)
options(scipen=999)

# these settings particular to my set up on this machine
#errisk.inputDir<-"c:/DailyRisk/runtime/Rcode/"
#errisk.outputDir<-"c:/DailyRisk/runtime/Rcode/"

isRob<-TRUE

if(isRob){
errisk.inputDir<-"d:/SA2/Input/"
errisk.outputDir<-"d:/SA2/Output/"
errisk.pathToRSourceCode<-"d:/SA2/src/utils/"
	print("hello rob")
}else{
errisk.inputDir<-"/Users/sidani/Documents/majeds_stuff/temp/"
errisk.outputDir<-"/Users/sidani/Documents/majeds_stuff/temp/"
errisk.pathToRSourceCode<-"/Users/sidani/devel/sa2/trunk/src/utils/"
}


ld.dbDateFormat<-"%Y-%m-%d"
errisk.dateFormat<-"%Y-%m-%d"


errisk.useDB<-TRUE
#    errisk.useDB<-FALSE

# connection/database info
ld.uid<-"majed"
ld.dsnName<-"ProductionRates"
ld.rateDescrTable<-"tbl_rate_header"
ld.rateDataTable<-"tbl_rate_data"

source(paste(errisk.pathToRSourceCode, "dbDataLoader.R",sep=""))

# end special settings

errisk.read.input<-function(pathToInputFile){
	# read input data: this results in an R "data frame"
	inDataFrame<-read.table(pathToInputFile,stringsAsFactors=FALSE,row.names=1,sep=",")
	inDataFrame<-data.frame(t(inDataFrame),stringsAsFactors=FALSE)

	inList<-as.list(inDataFrame)
	names(inList)<-names(inDataFrame)
	return(inList)
}

inList<-errisk.read.input(paste(errisk.inputDir,"ERRISK-INPUTS.csv",sep=""))

errisk.assetHeaders<-inList[["errisk.assetHeaders"]]
errisk.inflation<-as.numeric(inList[["errisk.inflation"]])
errisk.LongTermGrowth<-as.numeric(inList[["errisk.LongTermGrowth"]])
errisk.LTPEtoPeak<-as.numeric(inList[["errisk.LTPEtoPeak"]])
errisk.haircut<-as.numeric(inList[["errisk.haircut"]])
errisk.bloombergfld<-inList[["errisk.bloombergfld"]]
errisk.usetrendadjearnings<-as.logical(inList[["errisk.usetrendadjearnings"]])
errisk.assetclass<-inList[["errisk.assetclass"]]
errisk.yearsduration<-as.numeric(inList[["errisk.yearsduration"]])
errisk.defaultpremium<-as.numeric(inList[["errisk.defaultpremium"]])
errisk.illiquidityspread<-as.numeric(inList[["errisk.illiquidityspread"]])
errisk.couponsperyear<-as.numeric(inList[["errisk.couponsperyear"]])

# the code for equities below assumes that the suffixes are written in THIS order: empty string, .earnings, .dvd.yld
errisk.listOfSuffixes<-list(equity=c("",".earnings",".dvd.yld"),reit=c(".ev",".evebitda",".netdebt"),bond=c(""))
errisk.listOfAssets<-c("equity","reit","bond")

errisk.getEarnings<-function(haircut,assetHeaders,secData){
	numAssets<-ncol(secData)/3
	earningsData<-secData[,(numAssets+1):(2*numAssets)]
	nrows<-nrow(secData)
	assetNames<-names(secData)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)
		
		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(earningsData[,i]))
		newCol[nonNAs]<-haircut[j]*earningsData[nonNAs,i]
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}


errisk.getPtoEPeak<-function(secData,secEarnings){
	numAssets<-ncol(secData)/3
	secPrices<-secData[,1:numAssets]

	ret<-NULL
	for(i in 1:numAssets){
		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secPrices[,i])&is.finite(secEarnings[,i]))
		newCol[nonNAs]<-secPrices[nonNAs,i]/rev(cummax(rev(secEarnings[nonNAs,i])))
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}

errisk.calcEquityER<-function(secPtoEPeak,ltg,ltpe,assetHeaders,secData){
	numAssets<-ncol(secData)/3
	secDvds<-secData[,(2*numAssets+1):ncol(secData)]
	assetNames<-names(secData)[1:numAssets]

	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)

		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secPtoEPeak[,i])&is.finite(secDvds[,i]))

		newCol[nonNAs]<-(1+ltg[j])*(ltpe[j]/secPtoEPeak[nonNAs,i])^.1-1+secDvds[nonNAs,i]*(1+secPtoEPeak[nonNAs,i]/ltpe[j])/2
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)

	return(ret)
}

errisk.calcEquityRisk<-function(secPtoEPeak,inflation,assetHeaders){
	numAssets<-ncol(secPtoEPeak)
	assetNames<-names(secPtoEPeak)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)
		
		newCol<-rep(NA,nrow(secPtoEPeak))
		nonNAs<-as.logical(is.finite(secPtoEPeak[,i]))
		newCol[nonNAs]<-1/(inflation[j]+1/secPtoEPeak[nonNAs,i])
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secPtoEPeak)[1:numAssets]
	row.names(ret)<-row.names(secPtoEPeak)
	return(ret)
}

errisk.calcReitER<-function(ltg,ltpe,durations,assetHeaders,secData){
	numAssets<-ncol(secData)/3
	assetNames<-names(secData)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)

		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secData[,i]))
		leverage<-secData[nonNAs,i+2*numAssets]/secData[nonNAs,i]
		evebitda<-secData[nonNAs,i+numAssets]
		newCol[nonNAs]<-(1+ltg[j])*(((ltpe[j]/evebitda)^(1/durations[j])-1)/(1-leverage)+1)-1+1/evebitda*(1+evebitda/ltpe[j])/2
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}

errisk.calcReitRisk<-function(inflation,assetHeaders,secData){
	numAssets<-ncol(secData)/3
	assetNames<-names(secData)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)
		
		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secData[,i]))
		newCol[nonNAs]<-1/(inflation[j]+1/secData[nonNAs,i+numAssets])
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}

errisk.calcBondER<-function(defaults,assetHeaders,secData,CPY){
	numAssets<-ncol(secData)
	assetNames<-names(secData)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)
		
		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secData[,i]))
		newCol[nonNAs]<-(((secData[nonNAs,i]-defaults[j])/CPY[j]+1)^CPY[j]-1)
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}


#modified duration of annual coupon par bond, with maturity an integer years
errisk.duration<-function(yld,maturity){
	y<-yld
	m<-maturity
	cfPeriods<-1:m
	ret<-c()
	for(i in 1:length(y)){
		dfs<-1/(1+y[i])^cfPeriods
		ret<-c(ret,(y[i]*sum(cfPeriods*dfs)+m/(1+y[i])^m)/(1+y[i]))
	}
	return(ret)
}


errisk.calcBondRisk<-function(defaults,spreads,durations,assetHeaders,secData,CPY){
	numAssets<-ncol(secData)
	assetNames<-names(secData)[1:numAssets]
	
	ret<-NULL
	for(i in 1:numAssets){
		j<-match(assetNames[i],assetHeaders)
		
		newCol<-rep(NA,nrow(secData))
		nonNAs<-as.logical(is.finite(secData[,i]))
		adjYld<-((secData[nonNAs,i]-defaults[j]-spreads[j])/CPY[j]+1)^CPY[j]-1
		newCol[nonNAs]<-errisk.duration(adjYld,durations[j])
		ret<-cbind(ret,timeSeries(newCol,charvec=NULL))
	}
	
	names(ret)<-names(secData)[1:numAssets]
	row.names(ret)<-row.names(secData)
	return(ret)
}

errisk.segregate<-function(instrByAssetClass,tempResults,listOfSuffixes){
	# remove ".trendadj" from columns names
	names(tempResults)<-gsub(".earnings.trendadj",".earnings",names(tempResults))
	
	counter<-1
	ret<-list()
	for(i in 1:length(instrByAssetClass)){
		assetClass<-names(instrByAssetClass)[i]
		ids<-instrByAssetClass[[i]]
		names(tempResults)[counter:(counter+length(ids)-1)]<-ids
		numSuffixes<-length(listOfSuffixes[[assetClass]])
		newList<-list(tempResults[,counter:(counter+length(ids)*numSuffixes-1)])
		names(newList)<-assetClass
		ret<-append(ret,newList)
		
		counter<-counter+length(ids)*numSuffixes
	}
	
	return(ret)
}

errisk.stripSuffixes<-function(ids,listOfSuffixes){
	for(i in 1:length(listOfSuffixes)){
		suffixes<-listOfSuffixes[[i]]
		for(j in 1:length(suffixes)){
			ids<-gsub(suffixes[j],"",ids)
		}
	}
	ids<-gsub(".trendadj","",ids)
	return(unique(ids))
}
	
# this reads the data from a file; the first column of the file should contain dates; the next numAssets columns should contain the er's; and the numAssets columns after those, the risks; er's and risks should be in the same asset order
errisk.getDataFromFile<-function(inputDir,inputFileName,listOfIds,assetClasses,listOfAssets,listOfSuffixes,useAdjEarnings){
	rateData<-read.csv(paste(inputDir,inputFileName,sep=""),header=TRUE,stringsAsFactors=FALSE,strip.white=TRUE)
	rateData$Date<-as.Date(rateData$Date,format=errisk.dateFormat)

	secIds<-errisk.stripSuffixes(names(rateData),listOfSuffixes)
	secFields<-names(rateData)
	
	tempResults<-NULL
	instrByAssetClass<-list()
	for(a in 1:length(listOfAssets)){
		allClassIds<-listOfIds[assetClasses==listOfAssets[a]]
		instruments <-secIds[is.element(secIds,allClassIds)]
		instrUseAdjEarnings<-useAdjEarnings[is.element(allClassIds,instruments)]
		
		instrByAssetClass<-append(instrByAssetClass,list(instruments))
		names(instrByAssetClass)[a]<-listOfAssets[a]
		instrSuffixes<-listOfSuffixes[[listOfAssets[a]]]
		for(i in 1:length(instrSuffixes)){
			for(j in 1:length(instruments)){
				if(!instrUseAdjEarnings[j] || instrSuffixes[i]!=".earnings"){
					secId<-paste(instruments[j], instrSuffixes[i],sep="")
				}else if(instrUseAdjEarnings[j] &&listOfAssets[a]=="equity"){
					secId<-paste(instruments[j],".earnings.trendadj",sep="")
				}else{
					stop("error in errisk.getDataFromFile(): I shouldn't be here; are we looking for the earnings of a non-equity asset?")
				}
				index<-match(secId,secFields)
				if(is.finite(index)){
					tempResults<-cbind(tempResults,rateData[,index])
				}else{
					stop(paste("error in errisk.getDataFromFile(): not all data for",instruments[j],"is available"))
				}
			}
		}
	}
		

	tempResults<-timeSeries(tempResults,charvec=rateData$Date)
	return(errisk.segregate(instrByAssetClass,tempResults,listOfSuffixes))
}

# get data from db; listOfIds is a vector of asset names; we append a suffix to each asset name and then submit the db request
# returns a timeSeries object in reverse chronological order
errisk.getDataFromDB<-function(listOfIds,assetClasses,listOfAssets,listOfSuffixes,useAdjEarnings){
	idsToGet<-c()
	instrByAssetClass<-list()
	for(a in 1:length(listOfAssets)){
		instruments<-listOfIds[assetClasses==listOfAssets[a]]
		instrByAssetClass<-append(instrByAssetClass,list(instruments))
		names(instrByAssetClass)[a]<-listOfAssets[a]
		instrSuffixes<-listOfSuffixes[[listOfAssets[a]]]
		for(i in 1:length(instrSuffixes)){
			for(j in 1:length(instruments)){
				if(!useAdjEarnings[j] || instrSuffixes[i]!=".earnings"){
					idsToGet<-c(idsToGet,paste(instruments[j], instrSuffixes[i],sep=""))
				}else if(useAdjEarnings[j] && listOfAssets[a]=="equity"){
					idsToGet<-c(idsToGet,paste(instruments[j],".earnings.trendadj",sep=""))
				}else{
					stop("error in errisk.getDataFromDB(): I shouldn't be here; are we looking for the earnings of a non-equity asset?")
				}
			}
		}
	}

	tempResults<-ld.getDataList(idsToGet)
	
	return(errisk.segregate(instrByAssetClass,tempResults,listOfSuffixes))
}

# main function
errisk.getERRisks<-function(inputDir,inputFileName,outputDir){
	onlyassetsbeingrun<-unique(errisk.assetclass)
	if(errisk.useDB){
		allData<-errisk.getDataFromDB(errisk.assetHeaders,errisk.assetclass,onlyassetsbeingrun,errisk.listOfSuffixes,errisk.usetrendadjearnings)
	}else{
		#TODO have to break data into asset classes
		allData<-errisk.getDataFromFile(inputDir,inputFileName,errisk.assetHeaders,errisk.assetclass,onlyassetsbeingrun,errisk.listOfSuffixes,errisk.usetrendadjearnings)
	}
	
	errisk.haircut[errisk.usetrendadjearnings]<-1

	outputER<-NULL
	outputRisks<-NULL
	inputdata<-NULL
	
	for(a in 1:length(onlyassetsbeingrun)){
		assetClass<-onlyassetsbeingrun[a]
		secData<-allData[[assetClass]]

		if(assetClass=="equity"){

			secEarnings<-errisk.getEarnings(errisk.haircut,errisk.assetHeaders,secData)
			ptoEPeak<-errisk.getPtoEPeak(secData,secEarnings)
			ERs<-errisk.calcEquityER(ptoEPeak,errisk.LongTermGrowth,errisk.LTPEtoPeak,errisk.assetHeaders,secData)
			names(ERs)<-paste(names(ERs),".er",sep="")

			Risks<-errisk.calcEquityRisk(ptoEPeak,errisk.inflation,errisk.assetHeaders)
			names(Risks)<-paste(names(Risks),".risk",sep="")
		}else if(assetClass=="reit"){
			ERs<-errisk.calcReitER(errisk.LongTermGrowth,errisk.LTPEtoPeak,errisk.yearsduration,errisk.assetHeaders,secData)
			names(ERs)<-paste(names(ERs),".er",sep="")

			Risks<-errisk.calcReitRisk(errisk.inflation,errisk.assetHeaders,secData)
			names(Risks)<-paste(names(Risks),".risk",sep="")
		}else if(assetClass=="bond"){
			ERs<-errisk.calcBondER(errisk.defaultpremium,errisk.assetHeaders,secData,errisk.couponsperyear)
			names(ERs)<-paste(names(ERs),".er",sep="")
			Risks<-errisk.calcBondRisk(errisk.defaultpremium,errisk.illiquidityspread,errisk.yearsduration,errisk.assetHeaders,secData,errisk.couponsperyear)
			names(Risks)<-paste(names(Risks),".risk",sep="")
		}else{
			print(paste("error: unknown asset class",assetClass))
			stop()
		}
		if(is.null(outputER)){ 
			outputER<-data.frame(Date=row.names(ERs),data.frame(ERs,row.names=NULL))
			outputRisks<-data.frame(Risks,row.names=NULL)
			inputdata<-data.frame(Date=row.names(secData),data.frame(secData,row.names=NULL))
		}
		else
		{
			outputER<-cbind(outputER,(data.frame(ERs,row.names=NULL)))
			outputRisks<-cbind(outputRisks,(data.frame(Risks,row.names=NULL)))
			inputdata<-cbind(inputdata,data.frame(secData,row.names=NULL))
		}

	}
	output<-cbind(outputER,outputRisks)

	write.csv(inputdata,paste(outputDir,"rawInputs.csv",sep=""),row.names=FALSE)
	write.csv(output,paste(outputDir,"ersRisks.csv",sep=""),row.names=FALSE)
	write.csv(output[1:20,],paste(outputDir,"ersRisks-LAST20DAYS.csv",sep=""),row.names=FALSE)
	return
}

#execute
###########################errisk.getERRisks(errisk.inputDir,"rsp500.csv",errisk.outputDir)


