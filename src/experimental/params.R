#!/usr/bin/RScript --vanilla --default-packages=utils

library(MASS)
library(bootstrap)
options(stringsAsFactors=FALSE)
isRob<-TRUE

if(isRob){
	params.pathToRSourceCode<-"d:/SA2/src/"
	params.pathToInput<-"d:/SA2/Input/"
	params.pathToOutput<-"d:/SA2/Output/"
	print("hello rob")
}else{
	params.pathToRSourceCode<-"/Users/sidani/devel/sa2/trunk/src/"
	params.pathToInput<-"/Users/sidani/devel/temp/"
	params.pathToOutput<-"/Users/sidani/devel/temp/"
}

params.plotModels<-TRUE
params.numberOfColumnsToPlot<-9 # starting from each "new.*" column and including it, put this many columns in the graph

###### set assets to test here ##########

# assets we want to run

#params.assets <-c("us.equity","us.dji","us.nasdaq100","us.snp400","us.snp600","us.djutil","us.msciworld","us.mscieafe","us.msciem","jp.equity")
#params.assets <-c("us.dji","us.djutil","us.equity","us.msciworld","us.mscieafe","us.snp400","us.snp600","eu.stxe600","ge.cdax","fr.sbf250","sw.equity","it.mib40","sz.equity","au.equity","au.asx200","uk.property","uk.asx","ca.equity","hk.equity","jp.equity")
#params.assets <-c("us.equity","us.dji","us.nasdaq100","us.nasdaqcomp","us.russell2000","us.snp400","us.snp600","us.djutil","ca.equity","us.msciworld","us.mscieafe","us.msciem","us.mscilatin","us.msciasiapacxjpn","us.rafi","eu.rafi","jp.rafi","eu.estx50","eu.stxe600","eu.eurostx","eu.stxe50","eu.utileurope","eu.euroutil","uk.equity","uk.asx","uk.ftse250","ge.cdax","sw.equity","sw.omx30","fr.sbf250","fr.cac40","sp.ibex35","it.mib40","sz.equity","jp.equity","jp.EPG","jp.topix400","jp.n225","hk.equity","au.equity","au.asx200","uk.property","au.property","eu.epraeurozone","eu.deveuropeepra","ge.equity","no.equity","ru.equity","kr.equity","kr.kospi200","tw.equity","sg.equity","sg.msci","nz.equity","in.equity","ch.equity","pt.psi20","pt.equity","gr.equity","nl.equity","at.equity","ch.szcomp","ch.shcomp","my.equity","id.equity","id.j45","th.equity","th.set50","us.snp100","us.snp500inft")
#params.assets <-c("us.equity","us.dji","us.nasdaq100","us.nasdaqcomp","us.russell2000","us.snp400","us.snp600","us.djutil","us.snp100","us.snp500inft","ca.equity","us.msciworld","us.mscieafe","us.msciem","us.mscilatin","us.msciasiapacxjpn","us.rafi","eu.rafi","jp.rafi","eu.stxe600","eu.eurostx","eu.utileurope","eu.euroutil","uk.equity","uk.asx","uk.ftse250","ge.cdax","sw.equity","sw.omx30","fr.sbf250","fr.cac40","it.mib40","sz.equity","jp.equity","jp.EPG","jp.topix400","hk.equity","au.equity","au.asx200","uk.property","au.property","eu.epraeurozone","sg.equity")
#params.assets <-c("us.dji","us.equity","us.msciworld","sz.equity","uk.property","ca.equity","sp.ibex35","kr.equity","kr.kospi200","tw.equity","nz.equity","in.equity","ch.equity","nl.equity","ch.szcomp","ch.shcomp","id.equity","id.j45","th.equity","th.set50")
params.assets <-c("ko.kospi200")
# ran with success 
# "us.equity","us.dji","us.nasdaq100","us.nasdaqcomp","us.russell2000","us.snp400","us.snp600","us.djutil","ca.equity","us.msciworld","us.mscieafe","us.msciem","us.mscilatin","us.msciasiapacxjpn","us.rafi","eu.rafi","jp.rafi","eu.stxe600","eu.eurostx","eu.utileurope","eu.euroutil","uk.equity","uk.asx","uk.ftse250","ge.cdax","sw.equity","sw.omx30","fr.sbf250","fr.cac40","it.mib40","sz.equity","jp.equity","jp.EPG","jp.topix400","hk.equity","au.equity","au.asx200","uk.property","au.property","eu.epraeurozone","sg.equity","us.snp100","us.snp500inft"

#failed on "eu.estx50","eu.stxe50"  error was Error: only 0's may be mixed with negative subscripts
#failed on "sp.ibex35" Error in utils.getDataFromFile(paste(cc, ".gdp", sep = ""), NULL, gdpFileNameQuarterly,  : error in utils.getDataFromFile(): sp.gdp was/were not found in file c:/SA2/Input/GDP-new.csv
#failed on "jp.n225" Error in utils.getDataFromFile(paste(assets, ".dtr", sep = ""), NULL,  :   error in utils.getDataFromFile(): jp.n225.dtr was/were not found in file c:/SA2/Input/dTrCr.csv
#failed on "eu.deveuropeepra" Error in errisk.getDataFromFile(errisk.inputDir, "rawInputs.csv", asset,  :   error in errisk.getDataFromFile(): not all data for NA is available
#failed on "ge.equity" Error in utils.getDataFromFile(paste(assets, ".dtr", sep = ""), NULL,  :  error in utils.getDataFromFile(): ge.equity.dtr was/were not found in file c:/SA2/Input/dTrCr.csv
#failed on "no.equity", "ru.equity","kr.equity","kr.kospi200","tw.equity","nz.equity","in.equity","ch.equity","pt.psi20","pt.equity","gr.equity","nl.equity","at.equity","ch.szcomp","ch.shcomp","my.equity","id.equity","id.j45","th.equity","th.set50", Error in utils.getDataFromFile(paste(cc, ".gg.210curve", sep = ""), NULL,  :   error in utils.getDataFromFile(): no.gg.210curve was/were not found in file c:/SA2/Input/slopes.csv
#failed on "sg.msci", Error in params.rateOfReturn(dtrData, as.character(row.names(ptoEPeak)[nrow(ptoEPeak)]),  :  error in params.rateOfReturn(): 1996-03-29 was not found in dtrHist


#params.assets <-c("eu.stxe600","ge.cdax","fr.sbf250","uk.asx","us.dji","us.equity","us.msciworld","us.mscieafe","hk.equity","jp.equity")

#params.assets <-c("ge.cdax","fr.sbf250","sw.equity","au.equity","au.asx200","uk.property","uk.asx","ca.equity","us.equity","us.msciworld","us.mscieafe","hk.equity","jp.equity")#params.assets <-c("jp.equity")####"uk.asx","uk.equity","uk.FTSE250","uk.property","au.equity","au.asx200","au.property")
#params.assets <-c("us.equity","us.dji","us.nasdaq100","us.nasdaqcomp","us.russell2000","us.snp400","us.snp600","us.djutil","us.msciworld","us.mscieafe","us.msciem","us.mscilatin","us.snp500inft","jp.equity")
#params.assets <-c("eu.stxe600")####"uk.asx","uk.equity","uk.FTSE250","uk.property","au.equity","au.asx200","au.property")

# all CPI info in this file 
params.inflationFile<-c("CPI-new.csv")
# all countries GDP info is in this one file
params.gdpFileNameQuarterlyFiles<-c("GDP-new.csv")
# and in this for older data
params.gdpFileNameYearlyFiles<-c("gdp-yearly-GFD.csv")

params.gniFileName<-c("GNI.csv")
params.useGNI<-c()

#########################################

params.countriesWithQuarterlyInflation <- c()

# SETTINGS here

params.unitsPerYear<-12######260.24
params.periodInMonths<-120######round(120*260.24/12)  # in months
params.exponent<-params.unitsPerYear/params.periodInMonths
params.halfLife<-Inf
params.bDaysPerYear<-261
params.numGDPYears<-10
params.numberOfMonthsForInflation<-params.periodInMonths########################36

params.adjustEarningsGrowthForInflation<-TRUE
params.useActualPeriod<-FALSE # use actual period between peaks?

params.inflationFileName<-paste(params.pathToInput, params.inflationFile,sep="")
params.gdpFileNameQuarterly<-paste(params.pathToInput, params.gdpFileNameQuarterlyFiles,sep="")
params.gdpFileNameYearly<-paste(params.pathToInput, params.gdpFileNameYearlyFiles,sep="")
params.slopesFileName<-paste(params.pathToInput,"slopes.csv",sep="")

params.extrapolateGDP<-TRUE

params.numAveragedEarnings<-2 # in years
#params.dependsOn<-list(uk.equity="uk.asx",us.equity="us.dji",uk.ftse250="uk.asx",us.snp100="us.equity",us.snp400="us.equity",us.snp600="us.equity",us.rafi="us.equity",eu.eurostx="eu.stxe600",eu.rafi="eu.stxe600",jp.EPG="jp.equity",jp.topix400="jp.equity",jp.rafi="jp.equity",fr.cac40="fr.sbf250")
params.dependsOn<-list(it.mib40="ge.cdax",eu.stxe600="ge.cdax",eu.eurostx="eu.stxe600",eu.rafi="ge.cdax",eu.utileurope="eu.stxe600",eu.euroutil="eu.stxe600",sg.equity="us.dji")
params.dependsOn<-list(it.mib40="ge.cdax",eu.stxe600="ge.cdax",eu.eurostx="eu.stxe600",eu.rafi="ge.cdax",eu.utileurope="eu.stxe600",eu.euroutil="eu.stxe600",us.equity="us.dji",us.msciworld="us.equity",sg.equity="us.msciworld",sp.ibex35="us.equity",tw.equity="us.equity")
params.ltpesLtgs<-NULL

#params.growthBenchmarks<-list(us.snp400="us.equity",us.nasdaq100="us.equity",us.nasdaqcomp="us.equity",us.snp600="us.equity",us.djutil="us.equity",us.rafi="us.equity",uk.ftse250="uk.asx",eu.utileurope="eu.stxe600",eu.euroutil="eu.stxe600",jp.EPG="jp.equity",jp.topix400="jp.equity",jp.rafi="jp.equity",uk.property="uk.asx",au.property="au.equity")
params.growthBenchmarks<-list(it.mib40="ge.cdax",eu.stxe600="ge.cdax",eu.eurostx="eu.stxe600",eu.rafi="ge.cdax",eu.utileurope="eu.stxe600",eu.euroutil="eu.stxe600",us.equity="us.dji",sg.equity="us.msciworld",sp.ibex35="us.equity",tw.equity="us.equity")
#params.growthBenchmarks<-list(it.mib40="ge.cdax",eu.stxe600="ge.cdax",eu.eurostx="eu.stxe600",eu.rafi="ge.cdax",eu.utileurope="eu.stxe600",eu.euroutil="eu.stxe600",sg.equity="us.equity"
params.globalSecData<-NULL
params.regEarningsPeriodInMonths<-120

params.useRegs<-"xPlusAlpha" ## or "multilinear"

# correlations/covariances settings

params.rollingWindow<-12 # in months
params.corCovMethod<-"pearson" # can also be "spearman" or "kendall"

# end SETTINGS


####### code starts below this line 

source(paste(params.pathToRSourceCode,"experimental/paramsErRisk.R",sep=""))
source(paste(params.pathToRSourceCode,"experimental/paramsPlot.R",sep=""))
source(paste(params.pathToRSourceCode,"utils/miscUtils.R",sep=""))

params.regressXPlusAlpha<-function(x,y){
	return(rlm(matrix(rep(1,length(x)),ncol=1),y-x,maxit=500)$coefficients)
}

params.regressMultilinear<-function(x,y){
	##return(lm.fit(cbind(x,rep(1,nrow(x)),deparse.level=0),y)$coefficients)
	return(rlm(x,y,maxit=500)$coefficients)
}

params.getGeoOrArithRet<-function(v1,v2,periodInMonths){
	ret<-v2/v1
	postvs<-ret>=0
	ret[postvs]<-ret[postvs]^(12/periodInMonths)-1
	ret[!postvs]<-(ret[!postvs]-1)*(12/periodInMonths)
	
	return(ret)	
}

params.getGrowthReg<-function(benchEarnings,earnings,ltgs,infls){
	if(params.useRegs=="xPlusAlpha" ){ ######## && !is.null(benchEarnings)
		numPoints<-min(length(benchEarnings),length(earnings))
		if(numPoints<= params.regEarningsPeriodInMonths){
			return(mean(ltgs)+infls[1])
		}
		benchEarningsGrowth<-params.getGeoOrArithRet(benchEarnings[(1+ params.regEarningsPeriodInMonths):length(benchEarnings)],benchEarnings[1:(length(benchEarnings)-params.regEarningsPeriodInMonths)],params.regEarningsPeriodInMonths)
		earningsGrowth<-params.getGeoOrArithRet(earnings[(1+ params.regEarningsPeriodInMonths):length(earnings)],earnings[1:(length(earnings)-params.regEarningsPeriodInMonths)],params.regEarningsPeriodInMonths)
	
		numPoints<-min(length(benchEarnings)-params.regEarningsPeriodInMonths,length(earnings)-params.regEarningsPeriodInMonths)
		alpha<-params.regressXPlusAlpha(benchEarningsGrowth[1:numPoints], earningsGrowth[1:numPoints])
		return(mean(ltgs)+alpha+infls[1])
	}else {
stop("error in params.getGrowthReg(): multilinear approach needs more work. stopping...")
		numPoints<-length(earnings)
		if(numPoints<=params.regEarningsPeriodInMonths+2){ # "+2" because we have more parameters to determine here
			return(mean(ltgs)+infls[1])
		}
		earningsGrowth<-(earnings[1:(length(earnings)-params.regEarningsPeriodInMonths)]/earnings[(1+ params.regEarningsPeriodInMonths):length(earnings)])^(12/params.regEarningsPeriodInMonths)-1
		numPoints<-length(earnings)-params.regEarningsPeriodInMonths
		coefficients<-params.regressMultilinear(cbind(matrix(ltgs[1:numPoints],ncol=1),infls[1:numPoints],deparse.level=0),earningsGrowth[1:numPoints])
if(sum(is.finite(coefficients))!=2){
	print(earningsGrowth[1:numPoints])
	print(ltgs[1:numPoints])
	print(infls[1:numPoints])
}
		return(coefficients[1]*ltgs[1]+coefficients[2]*infls[1])
	}
}

# rate for periodInMonths and cumulative rets
params.rateOfReturn<-function(dtrHist,firstDate,periodInMonths,unitsPerYear){
	# NA's are expected to have been removed in the calling function
	cumRets<-cumprod(rev(1+dtrHist[,1]))	
	cumRets<-timeSeries(rev(cumRets),charvec=row.names(dtrHist))
	cumRetsEOM<-utils.getEOMData(cumRets,TRUE)
	
	index<-match(firstDate,row.names(cumRetsEOM))
	if(!is.finite(index)){
		stop(paste("error in params.rateOfReturn():",firstDate,"was not found in dtrHist"))
	}
	cumRetsEOM <-cumRetsEOM[1:index,]
	
	retsPerPeriod<-(as.numeric(cumRetsEOM[1:(nrow(cumRetsEOM)-periodInMonths),])/as.numeric(cumRetsEOM[(periodInMonths+1):nrow(cumRetsEOM),]))^(unitsPerYear/periodInMonths)-1.

	return(list(ror=timeSeries(retsPerPeriod,charvec=row.names(cumRetsEOM)[1:(nrow(cumRetsEOM)-periodInMonths)]),cum=cumRetsEOM))
}

params.dtr<-function(assets){
	return(utils.getDataFromFile(paste(assets,".dtr",sep=""),NULL,paste(errisk.inputDir,"dTrCr.csv",sep=""),"%Y-%m-%d"))
}

params.readCPIData<-function(inflationFileName,cc){
	inflationData<-utils.getDataFromFile(paste(cc,".cpi",sep=""),NULL,inflationFileName,"%Y-%m-%d")
	return(inflationData[as.logical(is.finite(inflationData[,1])),1])
}

params.zscore<-function(observations){
	return(utils.zscore(observations))
}

# yoy returns
params.getGDPSeries<-function(cc,gdpFileNameQuarterly,gdpFileNameYearly){
	if(!is.element(cc,params.useGNI)){
		gdpDataQuarterly<-utils.getDataFromFile(paste(cc,".gdp",sep=""),NULL,gdpFileNameQuarterly,"%Y-%m-%d")
		gdpDataQuarterly <-gdpDataQuarterly[as.logical(is.finite(gdpDataQuarterly[,1])),1]
		yearlyInds<-(1:nrow(gdpDataQuarterly))[(1:nrow(gdpDataQuarterly))%%4==1]
		#####gdpDataQuarterly<-gdpDataQuarterly[yearlyInds,]
		gdpGrowthYoY<-as.numeric(gdpDataQuarterly[1:(nrow(gdpDataQuarterly)-4),1])/as.numeric(gdpDataQuarterly[5:nrow(gdpDataQuarterly)])-1
		gdpGrowthYoYDates<-row.names(gdpDataQuarterly)[1:(nrow(gdpDataQuarterly)-4)]
		
		gdpGrowthYoY2<-c()
		gdpGrowthYoYDates2<-c()
		gfdGDP<-paste(cc,".gdpreal",sep="")
		gfdGDPData<-read.csv(gdpFileNameYearly,header=TRUE,stringsAsFactors=FALSE)
		extrapolate<-FALSE
		if(is.element(gfdGDP,names(gfdGDPData))){
			gdpDataYearly<-timeSeries(gfdGDPData[gfdGDP],charvec=as.Date(gfdGDPData$Date))
			gdpDataYearly<-gdpDataYearly[as.logical(is.finite(gdpDataYearly[,1])),1]
			ind<-match(TRUE,as.Date(row.names(gdpDataYearly))<as.Date(gdpGrowthYoYDates[length(gdpGrowthYoYDates)]))
			if(is.finite(ind) && ind < nrow(gdpDataYearly)){
				gdpGrowthYoY2<-as.numeric(gdpDataYearly[ind:(nrow(gdpDataYearly)-1),])/as.numeric(gdpDataYearly[(ind+1):nrow(gdpDataYearly),])-1
				gdpGrowthYoYDates2<-row.names(gdpDataYearly)[ind:(nrow(gdpDataYearly)-1)]
			} else{
				extrapolate<-TRUE
			}
		} else {
			extrapolate<-TRUE
		}
		
		if(params.extrapolateGDP && extrapolate){
			firstDate<-gdpGrowthYoYDates[length(gdpGrowthYoYDates)]
			year<-utils.getYear(firstDate)-1
			gdpGrowthYoYDates2 <-paste(year:(year-25),"-12-31",sep="")
			gdpGrowthYoY2<-rep(gdpGrowthYoY[length(gdpGrowthYoY)],length(gdpGrowthYoYDates2))
		}
	
		gdpGrowthYoY<-c(gdpGrowthYoY, gdpGrowthYoY2)
		gdpGrowthYoYDates<-c(gdpGrowthYoYDates, gdpGrowthYoYDates2)
		ret<-timeSeries(gdpGrowthYoY,charvec= gdpGrowthYoYDates)
		names(ret)<-names(gdpDataQuarterly)

		return(ret)
	} else {
		gniData<-utils.getDataFromFile(cc,".gni",paste(params.pathToInput,params.gniFileName,sep=""),"%Y-%m-%d")
		gniData<-gniData[as.logical(is.finite(gniData[,1])),]
		gniGrowth<-as.numeric(gniData[1:(nrow(gniData)-1),1])/as.numeric(gniData[2:nrow(gniData),1])-1
		ret<-timeSeries(gniGrowth,charvec=row.names(gniData)[1:(nrow(gniData)-1)])
		return(ret)
		
	}
}

# annualized inflation rates over numberOfMonths periods and averages of yoy inflation rates
params.getInflationSeries<-function(inflationData,cc,numberOfMonths){
	if(is.element(cc, params.countriesWithQuarterlyInflation)){
		if(numberOfMonths%%3!=0){
			stop(paste("error in params.getInflationSeries(): country",cc,"has quarterly inflation numbers only and therefor for this country numberOfMonths must be a multiple of 3 but I got numberOfMonths=",numberOfMonths))
		}
		numberOfQuarters<-numberOfMonths/3
		inflationRates<-(as.numeric(inflationData[1:(nrow(inflationData)-numberOfQuarters),1])/as.numeric(inflationData[(numberOfQuarters+1):nrow(inflationData),1]))^(4/numberOfQuarters)-1
		
		inflationRatesYOY<-as.numeric(inflationData[1:(nrow(inflationData)-4),1])/as.numeric(inflationData[5:nrow(inflationData),1])-1
		annualAvgs<-timeSeries(cumsum(rev(inflationRatesYOY))/(1:length(inflationRatesYOY)),charvec=row.names(inflationData)[(nrow(inflationData)-4):1])
		annualAvgs<-annualAvgs[nrow(annualAvgs):1,]
		names(annualAvgs)<-paste(cc,".cpi",sep="")
		
		periodRates<-timeSeries(inflationRates,charvec=row.names(inflationData)[1:(nrow(inflationData)-numberOfQuarters)])
		names(periodRates)<-paste(cc,".cpi",sep="")
		inflationRatesYOY<-timeSeries(inflationRatesYOY,charvec=row.names(inflationData)[1:(nrow(inflationData)-4)])
		names(inflationRatesYOY)<-paste(cc,".cpi",sep="")
		
	} else {
		inflationEOMData<-utils.getEOMData(inflationData,TRUE)
		inflationRates<-(as.numeric(inflationEOMData[1:(nrow(inflationEOMData)-numberOfMonths),1])/as.numeric(inflationEOMData[(numberOfMonths+1):nrow(inflationEOMData),1]))^(12/numberOfMonths)-1
		
		inflationRatesYOY<-as.numeric(inflationEOMData[1:(nrow(inflationEOMData)-12),1])/as.numeric(inflationEOMData[13:nrow(inflationEOMData),1])-1
		annualAvgs<-timeSeries(cumsum(rev(inflationRatesYOY))/(1:length(inflationRatesYOY)),charvec=row.names(inflationEOMData)[(nrow(inflationEOMData)-12):1])
		annualAvgs<-annualAvgs[nrow(annualAvgs):1,]
		names(annualAvgs)<-paste(cc,".cpi",sep="")
	
		periodRates<-timeSeries(inflationRates,charvec=row.names(inflationEOMData)[1:(nrow(inflationEOMData)-numberOfMonths)])
		names(periodRates)<-paste(cc,".cpi",sep="")
		inflationRatesYOY<-timeSeries(inflationRatesYOY,charvec=row.names(inflationEOMData)[1:(nrow(inflationEOMData)-12)])
		names(inflationRatesYOY)<-paste(cc,".cpi",sep="")
	}
	
	ret<-list(periodRates=periodRates,yoy= inflationRatesYOY)
	return(ret)
}

# extract from tSeries last data available before dates
params.getDataForDates<-function(dates,tSeries,tSeriesDates){
	ret<-c()
	for(d in dates){
		dD<-as.Date(d)
		ind<-match(TRUE,dD>= tSeriesDates)
		ret<-c(ret, tSeries[ind,1])
	}
	
	return(ret)
}

# dividend/peak_earnings; can easily change to dividend/earnings by replacing peakEarnings[,1] with secData[,2]
params.getPayoutRatio<-function(secData,peakEarnings){
	# divYield * price / earnings = dividend / earnings
	ratios<-as.numeric(secData[,3])*as.numeric(secData[,1])/as.numeric(peakEarnings[,1])
	ret<-timeSeries(ratios,charvec=row.names(secData))
	names(ret)<-paste(names(secData)[1],".payoutRatio",sep="")
	
	return(ret)
}

# dividend discount rate implied by DDM
params.getDDR<-function(secData,growthRate){
	ddr<-as.numeric(secData[,3])+as.numeric(growthRate[,1])
	ret<-timeSeries(ddr,charvec=row.names(price))
	names(ret)<-paste(names(price)[1],".ddr",sep="")
	
	return(ret)
}

params.getRiskPremium<-function(ddr,rfRate){
	erp<-as.numeric(ddr[,1])-as.numeric(rfRate[,1])
	ret<-timeSeries(erp,charvec=row.names(ddr))
	names(ret)<-paste(sub("\\.ddr","",names(ddr)[1]),".erp",sep="")
	
	return(ret)
}

params.getAveragePeakGrowth<-function(thisDate,peakGrowths,peakGrowthsDates){
	ind<-match(TRUE,as.Date(thisDate)>=peakGrowthsDates)
	if(!is.finite(ind)){
		print("warning in params.getAveragePeakGrowth(): no peak earnings growth was observed over period; returning 0 growth")
		return(0)
	}

	return(mean(as.numeric(peakGrowths[ind:nrow(peakGrowths),1])))
}

params.getAverageGDP<-function(gdpSeries){
	gdpDates<-as.Date(row.names(gdpSeries))
	ret<-NULL
	for(i in 1:length(gdpDates)){
		dD<-gdpDates[i]
		dates<-seq(dD,by="-1 year",length.out=params.numGDPYears)
		datesInds<-c()
		for(d in dates){
			datesInds<-c(datesInds,match(TRUE,d>=gdpDates))
		}
		
		datesInds<-datesInds[is.finite(datesInds)]
		if(length(datesInds)!=0){
			newRec<-data.frame(Date=as.character(dD),gdp=mean(gdpSeries[datesInds,1],na.rm=TRUE),stringsAsFactors=FALSE)
			ret<-rbind(ret,newRec)
		}
	}
	names(ret)[2]<-names(gdpSeries)
	return(timeSeries(ret[2],charvec=ret$Date))
}

params.avgEarnings<-function(earningsData){
	if(params.numAveragedEarnings<2) return(earningsData)
	
	initRange<-1:(nrow(earningsData)-(params.numAveragedEarnings-1)* params.bDaysPerYear)
	ret<-rep(0,length(initRange))
	for (i in 1:params.numAveragedEarnings){
		ret<-ret+ as.numeric(earningsData[initRange,1])
		initRange<-initRange+ params.bDaysPerYear
	}
	
	return(ret/params.numAveragedEarnings)
}

params.getLastRow<-function(gdpDates,inflationDates,assetDates){
	assetDates<-as.Date(assetDates)
	indInfl<-match(FALSE,assetDates>=inflationDates[length(inflationDates)])
	if(!is.finite(indInfl)){
		indInfl<-length(assetDates)
	}else{
		indInfl<-indInfl-1
	}
	indGdp<-match(FALSE,assetDates>= gdpDates[length(gdpDates)])
	if(!is.finite(indGdp)){
		indGdp <-length(assetDates)
	}else{
		indGdp <-indGdp-1
	}	
	# min because data is in revere chronological order
	return(min(indInfl,indGdp))
}

# assumes data in reverse chronological order
params.cumCov<-function(v,w,rollingWindow,method){
	if(length(v)<rollingWindow)return(NA)
	if(length(v)!=length(w))stop(paste("error in params.cumCor(): v & w must have the same length but I got",length(v),length(w)))
	
	ret<-rep(NA,length(v))
	for(i in 1:(length(v)-rollingWindow+1)){
		ret[i]<-cov(v[i:(i+rollingWindow-1)],w[i:(i+rollingWindow-1)],method=method)
	}

	return(ret)
}

# assumes data in reverse chronological order
params.cumCor<-function(v,w,rollingWindow,method){
	if(length(v)<rollingWindow)return(NA)
	if(length(v)!=length(w))stop(paste("error in params.cumCor(): v & w must have the same length but I got",length(v),length(w)))
	
	ret<-rep(NA,length(v))
	for(i in 1:(length(v)-rollingWindow+1)){
		ret[i]<-cor(v[i:(i+rollingWindow-1)],w[i:(i+rollingWindow-1)],method=method)
	}
	return(ret)
}

params.doOne<-function(asset,inflationFileName,gdpFileNameQuarterly,gdpFileNameYearly){	
	onlyassetsbeingrun<-unique(errisk.assetclass)
	cc<-substr(asset,1,2)
	assetClasses<-c("equity")
	
	slopes<-utils.getDataFromFile(paste(cc,".gg.210curve",sep=""),NULL,params.slopesFileName,"%Y-%m-%d")
	slopesDates<-as.Date(row.names(slopes))
	
	# get inflation
	cpiData<-params.readCPIData(inflationFileName,cc)
	inflationStuff<-params.getInflationSeries(cpiData,cc,params.numberOfMonthsForInflation)
	inflationSeries<-inflationStuff$periodRates
	inflationDates<-as.Date(row.names(inflationSeries))
	inflationAnnual<-inflationStuff$yoy
	inflationAnnualDates<-as.Date(row.names(inflationAnnual))
	
	# get earnings, prices and ptoepeak monthly
	rawData<-errisk.getDataFromFile(errisk.inputDir,"prdInputs.csv",asset,errisk.assetclass,assetClasses,errisk.listOfSuffixes,FALSE)
	errisk.haircut[errisk.usetrendadjearnings]<-1
	secData<-rawData[["equity"]]
	nonNAs<-is.finite(secData[,1])& is.finite(secData[,2])&is.finite(secData[,3])
	secData<-secData[nonNAs,]
	# average earnings over params.numAveragedEarnings years
	avgEarnings<-params.avgEarnings(secData[,2])
	secData<-secData[1:length(avgEarnings),]
	secData[,2]<-avgEarnings
	secDvds<-secData[,3]
	secEarnings<-secData[,2]
	secEarnings<-secEarnings[is.finite(secEarnings[,1]),]
	peakEarnings<-utils.getEOMData(timeSeries(rev(cummax(secEarnings[nrow(secEarnings):1,1])),charvec=row.names(secEarnings)),TRUE)
	ptoEPeak<-errisk.getPtoEPeak(secData,secEarnings)
	ptoEPeak<-ptoEPeak[is.finite(ptoEPeak[,1]),]
	ptoEPeak<-utils.getEOMData(ptoEPeak,TRUE)
	secDvds<-utils.getEOMData(secDvds,TRUE)
	secEarnings<-utils.getEOMData(secEarnings,TRUE)
	secPrices<-utils.getEOMData(secData[,1],TRUE)
	
	# manage growth offsets by differences in div yields
	params.globalSecData<<-append(params.globalSecData,list(secEarnings = secEarnings))
	names(params.globalSecData)[length(params.globalSecData)]<<-asset
	
	# correlations/covariances
	secRets<-as.numeric(secPrices[1:(nrow(secPrices)-1),1])/as.numeric(secPrices[2:nrow(secPrices),1])-1
	secSDs<-timeSeries(c(sqrt(params.cumCov(secRets,secRets, params.rollingWindow, params.corCovMethod)),NA),charvec=row.names(secPrices))
	
	# get total returns of asset
	dtrData<-params.dtr(asset)
	dtrData<-dtrData[is.finite(dtrData[,1]),]
	results<-params.rateOfReturn(dtrData,as.character(row.names(ptoEPeak)[nrow(ptoEPeak)]), params.periodInMonths, params.unitsPerYear)
	irrs<-results$ror
	cumRets<-results$cum	
	# gdp data
	gdpYoY<-params.getGDPSeries(cc,gdpFileNameQuarterly,gdpFileNameYearly)
	gdpAvg<-params.getAverageGDP(gdpYoY)
	gdpAvgDates<-as.Date(row.names(gdpAvg))
	# risk free rates
	rfRateName<-paste(cc,".gg.10y",sep="")
	rfRates<-utils.getDataFromFile(rfRateName,NULL,paste(params.pathToInput,"prdInputs.csv",sep=""),"%Y-%m-%d")
	rfDates<-as.Date(row.names(rfRates))
	# rfDtrData<-params.dtr(rfRateName)
	# rfDtrData<-rfDtrData[is.finite(rfDtrData[,1]),]
	# rfRets<-params.rateOfReturn(rfDtrData,as.character(row.names(rfDtrData)[nrow(rfDtrData)]), params.periodInMonths, params.unitsPerYear)$ror
	
	# payoutRatios
	payoutRatios<-params.getPayoutRatio(utils.getEOMData(secData,TRUE),peakEarnings)

	output<-NULL
	output2<-NULL
	erps<-c()
	allDates<-c()
	ltgs<-c()
	altLtpes<-c()
	ddrs<-c()
	payouts<-c()
	rfRs<-c()
	inflations<-c()
	lastRow<-params.getLastRow(gdpAvgDates,inflationDates,row.names(ptoEPeak))
	if(is.finite(match(asset,names(params.dependsOn)))){
		dates<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$Date
		firstNewDate<-row.names(ptoEPeak)[lastRow]
		index<-match(TRUE,as.Date(firstNewDate)<=as.Date(dates))
		if(!is.finite(index) || index==1){
			print(paste("WARNING in params.R: cannot seed",asset,"with data from",params.dependsOn[[asset]],"because the latter does not have earlier data"))
		}else{
			nrows<-nrow(params.ltpesLtgs[[params.dependsOn[[asset]]]])
			allDates<-as.character(params.ltpesLtgs[[params.dependsOn[[asset]]]]$Date[1:(index-1)])

			ltgs<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$ltgs[1:(index-1)]
			altLtpes<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$altLtpes[1:(index-1)]
			ddrs<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$ddrs[1:(index-1)]
			payouts<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$payouts[1:(index-1)]
			rfRs<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$rfRs[1:(index-1)]
			inflations<-params.ltpesLtgs[[params.dependsOn[[asset]]]]$inflations[1:(index-1)]
		}
	}
	
	constants<-c(errisk.LongTermGrowth[match(asset[1],errisk.assetHeaders)], errisk.LTPEtoPeak[match(asset[1],errisk.assetHeaders)])
	# ACHTUNG: ACHTUNG: ACHTUNG: ACHTUNG: ACHTUNG: ACHTUNG: ACHTUNG: I reversed the loop to calculate old ltpe/ltg first
	riskInfl<-c()
	riskNew<-c()
	sdRealized<-c()
	first<-TRUE
	for(i in lastRow:(params.periodInMonths+1)){
		thisDate<-row.names(ptoEPeak)[i]
		if(first && (asset=="ca.equity" || asset=="hk.equity")){
			first<-FALSE
			ind<-match(TRUE,thisDate>= gdpAvgDates)
			ltgs<-rev(as.numeric(gdpAvg[ind:nrow(gdpAvg),1]))
		}
		allDates<-c(allDates,thisDate)
		###targetRet<-as.numeric(irrs[i,1])	
		# calibrate using current pes/dvds
		pes<-as.numeric(ptoEPeak[i,1])
		dvds<-as.numeric(secDvds[i,1])
		inflationRates<-params.getDataForDates(thisDate,inflationSeries,inflationDates)
		annualInflationRate<-params.getDataForDates(thisDate,inflationAnnual,inflationAnnualDates)
		if(sum(is.finite(inflationRates))==length(inflationRates) && is.finite(annualInflationRate)){
			if(is.finite(annualInflationRate)){	
				
				rfDateInd<-match(TRUE,thisDate>=rfDates)
				rfRate<-as.numeric(rfRates[rfDateInd,1])
				rfRs<-c(rfRs,rfRate)	
				
				# weights for exponential weighting
				###expWeights<-2.^((1:(length(ltgs)+1))/params.halfLife)
				###expWeights<-expWeights/sum(expWeights)
				# MOD: this can be output of optimizer or peak-to-peak growth rate nominal or real
				thisRealG<-params.getDataForDates(thisDate, gdpAvg, gdpAvgDates)
				
				###realG<-sum(c(ltgs,thisRealG)*expWeights)
				realG<-mean(c(ltgs,thisRealG))
				ddr<-as.numeric(dvds[1])+realG
				erp<-ddr-rfRate+ inflationRates[1]
				payoutRatio<-as.numeric(payoutRatios[as.Date(thisDate)])
				altLtpe<-pes[1]
				#altLtpe2<-sum(c(payouts,payoutRatio)*expWeights)/(sum(c(erps,erp)*expWeights)+rfRate-inflationRates[1]-realG)
				###altLtpe2<-sum(c(payouts,payoutRatio)*expWeights)/(sum(c(ddrs,ddr)*expWeights)-realG)
				altLtpe2<-mean(c(payouts,payoutRatio))/(mean(c(ddrs,ddr))-realG)
if(altLtpe2 <0){
	###print(data.frame(thisDate,erp,avgerp=sum(c(erps,erp)*expWeights),rfRate,inflationRate= inflationRates[1],realG))

}
				
				# MOD: we can use inflationRates[1] or annualInflationRate here
				###altPar<-c(realG+ inflationRates[1],sum(c(altLtpes,altLtpe)*expWeights))
				altPar<-c(realG+ inflationRates[1],mean(c(altLtpes,altLtpe)))
				if(sum(!is.finite(rfRs))!=length(rfRs)){
					rfRs[!is.finite(rfRs)]<-0
					ialtPar<-c(realG+inflationRates[1],sum(c(altLtpes,altLtpe)* rfRs)/sum(rfRs))
					inew<-as.numeric((1+ ialtPar[1])*(ialtPar[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/ialtPar[2])/2)
				}else{
					ialtPar<-c(realG+inflationRates[1],NA)
					inew<-NA
				}
				ialtPar2<-c(realG+ inflationRates[1], altLtpe2)
				if(length(ltgs)==0){
					ialtPar3<-c(realG+ inflationRates[1], altLtpe2)
					stdev<-0
				}else{
					ialtPar3<-c(realG-0.5*sd(c(ltgs,thisRealG))^2+ inflationRates[1], altLtpe2)
					stdev<-sd(c(ltgs,thisRealG))
				}
				ialtPar4<-c(realG,altLtpe2)
				
				###ialtPar5<-c(realG+inflationRates[1],1/(sum((1/c(altLtpes,altLtpe)-c(inflations,inflationRates[1]))*expWeights)+inflationRates[1]))
				ialtPar5<-c(realG+inflationRates[1],1/(mean((1/c(altLtpes,altLtpe)-c(inflations,inflationRates[1])))+inflationRates[1]))
				
				if(params.useRegs=="multilinear" || !is.finite(match(asset,names(params.growthBenchmarks)))){
					###regAltPar<-c(params.getGrowthReg(NULL,as.numeric(secEarnings[i:nrow(secEarnings),]),rev(c(ltgs,thisRealG)) ,rev(c(inflations,inflationRates[1]))),sum(c(altLtpes,altLtpe)*expWeights))
					regAltPar<-c(params.getGrowthReg(NULL,as.numeric(secEarnings[i:nrow(secEarnings),]),rev(c(ltgs,thisRealG)) ,rev(c(inflations,inflationRates[1]))),mean(c(altLtpes,altLtpe)))
				}else{
					benchEarnings<-params.globalSecData[[params.growthBenchmarks[[asset]]]]
					bIndex<-match(thisDate,row.names(benchEarnings) )
					if(!is.finite(bIndex)){
						stop(paste("Oops! i made a wrong assumption for asset ",asset, "and its benchmark",params.growthBenchmarks[[asset]]))
					}
					###regAltPar<-c(params.getGrowthReg(as.numeric(benchEarnings[bIndex:nrow(benchEarnings),]),as.numeric(secEarnings[i:nrow(secEarnings),]),rev(c(ltgs,thisRealG)),rev(c(inflations,inflationRates[1]))),sum(c(altLtpes,altLtpe)*expWeights))
					regAltPar<-c(params.getGrowthReg(as.numeric(benchEarnings[bIndex:nrow(benchEarnings),]),as.numeric(secEarnings[i:nrow(secEarnings),]),rev(c(ltgs,thisRealG)),rev(c(inflations,inflationRates[1]))),mean(c(altLtpes,altLtpe)))
				}
				jregAltPar<-c(regAltPar[1], altLtpe2)
	

				nextPeriodRet<-(as.numeric(cumRets[i-params.periodInMonths,1])/as.numeric(cumRets[i,1]))^(params.unitsPerYear/params.periodInMonths)-1
				nextPeriodRet1M<-(as.numeric(cumRets[i-1,1])/as.numeric(cumRets[i,1]))-1
				nextPeriodRet12M<-(as.numeric(cumRets[i-12,1])/as.numeric(cumRets[i,1]))-1
				nextPeriodRealRet<-nextPeriodRet-params.getDataForDates(row.names(ptoEPeak)[i-params.periodInMonths],inflationSeries,inflationDates)
				
				slope<-params.getDataForDates(thisDate,slopes,slopesDates)

				riskInfl<-c(riskInfl,1/(inflationRates[1]+1/pes[1]))
				riskNew<-c(riskNew,1/secDvds[i,1])
				sdRealized<-c(sdRealized,secSDs[i-params.rollingWindow,1])
				newRec<-data.frame(Date=thisDate,payoutRatio=payoutRatio,realDDR=ddr,rfRate=rfRate,ltg=altPar[1],kltg=ialtPar3[1],realltg=realG,curRealG=thisRealG,regltg=regAltPar[1],regLtpe=regAltPar[2],infl= inflationRates[1],ltpe=altPar[2],iltpe=ialtPar[2],jltpe=ialtPar2[2],deriv=(altPar[2]/pes[1])^params.exponent,slope210=slope,price=secPrices[i,1],earnings=secEarnings[i,1],dvd.yld=secDvds[i,1],stdev=stdev,curPe=pes[1],new=as.numeric((1+ altPar[1])*(altPar[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/altPar[2])/2),inew=inew,jnew=as.numeric((1+ ialtPar2[1])*(ialtPar2[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/ialtPar2[2])/2),regnew=as.numeric((1+ regAltPar[1])*(regAltPar[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/regAltPar[2])/2),jregnew=as.numeric((1+ jregAltPar[1])*(jregAltPar[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/jregAltPar[2])/2),knew=as.numeric((1+ ialtPar3[1])*(ialtPar3[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/ialtPar3[2])/2),rnew=as.numeric((1+ ialtPar5[1])*(ialtPar5[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/ialtPar5[2])/2),fnew=as.numeric((1+ ialtPar4[1])*(ialtPar4[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/ialtPar4[2])/2),realizedPeriod=nextPeriodRet,realized1M=nextPeriodRet1M,realized1Y= nextPeriodRet12M,realRealized=nextPeriodRealRet,old=as.numeric((1+constants[1])*(constants[2]/pes[1])^params.exponent-1+dvds[1]*(1+pes[1]/constants[2])/2))
				names(newRec)[2:ncol(newRec)]<-paste(c("payoutRatio","realDDR","rfRate","ltg","kltg","realltg","curRealG","regltg","regLtpe","infl","ltpe","iltpe","jltpe","deriv","slope210","price","earnings","dvd.yld","stdev","curPe","new","inew","jnew","regnew","jregnew","knew","rnew","fnew","realizedPeriod","realized1M","realized1Y","realRealized","old"),".",asset,sep="")
				newRec2<-data.frame(Date=thisDate,new=newRec$new,inew=newRec$inew,jnew=newRec$jnew,regnew=newRec$regnew,jregnew=newRec$jregnew,knew=newRec$knew,rnew=newRec$rnew,fnew=newRec$fnew,realizedPeriod=newRec$realizedPeriod,realized1M=newRec$realized1M,realized1Y=newRec$realized1Y,realRealized= nextPeriodRealRet,old=newRec$old)
				names(newRec2)[2:ncol(newRec2)]<-paste(c("new","inew","jnew","regnew","jregnew","knew","rnew","fnew","realizedPeriod","realized1M","realized1Y","realRealized","old"),".",asset,sep="")
				output<-rbind(output,newRec)
				output2<-rbind(output2,newRec2)
		
				ltgs<-c(ltgs, thisRealG)
				altLtpes<-c(altLtpes,altLtpe)
				payouts<-c(payouts,payoutRatio)
				erps<-c(erps,erp)
				ddrs<-c(ddrs,ddr)
				inflations<-c(inflations,inflationRates[1])
			}
		}
	}
	
	erChanges<-output$jnew[2:nrow(output)]-output$jnew[1:(nrow(output)-1)]
	erRets<-erChanges/output$jnew[1:(nrow(output)-1)]
	erChanges<-rev(erChanges)
	erRets<-rev(erRets)
	erSDs<-c(rep(NA,length(riskInfl)-length(erChanges)),sqrt(rev(params.cumCov(erChanges, erChanges,params.rollingWindow,method= params.corCovMethod))))
	erCors<-c(rep(NA,length(riskInfl)-length(erRets)),rev(params.cumCor(erRets,erRets,params.rollingWindow,method= params.corCovMethod)))
	
	if(asset !="ca.equity" && asset !="hk.equity"){
		params.ltpesLtgs<<-append(params.ltpesLtgs,list(data.frame(Date=allDates,ltgs=ltgs,altLtpes=altLtpes,ddrs=ddrs,payouts=payouts,rfRs=rfRs,inflations=inflations)))
		names(params.ltpesLtgs)[length(params.ltpesLtgs)]<<-asset
	}

	lengthFiniteInew<-sum(is.finite(output$inew))
	
	numColsOrg<-ncol(output)
	output<-cbind(output,new.realized.rmse=sqrt(cumsum((output$new-output$realizedPeriod)^2)/(1:nrow(output))),inew.realized.rmse=c(rep(NA,nrow(output)-lengthFiniteInew),sqrt(cumsum((output$inew[is.finite(output$inew)]-output$realizedPeriod[is.finite(output$inew)])^2)/(1:lengthFiniteInew))),jnew.realized.rmse=sqrt(cumsum((output$jnew-output$realizedPeriod)^2)/(1:nrow(output))),regnew.realized.rmse=sqrt(cumsum((output$regnew-output$realizedPeriod)^2)/(1:nrow(output))),jregnew.realized.rmse=sqrt(cumsum((output$jregnew-output$realizedPeriod)^2)/(1:nrow(output))),knew.realized.rmse=sqrt(cumsum((output$knew-output$realizedPeriod)^2)/(1:nrow(output))),rnew.realized.rmse=sqrt(cumsum((output$rnew-output$realizedPeriod)^2)/(1:nrow(output))),fnew.realrealized.rmse=sqrt(cumsum((output$fnew-output$realRealized)^2)/(1:nrow(output))),fnew.realized.rmse=sqrt(cumsum((output$fnew-output$realizedPeriod)^2)/(1:nrow(output))),old.realized.rmse=sqrt(cumsum((output$old-output$realizedPeriod)^2)/(1:nrow(output))),riskInfl=riskInfl,riskNew=riskNew,sdRealized=sdRealized,predictedVolInfl=sqrt(erCors)*riskInfl,predictedVolInfl2=erSDs*riskInfl,predicedVolNew=erSDs*riskNew)

	names(output)[(numColsOrg+1):ncol(output)]<-paste(c("new.realized.rmse","inew.realized.rmse","jnew.realized.rmse","regnew.realized.rmse","jregnew.realized.rmse","knew.realized.rmse","rnew.realized.rmse","fnew.realrealized.rmse","fnew.realized.rmse","old.realized.rmse","riskInfl","riskNew","sdRealized","predictedVolInfl","predictedVolInfl2","predictedVolNew"),".",asset,sep="")
	output2<-cbind(output2,output[names(output)[(numColsOrg+1):ncol(output)]])
		
	output3<-params.zscore(output[c("Date",paste(c("payoutRatio","realDDR","rfRate","ltg","realltg","curRealG","infl","ltpe","deriv","slope210"),".",asset,sep=""))])

	output3<-cbind(output3,output[paste(c("price","curPe","new","inew","jnew","regnew","jregnew","rnew","realizedPeriod","realized1M","realized1Y","old"),".",asset,sep="")])
	
	output<-output[nrow(output):1,]
	output2<-output2[nrow(output2):1,]
	output3<-output3[nrow(output3):1,]
	
	return(list(output=output,output2=output2,output3=output3))
}

params.main<-function(){
	allOutputs<-list()
	allOutputs2<-list()
	allOutputs3<-list()
	allDates<-c()
	for(i in 1:length(params.assets)){
print(paste("now doing",params.assets[i]))
		outputs<-params.doOne(params.assets[i],params.inflationFileName,params.gdpFileNameQuarterly,params.gdpFileNameYearly)
		output<-outputs$output
		output2<-outputs$output2
		output3<-outputs$output3
		allOutputs<-append(allOutputs,list(data.frame(output)))
		allOutputs2<-append(allOutputs2,list(data.frame(output2)))
		allOutputs3<-append(allOutputs3,list(data.frame(output3)))
		allDates<-union(allDates,output$Date)
	}
	
	allDates<-sort(allDates,decreasing=TRUE)
	ret<-data.frame(Date=allDates)
	for (i in 1:length(allOutputs)){
		outDf<-as.data.frame(allOutputs[[i]])
		for(j in 1:ncol(outDf)){
			if(names(outDf)[j]!="Date"){
				newCol<-rep(NA,length(allDates))
				newCol[is.element(allDates,outDf$Date)]<-outDf[,j]
				ret<-cbind(ret,data.frame(newCol))
				names(ret)[ncol(ret)]<-names(outDf)[j]
			}
		}
	}

	ret2<-data.frame(Date=allDates)
	for (i in 1:length(allOutputs2)){
		outDf<-as.data.frame(allOutputs2[[i]])
		for(j in 1:ncol(outDf)){
			if(names(outDf)[j]!="Date"){
				newCol<-rep(NA,length(allDates))
				newCol[is.element(allDates,outDf$Date)]<-outDf[,j]
				ret2<-cbind(ret2,data.frame(newCol))
				names(ret2)[ncol(ret2)]<-names(outDf)[j]
			}
		}
	}
	
	ret3<-data.frame(Date=allDates)
	for (i in 1:length(allOutputs3)){
		outDf<-as.data.frame(allOutputs3[[i]])
		for(j in 1:ncol(outDf)){
			if(names(outDf)[j]!="Date"){
				newCol<-rep(NA,length(allDates))
				newCol[is.element(allDates,outDf$Date)]<-outDf[,j]
				ret3<-cbind(ret3,data.frame(newCol))
				names(ret3)[ncol(ret3)]<-names(outDf)[j]
			}
		}
	}
	
	if(params.plotModels){
		oneYearOneMonthColumns<-union(union(grep("realized1M",names(ret2)),grep("realized1Y",names(ret2))),grep("rnew",names(ret2)))
		ret2Reduced<-ret2[,setdiff(1:ncol(ret2),oneYearOneMonthColumns)]
		plt.models(ret2Reduced,params.numberOfColumnsToPlot) # count the columns to be output; they should be contiguous and start from the "new.*"
	}
	
	write.csv(ret,paste(params.pathToOutput ,"paramsOutputAllGDPInfl-multi-",params.periodInMonths,"-",params.halfLife,".csv",sep=""),row.names=FALSE)
	write.csv(ret2,paste(params.pathToOutput ,"paramsOutputAllGDPInfl-multiErOnly-",params.periodInMonths,"-",params.halfLife,".csv",sep=""),row.names=FALSE)
	write.csv(ret3,paste(params.pathToOutput ,"paramsOutputAllGDPInfl-zscores-",params.periodInMonths,"-",params.halfLife,".csv",sep=""),row.names=FALSE)
}

params.main()
