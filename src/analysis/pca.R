# input function
# read data: history is expected in the columns of the data frame; chronological order doesn't matter (can be oldest data first or newest date first)
# returns data frame
pca.input<-function(settings){
	inputDir<-settings[["pcaex.inputDir"]]
	inputFile<-settings[["pcaex.inputFile"]]
	
	return(read.csv(paste(inputDir,inputFile,sep=""),header=TRUE,stringsAsFactors=FALSE))
}

# output function and the most complicated printing we do anywhere in the code
pca.output<-function(outputs,settings){
	rollingWindow<-settings[["pcaex.rollingWindow"]]
	outputDir<-settings[["pcaex.outputDir"]]
	outputFile<-settings[["pcaex.outputFile"]]
	
	outputFileName<-paste(outputDir,outputFile,sep="")
	anglesFileName<-paste(outputDir,sub(".csv","",outputFile),".angles.csv",sep="")
	
	output<-outputs[["backTest"]]
	if(rollingWindow==-1){
		# only 1 PCA, for the entire history, was done
		write.csv(output[[1]][["facLoadings"]][,c(1,2)],paste(outputDir,"factorLoadings-", outputFile,sep=""),row.names=TRUE)
	}

	varComp1Cor<-NULL
	varComp2Cor<-NULL
	firstIter<-TRUE
	for(i in 1:length(output)){
		thisDate<-names(output)[i]
				
		if(!firstIter){
			write(thisDate,file=outputFileName,ncolumns=1,append=TRUE)
		}else{
			write(thisDate,file=outputFileName,ncolumns=1,append=FALSE)
			firstIter<-FALSE
		}
		write("RelVals",file=outputFileName,ncolumns=1,append=TRUE)
		numCols<-length(output[[thisDate]]$relVals)
		write(output[[thisDate]]$relVals,file=outputFileName,ncolumns=numCols,append=TRUE)	
		write("",file=outputFileName,ncolumns=1,append=TRUE)
		write("StandardDevs",file=outputFileName,ncolumns=1,append=TRUE)
		write(output[[thisDate]]$stdevs,file=outputFileName,ncolumns=numCols,append=TRUE)	
		write("",file=outputFileName,ncolumns=1,append=TRUE)
		write("Variances",file=outputFileName,ncolumns=1,append=TRUE)
		write(output[[thisDate]]$variances,file=outputFileName,ncolumns=numCols,append=TRUE)	
		write("",file=outputFileName,ncolumns=1,append=TRUE)
		write("Eigenvectors",file=outputFileName,ncolumns=1,append=TRUE)
		variables<-row.names(output[[thisDate]]$principalComponents)
		for(i in 1:length(variables)){
			newLine<-paste(output[[thisDate]]$principalComponents[i,],collapse=" ")
			newLine<-paste(variables[i],newLine)
			write(newLine,file=outputFileName,ncolumns=numCols+1,append=TRUE)
		}	
		write("",file=outputFileName,ncolumns=1,append=TRUE)
		write("FacLoadings",file=outputFileName,ncolumns=1,append=TRUE)
		varComp1Cor<-rbind(varComp1Cor,matrix(output[[thisDate]]$facLoadings[,1],nrow=1))
		varComp2Cor<-rbind(varComp2Cor,matrix(output[[thisDate]]$facLoadings[,2],nrow=1))
		for(i in 1:length(variables)){
			newLine<-paste(output[[thisDate]]$facLoadings[i,],collapse=" ")
			newLine<-paste(variables[i],newLine)
			write(newLine,file=outputFileName,ncolumns=numCols+1,append=TRUE)
		}		
		write("",file=outputFileName,ncolumns=1,append=TRUE)
	}

	angles<-outputs[["angles"]]
	firstIter<-TRUE
	for(i in 1:length(angles)){
		dataFrame<-angles[[i]]
		nCols<-ncol(dataFrame)
		if(!firstIter){
			write(names(angles)[i],file=anglesFileName,ncolumns=1,append=TRUE)
		}else{
			write(names(angles)[i],file=anglesFileName,ncolumns=1,append=FALSE)
			firstIter<-FALSE
		}
		newLine<-paste(names(dataFrame),collapse=" ")
		write(newLine,file=anglesFileName,ncolumns=nCols,append=TRUE)
		for(j in 1:nrow(dataFrame)){
			newLine<-paste(as.character(dataFrame[j,1]),paste(dataFrame[j,2:nCols],collapse=" "))
			write(newLine,file=anglesFileName,ncolumns=nCols,append=TRUE)
		}
		write("",file=anglesFileName,ncolumns=1,append=TRUE)
		averages<-colMeans(dataFrame[,2:nCols])
		newLine<-paste(names(angles)[i],"_averages ",paste(averages,collapse= " "),sep="")
		write(newLine,file=anglesFileName,ncolumns=nCols,append=TRUE)
 
		write("",file=anglesFileName,ncolumns=1,append=TRUE)
	}

}
	
#TODO: if monthly data is all we're going to be able to work with then this is probably not needed
pcaex.dataAtFreq<-function(indexData,freq){
	#select data with the right frequency; we have that code in the efficient frontier calculator; we might want to lift it and put it in some utils file that we can call from many different r files
	
	#do not do anything for now
	return(indexData)
}

# returns shortest history for which all indices have data; is this what we want? alternative way to do this is what we did in the efficient frontier: keep the indices that have data for a given set of dates and drop those who don't 
pcaex.rmNAs<-function(allData){
	availData<-rep(TRUE,nrow(allData))
	#first column has dates so we start from second column
	for(i in 2:ncol(allData)){
		nonMissingVals<-is.finite(allData[,i])
		numNonMissingVals<-sum(as.numeric(nonMissingVals))
		if(sum(as.numeric(is.finite(allData[1:numNonMissingVals,i])))!=numNonMissingVals){
			stop(paste("NA's are intermixed with non NA's for index",names(allData)[i]))
		}
		
		availData<-availData & nonMissingVals
	}
	
	return(allData[availData,])
}

# PCA using the covariance method; not used
pcaex.calculateCov<-function(rollingData){
	prinCompResults<-prcomp(rollingData)
	prcmpSds<-diag(prinCompResults$sdev) # standard deviations of principal components
	principalComponents<-prinCompResults$rotation
	
	prcmpVars<-prcmpSds%*%prcmpSds
	facLoadings<-principalComponents%*%prcmpSds
	relVals<-diag(prcmpVars)/sum(diag(prcmpVars))
	
	return(list(variances=prcmpVars,stdevs=diag(prcmpSds),relVals=relVals,principalComponents=principalComponents,facLoadings=facLoadings))
}

# PCA using the correlation method
pcaex.calculate<-function(rollingData){
	# calculate correlations
	cors<-cor(rollingData)
	# calculate eigenvectors/eigenvalues of correlation matrix
	eigs<-eigen(cors)
	
	prcmpVars<-eigs$values
	prcmpSds<-sqrt(prcmpVars)
	principalComponents<-eigs$vectors
	
	relVals<-eigs$values/sum(prcmpVars)
	#calculate the factor loadings of the variables relative to all components
	facLoadings<-principalComponents%*%diag(prcmpSds)
	
	# what's this? quite often, the first component has a sign that makes most of our series negatively correlated with it. since we're at it we check the second component as well. what we're doing is equivalent to replacing the eigenvector (the component) with its opposite if necessary
	maxAbsInd<-order(abs(facLoadings[,1]),decreasing=TRUE)[1]
	if(facLoadings[maxAbsInd,1]<0){
		facLoadings[,1]<--facLoadings[,1]
	}
	# ... now check second factor
	maxAbsInd<-order(abs(facLoadings[,2]),decreasing=TRUE)[1]
	if(facLoadings[maxAbsInd,2]<0){
		facLoadings[,2]<--facLoadings[,2]
	}
	
	row.names(facLoadings)<-names(rollingData)
	row.names(principalComponents)<-names(rollingData)
	
	return(list(variances=prcmpVars,stdevs=prcmpSds,relVals=relVals,principalComponents=principalComponents,facLoadings=facLoadings))
}
	
# this runs the backtest using a historical window; if we're not getting fund/index returns from a file, this should be called directly as opposed to through pcaex.main()
pcaex.backTest<-function(historicData,rollingWindow){
	if(names(historicData)[1]!="Date"){
		stop("error in pcaex.backTest(): the first column of the data frame historicData must be a column of strings representing Dates (not R Date objects) and named Date")
	}
	
	historicData<-pcaex.rmNAs(historicData)
	output<-NULL
	# if we want to do the PCA using all available data in one go we set rollingWindow to -1
	if(rollingWindow!=-1){		
		for(i in 1:(nrow(historicData)-rollingWindow+1)){
			dateData<-historicData[i:(i+rollingWindow-1),]
			# don't use the Date column in the PCA calculation (so, start from column 2)
			dateResults<-pcaex.calculate(dateData[,2:ncol(historicData)])
			output<-append(output,list(Date=dateResults))
			names(output)[length(output)]<-as.character(dateData$Date[1])	
		}
	}else{ 
		# we're here, therefore rollingWindow ==-1 and we do the PCA on all available data
		dateData<-historicData
		dateResults<-pcaex.calculate(dateData[,2:ncol(historicData)])
		output<-list(Date=dateResults)
		names(output)<-as.character(dateData$Date[1])
	}
	
	ret<-list(backTest=output,angles=pcaex.buildHistoricExposures(output,names(historicData)[2:ncol(historicData)]))
	
	return(ret)
}

# x and y can be matrices
pcaex.getPolars<-function(x,y){
	nCols<-ncol(x)
	if(nCols!=ncol(y) || nrow(x)!=nrow(y)){
		stop("error in pcaex.getPolars(): x and y must be conforming matrices or vectors")
	}
	
	# polar coordinates
	rho<-sqrt(x^2+y^2)
	theta<-atan2(y,x)
	
	return(list(rho=rho,theta=theta))
}

# returns list() of data.frame()'s indexed by variable name; the i-th data.frame() has 1 row, and columns $rho, having the rho value (radial coordinate) of the i-th variable, and columns varNames, containing the angles between all variables and the i-th variable. see comments below for more details.
pcaex.getAngles<-function(polars,varNames,dates){
	nRows<-nrow(polars$theta)
	nCols<-ncol(polars$theta)

	allCols<-c(1:nCols)
	ret<-NULL
	for(i in 1:nCols){
		# calculate the angular difference between the i-th component and the rest
		temp<-abs(polars$theta[,i]-polars$theta)
		inds<-temp>pi
		temp[inds]<-2*pi-temp[inds]
		newAngles<-data.frame(rho=polars$rho[,i],temp)
		names(newAngles)<-c("rho",varNames)
		ret<-append(ret,list(data.frame(Date=dates,newAngles)))
		names(ret)[length(ret)]<-varNames[i]
	}
	return(ret)		
}

# in this function we calculate the correlations of the different variables (the time series of the returns of the different indices/entities) with the factors obtained through the PCA. Note: the factors are assumed to be obtained the usual way, V*e_i, for the ith factor, where e_i is the i-th eigenvector corresponding to the i-th largest eigenvalue. however, in order for what we're calling factor loadings (the eigenvector scaled by the sqrt of its eigenvalue -- as done in pcaex.calculate()) to be the correlations of variables with the factors, the zscores of the variables have to be used in forming the factors, i.e., V has to have the z-scores of the original returns series in its columns
pcaex.buildHistoricExposures<-function(backTestOutput,varNames){
	varComp1Cor<-NULL
	varComp2Cor<-NULL
	for(i in 1:length(backTestOutput)){
		thisDate<-names(backTestOutput)[i]
		# the correlations of the different series with the first factor
		varComp1Cor<-rbind(varComp1Cor,matrix(backTestOutput[[thisDate]]$facLoadings[,1],nrow=1))
		# ... and the second factor
		varComp2Cor<-rbind(varComp2Cor,matrix(backTestOutput[[thisDate]]$facLoadings[,2],nrow=1))
	}
	
	# now some measures to discriminate between the returns series. assuming our 2D frame of reference has the first and second factors as axes, associate a point with each returns series and assume that the correlations of the series with the first and the second factors are its coordinates in that frame of reference (the j-th point has coordinates, (sqrt(lambda_1)*e_1_j, sqrt(lambda_2)*e_2_j), with obvious notation: e_x_j, x=1,2, is the j-th component of eigenvector x and lambda_x the corresponding eigenvalue). pcaex.getPolars() gets the polar coordinates (rho, theta) of each of those points
	polars<-pcaex.getPolars(varComp1Cor,varComp2Cor)
	# pcaex.getAngles() will return a radial and angular differences for each returns series in the history: the radial coordinate is the euclidean distance to the origin in the frame of reference just mentioned; the angular differences are between each variable and the other variables, over the entire history
	angles<-pcaex.getAngles(polars,varNames,names(backTestOutput))
	
	return(angles)
}

# SA2 portfolios returns series should be prefixed with "SA2." and if so they are assigned a special color
# results is the output of pcaex.backTest()
# no return value
pcaex.plot<-function(results,numSubPlots,rollingWindow,fontFactor){
	cols<-rainbow(3)
	cols<-cols[c(1,length(cols))] # red and blue
	
	# unit semi-circle
	semiCircle<-function(x){
		return(sqrt(1-x^2))
	}
	
	counter<-0
	for(d in names(results[["backTest"]])){
		# do we need a new plotting window?
		if(counter%%numSubPlots==0){
			dev.new()
			layout(matrix(1:numSubPlots,nrow=2))
		}
		counter<-counter+1
		
		# the correlations of the different series with the factors. we only take the first 2 below
		facLoadings<-results[["backTest"]][[d]][["facLoadings"]]
		
		# locate our portfolios
		sa2PortfoliosInds<-grep("^SA2\\.",row.names(facLoadings))
		thisCols<-rep(cols[2],ncol(facLoadings)) # blue
		thisCols[sa2PortfoliosInds]<- cols[1] # red for our portfolios
		
		plot(facLoadings[,1],facLoadings[,2],type="n",main=paste(ifelse(rollingWindow==-1,"",paste(rollingWindow,"-month",sep="")),"correlations for",d,sep=" "),xlab="1rst factor",ylab="2nd factor",xlim=c(-1,1),ylim=c(-1,1))
		pointLabels<-1:nrow(facLoadings)
		for(p in 1:nrow(facLoadings)){
			lines(c(0,facLoadings[p,1]),c(0,facLoadings[p,2]),col=thisCols[p],type="l",lty=1)
			text(facLoadings[p,1]+0.02,facLoadings[p,2]+0.02,col=thisCols[p],labels=pointLabels[p],cex= fontFactor)
		}
		# using curve() produced rough-looking circles; passing points explicitly to plot() produced much smoother circles
		x<-(0:10000)/10000
		revX<-rev(x)
		semiCircleX<-semiCircle(x)
		# semiCircle(rev(x))==rev(semiCircle(x))
		revSemiCircleX<-rev(semiCircleX)
		lines(c(-revX,x,revX,-x),c(revSemiCircleX,semiCircleX,-revSemiCircleX,-semiCircleX),type="l")
		lines(c(0,0),c(-1,1))
		lines(c(-1,1),c(0,0))
		legend("bottomleft",paste(1:nrow(facLoadings),row.names(facLoadings),sep=" - "),cex=fontFactor,col=thisCols,lty=rep(0,length(cols)),bty="n")
	}
}

# main logic
pca.calculate<-function(input,settings){
	rollingWindow<-settings[["pcaex.rollingWindow"]]
	freq<-settings[["pcaex.freq"]]
	numSubPlots<-settings[["pcaex.numSubPlots"]]
	fontFactor<-settings[["pcaex.fontFactor"]]

	historicData<-input
	historicData<-pcaex.dataAtFreq(historicData, freq)

	output<-pcaex.backTest(historicData, rollingWindow)
	
	pcaex.plot(output,numSubPlots,rollingWindow,fontFactor)
	
	return(output)
}
	
# this is just a test function to check our results against the output from xlStat in the spreadsheet labeled PCA in Graham's workbook. we should probably remove it
pcaex.xlStatTest<-function(){
	historicData<-input
	historicData<-pcaex.rmNAs(historicData)
	historicData<-pcaex.dataAtFreq(historicData,freq)
	
	# select the Date column (column 1) and the columns used in the spreadsheet: Eurekahedge CTA / Managed Fututures,Dow Jones Credit Suisse Hedge, MSCI Daily TR Gross World USD, Buffet, ABFM NET NET, ABFM - Yanis US WKLY
	indices<-1+c(0,4,5,6,8,14,15)	
	historicData<-historicData[,indices]
	
	return(pcaex.backTest(historicData,-1))
}
