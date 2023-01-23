fbUtils.getData<-function(freq,assetNames,baseCashName,rfNames,ersRisksFile,mktDataFile,desiredDates,erMapping,riskMapping,useDB,inputDir,dayOfWeek,dateFormat,dbSettings, cashBenchmark ){
# this gets the data with the desired frequency; if "daily", "weekly" or "monthly", desiredDates needn't be specified; if, however, we want to run the test on specific dates, then that list should be passed in desiredDates
	
	print(paste("INFO from frontierBuilder.R: the assets are:",paste(assetNames,collapse=", ")))
	# use the database?
	numAssets<-length(assetNames)
	
	isInMix <- is.element(cashBenchmark[["name"]], assetNames)
	benchmarkErsRisks <- paste( cashBenchmark[["name"]], c(cashBenchmark[["er"]], cashBenchmark[["risk"]] ), sep="")
	
	# the base cash er and risk are needed later in macro risk calculations
	ersRisksIds<-utils.generateIdsFromMapping(assetNames,erMapping)
	ersRisksIds<-c(ersRisksIds,utils.generateIdsFromMapping(assetNames,riskMapping))
	ersRisksIds<- union(ersRisksIds, benchmarkErsRisks )
	baseCashIds<-utils.generateIdsFromMapping(baseCashName,erMapping)
	baseCashIds<-c(baseCashIds,utils.generateIdsFromMapping(baseCashName,riskMapping))
	
	rfNames <- union( rfNames, cashBenchmark[["name"]] )
	
	# check if we're doing a run with future data. in this case, we override useDB, because we don't store future actual returns/risks (i.e., .irr and .sdev) in the DB. but if useDB==TRUE we still want to get the cash rates from the DB
	isIRR <- sum(grepl("\\.irr",ersRisksIds)) != 0
	# we need risk free rates and those don't require suffixes so just add them
	if(!useDB || isIRR){
		if(is.null(ersRisksFile) || is.null(mktDataFile)){
			stop("ERROR in fbUtils.getData(): useDB is FALSE and one or both data files is (are) NULL")
		}
		
		assetsData<-utils.getDataFromFile(ersRisksIds,NULL,paste(inputDir, ersRisksFile,sep=""),dateFormat)
		# mkt data file is different from the ers/risks file; this is why, here, unlike the DB case, we make two calls to retrieve all the data we need (in the DB all data is in one table)
		if(!useDB){
			rfData<-utils.getDataFromFile(rfNames,NULL,paste(inputDir, mktDataFile,sep=""),dateFormat)
		}else{
			print("INFO from frontierBuilder.R: ers/risks are forward actual (i.e., .irr and .sdev) therefore they will be loaded from file. risk free rate data is still coming from DB")
			rfData<-utils.getDataFromDB(rfNames,NULL,dbSettings)
		}
		
		rfData<-rfData[row.names(rfData)<=max(row.names(assetsData)),]
		numRows<-min(nrow(assetsData),nrow(rfData))
		if(sum(row.names(assetsData)[1:numRows]!=row.names(rfData)[1:numRows])!=0){
			stop(paste("ERROR in fbUtils.getData(): the dates in",ersRisksFile,"and",mktDataFile,"do not match"))
		} else {
			# dates match therefore OK to bind; NOTE: using cbind() to bind the 2 timeSeries resulted in the dates being reversed to be in chronological order; hence the acrobatics of first converting into data.frame()
			rateData<-timeSeries(cbind(data.frame(assetsData[1:numRows,]),data.frame(rfData[1:numRows,])),charvec=row.names(assetsData)[1:numRows])
		}
	}else{
		rateData<-utils.getDataFromDB(union(ersRisksIds,rfNames),NULL,dbSettings)
	}
	
	# now filter at the right frequency and return
	if(freq=="weekly"){
		allData<-utils.getWeeklyData(rateData,dayOfWeek)
	}else if(freq=="daily"){
		allData<-rateData
	}else if(freq=="monthly"){
		allData <-utils.getEOMData(rateData,TRUE)
	}else if(freq=="listOfDates"){
		# here's the only point where desiredDates is used; only if we already have a set of dates and we want to run on those
		allData <-utils.getDataForDates(desiredDates,rateData,as.Date(row.names(rateData)))
	}else if(freq == "semi-monthly"){
		allData <-utils.getSemiMonthlyData(rateData,TRUE)
	}else{
		stop(paste("ERROR in fbUtils.getData(): the frequency at which the data is desired,",freq,", is not a valid one"))
	}
	
	# rename columns of effFrontierData; er's lose their suffix and all risks are renamed .risk; kind of legacy, kind of convenient. Note: this relies on setdiff preserving the order of the ids
	if( isInMix ){
		effFrontierData<-allData[,setdiff(ersRisksIds,baseCashIds)]
	} else {
		effFrontierData<-allData[,setdiff(ersRisksIds,c(baseCashIds, benchmarkErsRisks)) ]
	}
	names(effFrontierData)[1:length(assetNames)]<-assetNames
	names(effFrontierData)[(length(assetNames)+1):ncol(effFrontierData)]<-paste(assetNames,".risk",sep="")
	
	return(list(effFrontierData=effFrontierData,rfRates=allData[,rfNames],cashBenchmarkData=allData[, benchmarkErsRisks]))
}

fbUtils.getPortfolioData<-function(portfolioName,namesOfInList,pathToRSourceCode){
	# all portfolio files have a function named portfolio.settings(); we take some precautions here
	portfolio.settings<-NULL
	
	# now source the file to reset portfolio.settings()
	source(paste(pathToRSourceCode,"inputFiles/constraints-",portfolioName,".R",sep=""))
	# by source'ing this file we have reset portfolio.settings(); portfolioName may or may not be used in portfolio.settings()
	inList<-portfolio.settings(portfolioName)
	if(!setequal(names(inList),namesOfInList)){
		stop(paste("ERROR in fbUtils.getPortfolioData(): data for portfolio",portfolioName,"is expected to have settings for the following and only the following,",paste(namesOfInList,collapse=", ")," and instead it has settings for ",paste(names(inList),collapse=", ")))
	}
	
	return(inList)
}

fbUtils.rmNAs<-function(tSeries){
# this function removes columns in the time series tSeries that have fewer than rollingWindow NON-missing values
# returns the original time series if no NA's were found in the first rollingWindow rows or a new time series with only those columns that had no NA's in the first rollingWindow rows
	
	numCols<-ncol(tSeries)
	numAssets<-numCols/2
	numRows<-nrow(tSeries)
	
	numNotNAsInColumn<-as.numeric(colSums(is.finite(tSeries)))
	columnsWithNAs<-(1:numCols)[numNotNAsInColumn<numRows]
	# if the er column has NA we want to remove the .risk column as well and vice versa. NOTE: this relies on the fact that we have er's and risks for assets in the same order.
	companionColumns<-(columnsWithNAs + numAssets) %% numCols
	# 0 is really the last column in tSeries
	companionColumns[companionColumns==0]<-numCols
	colsToRemove<-union(columnsWithNAs,companionColumns)
	
	# now return only those columns whose index is NOT in colsToRemove
	return(tSeries[,setdiff(1:numCols,colsToRemove)])
}

# a convenience function; we check to see if the data table has an even number of columns in several places in the code and want to print an error if it's not; rather than duplicating lines of code unnecessarily, we just call this function
# numCols is a number that we want to check 
# stops execution if numCols is odd
fbUtils.assertNumColsIsEven<-function(numCols){
	if(numCols%%2!=0){
		stop(paste("error in frontierBuilder.R: number of columns of data is odd",numCols))
	}
}

fbUtils.filterForHurdles<-function(tSeries,constraints, hurdleRowName, varScaleFactor){
# this removes from the time series, tSeries, the columns corresponding to assets that don't meet their ret/risk hurdles; constraints is a table containing the hurdles as returned from utils.getConstraints()
# returned the time series with the assets that met their ret/risk hurdles
	
	numCols<-ncol(tSeries)
	
	# imply the number of assets from the number of columns in tSeries
	numAssets<-numCols/2
	# names() returns the headings of the columns, which also are the names of assets
	assetsInSeries<-names(tSeries)[1:numAssets]
	retRiskRatios<-tSeries[,1:numAssets]/tSeries[,(numAssets+1):(2*numAssets)]/varScaleFactor
	# find the row in constraints in which we specify the ret/risk hurdles
	hurdles<-matrix(as.numeric(constraints[hurdleRowName,assetsInSeries]),nrow=1)
	hurdlesMatrix<-hurdles[rep(1,nrow(tSeries)),]
	tSeries[,1:numAssets][retRiskRatios < hurdlesMatrix] <- NA
	
	return(tSeries)
}

fbUtils.getWeightConstraints<-function(constraints,assetNames,assetsToShort,addLeverageConstraint,leverage,baseCc,cashNameSuffix,constraintsFileInfo){
# build the rows to be added to the constraint matrix in the QP problem
# constraints is a table as returned from fbUtils.getConstraint(); assetNames is a vector with the names of assets
# returns a list and two elements: $lhs, has the rows to be added to the constraint matrix; $rhs has the lower bounds for weights
# NOTE: the quadprog solver that we use, requires ">=" type inequalities (with the weight on the left-hand side); so, for assets with upper limits on the weights, we multiply by -1 to get ">=" 
	
	maxRowName <- constraintsFileInfo[["maxRowName"]]
	minRowName <- constraintsFileInfo[["minRowName"]]
	assetClassRowName <- constraintsFileInfo[["assetClassRowName"]]
	assetClassMaxWeightRowName <- constraintsFileInfo[["assetClassMaxWeightRowName"]]
	assetClassMinWeightRowName  <- constraintsFileInfo[["assetClassMinWeightRowName"]]
	
	numAssets<-length(assetNames)
	numEqs<-0
	
	# vector of TRUE/FALSE telling us which assets, if any, to short
	shortIt<-is.finite(match(assetNames,assetsToShort))
	# we want +1 for long assets and -1 for short
	weightSigns<-1-2*as.numeric(shortIt)
	
	assetsInConstraints<-names(constraints)
	maxIndex<-match(maxRowName,row.names(constraints))
	minIndex<-match(minRowName,row.names(constraints))
	
	# add asset constraints
	lhs<-NULL
	rhs<-c()
	for(i in 1:length(assetNames)){
		index<-match(assetNames[i],assetsInConstraints)
		# if following is TRUE, then we have specified weight constraints for that asset
		if(is.finite(index)){
			# get max weight constraint
			maxWeight<-as.numeric(constraints[maxIndex,index])
			# get min weight constraint
			minWeight<-as.numeric(constraints[minIndex,index])

			temp<-rep(0,numAssets)
			# we need a >= type inequality with the weight on the LHS; so we reverse signs
			temp[i]<-(-1) * weightSigns[ i ]
			# add a row to the constraints matrix
			lhs<-rbind(lhs,matrix(temp,nrow=1),deparse.level=0)
			# add an element to the constraints values
			rhs<-c(rhs,-maxWeight)
			temp<-rep(0,numAssets)
			temp[i]<-1 * weightSigns[ i ]
			# add a row to the constraints matrix
			lhs<-rbind(lhs,temp,deparse.level=0)
			# add an element to the constraints values
			rhs<-c(rhs,minWeight)
		} else {
			stop("error in fbUtils.getWeightConstraints(): all assets must have constraints specified")
		}
	} # end asset constraints
	
	# now add asset class constraints
	assetGroupsIndex<-match(assetClassRowName,row.names(constraints))
	if(is.finite(assetGroupsIndex) && length(assetsToShort)==0){ # no asset class constraints if we're shorting
		# we're here therefore asset class constraints were specified
		assetClasses<-unique(as.character(constraints[assetGroupsIndex,]))
		for(i in 1:length(assetClasses)){
			tempX<-rep(0,numAssets)
			tempN<-rep(0,numAssets)
			for(j in 1:numAssets){
				index<-match(assetNames[j],assetsInConstraints)
				if(as.character(constraints[assetGroupsIndex,index])==assetClasses[i]){
					# so we apply the asset class constraints to the NET exposure to that class
					tempX[j]<--1
					tempN[j]<-1
				}
			}
			lhs <-rbind(lhs,matrix(tempX,nrow=1),deparse.level=0)
			lhs <-rbind(lhs,matrix(tempN,nrow=1),deparse.level=0)
			acOccurs<-grep(assetClasses[i],as.character(constraints[assetGroupsIndex,]))
			acMax<-unique(as.numeric(constraints[assetClassMaxWeightRowName,][,acOccurs]))
			acMin<-unique(as.numeric(constraints[assetClassMinWeightRowName,][,acOccurs]))
			if(length(acMax)!=1 || length(acMin)!=1){
				stop("error in fbUtils.getWeightConstraints(): an asset class should have one max and one min weight specified")
			} else {
				rhs <-c(rhs,-acMax)
				rhs <-c(rhs,acMin)
			}
		}
	} # end asset class constraints
	
	# now check for cash positions; if we find any, we add non-base country constraints and global cash constraints; otherwise offsetting cash positions are added in fbUtils.addFxUnitHedge(). NOTE: highly unlikely to be used; we do not include cash positions in the asset mix prior to building the frontier
	cashInds<-grep(cashNameSuffix,assetNames)
	if(length(cashInds)!=0){
		# add non-base country equality constraints; remember that equality constraints should come first in the table as required by quadprog
		ccs<-unique(substr(assetNames,1,2))
		numNonBaseCc<-setdiff(ccs,baseCc)
		if(length(cashInds)!= length(ccs) || !is.element(baseCc,ccs)) {
			stop("error in fbUtils.getWeightConstraints(): cash (the funding rate) positions must be added to all countries or omitted from all. if cash positions are present, base country cash must also be present. check the constraints file.")
		}
		for(cc in numNonBaseCc){
			ccInds<-grep(paste("^",cc,"\\.",sep=""),assetNames)
			newCashConstraint<-rep(0,numAssets)
			# now implicitly assuming that if we're long all non-cash positions in cc, we should be short cc cash (that's how weights sum to 0)
			newCashConstraint[ccInds]<-1.0
			lhs<-rbind(matrix(newCashConstraint,nrow=1),lhs,deparse.level=0)
			rhs<-c(0.,rhs)
			numEqs<-numEqs + 1
		}
		
		# now add global cash constraint, also an equality; NOTE: if we have only one country, this constraint results in 0 cash
		newCashConstraint<-rep(0,numAssets)
		# again the following implicitly assumes that if we're short non-base cash, we must be long baseCc cash
		newCashConstraint[cashInds]<-1.
		lhs<-rbind(matrix(newCashConstraint,nrow=1),lhs,deparse.level=0)
		rhs<-c(0.,rhs)
		numEqs<-numEqs+1
	} # end global cash and non-base country constraints
	
	# finally, add leverage constraint if required. Note: no need for another check for consistency here. if "full investment" constraint is satisfied, then, a fortiori, the next will be since the sum of absolute values of weights is greater than the sum of the weights.
	if(addLeverageConstraint ){
		fbUtils.checkConsistency(lhs,rhs,numEqs)
		# sum of absolute values of weights is 1
		lhs<-rbind(rep(1,numAssets)*weightSigns,lhs,deparse.level=0)
		rhs<-c( leverage, rhs)
		numEqs<-numEqs+1
	} # end leverage and/or full investement constraint
	
	colnames(lhs)<-assetNames
	return(list(lhs=lhs,rhs=rhs,numEqs=numEqs))
}

fbUtils.setLPConstraints<-function(constraintMat,constraintVals,numEqs){
# this is its own separate function because it's needed in a couple of places
# input: constraintMat is the constraint matrix ; constraintVals is a vector with the constraints RHS; numEqs is the number of equality constraints, the remaining constraints are assumed to be ">="
# output: a list with the 4 matrices and vectors as required by linp() from package limSolve
	
	# e contains the equality constraints matrix; f is the RHS
	e<-NULL
	f<-NULL
	if(numEqs !=0){
		e<-constraintMat[1:numEqs,,drop=F]
		f<-constraintVals[1:numEqs]
	}
	
	# g contains the inequality constraints matrix, all assumed to be >= type constraints; h is the RHS
	g<-NULL
	h<-NULL
	if(numEqs<length(constraintVals)){
		g<-constraintMat[(numEqs+1):nrow(constraintMat),,drop=F]
		h<-constraintVals[(numEqs+1):length(constraintVals)]
	}
	
	return(list(e=e,f=f,g=g,h=h))
}

fbUtils.checkConsistency<-function(constraintMat,constraintVals,numEqs){
# making sure the constraints we impose on individual assets are consistent with those on asset classes can be tricky; this function is meant to help
	
	# break constraintMat into equality and inequality constraints as required by linp()
	constraints<-fbUtils.setLPConstraints(constraintMat,constraintVals,numEqs)
	# now solve; ispos=FALSE tells linp() that the solution vector can have negative components; it is this very capability of linp()'s that made us choose it over lp() from package lpSolve; we take the negative of the objective function because linp() solves the minimization problem only; if verbose =TRUE, then linp() prints error messages if any. Note that, under the covers, linp() calls lp()
	lpResults<-linp(E=constraints[["e"]],F=constraints[["f"]],G=constraints[["g"]],H=constraints[["h"]],-rep(1,ncol(constraintMat)),ispos=FALSE,verbose=TRUE)	
	if(-lpResults[["solutionNorm"]]<1){
		stop(paste("error in frontierBuilder: the constraints on asset classes weights are inconsistent with the constraints on asset weights; the sum of all asset weights cannot exceed: ",-lpResults$solutionNorm))
	}
}

# this determines the maximum target return
# solves a linear programming problem
fbUtils.getMaxTargetReturn<-function(ers, constraintMat, constraintVals,numEqs){
	# break constraintMat into equality and inequality constraints as required by linp()
	constraints<-fbUtils.setLPConstraints(constraintMat,constraintVals,numEqs)
	# see comments in fbUtils.checkConsistency() for more info on what we're having to do here
	lpResults<-linp(E=constraints[["e"]],F=constraints[["f"]],G=constraints[["g"]],H=constraints[["h"]],-ers,ispos=FALSE,verbose=TRUE)
	
	return(list(maxRet=-lpResults[["solutionNorm"]],portfolio=lpResults[["X"]]))
}

fbUtils.addFxUnitHedge<-function(baseCc,cashNameSuffix,effFrontier){
# add unit fx hedge
# baseCC is the base country code, e.g., "jp"; cashNameSuffix is for example ".gg.3m"; effFrontier is a data frame with the efficient portfolios as output by *.getFrontier()
# returns a data frame containing the original efficient frontier augmented by cash positions base and foreign
	
	# first 3 columns of effFrontier are "Date", "portRet" and "portRisk"; skip them
	assetNames<-colnames(effFrontier)[4:ncol(effFrontier)]
	
	baseCashName <- paste(baseCc,cashNameSuffix,sep="")
	# get country codes
	ccs<-setdiff(substr(assetNames,1,2),baseCc)
	numNonBaseCc<-length(ccs)
	if( !is.element( baseCashName, assetNames)){
		baseCashWeights<-rep(0,nrow(effFrontier))
	} else {
		baseCashWeights <- effFrontier[, baseCashName ]
	}
	# offset non-base country asset positions with short cash
	if(numNonBaseCc!=0){
		for(i in 1:numNonBaseCc){
			cc<-ccs[i]
			# find columns with assets in country cc
			ccInds<-grep(paste("^",cc,"\\.",sep=""),assetNames)
			# calculate total weights invested in cc; this is an array with one element for each point on the frontier; take a short position in cc cash with that weight
			ccCashWeights<- -rowSums(as.matrix(effFrontier[1:nrow(effFrontier),ccInds+3]),na.rm=TRUE)
			# offset it with position in baseCc cash
			baseCashWeights<-baseCashWeights-ccCashWeights
			# append non base country cash column to frontier portfolios
			effFrontier<-cbind(effFrontier,ccCashWeights,deparse.level=0)
			colnames(effFrontier)[ncol(effFrontier)]<-paste(cc,cashNameSuffix,sep="")
		}
	}
	
	# insert base country cash positions, one for each point on frontier; make the first column because in some other functions we take the first cash column to determine the base cc (for example, simulator)
	if( !is.element( baseCashName, assetNames)){
		baseCashColumn<-matrix(baseCashWeights,ncol=1)
		ret<-cbind(effFrontier[,1:3,drop=F],baseCashColumn,effFrontier[,4:ncol(effFrontier),drop=F],deparse.level=0)
		colnames(ret)<-c("Date","portRet","portRisk", baseCashName,colnames(effFrontier)[4:ncol(effFrontier)])
		return(ret)
	} else {
		effFrontier[, baseCashName ]<- baseCashWeights
		return( effFrontier )
	}
	
}

fbUtils.addCashBenchmark <- function( output, cashBenchmark, cashBenchmarkData ){
	
	outNames <- colnames( output )
	isZeroRow <- rowSums( output[, setdiff(outNames, c("Date", "portRet", "portRisk"))], na.rm= T ) == 0
	if( sum( isZeroRow ) != 0 ){
		if( !is.element(cashBenchmark, outNames)){
			output <- cbind(output[, c("Date", "portRet", "portRisk")], matrix(0, nrow = nrow( output ), ncol = 1), output[, setdiff(outNames, c("Date", "portRet", "portRisk"))], deparse.level= 0 )
			colnames( output ) <- c("Date", "portRet", "portRisk", cashBenchmark, setdiff(outNames, c("Date", "portRet", "portRisk")) )
		}
		output[ isZeroRow, c("portRet", "portRisk", cashBenchmark ) ] <- cbind( cashBenchmarkData[utils.ymdToHyphens(output[,"Date"][isZeroRow]),,drop=F] , matrix( 1, nrow=sum(isZeroRow), ncol = 1 ) , deparse.level = 0)
	}
	
	return( output )
}
