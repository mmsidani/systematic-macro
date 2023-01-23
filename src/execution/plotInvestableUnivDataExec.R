plotInvestableUnivDataExec.main <- function(){
	pathToMe <- dirname(sys.frame(1)$ofile)
	pathToRSourceCode <- paste(pathToMe, "/../", sep="")
	
	source(paste(pathToRSourceCode, "settings/GlobalVariables.R", sep=""))
	source(paste(pathToRSourceCode, "utils/miscUtils.R", sep=""))
	
	library(timeSeries)
	library(RODBC)
	
	constraintsFiles.portfolioNames<-NULL
	source(paste(pathToRSourceCode,"inputFiles/constraintsFiles.R",sep=""))
	portfolioNames<-constraintsFiles.portfolioNames()
	for (portfolioName in portfolioNames){
		portfolio.settings<-NULL
		
		# now source the file to reset portfolio.settings()
		source(paste(pathToRSourceCode,"inputFiles/constraints-",portfolioName,".R",sep=""))
		# by source'ing this file we have reset portfolio.settings(); portfolioName may or may not be used in portfolio.settings()
		constraintsName<-portfolio.settings(portfolioName)[["constraintsNAME"]]
		constrs <- utils.getConstraints(paste(pathToRSourceCode,"inputFiles/", sep=""), paste("constraints-",constraintsName,".csv",sep=""))
		
		dataNames <- names(constrs)
		classNames <- constrs["asset_class",]
		dataNames <- c(dataNames, paste(dataNames[ as.character(classNames) =="equity" ],c(".earnings", ".dvd.yld"), sep=""))
	}
	
	allData <- utils.getDataFromDB(dataNames, NULL, global.core()[["dbSettings"]])
	for(i in 1:ncol(allData)){
		dev.new()
		isNotNA <- as.logical(is.finite(allData[,i]) )
		plot(as.Date( rownames(allData)[isNotNA]), allData[isNotNA, i], type="l", col="salmon", xlab="Date", ylab=colnames(allData)[i], main=colnames(allData)[i])
	}
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
plotInvestableUnivDataExec.main()