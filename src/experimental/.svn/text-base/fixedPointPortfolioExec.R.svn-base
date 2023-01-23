fixedPointPortfolioExec.main <- function(frontiersFileName,numFrontierPoints,portFileNamePrefix,portFileNameSuffix){
# the output portfolios will be called paste(portFileNamePrefix,"-x-",portFileNameSuffix,sep=""), where x=1,...,numFrontierPoints. file can be .csv or .RData
	
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	

	coreSettings <- global.core()
	frontiers <- utils.read(paste(coreSettings[["input"]],frontiersFileName,sep=""))
	doRData <- F
	if(length(grep("\\.RData",portFileNameSuffix)) != 0) {
		doRData <- T
	}
	
	filenames <- c()
	for(i in 1:numFrontierPoints){
		if(!doRData){
			##############write.csv(frontiers[grep(i,frontiers$Port),setdiff(names(frontiers),"Port")], paste(coreSettings[["dataOutput"]],portFileNamePrefix,"-",i,"-",portFileNameSuffix,sep=""), row.names=F)
			filenames<-c(filenames, paste(portFileNamePrefix,"-",i,"-",portFileNameSuffix,sep=""))
		} else {
			temp <- frontiers[grep(i,frontiers$Port),setdiff(names(frontiers),"Port")]
			save(temp, file= paste(coreSettings[["dataOutput"]],portFileNamePrefix,"-",i,"-",portFileNameSuffix,sep=""))
		}
	}
print(filenames)
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
fixedPointPortfolioExec.main("Extended-Monday-Opt-FULL.RData", 31, "Extended","Monday.csv")