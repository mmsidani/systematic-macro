shiftWeeksExec.main <- function(portfolioHistoryFileName, numberOfWeeks){
	print("INFO from shiftWeeksExec.main(): this function only works if the portfolio was built weekly.")
	
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	
	# get input/output settings
	coreSettings <- global.core()
	
	outputFile <- paste("shiftWeeks",portfolioHistoryFileName,sep="")
	portfolioHistoryFileName <- paste(coreSettings[["optOutput"]],portfolioHistoryFileName,sep="")
	
	if(grepl("\\.RData$",portfolioHistoryFileName)){
		portfolioHist <- utils.load(portfolioHistoryFileName)
	}else{
		portfolioHist <- read.csv(portfolioHistoryFileName, stringsAsFactors=FALSE)
	}
	
	dates <- portfolioHist[["Dates"]]
	shiftedPortfolioHistory <- portfolioHist[1:(nrow(portfolioHist)-numberOfWeeks),]
	shiftedPortfolioHistory[["Date"]] <- dates[(numberOfWeeks+1):length(dates)]
	
	write.csv(shiftedPortfolioHistory,paste(coreSettings[["dataOutput"]],outputFile,sep=""),row.names=FALSE)
	return(shiftedPortfolioHistory)
}

# execute
shiftWeeksExec.main("Extended-M-Monday.csv",4)
