resamplePortfolio.main <- function(numSamples, portStatsRows ){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe, "/../settings/GlobalVariables.R", sep=""))
	source(paste(pathToMe, "/../utils/miscUtils.R", sep=""))
	source(paste(pathToMe, "/../utils/dbDataLoader.R", sep=""))
	source(paste(pathToMe, "/../simulation/portfolioSimStats.R", sep=""))
	
	library(RODBC)
	library(timeSeries)
	
	outputDir <- global.core()[["dataOutput"]]
	
	settings <- global.portfolioSimStatsExecSettings()
	portfolioName <- settings[["psstats.portfolioNames"]]
	if(length( portfolioName ) != 1){
		stop(paste("ERROR in resamplePortfolio.main(): can only have 1 portfolio."))
	}
	simStatInput <- portfolioSimStats.input( settings )

	portRets <- simStatInput[["allResults"]][[1]][["ret"]]
	for( i in 1:numSamples){
		rets<- sample(portRets, size=length(portRets), replace=T)
		simStatInput[["allResults"]][[1]][["ret"]] <- rets
		simStatOutput <- portfolioSimStats.calculate( simStatInput, settings )
		temp <- simStatOutput[["portStatsFirsts"]]

		if( i != 1){
			yearlyComparisons[, 2] <- yearlyComparisons[, 2] + simStatOutput[["yearlyComparisons1"]][, 2]
			portStats <- portStats + as.numeric(temp[ is.element( rownames(temp), portStatsRows ), 1] )
		} else {
			yearlyComparisons <- simStatOutput[["yearlyComparisons1"]]
			portStats <- as.numeric( temp[ is.element( rownames(temp), portStatsRows ), 1])
		}
	}
	
	portStats <- data.frame(matrix(portStats/ numSamples, ncol=1))
	rownames(portStats)<- rownames(temp)[ is.element( rownames(temp), portStatsRows )]
	names(portStats) <- names( temp )[1]
	yearlyComparisons[, 2] <- yearlyComparisons[, 2] / numSamples
	
	write.csv(portStats,paste(outputDir,portfolioName, ".resampled.stats.csv", sep=""), row.names = T)
	write.csv( yearlyComparisons, paste(outputDir,portfolioName, ".resampled.yearlyreturns.csv", sep=""), row.names= F)
}

# execute
resamplePortfolio.main(1000, c("IRR", "yearly.AverageYear", "monthly.MaxDrawdown", "monthly.AverageSHARPE") )
