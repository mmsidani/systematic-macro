presentationTests.plot <- function( nodes, v1, v2, functionToUse, graphicsDevice, outputFile, plotTitle, subTitle, labels, xLabel, yLabel, ourPalette, pngSettings ){
	if( length(nodes)+1 != length(labels) || length(v1) != length(v2) ){
		stop(paste("ERROR in presentationTests.plot(): we should have length(labels) == length(nodes)+1 and length(v1) == length(v2) "))
	}
	
	ret <- utils.aggregateForBarplot(nodes, v1, v2, functionToUse)
	labels <- labels[ret[,1]]
	
	draw.barPlot(graphicsDevice, outputFile, ret[,2], plotTitle, subTitle, labels, xLabel, yLabel, ourPalette, pngSettings, list(x="topleft"))
}

presentationTests.fromFvP2EPeak <- function(a, inputDir){
	
	x1 <- read.csv(paste(inputDir, a, ".", "fairvalue.csv", sep=""), stringsAsFactors=F)
	x1[, 1] <- utils.ymdToHyphens(x1[,1])
	x2 <- read.csv(paste(inputDir, a, ".", "ptoepeak.csv", sep=""), stringsAsFactors=F)
	
	commonDates <- intersect( x1[, 1], x2[, 1]) 
	
	x1 <- x1[is.element(x1[,1], commonDates),]
	x2 <- x2[is.element(x2[,1], commonDates),]
	
	return( data.frame (Date=commonDates, value =  (x1[,2]/x2[,2])^.1 - 1 ) )
	
}

presentationTests.jer <- function( a , inputDir ) {

	ers <- utils.load(paste(inputDir, "forecasts.RData", sep=""))
	
	aErs <- ers[ , c("Date", paste(a, ".jer", sep="") )]
	
	return ( aErs[ is.finite(aErs[[2]]), ])
}

presentationTests.ner <- function( a , inputDir ) {
	
	ers <- utils.load(paste(inputDir, "forecasts.RData", sep=""))
	
	aErs <- ers[ , c("Date", paste(a, ".ner", sep="") )]
	
	return ( aErs[ is.finite(aErs[[2]]), ])
}

presentationTests.main <- function(a, functionToPredict, numYears, numSubs, inputDir, nodes, functionToUse,graphicsDevice, outputDir, outputFile, plotTitle, subTitle, xLabel, yLabel){
# input files: the files ".fairvalue", ".ptoepeak", ".avgearnings" were obtained by sticking write.csv() statements at the right places in equityReturnModels.R
# produces bar plots for a function of returns over the next year (mean() for example) given buckets of mispricing (V_inf/V_current)^1/T -1
	
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	source(paste(pathToMe,"/../utils/dbDataLoader.R",sep=""))
	source(paste(pathToMe,"/../utils/draw3.R"))
	
	library(RODBC)
	library(timeSeries)
	
	predictor <- functionToPredict( a , inputDir )
	
	y <- utils.getDataFromDB(paste(a,".dtr",sep=""),NULL,global.core()[["dbSettings"]])
	y <- cbind(data.frame(Date=rownames(y),data.frame(y)))
	y<- y[as.logical(is.finite(y[,2])), ]
	y <- y[nrow(y):1, ]
	cumRets <- cumprod(1+as.numeric(y[,2]))
	cumY <- data.frame( Date=rev(y[,1]), cumret=rev(cumRets))
	
	commonDates <- intersect( predictor$Date,cumY[,1])
	cumY <- cumY[is.element(cumY[,1], commonDates),]	
	
	ind1 <- round(numYears * 261,0)
	ind2 <- ind1 + 1
	predictor <- predictor[ind2:length(commonDates) , 2]
	cumY <- cumY[1:(length(commonDates)-ind1), 2] / cumY[ind2:length(commonDates), 2] -1
	cumY <- cumY[1:(length(commonDates)-ind1)]

	nodes <- round(quantile(predictor,probs=seq(0, 1, 1/numSubs)), 2)

	labels <- c(paste("mp <=",nodes[1]),paste(nodes[1:(length(nodes)-1)],"< mp <=",nodes[2:length(nodes)]),paste("mp >",nodes[length(nodes)],sep=""))
	presentationTests.plot(nodes,predictor,cumY,functionToUse,graphicsDevice, paste(outputDir,a,".",outputFile,sep=""), plotTitle, subTitle, labels, xLabel, yLabel, global.palette(), global.pngSettings())
	
	print(paste(sys.frame(1)$ofile, "done."))
}

secs <- c( "ge.equity","fr.cac40","uk.equity","sw.omx30","eu.stxe600","jp.equity","au.asx200","sg.2mscifree","ca.tsx60","hk.equity","nl.equity","sp.ibex35","it.mib40","no.equity","sz.equity","tw.mscifree")
#execute

for ( sec in secs ) {
	presentationTests.main(sec, presentationTests.jer, 1, 20, "c:/Users/majed/devel/InOut/", nodes, mean, "png", "c:/Users/majed/devel/InOut/", "returnsJ.png", "1 year ahead", "", "ER", "Return")
	presentationTests.main(sec, presentationTests.ner, 1, 20, "c:/Users/majed/devel/InOut/", nodes, mean, "png", "c:/Users/majed/devel/InOut/", "returnsN.png", "1 year ahead", "", "ER", "Return")
}