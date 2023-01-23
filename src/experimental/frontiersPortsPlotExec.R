frontiersPortsPlotExec.main <- function(dates, targetPortsFile, cPortsFile, heldPortsFile, frontiersFile, reallocFile, inputDir, outputDir){
	pathToMe <- dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	
	pngSettings <- global.pngSettings()
	
	frontiers <- read.csv(paste(inputDir,frontiersFile,sep=""))
	frontiers <- frontiers[is.element(frontiers$Date, dates),]
	targetPorts <- read.csv(paste(inputDir,targetPortsFile,sep=""))
	targetPorts <- targetPorts[is.element(targetPorts$Date, dates),]
	heldPorts <- read.csv(paste(inputDir,heldPortsFile,sep=""))
	heldPorts <- heldPorts[is.element(heldPorts$Date, dates),]
	cPorts <- read.csv(paste(inputDir,cPortsFile,sep=""))
	cPorts <- cPorts[is.element(cPorts$Date, dates),]
	reallocDates <- read.csv(paste(inputDir,reallocFile,sep=""))[["Date"]]
	
	dates <- sort(intersect(frontiers$Date,intersect(targetPorts$Date,intersect(heldPorts$Date, cPorts$Date))))
	
	for(i in 1:length(dates)){
		fileName <- paste(outputDir,dates[i],".plot.png",sep="")
		png(filename=fileName, width = pngSettings[["width"]], height = pngSettings[["height"]],res= pngSettings[["res"]],pointsize= pngSettings[["pointsize"]], bg = pngSettings[["bg"]])
		
		frontier <- frontiers[frontiers$Date==dates[i],]
		targetPort <- targetPorts[targetPorts$Date==dates[i],]
		heldPort <- heldPorts[heldPorts$Date==dates[i],]
		cPort <- cPorts[cPorts$Date == dates[i],]

		# plot frontier
		mainLabel <- ifelse(!is.element(dates[i],reallocDates),dates[i],paste(dates[i],"(rebalanced)"))
		plot(frontier$portRisk,frontier$portRet, col="green",main=mainLabel) ################### ,xlim=c(0,max(frontier$portRisk)),ylim=c(0,max(frontier$portRet)))
		# plot line from origin to target portfolio
		lines(c(0,targetPort$portRisk),c(0,targetPort$portRet), col="red")
		# show target portfolio
		points(targetPort$portRisk,targetPort$portRet, pch=19, col="red")
		# show the currently held portfolio
		points(heldPort$portRisk,heldPort$portRet, pch=25, col="magenta")
		# show best sharpe point
		points(cPort$portRisk, cPort$portRet, pch= 22, col="blue")
		# draw line from origin to sharpe 
		lines(c(0,cPort$portRisk),c(0,cPort$portRet),col="blue")
		# draw horizontal line from sharpe to the line showing the slope of the M point
		lines(c(cPort$portRisk,cPort$portRet / (targetPort$portRet/targetPort$portRisk)), c(cPort$portRet,cPort$portRet), col="gold4")
		# label the points
		text(c(targetPort$portRisk,heldPort$portRisk,cPort$portRisk),c(targetPort$portRet,heldPort$portRet,cPort$portRet),labels=c(paste("(",round(targetPort$portRisk,3),", ",round(targetPort$portRet,3),")",sep=""),paste("(",round(heldPort$portRisk,3),", ",round(heldPort$portRet,3),")",sep=""),paste("(",round(cPort$portRisk,3),", ",round(cPort$portRet,3),")",sep="")),cex=0.75)
		
		# flush into png file
		graphics.off()
	}
}

# execute. NOTE: the simPort.csv file was obtained by inserting a print() statement of curPortRetRisk in rebalancing.R.
frontiersPortsPlotExec.main(as.character(seq(as.Date("1970-01-05"),as.Date("2012-07-30"),by="1 week")),"Extended-M-Monday.csv","Extended-C-Monday.csv","simPort.csv","Extended-Monday-Opt-FULL.csv","Extended-M-Monday.realloc.csv","c:/Users/Majed/devel/InOut/", "c:/Users/Majed/devel/InOut/recentfrontiers/")