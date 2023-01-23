actualVsPredictedFrontiers.main<-function(whichDates){
	pathToMe<-dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	
	coreSettings<-global.core()
	pngSettings<-global.pngSettings()
	
	iWantPNG<-TRUE
	iWantGrid<-TRUE
	
	actualFrontiersNames<-utils.load(file=paste(coreSettings[["optOutput"]],"NonUSFutures2-test-PROD-Forward-104-Monday-vol-Monday-Opt-FULL.RData",sep=""))
	actualFrontierData<-as.data.frame(get(actualFrontiersNames))
	
	predictedFrontierNames<-utils.load(file=paste(coreSettings[["optOutput"]],"NonUSFutures2-test-PROD-FINAL-JTO-104-Monday-vol-Monday-Opt-FULL.RData",sep=""))
	predictedFrontierData<-as.data.frame(get(predictedFrontierNames))
	
	historicbasedFrontierNames<-utils.load(file=paste(coreSettings[["optOutput"]],"NonUSFutures2-test-PROD-Historic-104-Monday-his-Monday-Opt-FULL.RData",sep=""))
	historicbasedFrontierData<-as.data.frame(get(historicbasedFrontierNames))

	for(d in whichDates){
		aFrontier<-actualFrontierData[actualFrontierData[["Date"]]==d,]
		pFrontier<-predictedFrontierData[predictedFrontierData[["Date"]]==d,]
		hFrontier<-historicbasedFrontierData[historicbasedFrontierData[["Date"]]==d,]	

		if(iWantPNG){
			png(filename=paste(coreSettings[["optOutput"]],"actVsPredVsHist.",d,".png",sep=""), width = pngSettings[["width"]], height = pngSettings[["height"]],res= pngSettings[["res"]],pointsize= pngSettings[["pointsize"]], bg = pngSettings[["bg"]])
		}else{
			dev.new()
		}
		
		minx<-min(min(aFrontier[["portRisk"]]),min(pFrontier[["portRisk"]]),min(hFrontier[["portRisk"]]))
		maxx<-max(max(aFrontier[["portRisk"]]),max(pFrontier[["portRisk"]]),max(hFrontier[["portRisk"]]))
		miny<-min(min(aFrontier[["portRet"]]),min(pFrontier[["portRet"]]),min(hFrontier[["portRet"]]))
		maxy<-max(max(aFrontier[["portRet"]]),max(pFrontier[["portRet"]]),max(hFrontier[["portRet"]]))

		plot(aFrontier[["portRisk"]],aFrontier[["portRet"]],xlim=c(minx,maxx),ylim=c(miny,maxy),xlab="risk",ylab="return",main=d,col=2,type="p")
		if(iWantGrid){
			grid()
		}
		points(pFrontier[["portRisk"]],pFrontier[["portRet"]],col=3)
		points(hFrontier[["portRisk"]],hFrontier[["portRet"]],col=4)
		legend("topleft",legend=c("actual","predicted","historicreturnbased"),lty=1,col=2:4)
				
		if(iWantPNG){
			# flush into file
			graphics.off()
		}	
	}
}

actualVsPredictedFrontiers.main(c("1999-12-27","1998-09-28","1997-06-30","1993-12-27","1987-12-28","1982-10-25","1981-01-26","1974-12-30","1972-12-25"))
