dataCleanup.suspects <- function(){
	load("\\\\SOL\\SA2Trader\\output\\Production-IO\\DATA-Output\\2013-04-22\\forecasts.RData")
	forecasts<-output
	erForecasts<-forecasts[1, !grepl("risk$", names(forecasts))]
	erSuspects <- erForecasts[1, (2:ncol(erForecasts))[erForecasts[1,2:ncol(erForecasts)] > 0.15]]
	
	# got this output from last statement
#	us.msciasiapacxjpn.ner us.msciasiapacxjpn.rer us.msciasiapacxjpn.jer us.msciasiapacxjpn.ser us.msciasiapacxjpn.ter us.msciasiapacxjpn.fer us.russell2000.ner us.russell2000.rer us.russell2000.jer us.russell2000.ser
#	1              0.2774879              0.3217763              0.2699354              0.2418703              0.3139524              0.2492497           0.646086           0.646086          0.6315099          0.5951537
#	us.russell2000.ter us.russell2000.fer ge.cdax.ner ge.cdax.rer ge.cdax.jer ge.cdax.ser ge.cdax.ter ge.cdax.fer it.mib40.ner it.mib40.rer sp.ibex35.ner sp.ibex35.rer eu.utileurope.ner eu.utileurope.rer eu.euroutil.ner
#	1          0.6315099          0.6094027    1.335362    1.335362    1.286554     1.25057    1.286554    1.298605    0.1645729    0.1645854     0.1737367     0.1737367         0.1592371         0.1592371       0.1842693
#	eu.euroutil.rer eu.euroutil.jer eu.euroutil.ter eu.euroutil.fer sg.equity.ner sg.equity.rer sg.equity.jer sg.equity.ter sg.2mscifree.ner sg.2mscifree.rer
#	1       0.1842693       0.1612771       0.1612771       0.1615009     0.1561561     0.1561561     0.1520619     0.1520619         0.150473         0.150473
	
	suspects <- c("us.msciasiapacxjpn","us.russell2000","ge.cdax","it.mib40","sp.ibex35","eu.utileurope","eu.euroutil","sg.equity","sg.2mscifree")
	load("\\\\SOL\\SA2Trader\\output\\Production-IO\\DATA-Output\\2013-04-22\\DBF.marketData.RData")
	marketData <- tempDF
	
	for( ss in suspects){
		print(ss)
		temp <- marketData[,c("Date", paste(ss,c("",".earnings",".dvd.yld"), sep=""))]
		for(i in 2:ncol(temp)){
			dev.new()
			iTemp <- temp[is.finite(temp[[i]]), c(1, i)]
			plot(as.Date(iTemp[[1]]), iTemp[[2]], main=names(temp)[i])
		}
		write.csv( temp, paste("c:/Users/majed/devel/InOut/",ss,".data.csv",sep=""), row.names=F)
	}
}

dataCleanup.replace_tbl_rate_data <- function(){
# logic used to move data from the old tbl_rate_data to the revisable tbl_sa2_mkt_proc_data
	
	pathToMe <- dirname( sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../utils/dbDataLoader.R",sep=""))
	source(paste(pathToMe,"/../utils/miscUtils.R",sep=""))
	
	library(RODBC)
	
	dbSettings <- global.core()[["dbSettings"]]
	channel <- dbDataLoader.getConnection(dbSettings[["ld.uid"]], dbSettings[["ld.dsnName"]])
	rateIds <- dbDataLoader.getTable(channel, "tbl_rate_header")
	allData <- dbDataLoader.getTable(channel, "tbl_rate_data")
	save(allData, file="c:/Users/majed/devel/allDBData.RData")
	
	inds <- match(allData[["Rate_Id"]], rateIds[["Rate_Id"]])
	if(any(!is.finite(inds))){
		stop(paste("ERROR in dataCleanup.replace_tbl_rate_data(): what's happening?", paste(allData[["Rate_Id"]][!is.finite(inds)], collapse=", ")))
	}
	allData <- cbind(allData[,c("Date","Rate")], data.frame(name=rateIds[inds,"descr"]))
	
	print("uploading")
	
	utils.uploadStackedDataFromDataFrame(allData, "name", "Rate", "R_CON",FALSE,dbSettings, "DD")
	dbDataLoader.close(channel)
}

# execute
currentData<-dataCleanup.replace_tbl_rate_data()
