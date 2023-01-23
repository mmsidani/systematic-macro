portfolio.settings<-function(x){

portfolio.inList<-list()
# this should be the same as this file
portfolio.inList[["constraintsNAME"]]<-"Base"
# this should be "monthly", "weekly" or "daily"; the frequency at which we build the efficient frontier
portfolio.inList[["effFrontierFreq"]]<-"weekly"
# this is the frequency of the data we use to calculate data correlations
portfolio.inList[["correlationDataFreq"]]<-"weekly"
# if we want to build the frontier on custom dates only, we set this to a vector (i.e., c()) of dates
portfolio.inList[["desiredDates"]]<-NULL
# the start date for the backtesting
portfolio.inList[["startDateForBackTest"]]<-"1974-12-29"
# the end date for backtesting
portfolio.inList[["endDateForBackTest"]]<- "today"
# if we're doing weekly data, this is the day of week we want to build on
portfolio.inList[["dayOfWeek"]]<-"Monday"
# number of frontier points
portfolio.inList[["numFrontierPoints"]]<-31
# rolling window used to build correlations
portfolio.inList[["rollingWindow"]]<-104
# base country code. should be "us", "uk", etc.
portfolio.inList[["baseCc"]]<-"us"
# for exponential weighting data when building correlations; this is in units of portfolio.inList[["correlationDataFreq"]]; use 0 to get equal weights
portfolio.inList[["halfLife"]]<-0
# which variance measure?
portfolio.inList[["riskMeasure"]]<-"volatility" # or "volatility" or "erCovariance"
# how much leverage?
portfolio.inList[["leverage"]]<-1.0
# which er model?
portfolio.inList[["erMapping"]]<-list(.ber=c("tw.equity","sg.2mscifree","tw.2mscifree","ko.kospi200","ca.tsx60","sg.mscifree","tw.mscifree","sg.equity","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.utileurope","eu.euroutil","au.equity","au.asx200","hk.equity","jp.equity","jp.EPG","jp.topix400","eu.epraeurozone","ca.equity","sp.ibex35","nl.equity","sz.equity","it.mib40","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","eu.rafi","ch.a50","hk.chinaent","in.nifty","pl.wig20","id.mscifree","in.equity","tk.ise30","th.set50","mx.equity","my.equity","sa.equity"),.er=c("us.property","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","sp.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m","sa.gg.3m","pl.gg.3m","tk.gg.3m","my.gg.3m","th.gg.3m","in.gg.3m","id.gg.3m","ch.gg.3m","ru.gg.3m","bz.gg.3m"))

# which risk model?,"no.equity"
portfolio.inList[["riskMapping"]]<-list(.orisk=c("sg.2mscifree","tw.2mscifree","sg.mscifree","tw.mscifree","ca.tsx60","ko.kospi200","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","jp.equity","jp.EPG","jp.topix400","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","au.equity","au.asx200","hk.equity","sg.equity","it.mib40","sz.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.rafi","eu.utileurope","eu.euroutil","eu.epraeurozone","tw.equity","ca.equity","sp.ibex35","nl.equity","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sp.gg.3m","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m","sa.gg.3m","pl.gg.3m","tk.gg.3m","my.gg.3m","th.gg.3m","in.gg.3m","id.gg.3m","ch.gg.3m","bz.gg.3m","ru.gg.3m","ch.a50","hk.chinaent","in.nifty","pl.wig20","id.mscifree","in.equity","tk.ise30","th.set50","mx.equity","my.equity","sa.equity"))


return(portfolio.inList)
}
