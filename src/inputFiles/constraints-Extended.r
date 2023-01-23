portfolio.settings<-function(x){

portfolio.inList<-list()
# this should be the same as this file
portfolio.inList[["constraintsNAME"]]<-"Extended"
# this should be "monthly", "weekly" or "daily"; the frequency at which we build the efficient frontier
portfolio.inList[["effFrontierFreq"]]<-"weekly"
# this is the frequency of the data we use to calculate data correlations
portfolio.inList[["correlationDataFreq"]]<-"weekly"
# if we want to build the frontier on custom dates only, we set this to a vector (i.e., c()) of dates
portfolio.inList[["desiredDates"]]<-NULL
# the start date for the backtesting
portfolio.inList[["startDateForBackTest"]]<-"1969-12-29"
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
#portfolio.inList[["erMapping"]]<-list(.fer=c("ko.kospi200"),.ter=c("it.mib40"),.ner=c("us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","eu.stxe600","eu.eurostx","eu.utileurope","eu.euroutil","au.equity","au.asx200","hk.equity","sg.equity","no.equity","tw.equity","ca.equity","sp.ibex35","nl.equity","sz.equity"),.jer=c("jp.equity","jp.EPG","jp.topix400","eu.epraeurozone"),.rer=c("us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","eu.rafi"),.er=c("us.property","us.gg.10y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m"))
portfolio.inList[["erMapping"]]<-list(.ner=c("tw.equity","sg.2mscifree","tw.2mscifree"),.fer=c("ko.kospi200"),.jer=c("ko.kospi200","ca.tsx60","sg.mscifree","tw.mscifree","sg.equity","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.utileurope","eu.euroutil","au.equity","au.asx200","hk.equity","jp.equity","jp.EPG","jp.topix400","eu.epraeurozone","ca.equity","sp.ibex35","nl.equity","sz.equity"),.ter=c("it.mib40","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","eu.rafi"),.er=c("us.property","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","sp.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m"))
portfolio.inList[["erMapping"]]<-list(.ner=c("tw.equity","sg.2mscifree","tw.2mscifree"),.jer=c("ko.kospi200","ca.tsx60","sg.mscifree","tw.mscifree","sg.equity","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.utileurope","eu.euroutil","au.equity","au.asx200","hk.equity","jp.equity","jp.EPG","jp.topix400","eu.epraeurozone","ca.equity","sp.ibex35","nl.equity","sz.equity"),.ter=c("it.mib40","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","eu.rafi"),.er=c("us.property","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","sp.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m"))

#portfolio.inList[["erMapping"]]<-list(.ner=c("no.equity","us.msciem","uk.equity","sw.omx30","fr.cac40","ge.equity","au.asx200","hk.equity","sg.equity","tw.equity","ca.equity","nl.equity"),.jer=c("jp.equity","eu.stxe600","eu.eurostx"),.er=c("us.gg.10y","us.gg.3m","jp.gg.10y","jp.gg.3m","au.gg.10y","au.gg.3m","uk.gg.10y","uk.gg.3m","ge.gg.10y","ge.gg.3m","fr.gg.3m","eu.gg.3m","sw.gg.10y","sw.gg.3m","ca.gg.10y","ca.gg.3m","sg.gg.3m","hk.gg.3m","it.gg.10y","it.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","tw.gg.3m","nl.gg.3m"))
#portfolio.inList[["erMapping"]]<-list(.ner=c("tw.equity"),.fer=c("ko.kospi200"),.jer=c("sg.equity","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","eu.stxe600","eu.eurostx","eu.utileurope","eu.euroutil","au.equity","au.asx200","hk.equity","jp.equity","jp.EPG","jp.topix400","eu.epraeurozone","ca.equity","sp.ibex35","nl.equity","sz.equity"),.ter=c("it.mib40","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","eu.rafi"),.er=c("us.property","us.gg.10y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","sp.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m"))


# which risk model?,"no.equity"
portfolio.inList[["riskMapping"]]<-list(.orisk=c("sg.2mscifree","tw.2mscifree","sg.mscifree","tw.mscifree","ca.tsx60","ko.kospi200","us.dji","no.equity","us.snp600","us.mscieafe","us.msciem","us.msciworld","us.russell2000","jp.rafi","uk.asx","uk.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","ge.cdax","ge.equity","jp.equity","jp.EPG","jp.topix400","us.equity","us.snp100","us.snp400","us.rafi","us.djutil","us.msciasiapacxjpn","us.nasdaq100","uk.ftse250","au.equity","au.asx200","hk.equity","sg.equity","it.mib40","sz.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.rafi","eu.utileurope","eu.euroutil","eu.epraeurozone","tw.equity","ca.equity","sp.ibex35","nl.equity","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","us.gcorp.a","jp.gg.10y","jp.gg.3m","jp.gcorp","au.gg.10y","au.gg.3m","au.gcorp","uk.gg.10y","uk.gg.3m","uk.gcorp","ge.gg.10y","ge.gg.3m","ge.gcorp","fr.gg.10y","fr.gg.3m","fr.gcorp","eu.gg.10y","eu.gg.3m","eu.gcorp","sw.gg.10y","sw.gg.3m","sw.gcorp","ca.gg.10y","ca.gg.3m","ca.gcorp","sg.gg.10y","sg.gg.3m","hk.gg.10y","hk.gg.3m","it.gg.10y","it.gg.3m","no.gg.10y","no.gg.3m","sz.gg.10y","sp.gg.3m","sz.gg.3m","mx.gg.10y","mx.gg.3m","tw.gg.3m","ko.gg.3m","nl.gg.3m"))


return(portfolio.inList)
}
