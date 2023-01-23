global.core<-function(){
	options(stringsAsFactors=FALSE)
	
	whoami <- Sys.getenv("USERNAME")
	global.directorymap<-"P:/" # P FOR PROUDCTION T FOR TESTING
	# db settings
	ld.dsnName<-"SA2MarketData" # "SA2MarketData" or "SA2MarketDataTerra"
	ld.rateDescrTable<-"tbl_rate_header"
	ld.rateDataTable<-"tbl_rate_data"
	ld.macroDataTable<-"tbl_sa2_macro_data"
	ld.dbDateFormat<-"%Y-%m-%d"
	# end db setting
	
	options(scipen=999)
	
	# .Platform$OS.type is an alternative that returns "unix" or "windows"
	if(whoami == "rob" || whoami == "Rob") {
		print("Rob")
		ld.uid<-"rob"
		
		global.isWindows<-TRUE
		global.useDB<-TRUE
		
		ret<-list(trades=paste(global.directorymap,"Input/",sep=""),input=paste(global.directorymap,"DATA-Output/",sep=""),dataOutput=paste(global.directorymap,"DATA-Output/",sep=""),optOutput=paste(global.directorymap,"OPT-Output/",sep=""),simOutput=paste(global.directorymap,"SIM-Output/",sep=""),useDB=global.useDB)
		
	}else if (whoami == "SA2Trader"){
		print("SA2Trader")
		ld.uid<-"SA2Trader"
		
		global.isWindows<-TRUE
		global.useDB<-TRUE	
		
		ret<-list(trades=paste(global.directorymap,"Trades/",sep=""),input=paste(global.directorymap,"Input/",sep=""),dataOutput=paste(global.directorymap,"DATA-Output/",sep=""),optOutput=paste(global.directorymap,"OPT-Output/",sep=""),simOutput=paste(global.directorymap,"SIM-Output/",sep=""),useDB=global.useDB)

	}else if (whoami == "Majed" || whoami == "majed" || whoami == "sidani"){
		print("Majed") 		
		ld.uid<-"majed"
		
		thisPlatform <- .Platform$OS.type
		
		if(thisPlatform == "windows") {
			# Majed on work machine
			global.isWindows<-T
			global.useDB<-T
			global.inputDir<- "c:/Users/Majed/devel/InOut/"
			global.outputDir<-"c:/Users/Majed/devel/InOut/"
		} else{
			# Majed on home machine (Mac)
			global.isWindows<-FALSE
			global.useDB<-FALSE
			global.inputDir<-"/Users/sidani/devel/temp/"
			global.outputDir<-"/Users/sidani/devel/temp/"
		}
		
		ret<-list(trades=global.outputDir,input=global.inputDir,dataOutput=global.outputDir,optOutput=global.outputDir,simOutput=global.outputDir,useDB=global.useDB)
		
	} else {
		stop(paste("ERROR in R: who is",whoami,"?"))
	}

	# set the maximum memory R could use to this number in Megabytes
	if( global.isWindows ) {
		memory.limit( size = 40000 )
	}
	
	ret <- append(ret, list(isWindows=global.isWindows))
	
	# asOfDate is only used for revisable tables. we set it for both anyway. when NULL we get the data from the revisable table that was entered last but BEFORE asOfDate
	dbSettings <- list(ld.uid=ld.uid,ld.dsnName=ld.dsnName,ld.rateDescrTable=ld.rateDescrTable, ld.rateDataTable=ld.rateDataTable,ld.dbDateFormat=ld.dbDateFormat,ld.isRevisable=FALSE, ld.asOfDate=NULL)
	macroDbSettings <- list(ld.uid=ld.uid,ld.dsnName=ld.dsnName,ld.rateDescrTable=ld.rateDescrTable,ld.rateDataTable=ld.macroDataTable,ld.dbDateFormat=ld.dbDateFormat,ld.isRevisable=TRUE, ld.asOfDate=NULL)
	
	return(append(ret,list(dbSettings=dbSettings, macroDbSettings=macroDbSettings)))

}

global.bondSettings<-function(){
	
	defaultPremia<-list(us.gcorp.a=0.006,uk.gcorp=0.0075,eu.gcorp=0.0075,ca.gcorp=0.0075,au.gcorp=0.0075,sw.gcorp=0.0075,sz.gcorp=0.0075,ge.gcorp=0.0075,fr.gcorp=0.0075,hk.gcorp=0.0075,sg.gcorp=0.0075,it.gcorp=0.0075,sp.gcorp=0.0075)
	illiquiditySpreads<-list(us.gcorp.a=0.008,uk.gcorp=0.009,eu.gcorp=0.009,ca.gcorp=0.009,au.gcorp=0.009,sw.gcorp=0.009,sz.gcorp=0.009,ge.gcorp=0.009,fr.gcorp=0.009,hk.gcorp=0.009,sg.gcorp=0.009,it.gcorp=0.009,sp.gcorp=0.009)
	couponsPerYear<-list(il.gg.3m=1,co.gg.3m=1,cl.gg.3m=1,dk.gg.3m=1,ru.gg.3m=1,bz.gg.3m=1,tk.gg.3m=1,ph.gg.10y=2,ph.gg.2y=2,ph.gg.3m=1,my.gg.10y=2,my.gg.2y=2,my.gg.3m=1,ko.gg.10y=2,ko.gg.3y=2,ko.gg.2y=2,ko.gg.3m=1,id.gg.10y=2,id.gg.2y=2,id.gg.3m=1,in.gg.10y=2,in.gg.2y=2,in.gg.3m=1,tw.gg.10y=2,tw.gg.2y=2,tw.gg.3m=1,ch.gg.10y=2,ch.gg.2y=2,ch.gg.3m=1,pl.gg.10y=1,pl.gg.2y=1,pl.gg.3m=1,th.gg.10y=2,th.gg.2y=2,th.gg.3m=1,sa.gg.10y=2,sa.gg.2y=2,sa.gg.3m=1,mx.gg.10y=2,mx.gg.2y=2,mx.gg.3m=1,nz.gg.10y=2,nz.gg.2y=2,nz.gg.3m=1,nl.gg.10y=1,nl.gg.2y=1,nl.gg.3m=1,no.gg.10y=1,no.gg.2y=1,no.gg.3m=1,us.gg.30y=2,us.gg.10y=2,us.gg.7y=2,us.gg.5y=2,us.gg.3y=2,us.gg.2y=2,us.gg.3m=1,jp.gg.10y=2,jp.gg.2y=2,jp.gg.3m=1,uk.gg.10y=2,uk.gg.2y=2,uk.gg.3m=1,au.gg.10y=2,au.gg.2y=2,au.gg.3m=1,ca.gg.10y=2,ca.gg.2y=2,ca.gg.3m=1,eu.gg.10y=1,eu.gg.2y=1,eu.gg.3m=1,ge.gg.10y=1,ge.gg.2y=1,ge.gg.3m=1,fr.gg.10y=1,fr.gg.2y=1,fr.gg.3m=1,it.gg.10y=2,it.gg.2y=2,it.gg.3m=1,sp.gg.10y=1,sp.gg.2y=1,sp.gg.3m=1,sz.gg.10y=1,sz.gg.2y=1,sz.gg.3m=1,sw.gg.10y=1,sw.gg.2y=1,sw.gg.3m=1,hk.gg.10y=4,hk.gg.2y=4,hk.gg.3m=1,sg.gg.10y=2,sg.gg.2y=2,sg.gg.3m=1,us.gcorp.a=2,uk.gcorp=2,jp.gcorp=2,eu.gcorp=2,ca.gcorp=2,au.gcorp=2,sw.gcorp=2,sz.gcorp=2,ge.gcorp=2,fr.gcorp=2,hk.gcorp=2,sg.gcorp=2,it.gcorp=2,sp.gcorp=2)
	durations<-list(il.gg.3m=0.25,co.gg.3m=0.25,cl.gg.3m=0.25,dk.gg.3m=0.25,ru.gg.3m=0.25,bz.gg.3m=0.25,tk.gg.3m=0.25,ph.gg.10y=10,ph.gg.2y=2,ph.gg.3m=.25,my.gg.10y=10,my.gg.2y=2,my.gg.3m=.25,ko.gg.10y=10,ko.gg.3y=3,ko.gg.2y=2,ko.gg.3m=0.25,id.gg.10y=10,id.gg.2y=2,id.gg.3m=0.25,in.gg.10y=10,in.gg.2y=2,in.gg.3m=0.25,tw.gg.10y=10,tw.gg.2y=2,tw.gg.3m=0.25,ch.gg.10y=10,ch.gg.2y=2,ch.gg.3m=0.25,pl.gg.10y=10,pl.gg.2y=2,pl.gg.3m=0.25,th.gg.10y=10,th.gg.2y=2,th.gg.3m=0.25,sa.gg.10y=10,sa.gg.2y=2,sa.gg.3m=0.25,mx.gg.10y=10,mx.gg.2y=2,mx.gg.3m=0.25,nz.gg.10y=10,nz.gg.2y=2,nz.gg.3m=0.25,nl.gg.10y=10,nl.gg.2y=2,nl.gg.3m=0.25,no.gg.10y=10,no.gg.2y=2,no.gg.3m=0.25,us.gg.30y=30,us.gg.10y=10,us.gg.7y=7,us.gg.5y=5,us.gg.3y=3,us.gg.2y=2,us.gg.3m=0.25,jp.gg.10y=10,jp.gg.2y=2,jp.gg.3m=0.25,uk.gg.10y=10,uk.gg.2y=2,uk.gg.3m=0.25,au.gg.10y=10,au.gg.2y=2,au.gg.3m=0.25,ca.gg.10y=10,ca.gg.2y=2,ca.gg.3m=0.25,eu.gg.10y=10,eu.gg.2y=2,eu.gg.3m=0.25,ge.gg.10y=10,ge.gg.2y=2,ge.gg.3m=0.25,fr.gg.10y=10,fr.gg.2y=2,fr.gg.3m=0.25,it.gg.10y=10,it.gg.2y=2,it.gg.3m=0.25,sp.gg.10y=10,sp.gg.2y=2,sp.gg.3m=0.25,sz.gg.10y=10,sz.gg.2y=2,sz.gg.3m=0.25,sw.gg.10y=10,sw.gg.2y=2,sw.gg.3m=0.25,hk.gg.10y=10,hk.gg.2y=2,hk.gg.3m=0.25,sg.gg.10y=10,sg.gg.2y=2,sg.gg.3m=0.25,us.gcorp.a=12,uk.gcorp=12,jp.gcorp=12,eu.gcorp=12,ca.gcorp=12,au.gcorp=12,sw.gcorp=12,sz.gcorp=12,ge.gcorp=12,fr.gcorp=12,hk.gcorp=12,sg.gcorp=12,it.gcorp=12,sp.gcorp=12)
	
	return(list(defaultPremia=defaultPremia, illiquiditySpreads= illiquiditySpreads, couponsPerYear= couponsPerYear, durations= durations))

}

global.tipsSettings<-function(){
	couponsPerYear <- list(us.gig.10y=2)
	durations <- list(us.gig.10y=10)
	
	return(list(couponsPerYear=couponsPerYear,durations=durations))
}

global.nonEquityIdentifiers<-function(){
	
	# an asset is a bond if it has one of these strings in its name
	bondIdentifier<-c("\\.gg\\.","\\.gcorp")
	# an asset is a reit if it has this string in its name
	propertyIdentifier<-c("\\.property")
	# an asset is a commodity if it has one of these stings in its name
	commodityIdentifier<-c("gold","silver","\\.fx\\.")
	# an asset is a tips if it has of these strings in its name
	tipsIdentifier <- c("\\.gig\\.")
	
	return(list(bond=bondIdentifier,reit=propertyIdentifier,commodity=commodityIdentifier,tips=tipsIdentifier))
}

global.securityAssetClasses<-function(){
	
	# Important note: always make Cash the first asset class in the list(). That's because it's the only asset class that can have a negative or positive weight and if it's first, the negative and positive areas related to Cash in an area plot end up being contiguous
	
	#return(list(Cash=c("us.gg.3m","ge.gg.3m","uk.gg.3m","jp.gg.3m","au.gg.3m","sw.gg.3m","ca.gg.3m","sz.gg.3m","it.gg.3m","fr.gg.3m","eu.gg.3m","sg.gg.3m","hk.gg.3m","no.gg.3m","tw.gg.3m","nl.gg.3m","ko.gg.3m","it.gg.3m","sp.gg.3m"),EuroGovt=c("ge.gg.10y","it.gg.10y"),EuropeGovtXEuro=c("uk.gg.10y","sw.gg.10y","sz.gg.10y"),JapanGovt=c("jp.gg.10y"),Ausgovt=c("au.gg.10y"),NAGovt=c("us.gg.30y","us.gg.10y","us.gg.7y","us.gg.5y","us.gg.3y","us.gg.2y","ca.gg.10y"),EuroEquity=c("eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50"),GermanyEquity=c("ge.equity"),FranceEquity=c("fr.cac40"),HollandEquity=c("nl.equity"),UKEquity=c("uk.equity"),SwedenEquity=c("sw.omx30"),NorwayEquity=c("no.equity"),SwitzerlandEquity=c("sz.equity"),SpainEquity=c("sp.ibex35"),ItalyEquity=c("it.mib40"),JapanEquity=c("jp.equity"),AustraliaEquity=c("au.asx200"),SKoreaEquity=c("ko.kospi200"),SingaporeEquity=c("sg.2mscifree","sg.mscifree","sg.equity"),HKEquity=c("hk.equity"),TaiwanEquity=c("tw.equity","tw.2mscifree","tw.mscifree"),USLargeCap=c("us.dji","us.equity","us.snp100"),USmidsmallCap=c("us.snp400","us.snp600","us.russell2000"),USTech=c("us.nasdaq100"),USUtil=c("us.djutil"),CanadaEquity=c("ca.tsx60","ca.equity","us.msciem"),WorldEquity=c("us.msciworld","us.mscilocalworld")))
	return(list(Cash=c("us.gg.3m","ge.gg.3m","uk.gg.3m","jp.gg.3m","au.gg.3m","sw.gg.3m","ca.gg.3m","sz.gg.3m","it.gg.3m","fr.gg.3m","eu.gg.3m","sg.gg.3m","hk.gg.3m","no.gg.3m","tw.gg.3m","nl.gg.3m","ko.gg.3m","it.gg.3m","sp.gg.3m","th.gg.3m","my.gg.3m","id.gg.3m","in.gg.3m","mx.gg.3m","ph.gg.3m","ru.gg.3m","bz.gg.3m","ch.gg.3m","nz.gg.3m","dk.gg.3m","il.gg.3m","hu.gg.3m","co.gg.3m","cl.gg.3m","gr.gg.3m","pt.gg.3m","pl.gg.3m","tk.gg.3m"),EuroGovt=c("ge.gg.10y","it.gg.10y"),EuropeGovtXEuro=c("uk.gg.10y","sw.gg.10y","sz.gg.10y"),APACGovt=c("jp.gg.10y","au.gg.10y"),NAGovt=c("us.gg.30y","us.gg.10y","us.gg.7y","us.gg.5y","us.gg.3y","us.gg.2y","ca.gg.10y"),EuroEquity=c("ge.equity","fr.cac40","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","nl.equity","sp.ibex35","it.mib40"),EuropeanEquityXEuro=c("uk.equity","sw.omx30","no.equity","sz.equity"),APACEquity=c("sg.2mscifree","sg.mscifree","tw.2mscifree","tw.mscifree","jp.equity","au.asx200","sg.equity","hk.equity","tw.equity","ko.kospi200"),CanadaEquity=c("ca.tsx60","ca.equity","us.msciem")))
}

global.transactionCosts<-function(){
	
	return(list(us.equity=-0.002,us.dji=-0.002,us.nasdaq100=-0.002,us.nasdaqcomp=-0.002,us.russell2000=-0.002,us.snp400=-0.002,us.snp600=-0.002,us.djutil=-0.002,ca.equity=-0.002,us.msciworld=-0.002,us.mscieafe=-0.002,us.msciem=-0.002,us.mscilatin=-0.002,us.msciasiapacxjpn=-0.002,us.rafi=-0.002,eu.rafi=-0.002,jp.rafi=-0.002,eu.estx50=-0.002,eu.stxe600=-0.002,eu.eurostx=-0.002,eu.stxe50=-0.002,eu.utileurope=-0.002,eu.euroutil=-0.002,uk.equity=-0.002,uk.asx=-0.002,uk.ftse250=-0.002,ge.cdax=-0.002,sw.equity=-0.002,sw.omx30=-0.002,fr.sbf250=-0.002,fr.cac40=-0.002,jp.equity=-0.002,jp.EPG=-0.002,jp.topix400=-0.002,hk.equity=-0.002,au.equity=-0.002,au.asx200=-0.002,us.property=-0.002,uk.property=-0.002,jp.property=-0.002,ca.property=-0.002,au.property=-0.002,eu.epraeurozone=-0.002,eu.deveuropeepra=-0.002,us.gcorp.a=-0.002,uk.gcorp=-0.002,jp.gcorp=-0.002,eu.gcorp=-0.002,ca.gcorp=-0.002,au.gcorp=-0.002,sw.gcorp=-0.002,sz.gcorp=-0.002,ge.gcorp=-0.002,fr.gcorp=-0.002,hk.gcorp=-0.002,sg.gcorp=-0.002,it.gcorp=-0.002,sp.gcorp=-0.002,us.gg.10y=-0.002,jp.gg.10y=-0.002,uk.gg.10y=-0.002,au.gg.10y=-0.002,ca.gg.10y=-0.002,fr.gg.10y=-0.002,ge.gg.10y=-0.002,sz.gg.10y=-0.002,sw.gg.10y=-0.002,us.gg.2y=-0.002,ca.gg.2y=-0.002,uk.gg.2y=-0.002,jp.gg.2y=-0.002,au.gg.2y=-0.002,sw.gg.2y=-0.002,fr.gg.2y=-0.002,ge.gg.2y=-0.002,sz.gg.2y=-0.002,sw.gg.2y.1=-0.002,uk.gg.3m=-0.002,jp.gg.3m=-0.002,sw.gg.3m=-0.002,ca.gg.3m=-0.002,us.gg.3m=-0.002,au.gg.3m=-0.002,fr.gg.3m=-0.002,ge.gg.3m=-0.002,sz.gg.3m=-0.002,sw.gg.3m.1=-0.002,eu.gg.10y=-0.002,eu.gg.2y=-0.002,eu.gg.3m=-0.002,hk.gg.10y=-0.002,hk.gg.2y=-0.002,hk.gg.3m=-0.002,sg.gg.10y=-0.002,sg.gg.2y=-0.002,sg.gg.3m=-0.002,it.gg.10y=-0.002,it.gg.2y=-0.002,it.gg.3m=-0.002,sp.gg.10y=-0.002,sp.gg.2y=-0.002,sp.gg.3m=-0.002,us.gold=-0.002,us.silver=-0.002,cad.usd.fx.b=-0.002,gbp.usd.fx.b=-0.002,jpy.usd.fx.b=-0.002,aud.usd.fx.b=-0.002,eur.usd.fx.b=-0.002,hkd.usd.fx.b=-0.002,sgd.usd.fx.b=-0.002,chf.usd.fx.b=-0.002,sek.usd.fx.b=-0.002,sp.equity=-0.002,sz.equity=-0.002,ru.equity=-0.002,tw.equity=-0.002,sg.equity=-0.002,sg.msci=-0.002,nz.equity=-0.002,in.equity=-0.002,pt.psi20=-0.002,gr.equity=-0.002,nl.equity=-0.002,at.equity=-0.002,my.equity=-0.002,th.equity=-0.002,th.set50=-0.002,us.snp100=-0.002,us.snp500inft=-0.002,us.emrafi=-0.002,us.asiaxjapanrafi=-0.002,uk.100rafi=-0.002,uk.rafi=-0.002,ca.rafi=-0.002,no.equity=-0.002,it.mib40=-.002,no.gg.3m=-.002))
	
}

global.defaultTransactionCost<-function(){
	
	# we use this for assets for which we did not specify TC in global.transactionCosts()
	defaultTransactionCost<--0.002
	
	return(defaultTransactionCost)
	
}

global.palette<-function(){
	
	# use this to define the palette of colors to use in our graphs. in our plotting functions, these colors are referred to as 1,2,3,... . colors() returns all "named" colors that R knows about and currently has 657 such colors. 
	colorPalette<-c("purple","green3","cornsilk","cyan","turquoise","yellow","sienna","violetred","thistle","wheat","yellowgreen","red","slateblue","green","lightpink1","orange1","gray40","seagreen4","salmon","gold4")
	
	return(colorPalette)
}

global.pngSettings<-function(){
	width<-800 # in pixels
	height<-600 # in pixels
	res<-100 # I don't think this one can be changed without mayhem ensuing
	pointsize<-12
	bg<-"white"
	
	return(list(width= width, height= height, res= res, pointsize= pointsize, bg= bg))
}

global.spreadOnShortCash <- function(){
	return( 0.002 )
}

global.sa2CountryCode <- function(){
	# why data.frame() and not list()? because country names can have space in them and so can't be names of list()
	return( data.frame(wbCountry=c("France","United States","United Kingdom","Australia","Canada","Korea","Hong Kong","Singapore","Norway","Japan","Switzerland","Italy","China","Netherlands","Germany","Sweden","South Africa","Euro area","Spain","Thailand","Malaysia","Indonesia","New Zealand","Mexico","Poland","Chile","Colombia","Denmark","Finland","Greece","Hungary","India","Ireland","Portugal","Brazil","Argentina","Austria","Turkey","Russia","Belgium","Luxembourg","Estonia","Lebanon","Slovakia","Slovenia","Vietnam","Czech","Romania","Bulgaria","Philippines","Taiwan"), sa2Country=c("fr","us","uk","au","ca","ko","hk","sg","no","jp","sz","it","ch","nl","ge","sw","sa","eu","sp","th","my","id","nz","mx","pl","cl","co","dk","fn","gr","hu","in","ir","pt","bz","ag","at","tk","ru","be","lu","ee","lb","sk","si","vn","cz","ro","bg","ph","tw") ) )
}

global.sa2WBCountryCode <- function(){
	# why data.frame() and not list()? because country names can have space in them and so can't be names of list()
	return( data.frame(wbCountry=c("France","United States","United Kingdom","Australia","Canada","Korea, Rep.","Hong Kong SAR, China","Singapore","Norway","Japan","Switzerland","Italy","China","Netherlands","Germany","Sweden","South Africa","Euro area","Spain","Thailand","Malaysia","Indonesia","New Zealand","Mexico","Poland","Chile","Colombia","Denmark","Finland","Greece","Hungary","India","Ireland","Portugal","Brazil","Argentina","Austria","Turkey","Russia","Belgium","Luxembourg","Estonia","Lebanon","Slovak Republic","Slovenia","Vietnam","Czech Republic","Romania","Bulgaria","Philippines"), sa2Country=c("fr","us","uk","au","ca","ko","hk","sg","no","jp","sz","it","ch","nl","ge","sw","sa","eu","sp","th","my","id","nz","mx","pl","cl","co","dk","fn","gr","hu","in","ir","pt","bz","ag","at","tk","ru","be","lu","ee","lb","sk","si","vn","cz","ro","bg","ph") ) )
}

global.pathToJava <- function(){
	return( '"c:/Program Files/Java/jdk1.7.0_15/bin/java.exe"'  )
}

global.pathToBlpJar <- function(){
	return( "C:/blp/API/APIv3/JavaAPI/v3.6.1.0/bin/blpapi-3.6.1-0.jar"  )
}

global.classpathSeparator <- function( isWindows ){
	return( ifelse(isWindows, ";", ":") )
}

global.futureContractMultiplier <- function(){
	ret <- list( Z=10, GX=25, CF=10, QC=100, SXO=50, TP=10000, XP=25, QZ=200, PT=200, HI=50, EO=200, IB=10, ID=1, HU=10, SM=10 , ST=5 , OI=100, TW=100, TY= 1000, RX=1000, IS=10, NZ=50, BC=1000, IDO=2, KM=500000, IK=50, WI=10, VG=10, AI=10, DM=5, ES=50, BZ=1, A5=100, AJ=5, VE=.02, RTA=100, NQ=20, FA=100)
	names(ret)[1] <- "Z "

	return( ret )
}

global.futureContractBasis <- function(){
	ret <- list( Z=0.998, GX=0.998, CF=0.998, QC=0.998, SXO=0.998, TP=0.998, XP=0.998, QZ=0.998, PT=0.998, HI=0.998, EO=0.998, IB=0.998, SM=0.998 , ST=0.998 , OI=1.37407, TW=0.998, IS=.998, NZ=.998, BC=.998, IDO=.998, KM=.998, IK=.988, WI=.998, VG=.998, AI=.998, DM=.998, ES=.998, BZ=.998, A5=.998, AJ=.998, VE=.998, RTA=.998, NQ=.998, FA=.998)
	names(ret)[1] <- "Z "
	
	return( ret )
}

global.futureContractMinis <- function(){
	
	# IMPORTANT NOTE: these MUST, again, MUST, be only equityIndexFutures. Otherwise a modification is needed to include the type in the structure. (only minis i know of are for that asset class anyway)
	
	ret <- list(hk.equity="HU", sp.ibex35="ID")
	
	return( ret )
}

global.corpsExecSettings<-function(){
	
	coreSettings<-global.core()

	corpex.inputDir<-coreSettings[["input"]]
	corpex.outputDir<-coreSettings[["dataOutput"]]
	corpex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
			
	# write to this file
	corpex.outputFile<-"gcorp.csv"
	# if not using DB, read data from this file; if using DB, this setting is ignored
	corpex.dataFileName<-paste(corpex.inputDir,"DBF.marketData.csv",sep="")
	# how many of the latest data to save to a separate file in order to not have to reload the entire history of gcorp rates into the DB?
	corpex.numLast<-40
	# use this name for the series of us gcorp data
	corpex.usDataSeriesName<-"us.gcorp.a"
	# use this suffix for the series of non-us gcorp data; prefix is country code, "uk", "jp", etc.
	corpex.nonUsDataSeriesNameSuffix<-".gcorp"
	# use this rate to convert from us data to non-US
	corpex.cashNameSuffix<-".gg.10y"
	# use this data to calculate the us gcorg data series
	corpex.sourceData<-c("us.gcorpa.10y","us.gcorpa.15y")
	# use these weights to calculate the us gcorp data series; must the same length as corpex.sourceData
	corpex.sourceDataWeights<-c(0.6,0.4)
	# use this US data series in calculating non-Us gcorp data series
	corpex.useForNonUs<-"us.gcorpa.10y"
	# use this date format when reading data from file
	corpex.dateFormat<-"%Y-%m-%d"
	
	corpex.currencies<-c("uk","jp","eu","ca","au","eu","fr","ge","sw","sz","hk","sg","it","sp")

	return(list(corpex.inputDir= corpex.inputDir, corpex.outputDir= corpex.outputDir, corpex.useDB= corpex.useDB, corpex.outputFile= corpex.outputFile, corpex.dataFileName= corpex.dataFileName, corpex.numLast= corpex.numLast, corpex.usDataSeriesName= corpex.usDataSeriesName, corpex.nonUsDataSeriesNameSuffix= corpex.nonUsDataSeriesNameSuffix, corpex.cashNameSuffix= corpex.cashNameSuffix, corpex.sourceData= corpex.sourceData, corpex.sourceDataWeights= corpex.sourceDataWeights, corpex.useForNonUs= corpex.useForNonUs, corpex.dateFormat= corpex.dateFormat, corpex.currencies= corpex.currencies,dbSettings=dbSettings))
}

global.dailyTotalCapitalExecSettings<-function(){
	
	coreSettings<-global.core()
			
	dtcex.inputDir<-coreSettings[["input"]]
	dtcex.outputDir<-coreSettings[["dataOutput"]]
	dtcex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
			
	# assets
	dtcex.assets<-c("us.equity","us.dji","us.nasdaq100","us.nasdaqcomp","us.russell2000","us.snp400","us.snp600","us.djutil","ca.equity","ca.tsx60","us.mscilocalworld","us.msciworld","us.mscilocaleafe","us.mscieafe","us.msciem","us.mscilatin","us.msciasiapacxjpn","us.rafi","eu.rafi","jp.rafi","eu.estx50","eu.stxe600","eu.eurostx","eu.stxe50","eu.utileurope","eu.euroutil","uk.equity","uk.asx","uk.ftse250","ge.cdax","ge.equity","sw.equity","sw.omx30","fr.sbf250","fr.cac40","sp.ibex35","it.mib40","sz.equity","nl.equity","no.equity","jp.equity","jp.EPG","jp.topix400","hk.equity","au.equity","au.asx200","ko.kospi200","tw.2mscifree","tw.mscifree","tw.equity","sg.equity","sg.2mscifree","sg.mscifree","nz.equity","th.set50","id.mscifree","us.snp100","mx.equity","bz.equity","eu.epraeurozone","eu.deveuropeepra","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","jp.gg.10y","jp.gg.2y","jp.gg.3m","uk.gg.10y","uk.gg.2y","uk.gg.3m","au.gg.10y","au.gg.2y","au.gg.3m","ca.gg.10y","ca.gg.2y","ca.gg.3m","eu.gg.10y","eu.gg.2y","eu.gg.3m","ge.gg.10y","ge.gg.2y","ge.gg.3m","fr.gg.10y","fr.gg.2y","fr.gg.3m","it.gg.10y","it.gg.2y","it.gg.3m","sp.gg.10y","sp.gg.2y","sp.gg.3m","sz.gg.10y","sz.gg.2y","sz.gg.3m","sw.gg.10y","sw.gg.2y","sw.gg.3m","hk.gg.10y","hk.gg.2y","hk.gg.3m","sg.gg.10y","sg.gg.2y","sg.gg.3m","nl.gg.10y","nl.gg.2y","nl.gg.3m","no.gg.3m","nz.gg.3m","nz.gg.10y","sa.gg.3m","sa.gg.2y","sa.gg.10y","ko.gg.3m","ko.gg.2y","ko.gg.3y","ko.gg.10y","ch.gg.3m","ch.gg.2y","ch.gg.10y","mx.gg.3m","mx.gg.2y","mx.gg.10y","pl.gg.3m","pl.gg.2y","pl.gg.10y","th.gg.3m","th.gg.2y","th.gg.10y","id.gg.3m","id.gg.2y","id.gg.10y","tk.gg.3m","in.gg.3m","in.gg.2y","in.gg.10y","tw.gg.3m","tw.gg.2y","tw.gg.10y","my.gg.3m","my.gg.2y","my.gg.10y","us.gold","us.silver","cad.usd.fx.b","gbp.usd.fx.b","jpy.usd.fx.b","aud.usd.fx.b","eur.usd.fx.b","hkd.usd.fx.b","sgd.usd.fx.b","chf.usd.fx.b","sek.usd.fx.b","dem.usd.fx.b","frf.usd.fx.b","itl.usd.fx.b","esp.usd.fx.b","nlg.usd.fx.b","zar.usd.fx.b","nzd.usd.fx.b","nok.usd.fx.b","twd.usd.fx.b","krw.usd.fx.b","thb.usd.fx.b","myr.usd.fx.b","idr.usd.fx.b","inr.usd.fx.b","php.usd.fx.b","cny.usd.fx.b","mxn.usd.fx.b","brl.usd.fx.b","clp.usd.fx.b","cop.usd.fx.b","trl.usd.fx.b","rub.usd.fx.b","pln.usd.fx.b","dkk.usd.fx.b","sa.equity","ru.equity","sg.msci","in.equity","pt.psi20","gr.equity","at.equity","my.equity","th.equity","us.snp500inft","us.emrafi","us.asiaxjapanrafi","uk.100rafi","uk.rafi","ca.rafi","cl.equity","tk.equity","pl.equity","pl.wig20","us.gig.10y",

	"jp.n225",
	"ch.a50",
	"ch.equity",
	"in.nifty",
	"ru.gg.3m","bz.gg.3m"
	)

	
	# asset identifiers
	dtcex.assetIdentifiers<-global.nonEquityIdentifiers()
	
	# bond settings
	dtcex.bondSettings<-global.bondSettings()
	
	# if not using DB use this file; otherwise ignored
	dtcex.inputDataFile<- "DBF.marketData.RData"
	# write to this file
	dtcex.outputFile<-"dTrCr.csv"
	# how many of the latest data to save to a separate file in order to not have to reload the entire history of gcorp rates into the DB?
	dtcex.numLast<-40
	# suffixes for input data
	#dtcex.listOfInSuffixes<-list(re=c("",".tr"),bond=c(""),commodity=c(""),fx=c(""))
	dtcex.listOfInSuffixes<-list(re=c("",".tr"),bond=c(""),commodity=c(""),tips=c(".tr"))
	# suffixes for output data
	dtcex.listOfOutSuffixes<-c(".dcr",".dtr")

	# return list()
	return(list(dtcex.listOfOutSuffixes= dtcex.listOfOutSuffixes, dtcex.listOfInSuffixes= dtcex.listOfInSuffixes, dtcex.numLast= dtcex.numLast, dtcex.outputFile= dtcex.outputFile, dtcex.inputDataFile= dtcex.inputDataFile, dtcex.bondSettings= dtcex.bondSettings, dtcex.assetIdentifiers= dtcex.assetIdentifiers, dtcex.assets= dtcex.assets, dtcex.useDB= dtcex.useDB, dtcex.outputDir= dtcex.outputDir, dtcex.inputDir= dtcex.inputDir, dbSettings= dbSettings))
}

global.predictExecSettings<-function(){
	
	coreSettings<-global.core()
				
	prd.inputDir<-coreSettings[["input"]]
	prd.outputDir<-coreSettings[["dataOutput"]]
	prd.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	macroDbSettings <- coreSettings[["macroDbSettings"]]
	
	# this controls macro data
	prd.useMacroDB<- TRUE
			
	prd.inputDataFile<-"DBF.marketData.RData"
	prd.outputFile<-"forecasts.csv"
	# how many of the latest data to save to a separate file in order to not have to reload the entire history of gcorp rates into the DB?
	prd.numLast<-40
	# calculate predicted returns/risks for these assets
	
	# Important note: we had a "bug" in an earlier version of the code that prevented growth rates from being seeded. we made this into a feature of the code to allow for continuity and the reproduction of earlier numbers. if eretm.allowGrowthRateSeeding is set to FALSE, we get the old "buggy" behavior. if set to TRUE, the growth rate is seeded and we get the behavior that we had in mind
	eretm.allowGrowthRateSeeding<-FALSE
	
	prd.assets<-c("bz.equity","us.dji","us.equity","us.nasdaq100","us.snp100","us.snp400","us.snp600","us.djutil","us.mscilocalworld","us.msciworld","us.mscieafe","us.msciem","us.msciasiapacxjpn","ca.equity","ca.tsx60","jp.equity","jp.EPG","jp.topix400","ge.cdax","ge.equity","fr.sbf250","fr.cac40","sw.equity","sw.omx30","it.mib40","no.equity","sp.ibex35","nl.equity","sz.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.utileurope","eu.euroutil","uk.asx","uk.equity","uk.ftse250","au.equity","au.asx200","hk.equity","sg.equity","sg.mscifree","sg.2mscifree","tw.2mscifree","tw.mscifree","tw.equity","nz.equity","ko.kospi200","mx.equity","th.set50","my.equity","id.mscifree","cl.equity","tk.ise30","tk.equity","in.equity","sa.equity","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","jp.gg.10y","jp.gg.2y","jp.gg.3m","uk.gg.10y","uk.gg.2y","uk.gg.3m","au.gg.10y","au.gg.2y","au.gg.3m","ca.gg.10y","ca.gg.2y","ca.gg.3m","eu.gg.10y","eu.gg.2y","eu.gg.3m","ge.gg.10y","ge.gg.2y","ge.gg.3m","fr.gg.10y","fr.gg.2y","fr.gg.3m","it.gg.10y","it.gg.2y","it.gg.3m","sp.gg.10y","sp.gg.2y","sp.gg.3m","sz.gg.10y","sz.gg.2y","sz.gg.3m","sw.gg.10y","sw.gg.2y","sw.gg.3m","hk.gg.10y","hk.gg.2y","hk.gg.3m","sg.gg.10y","sg.gg.2y","sg.gg.3m","nl.gg.10y","nl.gg.2y","nl.gg.3m","no.gg.3m","nz.gg.3m","nz.gg.10y","sa.gg.3m","sa.gg.2y","sa.gg.10y","ko.gg.3m","ko.gg.2y","ko.gg.3y","ko.gg.10y","tw.gg.3m","tw.gg.2y","tw.gg.10y","in.gg.3m","in.gg.2y","in.gg.10y","id.gg.3m","id.gg.2y","id.gg.10y","my.gg.3m","my.gg.2y","my.gg.10y","th.gg.3m","th.gg.2y","th.gg.10y","mx.gg.3m","mx.gg.2y","mx.gg.10y","us.gig.10y",
	"tk.gg.3m","pl.gg.3m","ch.gg.3m","ru.gg.3m","bz.gg.3m",
	"pl.wig20",
	
	"in.nifty",
	"ru.equity",
	
	"ch.a50",
	"ch.equity",
	"hk.chinaent"
	)
	if(!prd.useDB){
		prd.assets<-setdiff(prd.assets,"us.gcorp.a")
	}
	
	prd.dateFormat<-"%Y-%m-%d"
	# asset data suffixes: we need different types of data for different asset classes
	prd.listOfSuffixes<-list(equity=c("",".earnings",".dvd.yld"),reit=c(".ev",".evebitda",".netdebt"),bond=c(""),tips=c(""))
	# inflation numbers suffix
	prd.inflationSuffix<-".cpirate"	
	# asset identifiers
	prd.assetIdentifiers<-global.nonEquityIdentifiers()	
	# inflation file
	prd.inflationFile<-c("DBF.inflation.csv")	
	# specify macro data input
	prd.gdpFile<-c("DBF.gdp.csv")
	prd.gdpSuffix<-c(".gdprate")
	
	prd.gdpFrequency<-c("quarterly")
	prd.gdpExtendedFile<-c("GDP-yearly-GFD-over-WB.csv")
	prd.gdpExtendedSuffix<-c(".gdpreal")
	prd.gniFile<-c("DBF.gni.csv")
	prd.gniSuffix<-c(".gnirate")
	prd.gniFrequency<-c("yearly")
	prd.extrapolateGDP<-FALSE
	prd.extrapolateGNI<-FALSE
	prd.extrapolateNumYears<-25
	# calculate the forecast variables at this frequency
	prd.frequency<-"daily" # "weekly" or "monthly" or "daily"
	# this only matters if prd.frequency=="weekly"
	prd.dayOfWeek<-"Monday"
	
	# take annualized gdp & gni growth over this many years
	eretm.numGrowthYears<-10
	eretm.doAverageGrowthOverHistory <- FALSE
	eretm.numInflationMonths<-120	
	eretm.payoutRatioSuffix<-".payoutRatio"
	eretm.averageEarningsSuffix<-".avg"
	# average earnings over this many years
	eretm.averageEarningsYears<-2
	# number of business days; used to determine indices of data a number of years apart, instead of doing Date calcs
	eretm.busDaysPerYear<-261
	# this is the inverse of the number of years, typically 10, over which we seek annualized returns; used in returnModels.R
	eretm.annualize<-.1
	# maximum iterations we allow the robust linear regression function to do
	eretm.regMaxIt<-500
	# markets can be seeded with data from other markets
	eretm.seedSource<-list() ###(sg.2mscifree="us.equity",tw.2mscifree="us.msciworld",us.snp100="us.equity",us.snp400="us.equity",us.rafi="us.dji",us.djutil="us.equity",us.msciworld="us.equity",us.mscilocalworld="us.equity",us.mscieafe="us.equity",us.msciem="us.equity",us.msciasiapacxjpn="us.dji",jp.topix400="jp.equity",jp.rafi="jp.equity",eu.stxe600="ge.cdax",eu.eurostx="ge.cdax",eu.rafi="ge.cdax",eu.utileurope="ge.cdax",eu.euroutil="ge.cdax",it.mib40="ge.cdax",sg.equity="us.equity",sp.ibex35="us.msciworld",tw.equity="us.msciworld",sz.equity="us.msciworld",ko.kospi200="us.msciworld") 
	# to calculate offsets in earnings growth
	eretm.growthBenchmark<-list() ###(us.snp100="us.equity",us.snp400="us.equity",us.rafi="us.dji",us.djutil="us.equity",us.msciworld="us.equity",us.mscilocalworld="us.equity",us.mscieafe="us.equity",us.msciem="us.equity",us.msciasiapacxjpn="us.dji",us.nasdaq100="us.equity",uk.ftse250="uk.asx",eu.rafi="ge.cdax",it.mib40="ge.cdax")
	# we use GNI instead GDP for certain countries
	eretm.useGNI<-c()
	
	# for some markets we want non-local macro data or combinations 
	eretm.nonLocalMacroGrowth<-list()
	
	# for some markets we want non-local macro data or combinations 
	eretm.nonLocalInflation<-list()
	# regress earnings over periods of this many months
	eretm.regressEarningsInMonths<-120

	# get bond settings
	bondSettings<-global.bondSettings()
	bretm.defaultPremia<-bondSettings[["defaultPremia"]]
	bretm.illiquiditySpreads<-bondSettings[["illiquiditySpreads"]]
	bretm.couponsPerYear<-bondSettings[["couponsPerYear"]]
	bretm.durations<-bondSettings[["durations"]]
	
	# tips settings
	tipsSettings<-global.tipsSettings()
	tretm.couponsPerYear <- tipsSettings[["couponsPerYear"]]
	tretm.durations <- tipsSettings[["durations"]]
	# we use the output from tipsReturnModels to calculate the risk. If, in future, we have more than one model for tips er we would need the following statement to specify which of these goes into risk calcs
	triskm.whichTipsERForRisk <- c(".er")
	
	# now return list() of settings
	return(list(eretm.doAverageGrowthOverHistory=eretm.doAverageGrowthOverHistory,macroDbSettings=macroDbSettings, triskm.whichTipsERForRisk=triskm.whichTipsERForRisk,tretm.durations=tretm.durations,tretm.couponsPerYear=tretm.couponsPerYear,eretm.allowGrowthRateSeeding=eretm.allowGrowthRateSeeding,bretm.durations= bretm.durations, bretm.couponsPerYear= bretm.couponsPerYear, bretm.illiquiditySpreads= bretm.illiquiditySpreads, bretm.defaultPremia= bretm.defaultPremia, eretm.regressEarningsInMonths= eretm.regressEarningsInMonths, eretm.nonLocalInflation= eretm.nonLocalInflation, eretm.nonLocalMacroGrowth= eretm.nonLocalMacroGrowth, eretm.useGNI= eretm.useGNI, eretm.growthBenchmark= eretm.growthBenchmark, eretm.seedSource= eretm.seedSource, eretm.regMaxIt= eretm.regMaxIt, eretm.annualize= eretm.annualize, eretm.busDaysPerYear= eretm.busDaysPerYear, eretm.averageEarningsYears= eretm.averageEarningsYears, eretm.averageEarningsSuffix= eretm.averageEarningsSuffix, eretm.payoutRatioSuffix= eretm.payoutRatioSuffix, eretm.numInflationMonths= eretm.numInflationMonths, eretm.numGrowthYears= eretm.numGrowthYears, prd.dayOfWeek= prd.dayOfWeek, prd.frequency= prd.frequency, prd.extrapolateNumYears= prd.extrapolateNumYears, prd.extrapolateGNI= prd.extrapolateGNI, prd.extrapolateGDP= prd.extrapolateGDP, prd.gniFrequency= prd.gniFrequency, prd.gniSuffix= prd.gniSuffix, prd.gniFile= prd.gniFile, prd.gdpExtendedSuffix= prd.gdpExtendedSuffix, prd.gdpExtendedFile= prd.gdpExtendedFile, prd.gdpFrequency= prd.gdpFrequency, prd.gdpSuffix= prd.gdpSuffix, prd.gdpFile= prd.gdpFile, prd.inflationFile= prd.inflationFile, prd.assetIdentifiers= prd.assetIdentifiers, prd.inflationSuffix= prd.inflationSuffix, prd.listOfSuffixes= prd.listOfSuffixes, prd.dateFormat= prd.dateFormat, prd.assets= prd.assets, prd.numLast= prd.numLast, prd.outputFile= prd.outputFile, prd.inputDataFile= prd.inputDataFile, prd.useMacroDB= prd.useMacroDB, prd.useDB= prd.useDB, prd.outputDir= prd.outputDir, prd.inputDir= prd.inputDir,dbSettings=dbSettings))
}

global.slopesExecSettings<-function(){
	
	coreSettings<-global.core()
			
	slex.inputDir<-coreSettings[["input"]]
	slex.outputDir<-coreSettings[["dataOutput"]]
	slex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
			
	# input file if we're not using DB otherwise ignored
	slex.inputFile<-"forecasts.csv"
	# write to this
	slex.outputFile<-"slopes.csv"
	# how many of the latest data to save to a separate file in order to not have to reload the entire history of gcorp rates into the DB?
	slex.numLast<-40
	# country codes we recognize
	slex.currencies<-c("us","ca","uk","jp","au","eu","fr","ge","sw","sz","hk","sg","it","sp","ko","tw","nl")
	# suffixes for data we need
	slex.listOfSuffixes<-c(".gg.10y.er",".gg.2y.er")
	slex.dateFormat<-"%Y-%m-%d"
	
	return(list(slex.dateFormat= slex.dateFormat, slex.listOfSuffixes= slex.listOfSuffixes, slex.currencies= slex.currencies, slex.numLast= slex.numLast, slex.outputFile= slex.outputFile, slex.inputFile= slex.inputFile, slex.useDB= slex.useDB, slex.outputDir= slex.outputDir, slex.inputDir= slex.inputDir, dbSettings= dbSettings))
}

global.eurolegacyExecSettings<-function(){
	
	coreSettings<-global.core()
	
	euroleg.inputDir<-coreSettings[["input"]]
	euroleg.outputDir<-coreSettings[["dataOutput"]]
	euroleg.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
			
	# input file if we're not using DB otherwise ignored
	euroleg.inputFile<-"prdInputsTest.csv"
	# write to this
	euroleg.outputFile<-"eurolegacy.csv"
	# how many of the latest data to save to a separate file in order to not have to reload the entire history of gcorp rates into the DB?
	euroleg.numLast<-40
	# country codes we recognize
	euroleg.inputcurrencies<-c("usd.dem.fx.b","usd.frf.fx.b","usd.itl.fx.b","usd.esp.fx.b","usd.nlg.fx.b")
	euroleg.outputcurrencies<-c("dem.usd.fx.b","frf.usd.fx.b","itl.usd.fx.b","esp.usd.fx.b","nlg.usd.fx.b")
	
	euroleg.dateFormat<-"%Y-%m-%d"
	
	return(list(euroleg.dateFormat= euroleg.dateFormat, euroleg.outputcurrencies= euroleg.outputcurrencies, euroleg.inputcurrencies= euroleg.inputcurrencies, euroleg.numLast= euroleg.numLast, euroleg.outputFile= euroleg.outputFile, euroleg.inputFile= euroleg.inputFile, euroleg.useDB= euroleg.useDB, euroleg.outputDir= euroleg.outputDir, euroleg.inputDir= euroleg.inputDir,dbSettings=dbSettings))
}

global.portfolioSimulationExecSettings<-function(){
	
	coreSettings<-global.core()
	
	psex.inputDir<-coreSettings[["optOutput"]]
	psex.inputDirER<-coreSettings[["dataOutput"]]
	psex.outputDir<-coreSettings[["simOutput"]]
	psex.inputDirCOSTS<-coreSettings[["input"]]
	psex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	psex.hack.numberOfWeeksToShift <-0
	
	# expected returns 
	#psex.ersRisksFile<-"forecasts.csv"
	psex.ersRisksFile<-"forecasts.RData"
	psex.mktDataFile<-"DBF.marketData.RData"
	# we need this data when we're NOT using the DB
	psex.retHistFile<-"dTrCr.RData"

	# the portfolios we run are specified in this file if not passed through the arguments of psex.main()
	psex.constraints<-"constraintsFiles.R"
	# form the names of the special frontier points we simulate. Note: if psex.portfolioDesignators is "" then we take the name of the portfolio from constraintsFiles.R and add ".csv" to it. This is useful for portfolio that were not produced by FrontierBuilder
	psex.portfolioDesignators<-c("A","M","C","E","V","DS.0.05","DS.0.1","DS.0.15","MADU.5","MADU.10","MADU.15","MADU.20","MADU.25","MADU.30","MADU.35","MADU.40","MAD.A","MAD.M","MAD.C","MAD.E","MAD.V") # "", "M" ,"E","C","A","V"
	psex.portfolioDesignators<-c("MVU.200","MVU.350","MVU.400","MVU.450")
	
	# sanity check: we use constraints-*.R files to get settings for a particular portfolios. this list helps us make sure every setting we need was specified
	psex.namesOfInList<-c("effFrontierFreq","correlationDataFreq","dayOfWeek","rollingWindow","halfLife","riskMeasure","erMapping","riskMapping")
	
	# output files suffixes
	psex.portfolioReturnHistFileSuffix<-".sim.csv"
	psex.portfolioReallocationHistFileSuffix<-".realloc.csv"
	
	# default transaction cost
	psex.defaultTransactionCost<-global.defaultTransactionCost()
	
	# this used to be in DTR-INPUTS.csv
	psex.transactionCosts<-global.transactionCosts()
	
	psex.listOfSuffixes<-list(capRetSuffix=".dcr",totalRetSuffix=".dtr")
	psex.cashNameSuffix<-".gg.3m"
	# use this rate in converging to a domestic perspective
	psex.conversionRateNameSuffix<-".gg.3m"
	# do we include risks in the conversion (it's wrong to do that; it's here for continuity)?
	psex.doRisks<-T
	# this is needed to annualize the yields on the 10Y and 2Y if psex.cashNameSuffix is set to ".gg.10Y" or ".gg.2Y"
	psex.couponsPerYear<-global.bondSettings()[["couponsPerYear"]]
	# this is assumed to be annual simple rate
	psex.negativeCashSpread<- global.spreadOnShortCash()
	psex.bDaysPerYear<-260.8857143
	psex.negativeCashDailyInterest<-(1+psex.negativeCashSpread)^(1/psex.bDaysPerYear)-1
	# this is the suffix of fx dtr's in the db
	psex.fxDtrSuffix <- ".usd.fx.b.dtr"
	# this is the suffix that we add to the fx dtr in our table of asset returns history
	psex.countryDtrSuffix<-".co.dtr"
	psex.dateFormat<-"%Y-%m-%d"
	
	# rebalance hurdle. Note: the number of rebalancing policies must be 1 or match the number of designators above in psex.portfolioDesignators if the number of designators is not 0
	reb.reallocDecision<-c(rep("euclidean", 4)) # or "turnover" or "expectedReturn" or "erRiskRatio" or "always" or "KL" or "euclidean". Note: "KL" does NOT work if we shorted risky assets
	# if "turnover" hurdle compare this value to total portfolio turnover when deciding to reallocate
	reb.weightsHurdle<-0.25
	# if "expectedReturn" add this value to the current portfolio return when deciding to reallocate
	reb.erHurdle<-0.01
	# for "erRiskRatio" add this penalty to the current portfolio return when deciding to reallocate
	reb.erRiskRatioHurdle<-0.0025
	# some unintended leverage does creep into our positions. it happens when the net interest on our cash positions is negative and exceeds (in absolute value) the net interest and dividends on our non-cash positions. this is percent of portfolio
	reb.leverageThreshold<-0.02
	# the tolerance to use for kullback-leibler rebalancing. we rebalance if KL divergence between weights is more than that or if current has a higher risk than target
	reb.klHurdle <- .1
	# the tolerance to use for euclidean distance based rebalancing. we rebalance if weights differ by more than that or if current has a higher risk than target
	reb.euclidHurdle <- .1
	
	# if psex.rebalanceOnly is TRUE, we rebalance when we exceed the threshold in cash holdings
	psex.rebalanceOnly <- FALSE
	
	# cash reinvestment settings
	reinv.reinvestmentHurdle<-0.05
	reinv.reinvestmentDecision<-"riskyAssets" # "riskyAssets" or "noReinvestment"
	
	# note the risk in our DB really should be stored divided by 100
	vc.varScaleFactor<-100.0 # divide retrieved risks by this factor
	
	# now return list()
	return(list(reb.klHurdle=reb.klHurdle,reb.euclidHurdle=reb.euclidHurdle,psex.rebalanceOnly=psex.rebalanceOnly,psex.hack.numberOfWeeksToShift=psex.hack.numberOfWeeksToShift,psex.doRisks=psex.doRisks,psex.conversionRateNameSuffix=psex.conversionRateNameSuffix,psex.inputDirER = psex.inputDirER ,vc.varScaleFactor= vc.varScaleFactor, reinv.reinvestmentDecision= reinv.reinvestmentDecision, reinv.reinvestmentHurdle= reinv.reinvestmentHurdle, reb.leverageThreshold= reb.leverageThreshold, reb.erRiskRatioHurdle= reb.erRiskRatioHurdle, reb.erHurdle= reb.erHurdle, reb.weightsHurdle= reb.weightsHurdle, reb.reallocDecision= reb.reallocDecision, psex.dateFormat= psex.dateFormat, psex.countryDtrSuffix= psex.countryDtrSuffix, psex.fxDtrSuffix= psex.fxDtrSuffix, psex.negativeCashDailyInterest= psex.negativeCashDailyInterest, psex.bDaysPerYear= psex.bDaysPerYear, psex.negativeCashSpread= psex.negativeCashSpread, psex.couponsPerYear= psex.couponsPerYear, psex.cashNameSuffix= psex.cashNameSuffix, psex.listOfSuffixes= psex.listOfSuffixes, psex.retHistFile= psex.retHistFile, psex.transactionCosts= psex.transactionCosts, psex.defaultTransactionCost= psex.defaultTransactionCost, psex.portfolioReallocationHistFileSuffix= psex.portfolioReallocationHistFileSuffix, psex.portfolioReturnHistFileSuffix= psex.portfolioReturnHistFileSuffix, psex.namesOfInList= psex.namesOfInList, psex.portfolioDesignators= psex.portfolioDesignators, psex.constraints= psex.constraints, psex.mktDataFile= psex.mktDataFile, psex.ersRisksFile= psex.ersRisksFile, psex.inputDir= psex.inputDir, psex.outputDir= psex.outputDir, psex.inputDirCOSTS= psex.inputDirCOSTS, psex.useDB= psex.useDB, dbSettings= dbSettings))
	
}

global.portfolioSimStatsExecSettings<-function(){
	
	coreSettings<-global.core()
			
	psstats.inputDir<-coreSettings[["simOutput"]]
	psstats.inputDirER<-coreSettings[["dataOutput"]]
	psstats.outputDir<-coreSettings[["simOutput"]]
	psstats.inputDirCOSTS<-coreSettings[["input"]]
	psstats.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	# if the portfolio weights history file is called "US9-RM1-monthly.csv", then we say "US9-RM1-monthly" in here
	
	psstats.portfolioNames<-c(paste("Base-MVU.",c(200,350,400,450),"-Monday.euclidean",sep=""))

	# input files suffixes
	psstats.portfolioReturnHistFileSuffix<-".sim.csv"
	psstats.portfolioReallocationHistFileSuffix<-".realloc.csv"
	# we need this data when we're NOT using the DB
	psstats.retHistFile<-"dTrCr.RData"
	psstats.cashNameSuffix<-".gg.3m"
	psstats.dateFormat<-"%Y-%m-%d"
	psstats.totalRetSuffix<-".dtr"
	
	psstats.bDaysPerYear<-260.8857143
	
	# how many area plots for the portfolios do we want? we start counting from the first portfolio in psstats.portfolioNames
	psstats.numPortfoliosToPlot<-length(psstats.portfolioNames) # set to length(psstats.portfolioNames) if we want an area plot for every portfolio in the list; 0 if no plot()'s desired
	psstats.securityAssetClasses<-global.securityAssetClasses()
	# setting this one to FALSE also turns off all plots as a side effect
	psstats.doAssetClassStats<-FALSE
	# periods for which we want plots. -1 means the whole period for which we simulated the portfolio; positive numbers are in units of years
	psstats.plotPeriods<-c(-1,10,5)
	psstats.palette<-global.palette()
	
	# what titles to give plots? area plots are for portfolio weights; bar plots and pie charts are for asset contribution to portfolio returns
	psstats.areaPlotTitle<-"SA2 Bellwether Risk Profile"
	psstats.barPlotTitle<-"SA2 Bellwether Asset Contribution"
	psstats.graphicsDevice<-"png" # "png" or "pdf" or "screen"  . in the first 2 cases plots are saved to files
	psstats.pngSettings<-global.pngSettings()
	
	# return settings
	return(list(psstats.pngSettings = psstats.pngSettings ,psstats.graphicsDevice = psstats.graphicsDevice , psstats.barPlotTitle = psstats.barPlotTitle , psstats.areaPlotTitle = psstats.areaPlotTitle ,psstats.palette = psstats.palette, psstats.plotPeriods = psstats.plotPeriods,psstats.doAssetClassStats= psstats.doAssetClassStats, psstats.securityAssetClasses= psstats.securityAssetClasses, psstats.numPortfoliosToPlot=psstats.numPortfoliosToPlot,psstats.bDaysPerYear= psstats.bDaysPerYear, psstats.totalRetSuffix= psstats.totalRetSuffix, psstats.dateFormat= psstats.dateFormat, psstats.cashNameSuffix= psstats.cashNameSuffix, psstats.retHistFile= psstats.retHistFile, psstats.portfolioReallocationHistFileSuffix= psstats.portfolioReallocationHistFileSuffix, psstats.portfolioReturnHistFileSuffix= psstats.portfolioReturnHistFileSuffix, psstats.portfolioNames= psstats.portfolioNames, psstats.useDB= psstats.useDB, psstats.inputDirCOSTS= psstats.inputDirCOSTS, psstats.outputDir= psstats.outputDir, psstats.inputDir= psstats.inputDir,psstats.inputDirER=psstats.inputDirER,dbSettings=dbSettings))
}

global.frontierBuilderExecSettings<-function(){
	
	coreSettings<-global.core()
	
	fbex.inputDir<-coreSettings[["dataOutput"]]
	fbex.outputDir<-coreSettings[["optOutput"]]
	fbex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	# we need these files if we're not using the DB
	fbex.ersRisksFile<-"forecasts.RData"
	fbex.marketDataFile<-"DBF.marketData.RData"
	# date formats
	fbex.dateFormat<-"%Y-%m-%d"
	fbex.dbDateFormat<-"%Y-%m-%d"
	
	# short assets with er below this threshold. Note: if the code decides to short any assets than asset class constraints in the constraints.csv file are ignored
	# NOTE: doShort can be "threshold" or "securityER" or "none"
	fbex.shortingControl <- list( doShort="none", threshold=list(equityThreshold = 0.02, bondThreshold=0.0), securityER=list(security="us.gg.10y") ) 

	# this is used in some methods. downside risk at 09/2012
	fbex.cashBenchmark <- list( name="us.gg.3m", er=".er", risk=".orisk" )
	
	# file specifying different asset universes to be used in building the frontiers
	fbex.constraints<-"constraintsFiles.R"
	# sanity check: we make sure inList which is specified in the constraints file has the following indexes
	fbex.namesOfInList<-c("constraintsNAME","effFrontierFreq","correlationDataFreq","startDateForBackTest","endDateForBackTest","dayOfWeek","numFrontierPoints","rollingWindow","baseCc","halfLife","riskMeasure","erMapping","riskMapping","leverage")
	
	# flexible setting of row names in constraints file
	fbex.constraintsFileInfo <- list(hurdleRowName="ret_risk_hurdle",maxRowName="max_weight",minRowName="min_weight",assetClassRowName="asset_class",assetClassMaxWeightRowName="max_weight_class",assetClassMinWeightRowName="min_weight_class")
	# flexible setting of parameters needed for optimizer
	fbex.optimParams <- list(reltol= 1.0e-6, outer.eps=1.0e-3, interiorWidth=0.001)
	
	# which construction methods do we use: "MV" is the old mean-variance methodology; "MVU" is mean-variance utility maximization; "MAD" is mean-absolute deviation; "MADU" is MAD "utility" maximization; "DS" is downside risk; "HMU" is higher-moment (i.e., skew/kurtosis) utility maximization
	fbex.methods <- c("MVU") # or combination of: "MV", "MVU", "MAD", "MADU", "HMU", "DS1", "DS2"
	# MVU parameters are the risk aversion parameters for which we want portfolios by mean-variance; HMU is an array of triplets for 
	fbex.parametersList <- list(MVU=c(200,350,400,450),HMU = c(list(list(lambda=1,doMoments=3)), list(list(lambda=5,doMoments=3)), list(list(lambda=10,doMoments=3)), list(list(lambda=20,doMoments=3)), list(list(lambda=30,doMoments=3)),  list(list(lambda=40, doMoments=3))), MADU =(1:8)*5, DS1=c(.05, .1), DS2=c(.05, .1))

	# suffix for rates used in converting to a domestic perspective
	frnt.conversionRateNameSuffix<-".gg.3m"
	# do we include risks in the conversion? (it's wrong to do that; it's here for continuity)
	frnt.doRisks<-T
	# suffix for cash name
	frnt.cashNameSuffix<-".gg.3m"
	# needed to annualize the yields on the 10Y and 2Y if fbex.cashNameSuffix is set to ".gg.10Y" or ".gg.2Y"
	frnt.couponsPerYear<-global.bondSettings()[["couponsPerYear"]]
	# note the durations in our DB are divided by 100 when the risk measure is "duration"
	frnt.varScaleFactor<-100.0 # divide retrieved risks by this factor
	
	# now return list()
	return(list(fbex.shortingControl=fbex.shortingControl,fbex.cashBenchmark=fbex.cashBenchmark,fbex.parametersList=fbex.parametersList,fbex.constraintsFileInfo=fbex.constraintsFileInfo,fbex.optimParams=fbex.optimParams,fbex.methods=fbex.methods,frnt.doRisks=frnt.doRisks,frnt.conversionRateNameSuffix=frnt.conversionRateNameSuffix,frnt.varScaleFactor= frnt.varScaleFactor, frnt.couponsPerYear= frnt.couponsPerYear, frnt.cashNameSuffix= frnt.cashNameSuffix, fbex.namesOfInList= fbex.namesOfInList, fbex.constraints= fbex.constraints, fbex.dbDateFormat= fbex.dbDateFormat, fbex.dateFormat= fbex.dateFormat, fbex.marketDataFile= fbex.marketDataFile, fbex.ersRisksFile= fbex.ersRisksFile, fbex.useDB= fbex.useDB, fbex.outputDir= fbex.outputDir, fbex.inputDir= fbex.inputDir, dbSettings= dbSettings))
}

global.portfolioMixerExecSettings<-function(){
	
	coreSettings<-global.core()
			
	pmex.inputDir<-coreSettings[["optOutput"]]
	pmex.inputDATADir<-coreSettings[["dataOutput"]]
	pmex.outputDir<-coreSettings[["dataOutput"]]
	pmex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
			
	# write output to this file
	pmex.outputFile<-"Base-MVU.450.350-Monday.csv"
	# use this file to get risk free rate data; ignored if pmex.useDB==TRUE
	pmex.dataFileName<-"DBF.marketData.RData"
	# portfolios to mix
	pmex.inputPortfolioFiles<-c("Base-MVU.450-Monday.csv", "Base-MVU.350-Monday.csv") 
	
	# this is only used when pmex.policyFunction == pmex.policyProgressiveRisk. if all portfolios are weekly choose sqrt(52); monthly sqrt(12); daily sqrt(261); Note: if the portfolios were not all built with the same frequency then the results from this are WRONG
	pmex.annualizeRisk <- sqrt(52)
	
	# use these weights when building the mixed portfolios; this is ignored when pmex.policyFunction == pmex.policyOptimalERWeights; for compatibility reasons, in the case pmex.policyProgressiveRisk, it must be set to 1 ; also note these weights are applied to the portfolios based on their rankings as determined by the policy function and NOT based on the order of the portfolios in pmex.inputPortfolioFiles. Note also that the length of pmex.portfolioWeights also determines how many portfolios are considered for inclusion
	pmex.portfolioWeights<-1 # c(.5,.5)
	# use these constraints on the portfolios in pmex.inputPortfolioFiles in the order of those portfolios (i.e., the first bounds apply to the first portfolio, etc.); this is ignored if pmex.policyFunction is NOT pmex.policyOptimalERWeights
	pmex.portfolioConstraints<-list(lowerBounds=c(0,0,0),upperBounds=c(1.,1.,1.))	
	# use this policy function
	pmex.policyFunction<-"pmex.policyProgressiveRisk"   #"pmex.policyOptimalERWeights" , "pmex.policyBestERSpread" , "pmex.policySharpeRatio",  "pmex.policyBestEROverRisk",  "pmex.policyBestER", "pmex.policyProgressiveRisk"
	# use this suffix for cash position in portfolios
	pmex.cashNameSuffix <-".gg.3m"
	# use this suffix for sharpe ratio and ER spread calculations and rankings
	pmex.riskFreeSuffix<-".gg.3m"
	# use this date format
	pmex.dateFormat<-"%Y-%m-%d"

	# ers/risks file
	pmex.erRisksFile<-"forecasts.RData"
	# which assets to run

	# this is ignored unless pmex.doOneSecPortfolios == TRUE
	pmex.assetsForOneSecPortfolios<-c("ge.gg.10y","uk.gg.10y","us.gg.10y","jp.gg.10y","au.gg.10y","sw.gg.10y","ca.gg.10y","sz.gg.10y","it.gg.10y","ge.cdax","fr.sbf250","fr.cac40","uk.asx","uk.equity","sw.equity","sw.omx30","eu.stxe600","eu.eurostx","us.mscieafe","us.msciem","jp.equity","au.asx200","sg.equity","hk.equity","no.equity","it.mib40","sp.ibex35","tw.equity","ca.equity","nl.equity","sz.equity","us.equity","us.dji","us.snp400","us.nasdaq100","us.russell2000")

	# list of er's to use
	pmex.ers<-c("ge.gg.10y.er","uk.gg.10y.er","us.gg.10y.er","jp.gg.10y.er","au.gg.10y.er","sw.gg.10y.er","ca.gg.10y.er","sz.gg.10y.er","it.gg.10y.er","ge.cdax.jer","fr.sbf250.jer","fr.cac40.jer","uk.asx.jer","uk.equity.jer","sw.equity.jer","sw.omx30.jer","eu.stxe600.jer","eu.eurostx.jer","us.mscieafe.jer","us.msciem.jer","jp.equity.jer","au.asx200.jer","sg.equity.ner","hk.equity.jer","no.equity.jer","it.mib40.ter","sp.ibex35.jer","tw.equity.ner","ca.equity.jer","nl.equity.jer","sz.equity.jer","us.equity.ter","us.dji.jer","us.snp400.ter","us.nasdaq100.jer","us.russell2000.jer")
	# best run pmex.ers<-c("ge.gg.10y.er","uk.gg.10y.er","us.gg.10y.er","jp.gg.10y.er","au.gg.10y.er","sw.gg.10y.er","ca.gg.10y.er","sz.gg.10y.er","it.gg.10y.er","ge.cdax.ner","fr.sbf250.ner","fr.cac40.ner","uk.asx.ner","uk.equity.ner","sw.equity.ner","sw.omx30.ner","eu.stxe600.ner","eu.eurostx.ner","us.mscieafe.ner","us.msciem.ner","jp.equity.jer","au.asx200.ner","sg.equity.ner","hk.equity.ner","no.equity.ner","it.mib40.ter","sp.ibex35.ner","tw.equity.ner","ca.equity.ner","nl.equity.ner","sz.equity.ner","us.equity.rer","us.dji.ner","us.snp400.rer","us.nasdaq100.ner","us.russell2000.ner")
	# list of risks to use
	pmex.risks<-c("ge.gg.10y.orisk","uk.gg.10y.orisk","us.gg.10y.orisk","jp.gg.10y.orisk","au.gg.10y.orisk","sw.gg.10y.orisk","ca.gg.10y.orisk","sz.gg.10y.orisk","it.gg.10y.orisk","ge.cdax.orisk","fr.sbf250.orisk","fr.cac40.orisk","uk.asx.orisk","uk.equity.orisk","sw.equity.orisk","sw.omx30.orisk","eu.stxe600.orisk","eu.eurostx.orisk","us.mscieafe.orisk","us.msciem.orisk","jp.equity.orisk","au.asx200.orisk","sg.equity.orisk","hk.equity.orisk","no.equity.orisk","it.mib40.orisk","sp.ibex35.orisk","tw.equity.orisk","ca.equity.orisk","nl.equity.orisk","sz.equity.orisk","us.equity.orisk","us.dji.orisk","us.snp400.orisk","us.nasdaq100.orisk","us.russell2000.orisk")
	
	# choose to mix single sec portfolios  FALSE is for portfolio mixing; TRUE for single portfolio
	pmex.doOneSecPortfolios<-FALSE
	
	# specify base cash name, portfolio will be viewed from this perspective
	pmex.baseCashName<-"us.gg.3m"
	
	# specify frequency of the mix. only applies when pmex.doOneSecPortfolios is TRUE
	pmex.mixedPortfolioFrequency<-"weekly" # or "daily" or "weekly" or "monthly
	# this only matters if pmex.mixedPortfolioFrequency is set to "weekly"
	pmex.dayOfWeek<-"Monday"
	
	return(list(pmex.annualizeRisk=pmex.annualizeRisk,pmex.dayOfWeek= pmex.dayOfWeek, pmex.mixedPortfolioFrequency= pmex.mixedPortfolioFrequency, pmex.baseCashName= pmex.baseCashName, pmex.doOneSecPortfolios= pmex.doOneSecPortfolios, pmex.risks= pmex.risks, pmex.ers= pmex.ers, pmex.assetsForOneSecPortfolios= pmex.assetsForOneSecPortfolios, pmex.erRisksFile= pmex.erRisksFile, pmex.dateFormat= pmex.dateFormat, pmex.riskFreeSuffix= pmex.riskFreeSuffix, pmex.cashNameSuffix= pmex.cashNameSuffix, pmex.policyFunction= pmex.policyFunction, pmex.portfolioConstraints= pmex.portfolioConstraints, pmex.portfolioWeights= pmex.portfolioWeights, pmex.inputPortfolioFiles= pmex.inputPortfolioFiles, pmex.dataFileName= pmex.dataFileName, pmex.outputFile= pmex.outputFile, pmex.useDB= pmex.useDB, pmex.outputDir= pmex.outputDir, pmex.inputDATADir= pmex.inputDATADir, pmex.inputDir= pmex.inputDir,dbSettings=dbSettings))
}

global.makeMacroBucketsExecSettings<-function(){
	
	coreSettings<-global.core()
			
	macroex.inputDir<-coreSettings[["input"]]
	macroex.outputDir<-coreSettings[["dataOutput"]]
	macroex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	macroDbSettings <- coreSettings[["macroDbSettings"]]
	
	macroex.useMacroDB<-TRUE
	
	# input/output files
	macroex.mktDataFile<-paste(macroex.inputDir,"DBF.marketData.RData",sep="")
	macroex.outputFile<-"macroBuckets.csv"
	
	# macro data is special. indeed we have macroex.useMacroDB to control where we look for it. here we tell macroBucketingExec how to recognize macro data
	macroex.suffixesFiles<-list(.cpi="DBF.inflation.csv",.gdp="DBF.gdp.csv",.gdpreal="gdp-yearly-GFD.csv",.gdpdeflator="gdpdeflator.csv")
	
	# NOTE: for all series, the code now allows only method to characterize a component in the series, for example, percent change or standard deviation. read comments in function macroex.buildVolatility() in makeMacroBucketsExec.R
	# inflation data
	macroex.inflationIds<-c("us.cpi","uk.cpi","jp.cpi","ge.cpi","fr.cpi","ca.cpi","it.cpi")
	
	# growth data
	macroex.growthIds<-c("us.gdprate","uk.gdprate","jp.gdprate","ge.gdprate","fr.cpi","ca.cpi","it.cpi")
	
	# volatility data
	macroex.volatilityIds<-c("us.dji","uk.asx","jp.equity","ge.equity","fr.cac40","ca.tsx60","it.mib40")
	
	# specify how to label the macro buckets. all numbers are bounds for zscores. for example, if we have severestagnation=c(-Inf,-2), this means, if the zscore is greater than -Inf and less than -2, call the macro environment "severestagnation". zscore > 2 means the value we're looking at is 2 standard deviations above the long term mean
	macroex.growthLabels<-list(stagnation=c(-Inf,0),boom=c(0,Inf))
	macroex.inflationLabels<-list(disinflationary=c(-Inf,0),inflationary=c(0,Inf))
	macroex.volatilityLabels<-list(stable=c(-Inf,0),volatile=c(0,Inf))
	
	# suffix for the series names to help store and retrieve them from DB
	macroex.inflationGrowthSuffix<-".inflationGrowth"
	macroex.volatilityGrowthSuffix<-".volatilityGrowth"
	
	macroex.dateFormat<-"%Y-%m-%d"
	
	return(list(macroDbSettings=macroDbSettings,macroex.dateFormat= macroex.dateFormat, macroex.volatilityGrowthSuffix= macroex.volatilityGrowthSuffix, macroex.inflationGrowthSuffix= macroex.inflationGrowthSuffix, macroex.volatilityLabels= macroex.volatilityLabels, macroex.inflationLabels= macroex.inflationLabels, macroex.growthLabels= macroex.growthLabels, macroex.volatilityIds= macroex.volatilityIds, macroex.growthIds= macroex.growthIds, macroex.inflationIds= macroex.inflationIds, macroex.suffixesFiles= macroex.suffixesFiles, macroex.outputFile= macroex.outputFile, macroex.mktDataFile= macroex.mktDataFile, macroex.useMacroDB= macroex.useMacroDB, macroex.useDB= macroex.useDB, macroex.outputDir= macroex.outputDir, macroex.inputDir= macroex.inputDir,dbSettings= dbSettings))
}

global.macroBucketExecSettings<-function(){
	
	coreSettings<-global.core()
			
	mbex.inputDir<-coreSettings[["simOutput"]]
	mbex.outputDir<-coreSettings[["dataOutput"]]

	# output file with returns and standard deviations averages in different macro environments
	mbex.outputFile<-"bucketedValues-FULL.csv"
	
	# this specifies the country using whose macro buckets we want to bucket our data. each item in the list should be a country code, as in "us","uk",etc., and multiple portfolios can be specified. the code will only bucket those portfolios. NOTE: "-"'s in the portfolio's name should be replaced with "."'s.
	portfolios <- c("LIVE.NonUS.Monday.M.Monday.erRiskRatio","LIVE.NonUS.Monday.MVU.450.Monday.euclidean","LIVE.NonUS.Monday.MVU.50.Monday.euclidean","prog1.450.50.p20.policyProgressiveRisk1","MIX.TARO.JIRO.policyConstantProportions","MIX.TARO.JIRO.policyConstantProportions.policyDownAndSuspend")
	mbex.countryPortfolioMapping<-list(us=portfolios, uk=portfolios, ca=portfolios, it=portfolios, fr=portfolios, ge=portfolios, jp=portfolios) 

	# file where quarters are mapped to a macro label. each column in the file is used separately; for example, if we had 2 columns, us.volatilityGrowth and us.inflationGrowth, then each portfolio that was mapped to "us" in mbex.countryPortfolioMapping will be bucketed using us.volatilityGrowth and us.inflationGrowth and the results stacked
	mbex.macroBucketDataFile<-"macroBuckets.csv"
	# file containing cumulative returns for portfolios we want to bucket
	mbex.cumulativeReturnDataFile<- "FULL-sim.allPortsdaily-CUMM.csv"
	
	return(list(mbex.cumulativeReturnDataFile= mbex.cumulativeReturnDataFile, mbex.macroBucketDataFile= mbex.macroBucketDataFile, mbex.countryPortfolioMapping= mbex.countryPortfolioMapping, mbex.outputFile= mbex.outputFile, mbex.outputDir= mbex.outputDir, mbex.inputDir= mbex.inputDir))
	
}

global.varExecSettings<-function(){
	
	coreSettings<-global.core()
			
	varex.inputDir<-coreSettings[["input"]]
	varex.outputDir<-coreSettings[["dataOutput"]]
	varex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	# file containing the DAILY returns to be used in calculating historical shocks. ignored if varex.useDB is TRUE
	varex.retFileName<-"dTrCr.csv"
	# file containing a history of portfolios. It can be a history of whole frontiers, as in the file "*-Opt-FULL.csv" that comes out of frontierBuilder, or a history of a single point on the frontier, as in the file "*-A-*.csv" also from frontier builder
	varex.portfoliosFileName<-"US15-monthly-Opt-FULL.csv"
	# output goes into 
	varex.outputFileExtension<-".VaR.csv"
	# in the specification of the frontier, the asset weights columns begin at this column
	varex.portfolioFirstAssetCol<-5
	
	# suffix for data to be used in calculating shocks
	varex.retSuffix<-".dtr"
	varex.dateFormat<-"%Y-%m-%d"
	
	# horizons over which we want to calculate vars. the unit is DAYS
	varex.horizons<-c(1,5,21)
	# how many historical shocks to use
	varex.numberOfShocks<-1305
	# confidence levels at which we want var's
	varex.confidenceLevels<-c(.01,.05)

	# return list() of settings
	return(list(varex.confidenceLevels= varex.confidenceLevels, varex.numberOfShocks= varex.numberOfShocks, varex.horizons= varex.horizons, varex.dateFormat= varex.dateFormat, varex.retSuffix= varex.retSuffix, varex.portfolioFirstAssetCol= varex.portfolioFirstAssetCol, varex.outputFileExtension= varex.outputFileExtension, varex.portfoliosFileName= varex.portfoliosFileName, varex.retFileName= varex.retFileName, varex.useDB= varex.useDB, varex.outputDir= varex.outputDir, varex.inputDir= varex.inputDir, dbSettings= dbSettings))
}

global.pcaExecSettings<-function(){
	
	coreSettings<-global.core()
			
	pcaex.inputDir<-coreSettings[["input"]]
	pcaex.outputDir<-coreSettings[["dataOutput"]]
	
	pcaex.inputFile<-"pcaInputIndices.csv" # "pcaInputAssets.csv" # "pcaInputIndices.csv"
	pcaex.outputFile<-"pcaOutputIndices.csv" # "pcaOutputAssets.csv" # "pcaOutputIndices.csv"
	pcaex.freq<-"monthly" # not used in this version
	pcaex.rollingWindow<--1 # set this to -1 if you want a single PCA over the entire history; otherwise number of months
	
	# when plotting we like to group many plots (corresponding to different dates in the history if pcaex.rollingWindow !=-1) in one window. this specifies how many plots are put in one plotting window. if we're calculating 1 PCA for the entire history, we want the one plot to fill the whole window
	pcaex.numSubPlots<-ifelse(pcaex.rollingWindow==-1,1,4)
	# this affects the size of the font of the text that appears in the graphs
	pcaex.fontFactor<-0.5
	
	return(list(pcaex.fontFactor= pcaex.fontFactor, pcaex.numSubPlots= pcaex.numSubPlots, pcaex.rollingWindow= pcaex.rollingWindow, pcaex.freq= pcaex.freq, pcaex.outputFile= pcaex.outputFile, pcaex.inputFile= pcaex.inputFile, pcaex.outputDir= pcaex.outputDir, pcaex.inputDir= pcaex.inputDir))
}

global.frontierAnalysisExecSettings<-function(){
	
	coreSettings<-global.core()
	
	faex.inputDir<-coreSettings[["optOutput"]]
	faex.outputDir<-coreSettings[["dataOutput"]]
	
	faex.numFrontierPoints <- 31
	faex.frontiersFile<-"Base-Monday-Opt-FULL.csv"
	faex.pcaRollingWindow<-12
	
	faex.assetWeightsBeginInColumn<-5
	
	return(list(faex.numFrontierPoints = faex.numFrontierPoints, faex.assetWeightsBeginInColumn= faex.assetWeightsBeginInColumn, faex.pcaRollingWindow= faex.pcaRollingWindow, faex.frontiersFile= faex.frontiersFile, faex.outputDir= faex.outputDir, faex.inputDir= faex.inputDir))
}

global.multiStrategyExecSettings<-function(){
	
	coreSettings<-global.core()
	
	msex.inputDir<-coreSettings[["simOutput"]]
	msex.inputDirER<-coreSettings[["dataOutput"]]
	msex.outputDir<-coreSettings[["simOutput"]]
	msex.useDB <-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	# only needed if msex.useDB is FALSE
	msex.dtrCrFile <- "dtrCr.RData"
	msex.dateFormat <- "%Y-%m-%d"
	
	# output files suffixes
	msex.portfolioReturnHistFileSuffix<-".sim.csv"
	msex.portfolioReallocationHistFileSuffix<-".realloc.csv"
	
	msex.cashNameSuffix<-".gg.3m"
	msex.transactionCosts<-global.transactionCosts()
	msex.defaultTransactionCost<-global.defaultTransactionCost()
	
	# the portfolios in the multi-strategy
	msex.portfolios<-c("Base-MVU.450-Monday.euclidean","Base-MVU.50-Monday.euclidean")

	# policy settings
	msex.policy<- "msex.policyDownAndSuspend" # "msex.policyConstantProportions" or "msex.policyBestReturn" or "msex.policyDownAndOut" or "msex.policyDownAndSuspend" or "msex.policyProgressiveRisk1" or "msex.policyProgressiveRisk2" or "msex.policyWRONGProgressiveRisk2"
	
	# the following only matters if msex.policy == "msex.policyConstantProportions", otherwise ignored. it instructs which weights to apply when combining the portfolios; i.e., it gives the constants in "ConstantProportions". Note that the weights do NOT have to add up to 1
	#msex.portfoliosWeights <- c(1, 1)
	msex.portfoliosWeights <- c(.5,.5)
	msex.reinvestmentDecision <- "riskyAssets" # "riskyAssets" or "noReinvestment"
	

	# the following only matter if msex.policy == "msex.policyBestReturn" or "msex.policyProgressiveRisk". otherwise ignored. first the index of the base portfolio in the msex.portfolios vector
	msex.basePortfolio<-1
	# outperformance required from non-base portfolios before we switch to them
	msex.bestReturnHurdle<-0.075
	# frequency of testing to switch between portfolios
	msex.reallocationFrequency<-1 # any integer
	# length of past period over which we calculate realized performance for the purpose of deciding which portfolio
	msex.performancePeriod<-12
	# unit for msex.reallocationFrequency and msex.performancePeriod
	msex.reallocationTimeUnit<-"year" # or "month" or "day" or "week" or "year" or "quarter". NOTE: not all policies accept all units
	# if weekly on which day?
	msex.dayOfWeek<-"Monday"

	# the following concerns msex.policy == "msex.policyDownAndOut" or "msex.policyDownAndSuspend" or "msex.policyProgressiveRisk"
	msex.downThreshold <- -0.02
	msex.controlFrequency <- "monthly"

	# the following only matters when msex.policy == "msex.policyProgressiveRisk1". Note: number of thresholds here is equal to number of portfolios LESS 1
	msex.progressiveThresholds1 <- c(.2)
	# the following only matters when msex.policy == "msex.policyProgressiveRisk2". Note: number of thresholds here is equal to number of portfolios
	msex.progressiveThresholds2 <- c(-.2, 0, .1, .2,.3)
	
	# assets weights begin in this column
	msex.firstAssetWeightColumn <- 4
	# input file names are obtained by paste()'ing the portfolio names in msex.portfolios and msex.simFileSuffix
	msex.simFileSuffix<-".sim.csv"
	msex.reallocFileSuffix<-".realloc.csv"
	msex.outputFileName<-"prog1-450-50"
	
	return(list(msex.progressiveThresholds1=msex.progressiveThresholds1, msex.progressiveThresholds2=msex.progressiveThresholds2, msex.controlFrequency=msex.controlFrequency,msex.downThreshold=msex.downThreshold,msex.dateFormat=msex.dateFormat,dbSettings=dbSettings,msex.dtrCrFile=msex.dtrCrFile,msex.useDB=msex.useDB,msex.reinvestmentDecision=msex.reinvestmentDecision,msex.reallocFileSuffix=msex.reallocFileSuffix,msex.portfoliosWeights=msex.portfoliosWeights,msex.dayOfWeek= msex.dayOfWeek, msex.reallocationTimeUnit= msex.reallocationTimeUnit, msex.performancePeriod= msex.performancePeriod, msex.reallocationFrequency= msex.reallocationFrequency, msex.outputFileName= msex.outputFileName, msex.simFileSuffix= msex.simFileSuffix, msex.firstAssetWeightColumn= msex.firstAssetWeightColumn, msex.bestReturnHurdle= msex.bestReturnHurdle, msex.basePortfolio= msex.basePortfolio, msex.policy= msex.policy, msex.portfolios= msex.portfolios, msex.defaultTransactionCost= msex.defaultTransactionCost, msex.transactionCosts= msex.transactionCosts, msex.cashNameSuffix= msex.cashNameSuffix, msex.portfolioReallocationHistFileSuffix= msex.portfolioReallocationHistFileSuffix, msex.portfolioReturnHistFileSuffix= msex.portfolioReturnHistFileSuffix, msex.outputDir= msex.outputDir, msex.inputDir= msex.inputDir, msex.inputDirER= msex.inputDirER))
}

global.makeHistoricalExecSettings<-function(){
	
	coreSettings<-global.core()
	
	hrr.inputDir<-coreSettings[["input"]]
	hrr.outputDir<-coreSettings[["dataOutput"]]
	hrr.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	hrr.assets<-c("us.dji","us.equity","us.snp100","us.snp400","us.snp600","us.rafi","us.djutil","us.msciworld","us.mscieafe","us.msciem","us.msciasiapacxjpn","us.russell2000","us.nasdaq100","ca.tsx60","ca.equity","jp.equity","jp.EPG","jp.topix400","jp.rafi","ge.cdax","ge.equity","fr.sbf250","fr.cac40","sw.equity","sw.omx30","it.mib40","no.equity","sp.ibex35","nl.equity","sz.equity","eu.stxe600","eu.eurostx","eu.rafi","eu.utileurope","eu.euroutil","eu.epraeurozone","uk.asx","uk.equity","uk.ftse250","au.equity","au.asx200","hk.equity","sg.mscifree","tw.mscifree","sg.2mscifree","tw.2mscifree","sg.equity","tw.equity","nz.equity","ko.kospi200","us.gg.10y","uk.gg.10y")
			
	hrr.mktDataFile<-"DBF.marketData.RData"
	hrr.forecastsFile <- "forecasts.RData"
	hrr.dateFormat<-"%Y-%m-%d"
	
	hrr.outputFile<-"historicalRetsRisks-Monday.csv"
	hrr.retSuffix<-".her"
	hrr.riskSuffix<-".hrisk"
	
	hrr.period<-"week" # "week" or "month"
	hrr.dayOfWeek<-"Monday"
	
	hrr.bondIndentifiers <- global.nonEquityIdentifiers()[["bond"]]
	# which risk gives us plain durations for bonds?
	hrr.durationRiskSuffix <- ".orisk"
	
	return(list(hrr.forecastsFile=hrr.forecastsFile,hrr.durationRiskSuffix=hrr.durationRiskSuffix,hrr.bondIndentifiers=hrr.bondIndentifiers,hrr.dayOfWeek= hrr.dayOfWeek, hrr.period= hrr.period, hrr.riskSuffix= hrr.riskSuffix, hrr.retSuffix= hrr.retSuffix, hrr.outputFile= hrr.outputFile, hrr.dateFormat= hrr.dateFormat, hrr.mktDataFile= hrr.mktDataFile, hrr.assets= hrr.assets, hrr.useDB= hrr.useDB, hrr.outputDir= hrr.outputDir, hrr.inputDir= hrr.inputDir, dbSettings= dbSettings))
}

global.modelEvaluatorExecSettings<-function(){
	
	coreSettings<-global.core()
	
	meeex.inputDir<-coreSettings[["dataOutput"]]
	meeex.outputDir<-coreSettings[["dataOutput"]]
	meeex.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	meeex.forecastsFile<-"forecasts.RData"
	meeex.drFile<-"dTrCr.RData"
	meeex.outputFile<-"forecasts-rmse.csv"
	
	meeex.assets<-c("us.dji","us.equity","us.nasdaq100","us.snp100","us.snp400","us.snp600","us.mscilocalworld","us.msciworld","us.mscieafe","us.msciem","us.msciasiapacxjpn","ca.equity","ca.tsx60","jp.equity","jp.EPG","jp.topix400","ge.cdax","ge.equity","fr.sbf250","fr.cac40","sw.equity","sw.omx30","it.mib40","no.equity","sp.ibex35","nl.equity","sz.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","uk.asx","uk.equity","uk.ftse250","au.equity","au.asx200","hk.equity","sg.equity","sg.mscifree","sg.2mscifree","tw.2mscifree","tw.mscifree","tw.equity","nz.equity","ko.kospi200","mx.equity","th.set50","my.equity","id.mscifree","cl.equity","fn.equity","in.equity","sa.equity") #,"us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","jp.gg.10y","jp.gg.2y","jp.gg.3m","uk.gg.10y","uk.gg.2y","uk.gg.3m","au.gg.10y","au.gg.2y","au.gg.3m","ca.gg.10y","ca.gg.2y","ca.gg.3m","eu.gg.10y","eu.gg.2y","eu.gg.3m","ge.gg.10y","ge.gg.2y","ge.gg.3m","fr.gg.10y","fr.gg.2y","fr.gg.3m","it.gg.10y","it.gg.2y","it.gg.3m","sp.gg.10y","sp.gg.2y","sp.gg.3m","sz.gg.10y","sz.gg.2y","sz.gg.3m","sw.gg.10y","sw.gg.2y","sw.gg.3m","hk.gg.10y","hk.gg.2y","hk.gg.3m","sg.gg.10y","sg.gg.2y","sg.gg.3m","nl.gg.10y","nl.gg.2y","nl.gg.3m","no.gg.3m","nz.gg.3m","nz.gg.10y","sa.gg.3m","sa.gg.2y","sa.gg.10y","ko.gg.3m","ko.gg.2y","ko.gg.3y","ko.gg.10y","tw.gg.3m","tw.gg.2y","tw.gg.10y","ph.gg.3m","ph.gg.2y","ph.gg.10y","in.gg.3m","in.gg.2y","in.gg.10y","id.gg.3m","id.gg.2y","id.gg.10y","my.gg.3m","my.gg.2y","my.gg.10y","th.gg.3m","th.gg.2y","th.gg.10y","mx.gg.3m","mx.gg.2y","mx.gg.10y","us.gig.10y")	
	#meeex.assets<-c("us.dji","us.equity","us.nasdaq100","us.snp100","us.snp400","us.snp600","us.djutil","us.mscilocalworld","us.msciworld","us.mscieafe","us.msciem","us.msciasiapacxjpn","ca.equity","ca.tsx60","jp.equity","jp.EPG","jp.topix400","ge.cdax","ge.equity","fr.sbf250","fr.cac40","sw.equity","sw.omx30","it.mib40","no.equity","sp.ibex35","nl.equity","sz.equity","eu.stxe600","eu.eurostx","eu.estx50","eu.stxe50","eu.utileurope","eu.euroutil","uk.asx","uk.equity","uk.ftse250","au.equity","au.asx200","hk.equity","sg.equity","sg.mscifree","sg.2mscifree","tw.2mscifree","tw.mscifree","tw.equity","nz.equity","ko.kospi200","mx.equity","th.set50","my.equity","id.mscifree","fn.equity","cl.equity","in.equity","sa.equity","us.gg.30y","us.gg.10y","us.gg.2y","us.gg.3m","jp.gg.10y","jp.gg.2y","jp.gg.3m","uk.gg.10y","uk.gg.2y","uk.gg.3m","au.gg.10y","au.gg.2y","au.gg.3m","ca.gg.10y","ca.gg.2y","ca.gg.3m","eu.gg.10y","eu.gg.2y","eu.gg.3m","ge.gg.10y","ge.gg.2y","ge.gg.3m","fr.gg.10y","fr.gg.2y","fr.gg.3m","it.gg.10y","it.gg.2y","it.gg.3m","sp.gg.10y","sp.gg.2y","sp.gg.3m","sz.gg.10y","sz.gg.2y","sz.gg.3m","sw.gg.10y","sw.gg.2y","sw.gg.3m","hk.gg.10y","hk.gg.2y","hk.gg.3m","sg.gg.10y","sg.gg.2y","sg.gg.3m","nl.gg.10y","nl.gg.2y","nl.gg.3m","no.gg.3m","nz.gg.3m","nz.gg.10y","sa.gg.3m","sa.gg.2y","sa.gg.10y","ko.gg.3m","ko.gg.2y","ko.gg.3y","ko.gg.10y","tw.gg.3m","tw.gg.2y","tw.gg.10y","ph.gg.3m","ph.gg.2y","ph.gg.10y","in.gg.3m","in.gg.2y","in.gg.10y","id.gg.3m","id.gg.2y","id.gg.10y","my.gg.3m","my.gg.2y","my.gg.10y","th.gg.3m","th.gg.2y","th.gg.10y","mx.gg.3m","mx.gg.2y","mx.gg.10y","us.gig.10y")	

	meeex.erSuffixes<-list(equity=c(".jer",".ner",".ber"), bond=c(".er"), tips=c(".er"))
	meeex.dtrSuffix<-c(".dtr")
	meeex.dcrSuffix<-c(".dcr")
	meeex.frequency<-"monthly"
	meeex.dayOfWeek<-"Monday"
	meeex.dateFormat<-"%Y-%m-%d"

	meeex.assetIdentifiers<-global.nonEquityIdentifiers()
	
	meeex.runPredictExec<-TRUE
	# do we check against capital returns?
	meeex.doCapitalReturns <- FALSE
	
	# calculate RMSE's against future, actual annual returns over this many years
	meeex.forecastPeriodInMonths<-120
	# calculate RMSE's for periods of this many years in the history
	meeex.numberOfYearsInSubperiods <- 10
	
	meeex.allowGraphs<-TRUE
	meeex.numSubPlots<-8
	
	return(list(meeex.doCapitalReturns=meeex.doCapitalReturns,meeex.assetIdentifiers=meeex.assetIdentifiers, meeex.dcrSuffix=meeex.dcrSuffix,meeex.numberOfYearsInSubperiods=meeex.numberOfYearsInSubperiods,meeex.numSubPlots= meeex.numSubPlots, meeex.allowGraphs= meeex.allowGraphs, meeex.forecastPeriodInMonths= meeex.forecastPeriodInMonths, meeex.runPredictExec= meeex.runPredictExec, meeex.dateFormat= meeex.dateFormat, meeex.dayOfWeek= meeex.dayOfWeek, meeex.frequency= meeex.frequency, meeex.dtrSuffix= meeex.dtrSuffix, meeex.erSuffixes= meeex.erSuffixes, meeex.assets= meeex.assets, meeex.outputFile = meeex.outputFile , meeex.drFile = meeex.drFile ,meeex.forecastsFile= meeex.forecastsFile, meeex.useDB= meeex.useDB, meeex.outputDir= meeex.outputDir, meeex.inputDir= meeex.inputDir, dbSettings= dbSettings))
}

global.totalReturnExecSettings<-function(){
	
	coreSettings<-global.core()
		
	totret.inputDir<-coreSettings[["input"]]
	totret.outputDir<-coreSettings[["dataOutput"]]
	totret.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	totret.mktDataFile<-"DBF.marketData.csv"
	totret.outputFile<-"totalReturn-korea.csv"
	
	totret.assets<-c("ko.kospi200","ch.equity")
	
	totret.priceSuffix<-c("")
	totret.yldSuffix<-c(".dvd.yld")
	totret.totRetSuffix<-c(".tr")
	
	totret.dateFormat<-"%Y-%m-%d"
	totret.daysPerYear<-365.24
	totret.numLast<-40
	
	return(list(totret.numLast= totret.numLast, totret.daysPerYear= totret.daysPerYear, totret.dateFormat= totret.dateFormat, totret.totRetSuffix= totret.totRetSuffix, totret.yldSuffix= totret.yldSuffix, totret.priceSuffix= totret.priceSuffix, totret.assets= totret.assets, totret.outputFile= totret.outputFile, totret.mktDataFile= totret.mktDataFile, totret.useDB= totret.useDB, totret.outputDir= totret.outputDir, totret.inputDir= totret.inputDir,dbSettings=dbSettings))
}

global.earnFromPEExecSettings<-function(){
	
	coreSettings<-global.core()
		
	earnFromPE.inputDir<-coreSettings[["input"]]
	earnFromPE.outputDir<-coreSettings[["dataOutput"]]
	earnFromPE.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	earnFromPE.mktDataFile<-"DBF.marketData.RData"
	earnFromPE.outputFile<-"earningsfromPE-singapore.csv"
	
	earnFromPE.assets<-c("sg.mscifree","sg.2mscifree","us.mscilocalworld")
	
	earnFromPE.priceSuffix<-c("")
	earnFromPE.peSuffix<-c(".pe")
	earnFromPE.earningsSuffix<-c(".earnings")
	
	earnFromPE.dateFormat<-"%Y-%m-%d"
	earnFromPE.numLast<-40
	
	return(list(earnFromPE.numLast= earnFromPE.numLast, earnFromPE.dateFormat= earnFromPE.dateFormat, earnFromPE.earningsSuffix= earnFromPE.earningsSuffix, earnFromPE.peSuffix= earnFromPE.peSuffix, earnFromPE.priceSuffix= earnFromPE.priceSuffix, earnFromPE.assets= earnFromPE.assets, earnFromPE.outputFile= earnFromPE.outputFile, earnFromPE.mktDataFile= earnFromPE.mktDataFile, earnFromPE.useDB= earnFromPE.useDB, earnFromPE.outputDir= earnFromPE.outputDir, earnFromPE.inputDir= earnFromPE.inputDir,dbSettings=dbSettings))
}

global.dbToFileExecSettings<-function(){
	
	coreSettings<-global.core()
	
	dbf.outputDir<-coreSettings[["dataOutput"]]
	
	dbf.marketDataDBSettings <- coreSettings[["dbSettings"]]
	dbf.macroDataDBSettings <- coreSettings[["macroDbSettings"]]
	dbf.positionsDBSettings <- list( uid=dbf.marketDataDBSettings[["ld.uid"]], dsnName = dbf.marketDataDBSettings[["ld.dsnName"]], positionsTableName="tbl_sa2_positions")
	
	# dbf.assets are crossed with dbf.suffixes
	dbf.dtrAssets <- global.dailyTotalCapitalExecSettings()[["dtcex.assets"]]
	prdSettings <- global.predictExecSettings()
	dbf.prdAssets <- prdSettings[["prd.assets"]]
	dbf.useGNI <- prdSettings[["eretm.useGNI"]]
	dbf.gdpSuffix <- prdSettings[["prd.gdpSuffix"]]
	dbf.gniSuffix <- prdSettings[["prd.gniSuffix"]]
	dbf.inflationSuffix <- prdSettings[["prd.inflationSuffix"]]

	dbf.suffixes<-list(equity=c("",".earnings",".dvd.yld",".tr"),bond=c(""),commodity=c(""),tips=c("",".tr")) # "reit" is left out. to add them, append: list(reit=c(".ev",".evebitda",".netdebt",".tr"))
	# dbf.ids are taken as is (i.e., no suffixes added) and are added to the list that is passed to the DB: we put here, for example, an equity asset for which not all suffixes in dbf.suffixes apply
	dbf.otherIds <- c("us.mscilocaleafe","us.mscilocaleafe.tr")
	# for rafis get only levels and tr's
	rafis <- dbf.dtrAssets[ grepl("rafi", dbf.dtrAssets)]
	dbf.otherIds <- c(dbf.otherIds, rafis, paste(rafis, ".tr", sep=""))
	
	dbf.classIdentifiers<-global.nonEquityIdentifiers()
	
	return(list(dbf.gdpSuffix=dbf.gdpSuffix,dbf.gniSuffix=dbf.gniSuffix,dbf.inflationSuffix=dbf.inflationSuffix,dbf.useGNI=dbf.useGNI,dbf.marketDataDBSettings=dbf.marketDataDBSettings,dbf.macroDataDBSettings=dbf.macroDataDBSettings,dbf.positionsDBSettings=dbf.positionsDBSettings,dbf.otherIds =dbf.otherIds ,dbf.suffixes= dbf.suffixes, dbf.classIdentifiers= dbf.classIdentifiers, dbf.dtrAssets= dbf.dtrAssets,dbf.prdAssets=dbf.prdAssets, dbf.outputDir= dbf.outputDir))
}

global.actualPortfolioReturnExecSettings<-function(){
	
	coreSettings<-global.core()

	apret.inputDir<-coreSettings[["optOutput"]]
	apret.outputDir<-coreSettings[["dataOutput"]]
	apret.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	# every row in the following file is a portfolio. we can therefore pass a file containing one or a history of a specific point on the frontier (e.g., point A) or a file containing a whole frontier or history of frontiers. we calculate the actual return of every portfolio (i.e., every row in the file) that is stamped with one of the apret.desiredDates. NOTE: the code knows that the A portfolios file format is different to that of the Opt-FULL file format
	apret.portfolioName<-c("NonUSFutures2-test-PROD-FINAL-JTO-104-Monday-vol-Monday-Opt-FULL.csv")
	# the dates for which we want the actual returns of the points of the frontier.
	apret.desiredDates<-c("1999-12-27","1998-09-28","1997-06-30","1993-12-27","1987-12-28","1982-10-25","1981-01-26","1974-12-30","1972-12-25") # set it to "all" if the calculation is to be done for every portfolio in the file
	
	apret.assetRetfile<-"dTrCr.csv"
	# format used to read dates in assetRetFile
	apret.dateFormat<-"%Y-%m-%d"
	# the type of future return we want to calculate
	apret.retSuffix<-".dtr"
	
	# specify the number of years ahead over which actual return/sd are desired
	apret.numberOfYears<-10
	# this is needed to scale the daily vol numbers
	apret.busDaysPerYear<-260.8857143
	
	# control plotting with this variable
	apret.doPlot<-TRUE
	# if desiredDates is set to "all" and we have portfolios for 4000 different dates, we can't plot them all without a very good chance of crashing the machine. here's the safety valve for that
	apret.maximumNumberPlotsAllowed<-40
	# use our palette
	apret.palette<-global.palette()
	
	return(list(apret.palette = apret.palette ,apret.maximumNumberPlotsAllowed = apret.maximumNumberPlotsAllowed ,apret.doPlot = apret.doPlot ,apret.desiredDates= apret.desiredDates, apret.busDaysPerYear= apret.busDaysPerYear, apret.numberOfYears= apret.numberOfYears, apret.retSuffix= apret.retSuffix, apret.dateFormat= apret.dateFormat, apret.assetRetfile = apret.assetRetfile, apret.portfolioName= apret.portfolioName, apret.useDB= apret.useDB, apret.outputDir= apret.outputDir, apret.inputDir= apret.inputDir, dbSettings= dbSettings))
}

global.makeForwardBackwardExecSettings<-function(){
	
	coreSettings<-global.core()
	
	frr.inputDir<-coreSettings[["input"]]
	frr.outputDir<-coreSettings[["dataOutput"]]
	frr.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	frr.whichWay<- "both" # "forward" or "backward" or "both"
	frr.assetRetFile<-"dTrCr.RData"
	frr.bondForecastsFile<-"forecasts.RData"
	frr.bondForecastsSuffixes<-c(".er",".orisk")
	frr.outputFile<-"retRisks.frr.csv"
	frr.dtrSuffix<-".dtr"
	frr.dateFormat<-"%Y-%m-%d"
	frr.retSuffix<-".irr"
	frr.riskSuffix<-".sdev"
	
	# for which assets do we want to calculate future actual returns? Note: equity assets only. bonds are ignored (with a message) in the code. for bonds, we should just use the .er and .orisk forecasts
	frr.assets<-c("ge.gg.10y","uk.gg.10y","us.gg.10y","jp.gg.10y","au.gg.10y","sw.gg.10y","ca.gg.10y","sz.gg.10y","it.gg.10y","ge.equity","fr.cac40","uk.equity","sw.omx30","eu.stxe600","eu.eurostx","jp.equity","au.asx200","sg.2mscifree","hk.equity","no.equity","tw.2mscifree","ca.tsx60","nl.equity","ge.gg.3m","uk.gg.3m","us.gg.3m","jp.gg.3m","au.gg.3m","sw.gg.3m","ca.gg.3m","sz.gg.3m","it.gg.3m","no.gg.3m","nl.gg.3m","fr.gg.3m","sp.gg.3m","hk.gg.3m","tw.gg.3m","sg.gg.3m","nz.gg.3m","ko.gg.3m","eu.gg.3m")

	# the future actual return, is the return over this many years
	frr.numberOfYears<-10
	# this is needed to scale the daily vol numbers
	frr.busDaysPerYear<-260.8857143	
	# how do we identify bonds?
	frr.bondIdentifiers<-global.nonEquityIdentifiers()[["bond"]]
	
	return(list(frr.outputFile = frr.outputFile ,frr.whichWay=frr.whichWay,frr.riskSuffix = frr.riskSuffix , frr.retSuffix = frr.retSuffix ,frr.bondForecastsFile = frr.bondForecastsFile , frr.bondForecastsSuffixes = frr.bondForecastsSuffixes ,frr.bondIdentifiers = frr.bondIdentifiers ,frr.busDaysPerYear = frr.busDaysPerYear ,frr.dateFormat= frr.dateFormat, frr.dtrSuffix= frr.dtrSuffix, frr.assetRetFile= frr.assetRetFile, frr.numberOfYears= frr.numberOfYears, frr.assets= frr.assets, frr.useDB= frr.useDB, frr.outputDir= frr.outputDir, frr.inputDir= frr.inputDir, dbSettings= dbSettings))
}

global.sharpeRatiosExecSettings<-function(){
	
	coreSettings<-global.core()
	
	shr.inputDir<-coreSettings[["input"]]
	shr.outputDir<-coreSettings[["dataOutput"]]
	shr.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]

	shr.mktDataFile<-"DBF.marketData.RData"
	shr.retHistFile<-"dTrCr.RData"
	shr.outputFile <-"sharpeRatios.sr.csv"
	
	shr.dateFormat<-"%Y-%m-%d"
	shr.baseCc<-"us"
	shr.assets<-c("us.gg.10y","nl.equity","uk.equity","jp.equity","eu.stxe600","hk.equity","ca.tsx60","sg.2mscifree","fr.cac40","ko.kospi200","au.asx200","sz.equity","tw.2mscifree","ge.equity","sw.omx30")
	shr.retSuffix<-".dtr"
	# used only to convert foreign returns to a domestic perspective
	shr.cashNameSuffix<-".gg.3m"
	shr.couponsPerYear <-global.bondSettings()[["couponsPerYear"]]
	shr.frequency <-"yearly" # or "monthly"

	return(list(shr.frequency= shr.frequency, shr.couponsPerYear= shr.couponsPerYear, shr.cashNameSuffix= shr.cashNameSuffix, shr.retSuffix= shr.retSuffix, shr.assets= shr.assets, shr.baseCc= shr.baseCc, shr.dateFormat= shr.dateFormat, shr.outputFile= shr.outputFile, shr.retHistFile= shr.retHistFile, shr.mktDataFile= shr.mktDataFile, shr.useDB= shr.useDB, shr.outputDir= shr.outputDir, shr.inputDir= shr.inputDir, dbSettings= dbSettings))
}

global.correlationsExecSettings<-function(){
	
	coreSettings<-global.core()
	
	corrs.inputDir<-coreSettings[["input"]]
	corrs.outputDir<-coreSettings[["dataOutput"]]
	corrs.useDB<-coreSettings[["useDB"]]
	
	dbSettings<-coreSettings[["dbSettings"]]
	
	corrs.portfolioSettingsFile<- "constraints-Base.r"
	corrs.mktDataFile<-"DBF.marketData.RData"
	corrs.forecastsFile<-"forecasts.RData"
	corrs.dateFormat<-"%Y-%m-%d"
	
	# only used to convert foreign returns to a domestic perspective
	corrs.cashNameSuffix<-".gg.3m"
	# do we include risks (wrongly) in the conversion?
	corrs.doRisks<-TRUE
	
	corrs.couponsPerYear<-global.bondSettings()[["couponsPerYear"]]
	corrs.varScaleFactor<-100.0
	
	return(list(corrs.doRisks=corrs.doRisks,corrs.varScaleFactor = corrs.varScaleFactor ,corrs.couponsPerYear= corrs.couponsPerYear, corrs.cashNameSuffix= corrs.cashNameSuffix, corrs.dateFormat= corrs.dateFormat, corrs.forecastsFile= corrs.forecastsFile, corrs.mktDataFile= corrs.mktDataFile, corrs.portfolioSettingsFile= corrs.portfolioSettingsFile, corrs.useDB= corrs.useDB,corrs.outputDir=corrs.outputDir, corrs.inputDir= corrs.inputDir, dbSettings= dbSettings))
}

global.predictBuildRebExecSettings<-function(){
	
	coreSettings <- global.core()
	
	marketDataDBSettings <- coreSettings[["dbSettings"]]
	pbreex.outputDir <- coreSettings[["trades"]]
	pbreex.inputDir <- coreSettings[["input"]]

	# this is the leverage creep threshold
	pbreex.leverageThreshold <- 0.02
	# what hurdle to use when rebalancing using erRiskRatio?
	pbreex.erRiskRatioHurdle <- 0.0025
	# what hurdle to use when rebalancing using euclidean?
	pbreex.euclideanHurdle <- .1
	# in a portfolio file, the first asset weight is in this column
	pbreex.firstColumn <- 4
	# use this to prefix all files output by predictBuildRebExec.R . since it calls predict.R and frontierBuilder.R, this guards against over-writing a file those produced
	pbreex.outputPrefix <- "PBRExec."
	# the portfolio weights of the positions we're in are suffixed with ".pos"
	pbreex.positionSuffix <- ".pos"
	# this one is not really used
	pbreex.varScaleFactor <- 100
	# which traders
	pbreex.traders <- c("Jiro", "Taro")
	# which portfolio for which trader
	pbreex.whichPortfolio <- list( Jiro="MVU.450", Taro="MVU.200")
	# which rebalancing decision for which trader; we index by trader to allow same portfolio to have different realloc policies if needed
	pbreex.reallocDecision <- list( Jiro="euclidean", Taro="euclidean")
	pbreex.universeToSecuritiesMappingFile <- "mapping-Universe2LiveData.csv"
	pbreex.futureContractMultiplier <- global.futureContractMultiplier()
	pbreex.livePricesFileNamePrefix <- "CLPF.liveData."
	pbreex.dirtyPriceSuffix <- ".dirty.price"	
	pbreex.cashNameSuffix <- ".gg.3m"
	pbreex.positionsDBSettings <- list( uid=marketDataDBSettings[["ld.uid"]], dsnName = marketDataDBSettings[["ld.dsnName"]], positionsTableName="tbl_sa2_positions")
	# weights below this (in abs()) are changed to 0
	pbreex.weightTolerance <- 1.0e-5
	pbreex.minis <- global.futureContractMinis()
	
	return(list(pbreex.minis=pbreex.minis,pbreex.weightTolerance=pbreex.weightTolerance,pbreex.positionsDBSettings=pbreex.positionsDBSettings,pbreex.cashNameSuffix=pbreex.cashNameSuffix,pbreex.dirtyPriceSuffix=pbreex.dirtyPriceSuffix,pbreex.livePricesFileNamePrefix=pbreex.livePricesFileNamePrefix,pbreex.futureContractMultiplier=pbreex.futureContractMultiplier,pbreex.universeToSecuritiesMappingFile=pbreex.universeToSecuritiesMappingFile,pbreex.reallocDecision=pbreex.reallocDecision,pbreex.traders=pbreex.traders,pbreex.whichPortfolio=pbreex.whichPortfolio,pbreex.euclideanHurdle=pbreex.euclideanHurdle,pbreex.varScaleFactor=pbreex.varScaleFactor,pbreex.positionSuffix=pbreex.positionSuffix,pbreex.outputPrefix=pbreex.outputPrefix,pbreex.firstColumn=pbreex.firstColumn,pbreex.erRiskRatioHurdle=pbreex.erRiskRatioHurdle,pbreex.leverageThreshold=pbreex.leverageThreshold,pbreex.inputDir=pbreex.inputDir,pbreex.outputDir=pbreex.outputDir))
}

global.createLivePriceFilesExecSettings <- function(){
	
	coreSettings <- global.core()
	
	clpf.marketDataDBSettings <- coreSettings[["dbSettings"]]	
	clpf.outputDir <- coreSettings[["input"]]
	clpf.inputDir <- coreSettings[["input"]]
	
	isWindows <- coreSettings[["isWindows"]]
	
	clpf.futureContractBasis <- global.futureContractBasis()
	clpf.specialTreatmentItems <- list( name=c("MXSG INDEX"), suffix=c(".earnings") )
	clpf.currencyBBField <- "PX_LAST"
	clpf.universeToSecuritiesMappingFile <- "mapping-Universe2LiveData.csv"
	clpf.expectedColumnNames <- c( "SA2Name", "price", "priceType", "earnings", "dividend_yield", "currency" , "otherHolding", "otherHoldingType")
	clpf.pathToJava <- global.pathToJava()
	clpf.pathToBlpJar <- global.pathToBlpJar()
	clpf.javaClass <- "BloombergData"
	clpf.classpathSeparator <- global.classpathSeparator( isWindows )
	clpf.fillNonTrading <- TRUE
	clpf.cashNameSuffix <- ".gg.3m"
	clpf.requestIndexesFile <- "request-Indexes.csv"
	clpf.priceSuffix <- ""
	clpf.livePricesFileNamePrefix <- "CLPF.liveData."
	clpf.minis <- global.futureContractMinis()

	# this is for instruments we might be holding but that do not go into the prescriptions of the position-instrument mapping file. NOTE: As of 4/10/2013, must be applicable to futures contracts only.
	clpf.bbOtherHoldings <- list( field="PX_MID", divisor=1.0 )
	# convention is: if it starts with a "." it must be appended to the name of the security and assigned that value
	equityIndexFuture <- list(suffixes=c(clpf.priceSuffix, ".last", ".earnings", ".dvd.yld"),fields=c("PX_MID","PX_LAST","TRAIL_12M_EPS","EQY_DVD_YLD_12M"), divisors=c(1,1,1,100), securityColumn = c("price","price","earnings","dividend_yield") )
	bondFuture <- list(suffixes=c(clpf.priceSuffix, ".last"),fields=c("PX_MID","PX_LAST"), divisors=c(1,1), securityColumn = c("price","price") )
	bond <- list(suffixes=c(clpf.priceSuffix, ".dirty.price"),fields=c("YLD_CNV_MID", "PX_DIRTY_MID"), divisors=c(100, 1), securityColumn = c("price", "price"))
	clpf.instrumentBBData <- list(equityIndexFuture=equityIndexFuture,  bond=bond, bondFuture=bondFuture )
	
	return( list(clpf.minis=clpf.minis,clpf.expectedColumnNames=clpf.expectedColumnNames,clpf.bbOtherHoldings=clpf.bbOtherHoldings, clpf.livePricesFileNamePrefix=clpf.livePricesFileNamePrefix,clpf.priceSuffix=clpf.priceSuffix,clpf.instrumentBBData=clpf.instrumentBBData,clpf.requestIndexesFile=clpf.requestIndexesFile,clpf.cashNameSuffix=clpf.cashNameSuffix,clpf.fillNonTrading=clpf.fillNonTrading,clpf.classpathSeparator=clpf.classpathSeparator,clpf.javaClass=clpf.javaClass, clpf.pathToBlpJar=clpf.pathToBlpJar,clpf.pathToJava=clpf.pathToJava,clpf.universeToSecuritiesMappingFile=clpf.universeToSecuritiesMappingFile, clpf.currencyBBField=clpf.currencyBBField, clpf.specialTreatmentItems=clpf.specialTreatmentItems,clpf.futureContractBasis=clpf.futureContractBasis, clpf.inputDir=clpf.inputDir,clpf.outputDir=clpf.outputDir,clpf.marketDataDBSettings=clpf.marketDataDBSettings))
}

global.bloombergDataExecSettings <- function(){
	
	coreSettings <- global.core()
	
	isWindows <- coreSettings[["isWindows"]]
	dbSettings<-coreSettings[["dbSettings"]]
	macroDbSettings <- coreSettings[["macroDbSettings"]]
	bbd.outputDir <- coreSettings[["dataOutput"]]
	
	bbd.javaClass <- "BloombergData"
	bbd.pathToJava <- global.pathToJava()
	bbd.pathToBlpJar <- global.pathToBlpJar()
	bbd.classpathSeparator <- global.classpathSeparator( isWindows )
	
	bbd.dayOfWeek <- "Monday"
	bbd.fieldMappings <- "mapping-field2Divisor.csv"
	bbd.ccMappings <- "mapping-ExchangeCountry.csv"
	bbd.equityColNumber <- 3
	bbd.requestHeader <- c("#bloomberg","bberg field","SA2 code", "Divisor" )
	bbd.sourceName <- "R_BB"
	
	# settings for all our tables in the DB
	bbd.dbTableSettings <- list(tbl_rate_data=append(dbSettings, list(dateEntered=FALSE,tblColumns=c("Date","name","Rate"))),tbl_sa2_rate_data=append(dbSettings, list(tblColumns=c("Date","name","Rate"),dateEntered=FALSE)),tbl_sa2_macro_data=append(macroDbSettings,list(tblColumns=c("Date","name","Rate"),dateEntered=TRUE)),tbl_sa2_index_members=append(dbSettings,list(tblColumns=c("Date", "name", "Member", "Weight"),dateEntered=FALSE)))
	
	# now specify pre-canned requests that we would expect to do on a regular basis and put them all in bbd.requests, a list(), and pass that
	# NOTE: set bbd.referenceDataDir to NULL if the reference data file, i.e., the request file is in src/inputFiles
	
	# request to get market data (prices, earnings, ...) for market indices and eventually single name equities
	requestIndexMktData <- list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-Indexes.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=FALSE, bbd.intermediateOutputFile="rawIndexMktData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=100, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	requestOtherIndexMktData <- list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-OtherEquityIndexes.csv", bbd.destinationTable="tbl_rate_data", bbd.loadDB=TRUE, bbd.intermediateOutputFile="rawOtherIndexMktData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=100, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	## requestEquityMktData <- list( bbd.requestType="HistoricalDataRequest", bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-Equities.csv", bbd.destinationTable="tbl_sa2_rate_data", bbd.loadDB=FALSE, bbd.intermediateOutputFile="rawIndexMktData.csv", bbd.overrideExistingData=FALSE,  bbd.numberOfDates=100, bbd.dataFrequency="DAILY", bbd.fillNonTrading="true")
	
	# request to get index members
	requestIndexMembers <- list( bbd.requestType="ReferenceDataRequest", bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-EquityIndexesMembers.csv",bbd.destinationTable="tbl_sa2_index_members", bbd.loadDB=TRUE, bbd.intermediateOutputFile="rawIndexMembers.csv", bbd.overrideExistingData=FALSE, bbd.referenceDataStartDate="2002-02-01", bbd.dataFrequency="MONTHLY")
	
	# request CPI and GDP
	requestGDP <- list( bbd.requestType="HistoricalDataRequest",bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-GDP.csv",bbd.destinationTable="tbl_sa2_macro_data",bbd.loadDB=TRUE,bbd.dbLabel="YQ",bbd.intermediateOutputFile="rawGDPData.csv",bbd.overrideExistingData=FALSE, bbd.numberOfDates=100, bbd.dataFrequency="QUARTERLY", bbd.fillNonTrading="false")
	requestCPI <- list( bbd.requestType="HistoricalDataRequest",bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-CPI.csv",bbd.destinationTable="tbl_sa2_macro_data",bbd.loadDB=TRUE,bbd.dbLabel="YM",bbd.intermediateOutputFile="rawCPIData.csv",bbd.overrideExistingData=FALSE, bbd.numberOfDates=100, bbd.dataFrequency="MONTHLY", bbd.fillNonTrading="false")
	requestCPIRate <- list( bbd.requestType="HistoricalDataRequest",bbd.referenceDataDir=NULL,bbd.referenceDataFile="request-CPIRate.csv",bbd.destinationTable="tbl_sa2_macro_data",bbd.loadDB=TRUE,bbd.dbLabel="YM",bbd.intermediateOutputFile="rawCPIRateData.csv",bbd.overrideExistingData=FALSE, bbd.numberOfDates=30000, bbd.dataFrequency="MONTHLY", bbd.fillNonTrading="false")

	# NOTE: only requests listed in bbd.requests will be run
	bbd.requests <- list( requestCPIRate )
		
	return(list(bbd.sourceName=bbd.sourceName,bbd.requests=bbd.requests,bbd.dbTableSettings=bbd.dbTableSettings,bbd.requestHeader=bbd.requestHeader,bbd.ccMappings=bbd.ccMappings,bbd.fieldMappings=bbd.fieldMappings,bbd.equityColNumber=bbd.equityColNumber,bbd.dayOfWeek=bbd.dayOfWeek,bbd.outputDir=bbd.outputDir,bbd.javaClass = bbd.javaClass, bbd.pathToJava=bbd.pathToJava, bbd.pathToBlpJar=bbd.pathToBlpJar, bbd.classpathSeparator=bbd.classpathSeparator ))
}

global.leverageItExecSettings <- function(){
	
	coreSettings <- global.core()
	
	lit.inputDir <- coreSettings[["simOutput"]]
	lit.inputDirER <- coreSettings[["dataOutput"]]
	lit.outputDir <- coreSettings[["simOutput"]]
	lit.useDB <- coreSettings[["useDB"]]
	
	dbSettings <- coreSettings[["dbSettings"]]
	
	lit.portfolioSimFile <- "production.mixed.taro.jiro.policyDownAndSuspend.sim.csv"
	lit.leverage <- 1.5
	lit.busDaysPerYear <- 261
	lit.financingSpread <- global.spreadOnShortCash()
	
	lit.baseCashName <- "us.gg.3m"
	lit.dtrSuffix <- ".dtr"
	lit.dtrCrFile <- "dtrCr.RData"
	lit.dateFormat <- "%Y-%m-%d"
	
	return(list(lit.dateFormat=lit.dateFormat,lit.dtrSuffix=lit.dtrSuffix,lit.baseCashName=lit.baseCashName,lit.financingSpread=lit.financingSpread,lit.busDaysPerYear=lit.busDaysPerYear,lit.leverage=lit.leverage,lit.portfolioSimFile=lit.portfolioSimFile,dbSettings=dbSettings,lit.useDB=lit.useDB,lit.outputDir=lit.outputDir,lit.inputDir=lit.inputDir,lit.inputDirER=lit.inputDirER,lit.dtrCrFile=lit.dtrCrFile))
}

global.uploadToRateHeaderTableExecSettings <- function(){
	
	coreSettings <- global.core()
	
	urht.inputDir <- coreSettings[["input"]]
	# the following file contains the new sa2 data items we want to create entries for in the rate header table. there should be one data item per line and the file is assumed to have a header
	urht.dataNamesFile <- "newItemsToLoad.csv"
	
	dbSettings <- coreSettings[["dbSettings"]]
	
	return( list(urht.inputDir=urht.inputDir, urht.dataNamesFile=urht.dataNamesFile, dbSettings=dbSettings ))
}

global.dailyBatchExecSettings <- function(){
	
	coreSettings <- global.core()
	
	dabaex.outputFilesDir <- coreSettings[["dataOutput"]]
	dbSettings <- coreSettings[["dbSettings"]]
	
	dabaex.override <- F
	dabaex.sourceName <- "R_DBE"

	return( list(dabaex.outputFilesDir=dabaex.outputFilesDir, dabaex.override = dabaex.override, dabaex.sourceName=dabaex.sourceName, dbSettings=dbSettings))
}

global.wbDataExecSettings <- function(){
	
	coreSettings <- global.core()
	
	wbd.inputFile <- "wb.2013-01-14.csv"
	wbd.inputDir <- coreSettings[["input"]]
	wbd.outputDir <- coreSettings[["dataOutput"]]
	
	wbd.indicatorColumnName <- "Indicator Code"
	wbd.yearColumnName <- "Year"
	
	wbd.countriesMap <- global.sa2WBCountryCode()
	# why a data.frame() in this and not list() as we do usually? because names of wb series could have white spaces in them and so can't be used as names of list(). Note also that logic in code allows the beginning of the WB name to be specified only -- no need to specify the full name. handy when the name has parens in it
	wbd.seriesNameMap <- data.frame(wbName=c("NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.KD.ZG","NY.GNP.MKTP.KD.ZG", "NY.GNP.PCAP.KD.ZG","SL.UEM.TOTL.ZS", "NY.GDP.DEFL.KD.ZG","NE.CON.PRVT.PC.KD.ZG"), sa2Name=c(".gdpreal",".gdprate",".gdpcaprate",".gnirate",".gnicaprate",".unemppercent",".gdpdeflatorrate",".hhconscaprate"))

	return( list(wbd.yearColumnName=wbd.yearColumnName,wbd.indicatorColumnName=wbd.indicatorColumnName,wbd.inputFile=wbd.inputFile,wbd.inputDir=wbd.inputDir,wbd.outputDir=wbd.outputDir,wbd.countriesMap=wbd.countriesMap,wbd.seriesNameMap=wbd.seriesNameMap) )
}

global.createLivePXExecSettings <- function(){
	
	# build settings list in here
	settings <- list()
	
	coreSettings <- global.core()
	clpx.outputDir <- coreSettings[["dataOutput"]]
	
	predictSettings <- global.predictExecSettings()
	clpx.seedSource <- predictSettings[["eretm.seedSource"]]
	clpx.growthBenchmark <- predictSettings[["eretm.growthBenchmark"]]
	clpx.listOfSuffixes <- predictSettings[["prd.listOfSuffixes"]]
	clpx.assetIdentifiers <- predictSettings[["prd.assetIdentifiers"]]
	
	frontierBuilderSettings <- global.frontierBuilderExecSettings()
	clpx.constraints <- frontierBuilderSettings[["fbex.constraints"]]
	clpx.cashNameSuffix <- frontierBuilderSettings[["frnt.cashNameSuffix"]]
	
	return( list(clpx.outputDir=clpx.outputDir, clpx.seedSource=clpx.seedSource, clpx.growthBenchmark=clpx.growthBenchmark, clpx.listOfSuffixes=clpx.listOfSuffixes, clpx.assetIdentifiers=clpx.assetIdentifiers, clpx.constraints=clpx.constraints, clpx.cashNameSuffix=clpx.cashNameSuffix))
}

global.positionsTemplateExecSettings <- function(){
	
	coreSettings <- global.core()
	
	ptex.outputDir <- coreSettings[["trades"]]
	ptex.inputDir <- coreSettings[["input"]]
	
	marketDataDBSettings <- coreSettings[["dbSettings"]]
	ptex.positionsTableSettings <- list( uid=marketDataDBSettings[["ld.uid"]], dsnName = marketDataDBSettings[["ld.dsnName"]], positionsTableName="tbl_sa2_positions")
	ptex.rateHeaderTableSettings <- list( uid=marketDataDBSettings[["ld.uid"]], dsnName = marketDataDBSettings[["ld.dsnName"]], headerTableName="tbl_rate_header")
	
	ptex.bondIdentifier <- "\\.gg\\."
	ptex.equityIsEquityIndexFuture <- TRUE
	
	ptex.universeToSecuritiesMappingFile <- "mapping-Universe2LiveData.csv"
	
	return(list(ptex.inputDir=ptex.inputDir,ptex.universeToSecuritiesMappingFile=ptex.universeToSecuritiesMappingFile,ptex.equityIsEquityIndexFuture=ptex.equityIsEquityIndexFuture,ptex.bondIdentifier=ptex.bondIdentifier, ptex.rateHeaderTableSettings=ptex.rateHeaderTableSettings,ptex.positionsTableSettings=ptex.positionsTableSettings, ptex.outputDir=ptex.outputDir))
}

global.positionsLoadExecSettings <- function(){
	
	coreSettings <- global.core()
	
	plex.inputDir <- coreSettings[["trades"]]
	marketDataDBSettings <- coreSettings[["dbSettings"]]
	
	plex.positionsTableSettings <- list( uid=marketDataDBSettings[["ld.uid"]], dsnName = marketDataDBSettings[["ld.dsnName"]], positionsTableName="tbl_sa2_positions")
	plex.primaryKeys <- c("Trader", "Transaction_ID")
	
	return( list(plex.primaryKeys=plex.primaryKeys, plex.inputDir=plex.inputDir, plex.positionsTableSettings=plex.positionsTableSettings))
}