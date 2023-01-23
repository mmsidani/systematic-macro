transactions.getTransactPenalty<-function(turnovers,tc,cashNameSuffix){
# input: turnovers is a data.frame of the changes to be transacted in weights of the portfolio; tc is a data.frame() of transaction penalty as percentage of quantity transacted
# output: the transaction cost as a percentage of the overall portfolio

	# remove cash from the assets bearing transaction fees
	nonCashAssets<-turnovers[,!grepl(cashNameSuffix,colnames(turnovers))]

	return(as.numeric(-sum(abs(nonCashAssets* tc[names(nonCashAssets)]))))
}

transactions.fillMissingTC<-function(assets,tc,defaultTransactionCost){
# if we have not set the transaction cost for an asset we take care of it here
# assets is a vector of strings; tc is a list() or data.frame() indexed by assets that gives the transaction cost for those assets
# returns tc converted to data.frame() and augmented by the missing assets with a transaction cost of psex.defaultTransactionCost

	tcNames<-names(tc)
	for(a in assets){
		if(!is.element(a, tcNames)){
			print(paste("info from transactions.fillMissingTC():", a,"is missing from transaction cost and will be assigned the default value",defaultTransactionCost))
			tc[[a]]<-defaultTransactionCost
		}
	}
	
	return(data.frame(tc))
}

