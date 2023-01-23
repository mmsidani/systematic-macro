reinvesting.riskyAssets<-function(port,excessCash,originalCashAllocation,baseCash,nonBaseAggregationList,reinvestmentHurdle){
# reinvest cash in excess of target allocation to cash in non-cash assets
# port is the portfolio to be rebalanced; excessCash is the portfolio weight over target cash allocation; originalCashAllocation is the original (i.e., what we intended originally based on our original target portfolio) net weight allocated to cash; baseCash is the name of the base cash asset; nonBaseAggregationList is a list() of ncol(port) items, typically, the names of cash assets in port, and such that nonBaseAggregationList[[i]] is the name of the domestic cash asset of names(port)[i]. Example: if names(port)==c("us.equity","us.gg.3m","uk.equity","uk.gg.3m","jp.equity","jp.gg.3m") and "us" is base, we will have nonBaseAggregationList==list(by=c(NA,NA,"uk.gg.3m",NA,"jp.gg.3m",NA), so cash names and all base country assets are mapped to NA and non-cash, non-base country assets mapped to their domestic cash names. NOTE: the way the function is written whereever we say a "name" is expected can be changed to "index in names(port)", i.e., we can pass indices in here instead of names
# returns NULL if no reinvestment of cash is necessary, or the portfolio with the appropriate weights after cash has been reinvested in risky assets

	if(excessCash <reinvestmentHurdle){
		# we didn't stray too far
		return(NULL)
	}

	# allocate the excess cash to the risky assets in the same proportions as the current portfolio allocations to risky assets. NOTE: yes, indeed, we're multiplying everything next, not just the risky assets, but it doesn't matter. it saves us from having to select particular columns, and, the erroneous numbers, i.e., the resulting cash assets weights are never used anyway
	isPositiveWeight <- port[1,] > 0
	totalLongNonBaseCash <- sum(port[1, isPositiveWeight]) - max(port[1,baseCash], 0)
	if(totalLongNonBaseCash == 0 ){
		return(NULL)
	}
	
	# Note: no, we do not want to scale baseCash weight but it's easier to do the following statement and then correct port[1, baseCash]
	port[1, ] <- port[1, ]*(1 + excessCash/totalLongNonBaseCash)
	
	# now some acrobatics to map the results of aggregate() to the appropriate assets in port. 2 things to keep in mind: we can't have NA subscripts and aggregate() sorts its results by a sort() of nonBaseAggregationList
	uniqueItems<-unique(nonBaseAggregationList[[1]])
	nonBaseAssets<-sort(uniqueItems[!is.na(uniqueItems)])
	if(length(nonBaseAssets) != 0){
		port[1,nonBaseAssets] <- -aggregate(as.numeric(port),by=nonBaseAggregationList,FUN="sum")[[2]]
	}
	# finally reset baseCash weight. assumes we're long base cash
	port[1,baseCash]<-originalCashAllocation - sum(port[1, !isPositiveWeight])
	
	return(port)
}

reinvesting.reinvest<-function(port,excessCash,originalCashAllocation,baseCash,nonBaseAggregationList,reinvestmentDecision,reinvestmentHurdle){
# this is the main entry point. see comments above reinvesting.riskyAssets() for the arguments

	if(reinvestmentDecision=="riskyAssets"){
		return(reinvesting.riskyAssets(port,excessCash,originalCashAllocation,baseCash,nonBaseAggregationList,reinvestmentHurdle))
	}else if(reinvestmentDecision=="noReinvestment"){
		# we never want to reinvest
		return(NULL)
	}else{
		stop(paste("error in reinvesting.reinvest(): unknown reinvestment policy reinvestmentDecision =", reinvestmentDecision))
	}
}

