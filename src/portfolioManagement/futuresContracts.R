futuresContracts.optimizeReturn <- function(assetNames, expectedReturns, assetAllocation, budget, futuresNotionals){
# assetNames is a vector of strings of assets with futures contracts we're allocating to; expectedReturns could be a column-named matrix() or a data.frame() giving the expected returns of the assets; allocationTolerance is a the percentage by which we're OK deviating from the target allocation for an asset; budget is our investable AUM; budgetTolerance is the percentage by which we're OK deviating from a full investment of our budget; futuresNotionals is column-named matrix() or a data.frame() giving the notionals of the futures contracts

	numAssets <- length(assetNames)
	notionals <- as.numeric(futuresNotionals[assetNames])
	allocations <- assetAllocation[assetNames]
	G <- matrix(0, nrow= 2*numAssets+1, ncol= numAssets)
	H <- rep(0, 2*numAssets+1)
	G[1, ] <- -notionals
	H[1] <- -budget
	for(i in 1:numAssets){
		G[(2*i):(2*i+1), i] <- c(notionals[i], -notionals[i])
		allocation <- budget * as.numeric(allocations[assetNames[i]])
		if(allocation >= notionals[i]){
			H[(2*i):(2*i+1)] <- c( floor(allocation/notionals[i]) * notionals[i], -allocation)
		}else{
			H[(2*i):(2*i+1)] <- c( 0, -allocation)
		}
	}

	# linp() minimizes so we take the negative next
	objective <- -as.numeric(as.matrix(expectedReturns[1,assetNames]))
	linpResults <- linp(E=NULL, F=NULL, G=G, H=H, objective, ispos = TRUE, int.vec= 1:numAssets)
	if(!is.null(linpResults$isError) && linpResults$isError){
		print("ERROR in futuresContracts.optimizeReturn(): could not solve integer programming problem")
	}
	
	ret <-linpResults[["X"]]
	names(ret) <- assetNames
	
	return(list(contracts=ret,cost=sum(ret*notionals)))
}

futuresContracts.optimizeBudget <- function(assetNames, assetAllocation, budget, futuresNotionals){
# assetNames is a vector of strings of assets with futures contracts we're allocating to; expectedReturns could be a column-named matrix() or a data.frame() giving the expected returns of the assets; allocationTolerance is a the percentage by which we're OK deviating from the target allocation for an asset; budget is our investable AUM; budgetTolerance is the percentage by which we're OK deviating from a full investment of our budget; futuresNotionals is column-named matrix() or a data.frame() giving the notionals of the futures contracts

	numAssets <- length(assetNames)
	notionals <- as.numeric(futuresNotionals[assetNames])
	allocations <- assetAllocation[assetNames]
	G <- matrix(0, nrow= 2*numAssets, ncol= numAssets)
	H <- rep(0, 2*numAssets)
	for(i in 1:numAssets){
		G[(2*i-1):(2*i), i] <- c(notionals[i], -notionals[i])
		allocation <- budget * as.numeric(allocations[assetNames[i]])
		if(allocation >= notionals[i]){
			H[(2*i-1):(2*i)] <- c( floor(allocation/notionals[i]) * notionals[i], -allocation)
		}else{
			H[(2*i-1):(2*i)] <- c( 0, -allocation)
		}
	}

	# linp() minimizes so we take the negative next
	objective <- -notionals
	linpResults <- linp(E=NULL, F=NULL, G=G, H=H, objective, ispos = TRUE, int.vec= 1:numAssets)
	if(!is.null(linpResults$isError) && linpResults$isError){
		print("ERROR in futuresContracts.optimizeReturn(): could not solve integer programming problem")
	}
	
	ret <-linpResults[["X"]]
	names(ret) <- assetNames
	
	return(list(contracts=ret,cost=sum(ret*notionals)))
}
