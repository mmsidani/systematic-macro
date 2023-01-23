algos.averageUniqueRepeats<-function(indices, v, isDataChronological){
# indices is a vector that tells us which components in v repeat; for example, indices[1:10]==1 means v[1:10] are a repeat of v[1]; v is a vector of values we want to average; isDataChronological is a logical that tells us if data is in chronological order (TRUE) or the reverse
# returns vector of the same length as v with every component being the average of the unique values that came before it (which means below it, if isDataChronological is TRUE or above it if FALSE)

	indexBlocks<-rle(indices)
	uniqueValuesIndices<-c(1,1+cumsum(indexBlocks$lengths))[1:length(indexBlocks$lengths)]
	indexBlocks$values<-algos.cumSampleMean(v[uniqueValuesIndices],isDataChronological)
	
	return(inverse.rle(indexBlocks))	
}

algos.mesh<-function(v1, v2){
# for every component in v1 find the largest component in v2 that is <= that component in v1. v1 and v2 are vectors.
# returns vector of indices of the desired components in v2

	reversed<-FALSE
	if(v2[1] < v2[2]){
		# we want vectors in reverse order
		v2<-rev(v2)
		reversed<-TRUE
	}
	
	if((reversed && v1[1] > v1[2]) || (!reversed && v1[1] < v1[2])){
		stop("error in algos.mesh(): v1 and v2 must both be increasing or both decreasing")
	}else if(reversed){
		v1<-rev(v1)
	}
	
	retNAs<-NULL
	isTooSmall<-v1[length(v1)] < v2[length(v2)]
	if(isTooSmall){
		retNAs<-rep(NA,sum(v1 < v2[length(v2)]))
		v1<-v1[v1 >= v2[length(v2)]]
	}

	numV2<-length(v2)
	# first find get the indices of those components in v2 that are also in v1
	includedInds<-(1:numV2)[is.element(v2,v1)]
	# now for those components in v1 that are not in v2, we have to find the largest component in v2 that comes before each
	stillToDo<-v1[!is.element(v1,v2[includedInds])]
	newInds<-NULL
	if(length(stillToDo)!=0){
		# looping and matching (the straightforward approach) resulted in code 366 times slower in one test case. so we change everything to vector operations. first c() stillToDo with v2 and sort() for a cheap way to compare all components in stillToDo with all components in v2
		combo<-sort(c(stillToDo,v2),decreasing=TRUE)
		# find the indices of the components of stillToDo in the resulting vector, combo, and then the indices (recall we're in decreasing order) right after those are the indices of the largest components in combo that come right before. Do note the "+ 1" and because of earlier the truncation newInds <= length(combo)
		newInds<-(1:length(combo))[is.element(combo,stillToDo)] + 1		
		# but... newInds are not necessarily indices in combo of components in v2. they could very well be indices of components from stillToDo. whichIndsAreIn are indices in v2 of components whose indices in combo are part of newInds. i.e., they are the indices of components in v2 that are the largest to fall before components in stillToDo
		whichIndsAreIn<-(1:numV2)[is.element(v2,combo[newInds])]
		# the rest of the work is about mapping the components in stillToDo to the right components in v2 that we've just calculated (of indices whichIndsAreIn). for components in combo[newInds] that are not in v2, replace them with the next component in combo[newInds] that is in v2
		whichAreIn<-is.element(combo[newInds],v2)
		# logic here was also used in utils.fillGapsSeries(). find the runs of those that are and are not in v2
		blocks<-rle(whichAreIn)
		blocksBoundaries<-c(1,1+cumsum(blocks$lengths))[1:length(blocks[["lengths"]])]
		falsesInds<-blocksBoundaries[blocks$values==FALSE]
		trueInds<-blocksBoundaries[blocks$values==TRUE]
		# counter into whichIndsAreIn
		counter<-1
		if(falsesInds[1]>trueInds[1]){
			# the very first run is a TRUE run. 
			counter<-2
		}
		# counter into blocks[["lengths"]]. we need those only to know how long a run of TRUE is
		blockNumber<-counter+1
		numWhichIndsAreIn<-length(whichIndsAreIn)
		for(i in 1:length(falsesInds)){
			# because of the earlier truncation there's always a TRUE block after the FALSE
			nextTrueInd<-trueInds[falsesInds[i]<trueInds][1]
			if(combo[newInds[nextTrueInd]]!=v2[whichIndsAreIn[counter]] || counter > numWhichIndsAreIn){
				stop(paste("ERROR in algos.mesh(): something off. value at TRUE doesn't match expected value"))
			}
			newInds[falsesInds[i]:nextTrueInd]<-whichIndsAreIn[counter]
			counter<-counter+1
			bLength<-blocks[["lengths"]][blockNumber]
			if(bLength > 1){
				for(b in 1:(bLength-1)){
					if(counter > numWhichIndsAreIn){
						stop(paste("ERROR in algos.mesh(): exceeded number of available values"))
					}
					newInds[nextTrueInd+b]<-whichIndsAreIn[counter]
					counter<-counter+1
				}
			}
				
			blockNumber<-blockNumber+2
		}
	}
	
	ret<-c(sort(c(includedInds,newInds)),retNAs)
	if(reversed){
		# reverse again
		ret<-rev(ret)
	}

	return(ret)
}

algos.cumSampleMean<-function(v,isDataChronological){
# calculate a cumulative sample mean.
# v is a vector; isDataChronological is a logical that tells if the data is in chronological order (TRUE) or not (FALSE)
# returns vector ret such that ret[i] is the mean of v[1:i], if isDataChronological is TRUE, or ret[i] is the mean of v[i:length(v)]

	if(isDataChronological){
		return(cumsum(v)/(1:length(v)))
	}else{
		temp<-rev(v)
		temp<-cumsum(temp)/(1:length(temp))
		return(rev(temp))
	}
}

algos.cumSampleSD<-function(v,isDataChronological){
# calculate a cumulative standard deviation
# v is a vector; isDataChronological is a logical that tells if data in v is in chronological order

	if(!isDataChronological){
		v<-rev(v)
	}

	ret<-rep(NA,length(v))
	for(i in 1:length(v)){
		ret[i]<-sd(v[1:i],na.rm=TRUE)
	}
	
	if(!isDataChronological){
		ret<-rev(ret)
	}
	
	return(ret)
}
 
algos.RSquared <- function( yObs, yFitted){
	
	yBar <- mean(yObs)
	totalVar <- sum( (yObs - yBar )^2 )
	totalErr <- sum( (yObs - yFitted)^2 )
	
	return( 1 - totalErr / totalVar )
}