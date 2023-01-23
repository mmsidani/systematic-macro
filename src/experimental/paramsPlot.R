plt.numSubPlots<-8
	
plt.models<-function(output,numberOfColumnsToPlot){
	results<-output

	modelColumns<-grep("^new\\.",names(results)) # we assume the first model column is always "new.*"
	
	cols<-rainbow(numberOfColumnsToPlot)
	counter<-0
	for(i in modelColumns){
		if(length(grep("rmse",names(results)[i]))==0){
			nonNAs<-is.finite(results[,i])
			for(j in 1:(numberOfColumnsToPlot-1)){
				temp<-is.finite(results[,i+j])
				if(sum(!temp)!=length(temp)){
					nonNAs<-nonNAs & temp
				}
			} 
			if(counter%% plt.numSubPlots==0){
				dev.new()
				layout(matrix(1:plt.numSubPlots,nrow=2))
			}
			
			dates<-as.Date(results$Date[nonNAs])
			plot(dates,results[nonNAs,i],ylab="returns",col=cols[1],main=sub("^new.","",names(results)[i]),ylim=c(min(results[nonNAs,i:(i+numberOfColumnsToPlot-1)],na.rm=TRUE),max(results[nonNAs,i:(i+numberOfColumnsToPlot-1)],na.rm=TRUE)),type="l")
			counter<-counter+1
			for(j in 1:(numberOfColumnsToPlot-1)){
				lines(dates,results[nonNAs,i+j],col=cols[1+j])
			}
			legend("topleft",names(results)[i:(i+numberOfColumnsToPlot-1)],col=cols,lty=rep(1,length(cols)),bty="n")
		}
	}
}

