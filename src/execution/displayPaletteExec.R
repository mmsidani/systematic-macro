displayPaletteExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	
	# get our current palette as set in GlobalVariables.R
	ourPalette<-global.palette()
	print(paste("we currently have",length(ourPalette),"colors specified in global.palette() in GlobalVariables.R"))
	# set the palette to ourPalette
	palette(ourPalette)
	# plot lines to see what these colors look like
	matplot(outer(1:100,1:(length(ourPalette))),lty=1,lwd=2,type='l',main="our current palette as set in global.palette() in GlobalVariables.R",ylab="",col=1:length(ourPalette),sub="(first color in our palette is at bottom, last at top)")
	legend("topleft",legend=ourPalette,col=1:length(ourPalette),lty=1,cex=0.6)
	# restore default palette
	palette("default")
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
displayPaletteExec.main()