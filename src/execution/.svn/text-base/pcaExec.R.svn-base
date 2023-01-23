# driver for analysis/pca.R
pcaExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	
	source(paste(pathToMe,"/../settings/GlobalVariables.R",sep=""))
	source(paste(pathToMe,"/../analysis/pca.R",sep=""))
	
	settings<-global.pcaExecSettings()
	input<-pca.input(settings)
	output<-pca.calculate(input,settings)
	pca.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
pcaExec.main()