frontierAnalysisExec.main<-function(){
	pathToMe<-dirname(sys.frame(1)$ofile)
	pathToRSourceCode<-paste(pathToMe,"/../",sep="")

	source(paste(pathToRSourceCode,"settings/GlobalVariables.R",sep=""))
	source(paste(pathToRSourceCode,"analysis/frontierAnalysis.R",sep=""))
	
	settings<-global.frontierAnalysisExecSettings()
	input<-frontierAnalysis.input(settings)
	output<-frontierAnalysis.calculate(input,settings)
	frontierAnalysis.output(output,settings)
	
	print(paste(sys.frame(1)$ofile,"done."))
}

# execute
frontierAnalysisExec.main()