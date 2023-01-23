#### GRAPH PLOTTING LIBRARY TO BE CALLED EXTERNALLY ####
#### Datasets must be in CSV format                 ####
#### USES: Math.r									####
#### Created by: Andrew Granger  6/4/11             ####
#### Last modified: 11/04/11                        ####

#Plot time series:
draw.TimeSeries <- function(dataset,DateTimeCol,Yvals,Title,Xlabel,Ylabel,colour,DateTimeFormat) 
					 #(list/string,int,vector,string,string,string,vector,string)
{

if (typeof(dataset) == "list") #This allows passing of dataset as a dataframe or a file address string
{
dat <- dataset
}
else
{
dat <- read.csv(dataset,head=TRUE) 
}

if (missing(DateTimeCol)){DateTimeCol<-1}
if (missing(Yvals)){Yvals<-2:ncol(dat)}
if (missing(Title)){Title<-""}
if (missing(Xlabel)){Xlabel<-""}
if (missing(Ylabel)){Ylabel<-""}
if (missing(colour)){colour<-2:ncol(dat)}
if (missing(DateTimeFormat)){DateTimeFormat<-"%Y-%m-%d"}

dat <- na.omit(dat) #Remove missing values to avoid plotting issues
DateTime <- as.Date(dat[[DateTimeCol]],DateTimeFormat) #Convert data to date/time format
Ydata <- c()
for (i in Yvals){Ydata <- c(Ydata,dat[[i]])} #Get all Ydata together for calculating max() 
plot(DateTime,dat[[Yvals[1]]],type='l',main=Title, ylab=Ylabel, xlab=Xlabel,col=colour[1],ylim=c(min(Ydata),max(Ydata))) #Plot first series

if (length(Yvals > 1))
{
for (i in 2:length(Yvals))
{
lines(DateTime,dat[[Yvals[i]]], col=colour[i]) #Plot each additional series
}
legend("topleft",colnames(dat)[2:ncol(dat)],col=colour[1:length(colour)],lty=rep(1,length(colour)),bty="n")

grid(NA,NULL,lwd=1,lty=1)
}
}

#Plot and write time series to Jpeg:
draw.TimeSeriesToJpeg <- function(fileloc,dataset,DateTimeCol,Yvals,Title,Xlabel,Ylabel,colour,DateTimeFormat)
{
jpeg(filename=fileloc, width = 480, height = 480, units = "px", pointsize = 12, quality = 75, bg = "white",) #invokes graphic driver
draw.TimeSeries(dataset,DateTimeCol,Yvals,Title,Xlabel,Ylabel,colour,DateTimeFormat) #writes to graphic file#
graphics.off() #disables driver
}

#Plot and write time series to PDF: 
draw.TimeSeriesToPDF <- function(fileloc,dataset,DateTimeCol,Yvals,Title,Xlabel,Ylabel,colour,DateTimeFormat)
{
pdf(file=fileloc)
draw.TimeSeries(dataset,DateTimeCol,Yvals,Title,Xlabel,Ylabel,colour,DateTimeFormat)
graphics.off()
}


#Plot and write XY to Jpeg:
draw.RainbowXYToJpeg <- function(fileloc,dataset,Xvals,Yvals,Title,Xlabel,Ylabel)
{
jpeg(filename=fileloc, width = 480, height = 480, units = "px", pointsize = 12, quality = 75, bg = "white",) #invokes graphic driver
draw.RainbowXY(dataset,Xvals,Yvals,Title,Xlabel,Ylabel)#writes to graphic file#
graphics.off() #disables driver
}

#Plot and write XY to PDF: 
draw.RainbowXYToPDF <- function(fileloc,dataset,Xvals,Yvals,Title,Xlabel,Ylabel)
{
pdf(file=fileloc)
draw.RainbowXY(dataset,Xvals,Yvals,Title,Xlabel,Ylabel)
graphics.off()
}




#Plot a scatter graph and regression line with index as colour:
draw.RainbowXY <- function(dataset,Xvals,Yvals,Title,Xlabel,Ylabel)
				#(list/string,int , int , string,string,string)
{

if (typeof(dataset) == "list") #This allows passing of dataset as a dataframe or a file address string
{
dat <- dataset
}
else
{
dat <- read.csv(dataset)
}
dat <- na.omit(dat)

if (missing(Xvals)){Xvals<-2}
if (missing(Yvals)){Yvals<-3:ncol(dat)}
if (missing(Title)){Title<-""}
if (missing(Xlabel)){Xlabel<-""}
if (missing(Ylabel)){Ylabel<-""}


plot(dat[[Xvals]],dat[[Yvals]],type="n",xlab=Xlabel,ylab=Ylabel,main=Title)
cols <- rev(rainbow(length(dat[[Xvals]]),end=0.35))
points(dat[[Xvals]],dat[[Yvals]],col=cols)
fit <- lm(dat[[Yvals]]~dat[[Xvals]])
fita <- fit$coefficients[1]
fitb <- fit$coefficients[2]
abline(fita,fitb,lwd=2)
}

#Find perpendiclar deviation from regression line:
draw.PerpDeviation <- function(dataset,Xvals,Yvals)
{

if (typeof(dataset) == "list") #This allows passing of dataset as a dataframe or a file address string
{
dat <- dataset
}
else
{
dat <- read.csv(dataset)
}

dat <- na.omit(dat)
fit <- lm(dat[[Yvals]]~dat[[Xvals]])
fita <- fit$coefficients[1]
fitb <- fit$coefficients[2]
dev <- S(dat[[Xvals]],dat[[Yvals]],fita,fitb)
return(dev)
}

#Plot perpendicular deviation from regression line with index as colour:
draw.RainbowDeviation <- function(dataset,Xvals,Yvals,Title,Xlabel,Ylabel,DateTimeFormat)
						#(list/string, int, int ,string,string,string,string)
{

if (typeof(dataset) == "list") #This allows passing of dataset as a dataframe or a file address string
{
dat <- dataset
}
else
{
dat <- read.csv(dataset)
} 
if (missing(Xvals)){Xvals<-2}
if (missing(Yvals)){Yvals<-3}
if (missing(Title)){Title<-""}
if (missing(Xlabel)){Xlabel<-""}
if (missing(Ylabel)){Ylabel<-""}
if (missing(DateTimeFormat)){DateTimeFormat<-"%Y-%m-%d"}

dat <- na.omit(dat)
dates <- as.Date(dat[["Date"]],format=DateTimeFormat)
devs <- draw.PerpDeviation(dataset,Xvals,Yvals)
cols <- rev(rainbow(length(dat[[Xvals]]),end=0.35))
plot(dates,devs,col=rev(cols),xlab=Xlabel,ylab=Ylabel,pch="*")
}

draw.areaPlot <- function(graphicsDevice,fileName,dataset,Title,Xlabel,Ylabel,DateTimeFormat, ourPalette,pngSettings){ #Must have X-values as leftmost column (e.g. date)
				#(data.frame()/string,string,string,string,string)
	if(graphicsDevice=="png"){
		png(filename=fileName, width = pngSettings[["width"]], height = pngSettings[["height"]],res= pngSettings[["res"]],pointsize= pngSettings[["pointsize"]], bg = pngSettings[["bg"]])
	}else if(graphicsDevice=="pdf"){
		pdf(file=fileName)
	}
	
	if (typeof(dataset) == "list"){ #This allows passing of dataset as a dataframe or a file address string
		dat <- dataset
	}else{
		dat <- read.csv(dataset)
	}
	dat <- na.omit(dat)
	if (!missing(DateTimeFormat)) {
		dat[[1]] <- as.Date(dat[[1]],format=DateTimeFormat)
	}
	
	if (missing(Title)){Title<-""}
	if (missing(Xlabel)){Xlabel<-""}
	if (missing(Ylabel)){Ylabel<-""}
	
	A<-NULL
	B<-NULL
	for(i in 2:ncol(dat)){
		dat[[i]][abs(dat[[i]])<=1.0e-6]<-0
		
		newColA<-rep(0,nrow(dat))
		newColA[dat[[i]]>0]<-dat[[i]][dat[[i]]>0]
		A<-append(A,list(newColA))
		
		newColB<-rep(0,nrow(dat))
		newColB[dat[[i]]<0]<-dat[[i]][dat[[i]]<0]
		B<-append(B,list(newColB))
	}
	
	if(ncol(dat)>2){
		for (i in 2:length(A)){
			A[[i]]<-A[[i-1]]+A[[i]]
		}
		for (i in 2:length(B)){
			B[[i]]<-B[[i-1]]+B[[i]]
		}
	}
	
	orgMar<-par()$mar
	par(xpd=TRUE,mar=orgMar+c(0,0,0,8))
	par(cex=0.7)
	
	if(length(ourPalette)<length(A)){
		print(paste("WARNING in draw.areaPlot(): the palette you passed has fewer colors than is needed. you passed",length(ourPalette),"colors to display",length(A),"items. Therefore some colors will have to be reused."))
	}
	
	minB<-min(B[[1]])
	maxA<-max(A[[1]])
	if(length(A)>1){
		for(i in 2:length(A)){
			minB<-min(minB,min(B[[i]]))
			maxA<-max(maxA,max(A[[i]]))
		}
	}
	
	palette(ourPalette)
	# init plot
	plot(dat[[1]],A[[1]],type="n",main=Title,xlab=Xlabel,ylab=Ylabel,ylim=c(minB, maxA))
	
	polygon(c(dat[[1]][1],dat[[1]],dat[[1]][length(dat[[1]])]),c(0,A[[1]],0),col=1,density=100,angle=0,border=NA)
	polygon(c(dat[[1]][1],dat[[1]],dat[[1]][length(dat[[1]])]),c(0,B[[1]],0),col=1,density=100,angle=0,border=NA)
	#^Plots first polygon, appending zero values at the first and last dates. 
	#This is neccessary because the polygon() function automatically joins first and last points. We want this line to go along the x-axis.
	
	if(ncol(dat)>2){
		for (i in 2:length(A)){
			# multiply by 5 to skip some colors. otherwise the colors vary slowly and some end up being hard to distinguish
			if(sum(A[[i-1]]!=A[[i]])!=0){
				polygon(c(dat[[1]],rev(dat[[1]])),c(A[[i-1]],rev(A[[i]])),col=i,density=100,angle=0,border=NA)
			}
			if(sum(B[[i-1]]!=B[[i]])!=0){
				polygon(c(dat[[1]],rev(dat[[1]])),c(B[[i-1]],rev(B[[i]])),col=i,density=100,angle=0,border=NA)
			}
			#For each subsequent polygon, the lower perimeter is composed of the top points of the previous category. 
			#The lower points are drawn from left to right (ascending on the x axis) and then top points back from right to left using the rev() function,
			#so that the polygon is eventually closed off where it started (i.e. the first and last points are the same point). Essentially,
			#the polygons are plotted as closed loops.
		}
	}

	usrSettings<-par("usr")
	legend(usrSettings[2],usrSettings[4],names(dat)[2:ncol(dat)],col=1:length(A),lty=rep(1,ncol(dat)-1),lwd=5,bty="o",cex=1.)
	
	par(mar=orgMar)
	if(is.element(graphicsDevice,c("png","pdf"))){
		graphics.off()
	}
}

draw.barPlot<-function(graphicsDevice,fileName,dataArray,plotTitle,subTitle,barLabels,xLabel,yLabel, ourPalette,pngSettings,argsLegend){
	if(graphicsDevice=="png"){
		png(filename=fileName, width = pngSettings[["width"]], height = pngSettings[["height"]],res= pngSettings[["res"]],pointsize= pngSettings[["pointsize"]], bg = pngSettings[["bg"]])
	}else if(graphicsDevice=="pdf"){
		pdf(file=fileName)
	}
	
	if(length(ourPalette)<length(dataArray)){
		print(paste("WARNING in draw.barPlot(): the palette you passed has fewer colors than is needed. you passed",length(ourPalette),"colors to display",length(dataArray),"items. Therefore some colors will have to be reused."))
	}
	# set palette to ourPalette
	palette(ourPalette)
	# 1 is then the first color in the palette, 2 the second, etc.
	cols<-1:length(dataArray)
	par(cex=0.7)
	barplot(dataArray,names.arg=NULL,width=0.5,density=100,angle=0,xlab=xLabel,ylab=yLabel,ylim=c(min(dataArray),max(dataArray)),main= plotTitle,sub= subTitle,cex.axis=1,cex.names=1,col=cols,legend.text=barLabels,args.legend=argsLegend)
	
	# restore default
	palette("default")
	if(is.element(graphicsDevice,c("png","pdf"))){
		graphics.off()
	}
}

draw.pie<-function(graphicsDevice,fileName,dataArray,plotTitle,subTitle,barLabels,ourPalette,pngSettings){
	if(graphicsDevice=="png"){
		png(filename=fileName, width = pngSettings[["width"]], height = pngSettings[["height"]],res= pngSettings[["res"]],pointsize= pngSettings[["pointsize"]], bg = pngSettings[["bg"]])
	}else if(graphicsDevice=="pdf"){
		pdf(file=fileName)
	}
	
	if(length(ourPalette)<length(dataArray)){
		print(paste("WARNING in draw.pie(): the palette you passed has fewer colors than is needed. you passed",length(ourPalette),"colors to display",length(dataArray),"items. Therefore some colors will have to be reused."))
	}
	# set palette to ourPalette
	palette(ourPalette)
	# 1 is then the first color in the palette, 2 the second, etc.
	cols<-1:length(dataArray)
	# anything less than 1.0e-3 is 0 and we take it out. why? we can't leave it to R's pie() to handle this: for labels corresponding to consecutive very small components (I've seen it happen for 0.0004) in dataArray, pie() puts the labels on top of each other in the graph
	isNotZeroEntry<-dataArray>1.0e-3
	dataArray<-dataArray[isNotZeroEntry]
	cols<-cols[isNotZeroEntry]
	barLabels<-barLabels[isNotZeroEntry]
	
	par(cex=0.7)

	pie(dataArray,labels=barLabels,edges=1000,radius=.9,density=100,angle=0,col=cols,main=plotTitle,sub=subTitle)
	
	# restore default palette
	palette("default")
	if(is.element(graphicsDevice,c("png","pdf"))){
		# this flushes the buffer it seems and creates the file
		graphics.off()
	}
}