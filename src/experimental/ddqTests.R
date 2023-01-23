
dailyMkt <- utils.getDataFromDB(c("us.equity","uk.equity","jp.equity","us.gg.10y","uk.gg.10y","jp.gg.10y","us.gg.3m"),NULL,global.core()[["dbSettings"]])
mkt <- utils.getYearData(dailyMkt[nrow(dailyMkt):1,])
mkt <- mkt[nrow(mkt):1,]

yearlyRets <- read.csv("FULL-sim.allPortsYearly.csv")
yearlyRets$Date <- as.Date(yearlyRets$Date )

stocks <- data.frame(Date=as.Date(rownames(mkt)[1:(nrow(mkt)-1)]), ret=as.matrix(mkt[1:(nrow(mkt)-1),c("us.equity","uk.equity","jp.equity")]) / as.matrix(mkt[2:(nrow(mkt)),c("us.equity","uk.equity","jp.equity")]) -1 )
stocks <- stocks[stocks$Date >= as.Date("1969-12-31"), ]
names(stocks)<-c("Date","us.equity","uk.equity","jp.equity")
mkt <- mkt[rownames(mkt) >= "1969-12-31", c("us.gg.10y","us.gg.3m","uk.gg.10y","jp.gg.10y") ]
bonds <- data.frame(Date=as.Date(rownames(mkt)), ret=as.matrix(mkt))
names(bonds) <- c("Date","us.gg.10y","us.gg.3m","uk.gg.10y","jp.gg.10y")

plot(stocks[,c("Date","us.equity")], col="green", type="l", ylim=c(min(stocks[,c("us.equity","uk.equity","jp.equity")]),max(stocks[,c("us.equity","uk.equity","jp.equity")])))
lines(stocks[,c("Date","uk.equity")], col="red")
lines(stocks[,c("Date","jp.equity")], col="blue")
lines(yearlyRets[,c("Date","MIX.TARO.JIRO.policyConstantProportions.policyDownAndSuspend")], col="cyan")

dev.new()
plot(bonds[,c("Date","us.gg.10y")], col="magenta", type="l", ylim=c(min(bonds[,c("us.gg.10y","us.gg.3m","uk.gg.10y","jp.gg.10y")]),max(bonds[,c("us.gg.10y","uk.gg.10y","jp.gg.10y")])))
lines(bonds[,c("Date","us.gg.3m")], col="green")
lines(bonds[,c("Date","uk.gg.10y")], col="yellow")
lines(bonds[,c("Date","jp.gg.10y")], col="black")

dev.new()
yearlyRets[, "Date"] <- as.Date(yearlyRets[, "Date"])
plot(yearlyRets[,c("Date","MIX.TARO.JIRO.policyConstantProportions.policyDownAndSuspend")], col="red", type="l")

for( p in setdiff(names(yearlyRets), "Date")){
	temp <- yearlyRets[, c("Date",p)]
	orderedRets <- order(temp[[p]], decreasing=T)
	print(p)
	print(temp[orderedRets[1:10],])
	#print(temp[orderedRets[(length(orderedRets)-10):length(orderedRets)],])
}

dailyMkt <- dailyMkt[ rownames(dailyMkt) >= "1970-01-01",]
irVol <- NULL
for( i in 1:(nrow(dailyMkt)-120)){
	if( i%%120 !=0) next
	irVol <- rbind(irVol, data.frame(Date=rownames(dailyMkt)[i], ir10y=sd(as.numeric(dailyMkt[i:(i+120),"us.gg.10y"]),na.rm=T), ir3m=sd(as.numeric(dailyMkt[i:(i+120),"us.gg.3m"]),na.rm=T)) )
}

print(irVol)