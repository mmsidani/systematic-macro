setwd("//SOL/Users/majed/devel/InOut/")

x<- read.csv("2012-10-22.bbd.reformattedPX.csv")

#temp <- as.numeric(x[1,setdiff(names(x), "Date")])
#names(temp) <- setdiff(names(x), "Date")
#x <- temp

#shares <- read.csv("c:/Users/Majed/devel/InOut/shares.csv")
#temp <- shares$shares
#names(temp) <- shares$X
#shares <- temp
#
#additionalShares <- c(3328.417,  8055.445, 108.745, 859.430) * 1000000
#names(additionalShares) <- c("sg.ct","sg.ihh","fr.tec","fr.stm")
#x <- c(x, additionalShares)
#
#combined <- data.frame(out=x[sort(names(shares))],num=shares[sort(names(shares))],percent=shares[sort(names(shares))]/x[sort(names(shares))] *100 )

principal <- 3e8
totalInv <- 4.5e8

treas <- totalInv * .3
equities <- totalInv

sgW <- .14
hkW <- .11
nlW <- .35
frW <- .08

eur <- 1.3
sgd <- 1/1.22
hkd <- 1/7.7

x[,grep("sg\\.",names(x))] <- x[,grep("sg\\.",names(x)) ] * sgd
x[,grep("hk\\.",names(x))] <- x[,grep("hk\\.",names(x))] * hkd
x[,grep("fr\\.",names(x))] <- x[,grep("fr\\.",names(x))] * eur
x[,grep("nl\\.",names(x))] <- x[,grep("nl\\.",names(x))] * eur
x[,grep("be\\.",names(x))] <- x[,grep("be\\.",names(x))] * eur

shares <- c()
shares <- c( shares, round(equities * sgW / length( x[,grep("sg\\.",names(x))] )/ as.numeric(x[,grep("sg\\.",names(x))]), 0) )
shares <- c( shares, round(equities * hkW / length( x[,grep("hk\\.",names(x))] )/ as.numeric(x[,grep("hk\\.",names(x))]), 0) )
# there's one and only one be. in dutch index
shares <- c( shares, round(equities * nlW / (length(x[,grep("nl\\.",names(x))]) + 1 ) / as.numeric(x[,grep("nl\\.",names(x))]), 0) )
shares <- c( shares, round(equities * nlW / (length(x[,grep("nl\\.",names(x))]) + 1) / as.numeric(x[,grep("be\\.",names(x))]), 0) )
shares <- c( shares, round(equities * frW / length( x[,grep("fr\\.",names(x))]) / as.numeric(x[,grep("fr\\.",names(x))]), 0) )

prices <- c()
prices <- c( prices, as.numeric(x[,grep("sg\\.",names(x))]))
prices <- c( prices, as.numeric(x[,grep("hk\\.",names(x))]))
prices <- c( prices, as.numeric(x[,grep("nl\\.",names(x))]))
prices <- c( prices, as.numeric(x[,grep("be\\.",names(x))]))
prices <- c( prices, as.numeric(x[,grep("fr\\.",names(x))]))

names(shares ) <- c(names(x)[grep("sg\\.",names(x))], names(x)[grep("hk\\.",names(x))], names(x)[grep("nl\\.",names(x))], names(x)[grep("be\\.",names(x))], names(x)[grep("fr\\.",names(x))] )

bbNames <- paste(sub("^sg\\.","",names(x)[grep("sg\\.",names(x))]), " SP EQUITY",sep="")
bbNames <- c( bbNames, paste(sub("^hk\\.","",names(x)[grep("hk\\.",names(x))]), " HK EQUITY",sep=""))
bbNames <- c( bbNames, paste(sub("^nl\\.","",names(x)[grep("nl\\.",names(x))]), " NA EQUITY",sep=""))
bbNames <- c( bbNames, paste(sub("^be\\.","",names(x)[grep("be\\.",names(x))]), " NA EQUITY",sep=""))
bbNames <- c( bbNames, paste(sub("^fr\\.","",names(x)[grep("fr\\.",names(x))]), " FR EQUITY",sep=""))

write.csv(data.frame(Shares= shares, Price= prices), "c:/Users/majed/devel/InOut/shares.csv", row.names = T)