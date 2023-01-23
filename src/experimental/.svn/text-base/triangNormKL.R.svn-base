test.normal <- function(a,b,c){
# test to verify that, given a triangular distribution with mean m and variance sig^2, a normal distribution with mean m and variance sig^2 is closest to the triangular distribution in a KL sense
	library(statmod)
	
	nw <- gauss.quad(3000,kind="legendre")
	
	trg <- function(x){
		ret <- rep(0,length(x))
		ret[ x <a ] <- 0
		ret[ x>=a & x <=c ] <- 2*(x[ x>=a & x <=c ]-a) / ((b-a) * (c-a))
		ret[  c < x & x <=b ] <-  2*(b-x[  c < x & x <=b ]) / ((b-a)*(b-c))
		ret[ x > b] <- 0
		
		return(ret)
	}
	
	mapAB <- function(fn, x, x0, x1){
		return( fn(((x+1)*(x1-x0)/2+x0)) * (x1-x0)/2 )
	}
	
	mapInf <- function(fn, x){
		return( fn( x/(1-x^2)) * (1+x^2)/(1-x^2)^2)
	}
	
	natparams <- function(){
		return(c((a+b+c)/3, sqrt((a^2+b^2+c^2-a*b-a*c-b*c)/18)))
	}
	
	objective <- function(params){
		integrand <- function(x) {
			if(params[2] <= 0 ) return(1.0e+50)
			
			trgx <- trg(x)
			ret <- rep(0, length(x))
			ret[ trgx!=0] <- trgx[trgx!=0] * log(trgx[trgx!=0] / dnorm(x[trgx!=0], mean=params[1], sd=params[2]) )
			#dnorm(x,mean=params[1],sd=params[2]) * log(dnorm(x, mean=params[1], sd=params[2]) / trg(x))
		
			return(ret)
		}
		
		ret<-(  sum(nw$weights * mapInf(integrand, nw$nodes)) )
		
		print(ret)
		return(ret)
	}
	
	print( sum(nw$weights * mapInf(trg, nw$nodes)))
	
	results <- optim(c(0,1), objective)
	print(results)
	print(natparams())
}

test.beta <- function(a,b,c){
	
	if ( a <0 || b > 1){
		stop("this test only works for a=0 b=1 since beta is only defined on that interval ")
	}
	
# test to verify that, given a triangular distribution with mean m and variance sig^2, a normal distribution with mean m and variance sig^2 is closest to the triangular distribution in a KL sense
	library(statmod)
	
	nw <- gauss.quad(3000,kind="legendre")
	
	trg <- function(x){
		ret <- rep(0,length(x))
		ret[ x <a ] <- 0
		ret[ x>=a & x <=c ] <- 2*(x[ x>=a & x <=c ]-a) / ((b-a) * (c-a))
		ret[  c < x & x <=b ] <-  2*(b-x[  c < x & x <=b ]) / ((b-a)*(b-c))
		ret[ x > b] <- 0
		
		return(ret)
	}
	
	mapAB <- function(fn, x, x0, x1){
		return( fn(((x+1)*(x1-x0)/2+x0)) * (x1-x0)/2 )
	}
	
	mapInf <- function(fn, x){
		return( fn( x/(1-x^2)) * (1+x^2)/(1-x^2)^2)
	}
	
	natparams <- function(){
		return(c((a+b+c)/3, sqrt((a^2+b^2+c^2-a*b-a*c-b*c)/18)))
	}
	
	objective <- function(params){
		integrand <- function(x) {
			if(params[1] <= 0 || params[2] <= 0 ) return(1.0e+50)
			
			trgx <- trg(x)
			ret <- rep(0, length(x))
			ret[ trgx!=0] <- trgx[trgx!=0] * log(trgx[trgx!=0] / dbeta(x[trgx!=0], params[1], params[2]) )
			#dnorm(x,mean=params[1],sd=params[2]) * log(dnorm(x, mean=params[1], sd=params[2]) / trg(x))
			
			return(ret)
		}
		
		ret<-(  sum(nw$weights * mapInf(integrand, nw$nodes)) )
		
		print(ret)
		return(ret)
	}
	
	print( sum(nw$weights * mapInf(trg, nw$nodes)))
	
	results <- optim(c(2, 3), objective)
	print(results)
	alpha <- results$par[1]
	beta <- results$par[2]
	print ( c(alpha/(alpha+beta), sqrt( alpha*beta/(alpha+beta)^2/(alpha+beta+1)) ))
	print(natparams())
}

#execute
test.beta(0,1,0.25)