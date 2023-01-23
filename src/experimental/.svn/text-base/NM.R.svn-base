NM.main <- function(){
	
#	opts <- list(opt1= list(strike=2500, maturity=0.063013698630137, type="call", undPrice=2511.83, optPrice=68), 
#			opt2=list( strike=2500, maturity=0.063013698630137, type="put", undPrice=2511.83, optPrice=57.8),
#			opt3=list(strike=2525, maturity=0.063013698630137, type="call", undPrice=2511.83, optPrice=54.5),
#			opt4=list(strike=2525, maturity=0.063013698630137, type="put", undPrice=2511.83, optPrice=69.3))
	
	opts <- list( opt1= list(strike=2650, maturity=0.219178082191781, type="call",undPrice=2712, optPrice=100.9),
			opt2=list(strike=2650, maturity=0.219178082191781, type="put",undPrice=2712, optPrice=88.3),
			opt3=list(strike=2675, maturity=0.219178082191781, type="call",undPrice= 2712, optPrice= 87.1),
			opt4=list(strike=2675, maturity= 0.219178082191781, type="put", undPrice= 2712, optPrice= 99.5) )
	
	if(opts[[1]][["strike"]] != opts[[2]][["strike"]] || opts[[1]][["maturity"]] != opts[[2]][["maturity"]] || opts[[1]][["type"]] != "call" || opts[[2]][["type"]] != "put"){
		stop("error: something wrong with opt1 and/or opt2")
	}
	if(opts[[3]][["strike"]] != opts[[4]][["strike"]] || opts[[3]][["maturity"]] != opts[[4]][["maturity"]] || opts[[3]][["type"]] != "call" || opts[[4]][["type"]] != "put"){
		stop("error: something wrong with opt3 and/or opt4")
	}
	if(opts[[1]][["undPrice"]]!=opts[[2]][["undPrice"]] || opts[[1]][["undPrice"]]!=opts[[3]][["undPrice"]] || opts[[1]][["undPrice"]]!=opts[[4]][["undPrice"]]){
		stop("error: something wrong with underlying price")
	}
	
	objective <- function(pars){
		optErr <- (opts[[1]][["optPrice"]]-opts[[2]][["optPrice"]]-pars[2] * (pars[1] - opts[[1]][["strike"]]) )^2 + (opts[[3]][["optPrice"]]-opts[[4]][["optPrice"]]-pars[2] * (pars[1] - opts[[3]][["strike"]]))^2
		sqrt(optErr + (opts[[1]][["undPrice"]]/ pars[2] - pars[1])^2)
	}
		
	x <- constrOptim( c( opts[[1]][["undPrice"]], 1.0), objective, NULL, rbind(matrix( c(1,0,0,-1),nrow=2,byrow=F),matrix(c(1,0,0,-1),nrow=2,byrow=F),deparse.level=F), c(0, -exp(30), 0, -exp(30)))
	print(x)
}

# execute
results <- NM.main()