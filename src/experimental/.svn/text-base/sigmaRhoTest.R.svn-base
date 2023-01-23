sigmaRhoTest.main <- function() {
	
	sig2 <- 0.2
	
	f <- function( omeg, sig1 , rho ) {
		
		omeg^2 * sig1^2 + (1-omeg)^2 * sig2^2 + 2 * rho*omeg*(1-omeg)*sig1*sig2
	}
	
	pts <- (1:100) * 0.01
	plot(pts, f(pts, 0.01, -0.3), type = "l", col=6)
	lines(pts, f(pts, 0.05, -0.3), col=4)
	lines( pts , f(pts, 0.1, -0.3), col=2)
	
	dev.new()
	plot(pts, f(pts, 0.01, -0.1), type = "l", col=6)
	lines(pts, f(pts, 0.05, -0.1), col=4)
	lines( pts , f(pts, 0.1, -0.1), col=2)
	
}

sigmaRhoTest.main()

