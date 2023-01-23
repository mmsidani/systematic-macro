kalman1d.update <- function(y, x0, p0, f, h, c, r, q ) {
# y_t=h*x_t+eps, eps=N(0,r)
# x_(t+1)=f*x_t+c_t+heta, heta=N(0,q)
# all coefficients depend on y_(t-1) and t and theta but not y_t
	
	# time update
	x0 <- f * x0 + c
	p0 <- f * p0 * f + q
	
	# measurement update
	k <- p0 * h / (h * p0 * h + r)
	p <- (1 - k * h ) * p0
	x <- x0 + k * ( y - h * x0 )
	
	return( list( x=x, p=p ) )
}