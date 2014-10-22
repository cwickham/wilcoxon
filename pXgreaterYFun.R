########################################
########################################

pXgreaterY <- function(distX, xpars, distY, ypars, ulims=NULL, nval=1e6, tol=1e-8){
	
	if(is.null(ulims)){
		invcdfX <- paste('q', distX, sep='')
		invcdfY <- paste('q', distY, sep='')
		xcall.inv <- xpars
		ycall.inv <- ypars
		xcall.inv$p <- c(tol, 1-tol)
		ycall.inv$p <- c(tol, 1-tol)
		ulims.x <- do.call(invcdfX, xcall.inv)
		ulims.y <- do.call(invcdfY, ycall.inv)
	}
	uvals <- seq(max(ulims.x[1], ulims.y[1]), min(ulims.x[2], ulims.y[2]),
		length=nval)
	delt.size <- uvals[2]-uvals[1]
	
	xcall.cdf <- xpars
	ycall.cdf <- ypars
	ycall.pdf <- ypars
	xcall.cdf$q <- uvals
	ycall.cdf$q <- c(min(uvals), max(uvals))
	ycall.pdf$x <- uvals
	
	
	cdfX <- paste('p', distX, sep='')
	cdfY <- paste('p', distY, sep='')
	pdfY <- paste('d', distY, sep='')
	
	x.cdf <- do.call(cdfX, xcall.cdf)
	y.cdf <- do.call(cdfY, ycall.cdf)
	y.pdf <- do.call(pdfY, ycall.pdf)
	adj <- x.cdf[1]*y.cdf[1] + x.cdf[nval]*(1-y.cdf[2])
	return(1 - sum(x.cdf*y.pdf*delt.size) - adj)
}

########################################

# # Checks using pXgreaterY
# 
# # Should be 0.5
# all.equal(pXgreaterY('norm', list(mean=0, sd=1), 'norm', list(mean=0, sd=1)), 0.5)
# 
# # Should be 0.5
# all.equal(pXgreaterY('exp', list(rate=1), 'exp', list(rate=1)), 0.5)
# 
# # Should be 0.875 = 0.5 + 0.5*0.5 + 0.5*0.25
# all.equal(pXgreaterY('unif', list(min=1, max=3), 'unif', list(min=0, max=2)), 0.875,
#   tol = 1e-5)
# 
# 
# # Should be 0.5
# all.equal(pXgreaterY('unif', list(min=1, max=3), 'unif', list(min=1, max=3)), 0.5,
#   tol = 1e-5)
# 
# # Should be 0.5
# all.equal(pXgreaterY('norm', list(mean=0, sd=1), 'norm', list(mean=0, sd=2)), 0.5, tol = 1e-5)
# 
# # Should be 0.5
# all.equal(pXgreaterY('norm', list(mean=0, sd=2), 'norm', list(mean=0, sd=1)), 0.5, tol = 1e-5)
# 
# 
# # Check: answer to first should be 1-answer to second
# all.equal(pXgreaterY('norm', list(mean=0, sd=1), 'norm', list(mean=2, sd=1)),
# 1 - pXgreaterY('norm', list(mean=2, sd=1), 'norm', list(mean=0, sd=1)), tol = 1e-5 )
