rmixnorm <- function(n, p1, mu1, sd1, mu2, sd2){
	mixInd <- 1 + rbinom(n, 1, (1-p1))
	muVec <- c(mu1, mu2)
	sdVec <- c(sd1, sd2)
	return(rnorm(n, mean=muVec[mixInd], sd=sdVec[mixInd]))
}

dmixnorm <- function(x, p1, mu1, sd1, mu2, sd2){
	return(p1*dnorm(x, mu1, sd1) + (1-p1)*dnorm(x, mu2, sd2))
}


pmixnorm <- function(q, p1, mu1, sd1, mu2, sd2){
	return(p1*pnorm(q, mu1, sd1) + (1-p1)*pnorm(q, mu2, sd2))
}

# needs to take vector valued p for pXgreaterY
qmixnorm_old <- function(p, p1, mu1, sd1, mu2, sd2, tol=1e-10, maxiter=20){
  q1 <- qnorm(p, mu1, sd1)
	q2 <- qnorm(p, mu2, sd2)
	q0.lo <- min(q1, q2)
	q0.hi <- max(q1, q2)
	cdf.lo <- pmixnorm(q0.lo, p1, mu1, sd1, mu2, sd2)
	cdf.hi <- pmixnorm(q0.hi, p1, mu1, sd1, mu2, sd2)
	cdf.new <- cdf.hi
	niter <- 0
	while(abs(cdf.new - p) > tol & niter < maxiter){		
		sf <- (p - cdf.lo)/(cdf.hi - cdf.lo)
		q0.new <- sf*(q0.hi - q0.lo) + q0.lo
		cdf.new <- pmixnorm(q0.new, p1, mu1, sd1, mu2, sd2)
		
		if(cdf.new > p){
			q0.hi <- q0.new
			cdf.hi <- cdf.new
			
			q0.temp <- q0.lo + 0.5*(q0.hi - q0.lo)
			cdf.temp <- pmixnorm(q0.temp, p1, mu1, sd1, mu2, sd2)
			if(cdf.temp < p){
				q0.lo <- q0.temp
				cdf.lo <- cdf.temp
			}
		}else{
			q0.lo <- q0.new
			cdf.lo <- cdf.new
			
			q0.temp <- q0.lo + 0.5*(q0.hi - q0.lo)
			cdf.temp <- pmixnorm(q0.temp, p1, mu1, sd1, mu2, sd2)
			if(cdf.temp > p){
				q0.hi <- q0.temp
				cdf.hi <- cdf.temp
			}

		}
				
		niter <- niter + 1
	}
	print(paste("niter = ", niter))
	return(q0.new)
}

qmixnorm <- Vectorize(qmixnorm_old, "p")

# ########################################
# 
# # Checks using qmixnorm
# 
# # Should be 2.5
# qmixnorm(0.5, p1=0.5, mu1=1, sd1=1, mu2=4, sd2=1)
# 
# # Should be 3
# qmixnorm(0.5, p1=0.5, mu1=1, sd1=2, mu2=4, sd2=1)
# 
# qmixnorm(0.3, p1=0.4, mu1=1, sd1=2, mu2=20, sd2=1)
# # Check, should be:
# qnorm(0.3/0.4, mean=1, sd=2)
# 
