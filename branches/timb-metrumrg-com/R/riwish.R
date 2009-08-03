`riwish` <-
function(s,df,Prec){
   if (df<=0) stop ("Inverse Wishart algorithm requires df>0")
   R <- diag(sqrt(2*rgamma(s,(df + s  - 1:s)/2)))
   R[outer(1:s, 1:s,  "<")] <- rnorm (s*(s-1)/2)
   S <- t(solve(R))%*% chol(Prec)
   return(t(S)%*%S)
}
`myriwish` <-
function(s,df,Cov) {
	nu <- df - (s-1)
	iwishV <- Omega.MtoV(CheckPositiveMatrix(riwish(s,nu,df*Cov)))
   return(iwishV)
}
`Omega.MtoV` <-
function(omegaMatrix) {
	omegaVector <- NULL
	for( i in 1:dim(omegaMatrix)[1]) {
	     for(j in 1:i) {
		    omegaVector <- c(omegaVector,omegaMatrix[j,i])
		}
	}
	return(omegaVector)
  }
`CheckPositiveMatrix` <-
function(PosMat) {
     if(any(diag(PosMat) <=0)) stop("OMEGA matrix cannot be made positive definite")
     n <- length(diag(PosMat))
     for(i in 1:n) for(j in 1:n) {
	       temp <-PosMat[i,j]
	       n1 <- nchar(abs(round(temp)))
	       if(n1 > 6) stop("Element of the matrix > 1E+07")
	       n2 <- 6 - n1
	       temp <- temp*10^n2
	       if(i == j) temp <- round(temp)/10^n2
	       if(i != j) temp <- floor(temp)/10^n2
	       PosMat[i,j] <- temp          
	 }
	 if(det(PosMat) <=0) {
		PosMat <- 0.97*PosMat + 0.03*diag(diag(PosMat))
       PosMat <- CheckPositiveMatrix(PosMat)
    }	
    return(PosMat)	
}
`CheckPositiveVector` <-
function(omegaVector) {
	omegaMatrix <- Omega.VtoM(omegaVector)
	omegaMatrix <- CheckPositiveMatrix(omegaMatrix)
	return(Omega.MtoV(omegaMatrix))
}
`Omega.VtoM` <-
function(omegaVector) {
	nomega <- round(0.5*(sqrt(1+8*length(omegaVector))-1))
	omegaMatrix <- matrix(0,nomega,nomega)
	pos <- 1
	for( i in 1:nomega) {
	     for(j in 1:i) {
		    omegaMatrix[i,j] <- omegaVector[pos]
		    omegaMatrix[j,i] <- omegaVector[pos]
		    pos <- pos+1
		}
	}
	return(omegaMatrix)
  }
`rinvchisq` <-
function(n,df,omega) {
	return(df*omega/rchisq(n, df))
}
`rinvgamma` <-
function(n,df,sigma) {
# shape=alpha=DF
# rate = beta
# mode(rinvgamma)=beta/(alpha+1)
# mean(rinvgamma)=beta/(alpha-1)
# var(rinvgamma)=beta^2/(alpha-1)^2/(alpha-2)

	shape <- df
	rate <- sigma*df
	return(1/rgamma(n, shape, rate))
}
`SimulateOmega` <-
function(nsim,df,Cov){
	nomega <- dim(Cov)[1]
	ncols <- nomega*(nomega+1)/2
	res <- matrix(nrow=nsim, ncol=ncols)
	for( isim in 1:nsim) {
	   res[isim,] <- myriwish(nomega,df,Cov)
    }
	return(res)
}

