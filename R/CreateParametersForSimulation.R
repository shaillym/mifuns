`CreateParametersForSimulation` <-
function(nsim,ThetaMean,ThetaCovar,
         OmegaModeList,OmegaDfList,SigmaModeList,SigmaDfList,digits=4) {	

  parameters <- NULL
  ntheta <- length(ThetaMean)
  if(ntheta == 1 & length(ThetaCovar)!= 1) stop("Incorrect THETA input")
  if(ntheta != 1) if(ntheta != dim(ThetaCovar)[1] |
     ntheta != dim(ThetaCovar)[2] ) stop("Incorrect THETA input")
  if(det(as.matrix(ThetaCovar)) < 0) stop("Input Error: ThetaCovar is not positive-definite.")
  if(!is.list(OmegaModeList)) OmegaModeList <- list(OmegaModeList)
  if(!is.list(SigmaModeList)) SigmaModeList <- list(SigmaModeList)

  parameters <- cbind(parameters,mvrnorm(nsim,ThetaMean,ThetaCovar))

  nOmegaBlocks <- length(OmegaDfList)
  for(iomega in 1:nOmegaBlocks) {
      omegaMode <- OmegaModeList[[iomega]]
      omegaDf <-  OmegaDfList[iomega]
      nomega <- length(omegaMode)
      if(nomega != 1) if(dim(omegaMode)[1] != dim(omegaMode)[2] ) stop("Incorrect OMEGA input")
      if(det(as.matrix(omegaMode)) < 0) stop(paste("OmegaModeList[[",iomega,
                                 "]] is not positive definite",sep=""))
      if(omegaDf < nomega) stop("Input Error: OMEGA DF value is less than matrix dimension.")
      if(nomega > 1) {
	     parameters <- cbind(parameters, SimulateOmega(nsim,omegaDf,omegaMode))
	} else {
 	     parameters <- cbind(parameters, rinvchisq(nsim,omegaDf,omegaMode))
	}   
  }

  nSigmaBlocks <- length(SigmaDfList)
  for(isigma in 1:nSigmaBlocks) {
      sigmaMode <- SigmaModeList[[isigma]]
      sigmaDf <-  SigmaDfList[isigma]
      nsigma <- length(sigmaMode)
      if(nsigma != 1) if(dim(sigmaMode)[1] != dim(sigmaMode)[2] ) stop("Incorrect SIGMA input")
      if(det(as.matrix(sigmaMode)) < 0) stop(paste("SigmaModeList[[",iomega,
                                 "]] is not positive definite",sep=""))
      if(sigmaDf < nsigma) stop("Input Error: SIGMA DF value is less than matrix dimension.")
      if(nsigma > 1) {
	     parameters <- cbind(parameters, SimulateOmega(nsim,sigmaDf,sigmaMode))
	} else {
 	     parameters <- cbind(parameters, rinvchisq(nsim,sigmaDf,sigmaMode))
	}   
  }
	
   return(round(signif(parameters,digits),6))
}
`TruncateParametersForSimulation` <-
function(parameters, bounds) {	
   nsim  <- dim(parameters)[1]
   npar  <- dim(parameters)[2]
   n1  <- range(bounds[,1])[1]
   n2  <- range(bounds[,1])[2]
   message <- ""
   if(any(round(bounds[,1]) != bounds[,1])) message <- "is not an integer"
   if(n1 < 1) message <- "is not positive"
   if(npar < n2 ) message <- "exceeds number of parameters"
   if(message != "")
  {
        print(paste("Error:","Some parameter number in bounds data frame",message),
              quote = FALSE)
        return(NULL)
   }
   for(i in 1:length(bounds[,1]) ) {
         parameters <- parameters[parameters[,bounds[i,1]] > bounds[i,2] & 
                                  parameters[,bounds[i,1]] < bounds[i,3],]
   }
   print(paste("Initial parameter set contained",nsim,"records"),quote = FALSE)
   print(paste(dim(parameters)[1],"records satisfied all bounds"),quote = FALSE)
   return(parameters)
}

