omegacor <-
function(x,...){
	x <- x[after(grepl('OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS',x))]
	x <- x[before(grepl('SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS',x))]
	x <- x[grep('^\\+',x)]
	x <- sub('^\\+ +','',x)
	x <- strsplit(x,' +')
	x <- unlist(x)
	x <- sub('\\.\\.+',0,x)
	x <- as.numeric(x)
	x <- as.halfmatrix(x)
	x <- as.matrix(x)
	cov2cor(x)
}

