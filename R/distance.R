#first.logical returns the position of the first TRUE in a vector, 'within' each level,
#repeated for each element sharing the level
first <- function(x,...)UseMethod('first')
first.logical <- function(x,within=rep(1,length(x)),...){
	x <- as.logical(x)
	len <- length(x)
	if (!is.list(within))within <- list(within)
	check <- sapply(within,length)
	if(any(check!=len))stop('All indicies must have same length as x.')
	na <- sapply(within,function(x)any(is.na(x)))
	if(any(na))warning('within has NA')
	unidex <- as.numeric(factor(do.call(paste,within)))
	match(paste(unidex,rep(TRUE,len)),paste(unidex,x))
}
#ordinal.logical returns the position of the nth TRUE in a vector, 'within' each level,
#repeated for each element sharing the level. Negative values count from the end of the vector.
ordinal <- function(x,...)UseMethod('ordinal')
enth <- ordinal
ordinal.logical <- function(x,n=1,within=rep(1,length(x)),...){
	n=as.integer(n)
	if(length(n)!=1)stop('n must have length one')
	x <- as.logical(x)
	len <- length(x)
	if(!is.list(within))within <- list(within)
	if(n==0)return(rep(NA,len))
	if(n<0)return(rev(len + 1 - ordinal(rev(x),-n,lapply(within,rev))))
	nominal <- first(x,within)
	if(n==1)return(nominal)
	x[unique(nominal)] <- FALSE
	ordinal(x,n-1,within)
}

#distance.logical returns the difference of ordinal.logical and the vector indicies.
distance <- function(x,...)UseMethod('distance')
distance.logical <- function(x,n=1,within=rep(1,length(x)),...){
	#if(length(x)==0)return(NA)
	1:length(x) - ordinal(x,n,within)
}

#where.logical returns a logical evaluating for the nth 'TRUE' in x 'within' each level.
where <- function(x,...)UseMethod('where')
where.logical <- function(x,n=1,within=rep(1,length(x)),...)distance(x,n,within)==0

unconditional <- function(x,...)UseMethod('unconditional')
unconditional.default <- function(x,...)rep(TRUE,length(x))
unconditional.data.frame <- function(x,...)rep(TRUE,nrow(x))
