constant <- function(x,...)UseMethod('constant')
constant.default <- function(x,within,...)x%nests%within
constant.data.frame <- function(x,within=NULL,...){
	if(!is.null(within))if(!is.character(within) | !all(within %in% names(x)))stop('within must be a character vector of names in x')
	if (is.null(within))index <- rep(1,nrow(x))
	else index <- do.call(paste,x[,within,drop=FALSE])
	cons <- sapply(x,constant,within=index)
	unique(x[, names(x)[cons],...])
}	