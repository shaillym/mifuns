`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}

`text2decimal` <-
function(x)as.numeric(sub("[^0-9.+-]*([+|-]?[0-9]+\\.?[0-9]*).*","\\1",as.character(x)))

reapply <- function(x, INDEX, FUN, ...){ 
	y <- tapply(x,INDEX) 
	z <- tapply(x,INDEX, FUN,...) 
	z[y] 
}
aug <- function(x,...){
	extras <- list(...)
	nms <- names(extras)
	for(name in nms)x[[name]] <- extras[[name]]
	x
}

