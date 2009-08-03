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
`stableMerge` <-
function(x,y){
#left join with stable row count, row order, column order, and row names
#returns equiv of x with values of y appended (looked up by common columns)
colnames <- names(x)
rowNames <- rownames(x)
if(all(names(y) %in% names(x))){
warning("nothing to merge")
return(x)
}
key <- names(y)[names(y) %in% names(x)]
if (
!identical(
nrow(y[,key,drop=FALSE]),
nrow(unique(y[,key,drop=FALSE]))
)
) stop("keys in y not unique")
x$stable_Index <- 1:nrow(x)
z <- merge(x,y,all.x=TRUE,all.y=FALSE,sort=FALSE)
z <- z[order(z$stable_Index),]
z$stable_Index <- NULL
z <- z[,c(colnames,names(z)[!names(z) %in% colnames])]
rownames(z) <- rowNames
return(z)
}

