`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}

`text2decimal` <-
function(x)as.numeric(sub("[^0-9.+-]*([+|-]?[0-9]+\\.?[0-9]*).*","\\1",as.character(x)))

reapply <- 
function (x, INDEX, FUN, ...) 
{
    if(!is.list(INDEX)) INDEX <- list(INDEX)
    INDEX <- lapply(INDEX,function(x)as.integer(factor(x)))
    INDEX <- as.integer(do.call(interaction,c(INDEX,drop=TRUE)))
    form <- tapply(x, INDEX)
    calc <- tapply(x, INDEX, FUN, ...,simplify=FALSE)
    need <- table(form)
    calc <- lapply(
    	seq_along(calc),
    	function(cell)rep(
    		calc[[cell]],
    		length.out=need[[
    			as.character(cell)
    		]]
    	)
    )
	grps <- split(form,form)
	grps <- lapply(
		grps, 
		seq_along
	)
	elem <- unsplit(grps,form)
	sapply(
		seq_along(form),
		function(i)if(is.na(form[[i]])) NA else calc[[
			form[[i]]
		]][[
			elem[[i]]
		]]
	)
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

