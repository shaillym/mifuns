subclass <- function(x,class,...){
	class(x) <- c(class,class(x))
	x
}
as.numeric.chartime <- function(x,format,...)as.numeric(unclass(as.POSIXct(strptime(x,format),tz="GMT")))
as.chartime <- function(x,...)UseMethod("as.chartime")
as.chartime.numeric <- function(x,format,mark=TRUE,...){
	y <- as.character(strftime(as.POSIXlt(as.numeric(x),tz="GMT",origin="1970-01-01"),format))
	y[is.infinite(x)] <- x[is.infinite(x)]
	z <- rep("",length(y))
	if(mark){
		s <- !is.na(x) & is.finite(x) & x%%60!=0
		z[s] <- "+"
	}
	paste(y,z,sep="")
}
as.miTime <- function(x,...)UseMethod("as.miTime")
as.miTime.numeric <- function(x,...){
	x <- round(x)
	x[is.finite(x)] <- x[is.finite(x)]%%(60*60*24)
	subclass(x,c("miTime"))
}
as.miTime.character <- function(x,format="%H:%M",...)as.miTime(as.numeric.chartime(x,format))

as.miDate <- function(x,...)UseMethod("as.miDate")
as.miDate.numeric <- function(x,...){
	x <- round(x)
	f <- is.finite(x)
	x[f] <- x[f] - x[f]%%(60*60*24)
	subclass(x,c("miDate"))
}
as.miDate.character <- function(x,format="%Y-%m-%d",...)as.miDate(as.numeric.chartime(x,format))

as.miDateTime <- function(x,...)UseMethod("as.miDateTime")
as.miDateTime.numeric <- function(x,...){
	x <- round(x)
	subclass(x,c("miDateTime"))
}
as.miDateTime.character <- function(x,format="%Y-%m-%d %H:%M",...)as.miDateTime(as.numeric.chartime(x,format))
as.miDateTime.miDate <- function(x,y=0,...)as.miDateTime(as.numeric(x)+as.numeric(y))
format.miTime <- function(x,format="%H:%M",mark=TRUE,...)as.chartime(x,format,mark)
format.miDate <- function(x,format="%m/%d/%Y",mark=TRUE,...)as.chartime(x,format,mark)
format.miDateTime <- function(x,format="%m/%d/%Y %H:%M",mark=TRUE,...)as.chartime(x,format,mark)
as.character.miDate <- 
as.character.miTime <- 
as.character.miDateTime <- 
function(x,...)format(x,...)
print.miDate <- 
print.miTime <- 
print.miDateTime <-
function(x,...){
	print(format(x,...),quote=FALSE)
	invisible(x)
}
c.miDate <- 
c.miTime <- 
c.miDateTime <- 
function (..., recursive = FALSE){
	args <- list(...)
	oldclass <- class(args[[1]])	
	structure(c(unlist(lapply(args, unclass))), class = oldclass)
}
seq.miDate <- 
seq.miDateTime <- function (from, to, by=NULL,length.out = NULL, along.with = NULL, ...){
	if(is.null(by))by=60*60*24
	x <- seq(
		from=as.numeric(from),
		to=as.numeric(to),
		by=by,
		...
	)
	class(x) <- class(from)
	x
}
seq.miTime  <- function (from, to, by=NULL,length.out = NULL, along.with = NULL, ...){
	if(is.null(by))by=60*60
	x <- seq(
		from=as.numeric(from),
		to=as.numeric(to),
		by=by,
		...
	)
	class(x) <- class(from)
	x
}
as.miTime.miTime <- 
as.miDate.miDate <- 
as.miDateTime.miDateTime <- 
function(x,...)x

`[.miDate` <- 
`[.miTime` <-
`[.miDateTime` <-
`[[.miDate` <- 
`[[.miTime` <-
`[[.miDateTime` <-
function (x, ..., drop = TRUE){
    cl <- oldClass(x)
    x <- unclass(x)
    structure(NextMethod(.Generic),class= cl)
}
`[<-.miDate` <- 
`[<-.miTime` <- 
`[<-.miDateTime` <- 
function (x, ..., value){
	cl <- oldClass(x)
	x <- unclass(x)
	value <- unclass(value)
	structure(NextMethod(.Generic),class = cl)
}
Ops.miDate <- 
Ops.miTime <-
Ops.miDateTime <- 
function (e1, e2){
    if (nargs() == 1) 
        stop("unary ", .Generic, " not defined for miTemporal objects")
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
        `<=` = , `>=` = TRUE, FALSE)
    if (boolean) e1 <- as.numeric(e1)
    if (boolean) e2 <- as.numeric(e2)
    NextMethod(.Generic)
}



