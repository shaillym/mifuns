`as.xml` <-
function(x,...)UseMethod('as.xml')
`as.xml.character` <-
function(x,tag,...)paste(
	sep='',
	bracket(tag,...),
	x,
	bracket(tag,close=TRUE)
)
`as.xml.data.frame` <-
function(x,keyname='row',keys=rownames(x),...){
	itemize <- function(col,frame,tag,key){
		values <- frame[,col]
		nest(as.xml(values,tag=tag,key=key),tag=col)
	}
	unlist(lapply(names(x),itemize,frame=x,tag=keyname,key=keys))
}
`as.xml.default` <-
function(x,tag,...)as.xml(as.character(x),tag,...)

