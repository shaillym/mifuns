`toXML` <-
function(x,...)UseMethod('toXML')
`toXML.character` <-
function(x,tag,...)paste(
	sep='',
	bracket(tag,...),
	x,
	bracket(tag,close=TRUE)
)
`toXML.data.frame` <-
function(x,keyname='row',keys=rownames(x),...){
	itemize <- function(col,frame,tag,key){
		values <- frame[,col]
		nest(toXML(values,tag=tag,key=key),tag=col)
	}
	unlist(lapply(names(x),itemize,frame=x,tag=keyname,key=keys))
}
`toXML.default` <-
function(x,tag,...)toXML(as.character(x),tag,...)

