`read.param` <-
function(file,lead=1,tag='param',...){
	x <- readLines(file)
	head <- seq(length.out=lead)
	note <- toXML(x[head],tag='note')
	body <- as.data.frame.block(x[-head])
	body <- toXML(body[,-1],keyname=names(body)[[1]], keys=body[[1]])
	nest(tag='param',x=c(note,body))
}

