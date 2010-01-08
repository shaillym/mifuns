`read.param` <-
function(file,lead=1,tag='param',...){
	x <- readLines(file)
	head <- seq(length.out=lead)
	note <- as.xml(x[head],tag='note')
	body <- as.data.frame.block(x[-head])
	body <- as.xml(body[,-1],keyname=names(body)[[1]], key=body[[1]])
	nest(tag='param',x=c(note,body))
}

