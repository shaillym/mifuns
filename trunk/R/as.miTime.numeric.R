`as.miTime.numeric` <-
function(x,...){
	x[is.finite(x)] <- x[is.finite(x)]%%1 #time part
	x <- trunc(x*24*60)/24/60 #truncate to nearest minute
	class(x) <- "miTime"
	x
}
	

