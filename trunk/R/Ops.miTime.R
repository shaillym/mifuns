`Ops.miTime` <-
function(e1,e2){
	f1 <- as.miTime(as.character(e1),format = "%H:%M")
	f2 <- as.miTime(as.character(e2),format = "%H:%M")
	e1[!is.na(f1)] <- f1[!is.na(f1)]
	e2[!is.na(f2)] <- f2[!is.na(f2)]
	NextMethod()
}

