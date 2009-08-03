`prev` <-
function(x)c(NA,x[-length(x)])#last observation
`runhead` <-
function(x){#not like last observation
	n <- x!=prev(x)
	n[[1]] <- TRUE
	n
}
`sorted` <-
function(x,on)identical(
	x,
	x[
		with(
			x,
			do.call(
				order,
				lapply(
					on,
					as.name
				)
			)
		),
	]
)
`maxChar` <-
function(x){
	x <- as.character(x)
	len <- nchar(x)
	max(len)
}

