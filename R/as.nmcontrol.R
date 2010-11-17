as.nmctl <-
function(x,...)UseMethod('as.nmctl')

as.character.nmctl <-
function(x,...){
	recname <- function(x,rec)if(
		!is.na(
			names(x)[[rec]]
		)
	)paste(
		sep='',
		'$',
		toupper(names(x)[[rec]])
	) else NULL
	
	unlist(
		lapply(
			seq(length.out=length(x)),
			function(rec,...){
				dat <- as.character(x[[rec]])
				nm <- recname(x,rec)
				if(length(dat))dat[[1]] <- paste(sep='',nm,dat[[1]])
				dat
			}
		)
	)
}

as.list.nmctl <-
function(x,...)unclass(x)

as.nmctl.character <-
function(
	x,
	pattern='^[[:blank:]]*\\$([^ ]+).*',
	name='\\1',
	...
){
	flag <- cumsum(contains(pattern,x))
	y <- split(x,flag)
	
	nms <- sapply(
		y,
		function(z){
			if(contains(pattern,z[[1]])){
				tolower(sub(pattern,name,z[[1]]))
			}else{
			 NA
			}
		}
	)
	names(y) <- nms
	y <- lapply(
		1:length(y),
		function(rec,dat){
			nm <- names(dat)[[rec]]
			rec <- dat[[rec]]
			if(!is.na(nm))rec[[1]] <- sub(
				paste(sep='','\\$',nm),
				'',
				rec[[1]],
				ignore.case=TRUE
			)
			rec
		},
		dat=y
	)
	names(y) <- nms
	#y <- sapply(y,function(r)if(r[[1]]=='')r[-1]else r,simplify=FALSE)
	class(y) <- c('nmctl',class(y))
	y
}

format.nmctl <-
function(x,...)as.character(x,...)

print.nmctl <-
function(x,...)print(format(x,...))

read.nmctl <-
function(con,...)as.nmctl(readLines(con,...),...)

write.nmctl <-
function(x, file='data',ncolumns=1,append=FALSE, sep=" ",...){
	out <- format(x)
	if(maxChar(clear(out,drop=';.*'))>80)warning('80 character limit exceeded')
	write(
		out,
		file=file,
		ncolumns=ncolumns,
		append=append, 
		sep=sep,
		...
	)
}

