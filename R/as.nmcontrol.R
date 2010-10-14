as.nmcontrol <-
function(x,...)UseMethod('as.nmcontrol')

as.character.nmcontrol <-
function(x,...)unlist(
	lapply(
		seq(length.out=length(x)),
		function(rec,...)c(
			if(!is.na(names(x)[[rec]]))paste(sep='','$',toupper(names(x)[[rec]])) else NULL,
			as.character(x[[rec]])
		)
	)
)

as.list.nmcontrol <-
function(x,...)unclass(x)

as.nmcontrol.character <-
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
				paste(sep='','\\$',nm,' *'),
				'',
				rec[[1]],
				ignore.case=TRUE
			)
			rec
		},
		dat=y
	)
	names(y) <- nms
	y <- sapply(y,function(r)if(r[[1]]=='')r[-1]else r)
	class(y) <- c('nmcontrol',class(y))
	y
}

format.nmcontrol <-
function(x,...)as.character(x,...)

print.nmcontrol <-
function(x,...)print(format(x,...))

read.nmcontrol <-
function(con,...)as.nmcontrol(readLines(con,...),...)

write.nmcontrol <-
function(x, file='data',ncolumns=1,append=FALSE, sep=" ",...)write(
	format(x),
	file=file,
	ncolumns=ncolumns,
	append=append, 
	sep=sep,
	...
)

