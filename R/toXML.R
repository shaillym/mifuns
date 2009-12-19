#function to convert character vector to xml where vector is 
#space delimited table with header and one (informative) skipped row

toXML <- function(x,...)UseMethod('toXML')
bracket <- function(x,close=FALSE,...)do.call(
	paste,
	c(
		list(
			sep='',
			'<',
			if(close) '/' else NULL,
			x
		),
		lapply(names(list(...)),function(x)attribute(list(...)[[x]],tag=x)),
		list(
			'>'
		)
	)
)
attribute <- function(x,tag,...)paste(' ',tag,"='",x,"'",sep='')
nest <- function(x,tag,...)c(bracket(tag),x,bracket(tag,close=TRUE))
toXML.character <- function(x,tag,...)paste(
	sep='',
	bracket(tag,...),
	x,
	bracket(tag,close=TRUE)
)

toXML.default <- function(x,tag,...)toXML(as.character(x),tag,...)
toXML.data.frame <- function(x,...){
	index <- x[,1,drop=FALSE]
	body <- x[,-1,drop=FALSE]
	itemize <- function(x,target,index){
		keyName <- names(index)
		keys <- index[,1]
		dataName <- x
		values <- target[,dataName]
		nest(
			toXML(
				values,
				tag=keyName,
				key=keys
			),
			tag=dataName
		)
	}
	data <- lapply(names(body),itemize,target=x,index=index)
	unlist(data)
}

as.data.frame.block <- function(x,...){
	con <- textConnection(x)
	dat <- read.table(con,header=TRUE,as.is=TRUE,check.names=FALSE,...)
	close(con)
	dat
}
read.param <- function(file,lead=1,tag='param',...){
	x <- readLines(file)
	head <- seq(length.out=lead)
	note <- toXML(x[head],tag='note')
	body <- toXML(as.data.frame.block(x[-head]))
	nest(tag='param',x=c(note,body))
}

rlogger <- function(
	run,
	outfile,
	estfile=file.path(
		dirname(outfile),
		paste(
			run,
			'ext',
			sep='.'
		)
	),
	filename='NonmemRunLog.csv',
	...
){
	out <- readLines(outfile)
	prob <- grep(pattern='^\\$PROB',out,value=TRUE)[[1]]
	min <- 0**length(grep('MINIMIZATION SUCCESSFUL',out))
	est <- paste(read.param(estfile),collapse='\n')
	est <- gsub('<SIGMA\\(([0-9]+),([0-9]+)\\)>',"<SIGMA row='\\1' col='\\2'>",est)
	est <- gsub('</SIGMA\\(([0-9]+),([0-9]+)\\)>',"</SIGMA>",est)
	est <- gsub('<OMEGA\\(([0-9]+),([0-9]+)\\)>',"<OMEGA row='\\1' col='\\2'>",est)
	est <- gsub('</OMEGA\\(([0-9]+),([0-9]+)\\)>',"</OMEGA>",est)
	est <- gsub('<SIGMA\\(([0-9]+),([0-9]+)\\)>',"<SIGMA row='\\1' col='\\2'>",est)
	est <- gsub('<THETA([0-9]+)>',"<THETA id='\\1'>",est)
	est <- gsub('</THETA([0-9]+)>',"</THETA>",est)
	tree <- xmlParse(est,asText=TRUE)
	par <- xpathSApply(tree,"//ITERATION[@key='-1000000000']/text()",fun=xmlValue)
	se  <- xpathSApply(tree,"//ITERATION[@key='-1000000001']/text()",fun=xmlValue)
	prse <- signif(digits=3, 100 * as.numeric(se)/as.numeric(par))
	free(tree)
	write.table(
		sep=',',
		quote=FALSE,
		row.names=FALSE,
		col.names=FALSE,
		file=filename,
		x=do.call(
			data.frame,
			c(
				list(
					prob=rep(prob,2),
					prse=c('','RSE'),
					min= c(min,''),
					cov=c('',''),
					obj=c(par[length(par)],'')
				),
				data.frame(rbind(par,prse))[,-length(par)]
			)
		)
	)
}
			
	
	
	
	
		