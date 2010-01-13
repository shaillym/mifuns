`runlog` <-
function(
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
	runlogName=file.path(dirname(outfile),'NonmemRunLog.csv'),
	...
){
	runlogName <- star(runlogName,run)
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
	if(is.null(se))se <- rep(Inf,length(par))
	prse <- signif(digits=3, 100 * as.numeric(se)/as.numeric(par))
	free(tree)
	write.table(
		sep=',',
		quote=FALSE,
		row.names=FALSE,
		col.names=FALSE,
		file=runlogName,
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

