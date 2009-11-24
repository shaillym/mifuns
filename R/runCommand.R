`runCommand` <-
  function (
  	NMcom,
	rdir,
	b,
	boot,
	urgent,
	SGEflgs,
	nochecksum,
	grid,
	udef,
	ctlfile,
	outfile,
	perl=NULL,
	option=NULL,
	intern=invisible,
	minimized=invisible,
	invisible=FALSE,
	split=grid,
	l=FALSE,
	mod=NULL,
	hold=FALSE,
	...
){

  #runCommand needs to split the run into compile and execute phases if
  #this is an intel fortran run, on the grid.
	
  #organize arguments
  if(nix())internal <- FALSE
  if(is.null(option))option <- if(nochecksum) 'nochecksum' else  NULL
  if(is.null(perl)) if(nix()) perl <- 'perl -S'
  if(is.null(perl)) if(win()) perl <- if(!invisible) 'cmd /K perl -S' else 'cmd /C perl -S'

  #draft a command
  theCommand <- regCommand
  if(grid) theCommand <- grdCommand
  command <- character(0)
  command[[1]] <- theCommand(
	NMcom=NMcom,
  	ctlfile=ctlfile,
	outfile=outfile,
	perl=perl,
	option=option,
	boot=boot,
	urgent=urgent,
	N=paste('Run',b,sep=''),
	o=rdir,
	e=rdir,
	SGEflgs=SGEflgs,
	l=l,
	mod=mod,
	hold=hold,
	...
  )
  if(split){
	command[[1]] <- theCommand(
	    NMcom=NMcom,
	    ctlfile=ctlfile,
	    perl=perl,
	    option=option,
	    boot=boot,
	    urgent=urgent,
	    N=paste('Run',b,sep=''),
	    o=rdir,
	    e=rdir,
	    SGEflgs=SGEflgs,
	    l=TRUE,
	    mod='c',
	    hold=FALSE,
	    ...
	)
        command[[2]] <- theCommand(
	    NMcom=NMcom,
	    ctlfile=ctlfile,
	    perl=perl,
	    option=option,
	    boot=boot,
	    urgent=urgent,
	    N=paste('Run',b,sep=''),
	    o=rdir,
	    e=rdir,
	    SGEflgs=SGEflgs,
	    l=FALSE,
	    mod='e',
	    hold=TRUE,
	    ...
	)
  }
  if(!is.null(udef))command <- udef #trumps above

  #set up the call
  execute <- function(command,intern,minimized,invisible,win){
	args <- list(command, intern=intern)
        if (win()) args <- c(args,list(minimized=minimized, invisible=invisible))
        do.call(system,args)
  }
  lapply(command,execute,intern=intern,minimized=minimized,invisible=invisible,win=win)
}

grdCommand <- function(
        NMcom,
	mod,
	ctlfile,
	outfile,
	boot,
	urgent,
  	N,
	o,
	e,
	nmhome = '/common/NONMEM',
	V='',
	j='y',
	q=NULL,
	SGEflgs=NULL,
	sync = 'y',
	shell='n',
	b='y',
	l=FALSE,
	mod=NULL,
	hold=FALSE,
	...
){
  if(!file.exists(NMcom)){
      #grab the last path element.
      exec <- rev(strsplit(NMcom,'/')[[1]])[[1]]
      #guess the nonmemdir
      nmdr <- gsub('.pl?','',exec)
      #guess the full path and command
      full <- paste(nmhome,nmdr,'test',NMcom,sep='/')
      if(!file.exists(full))stop(paste('not found:\n',NMcom,'\n',full,'\n'))
      NMcom <- full
  }
  NMcom <- paste(NMcom, mod)
  que <- 'all.q'
  if (!urgent) que <- 'bootstrap.q'
  if(is.null(q)) q <- que	  
  if (boot) sync <- 'n'
  if(is.logical(l))if(l) l <- resource(compiler(config(dir(NMcom))),...) else l <- NULL
  if (!is.null(l)) l <- paste('-l',l)
  jid <- NULL
  if(hold) jid <- paste(N,'c',sep='')
  if(!is.null(jid))jid <- paste('-hold_jid',jid)
  command <- paste(
  	'qsub -V',
	'-j',j,
	'-e',e,
	'-o',o,
	'-q',q,
	SGEflgs,
	'-N',paste(N,mod,sep=''),
	l,
	jid,
	'-sync',sync,
	'-shell',shell,
	'-b',b,
	'-cwd',NMcom,
	ctlfile,
	outfile
  )
  if(boot) command <- paste(command,'&')
  command
}

regCommand <- function(
	NMcom,
	ctlfile,
	outfile,
	perl,
	option,
	mod,
	...
)paste(
	perl,
	NMcom,
	mod,
	ctlfile,
	outfile,
	option
)

qsub <- function(#a thin wrapper for SGE qsub, returns a character vector of commandlines.
	command,
	at=NULL,#@
	a=NULL,
	ac=NULL,
	A=NULL,
	b=NULL,
	c=NULL,
	ckpt=NULL,
	clear=NULL,
	cwd=NULL,
	C=NULL,
	dc=NULL,
	dl=NULL,
	e=NULL,
	hard=NULL,
	permutation=NULL,
	h=NULL,
	help=NULL,
	hold_jid=NULL,
	input_stream=NULL,#i
	j=NULL,
	js=NULL,
	l=NULL,
	m=NULL,
	M=NULL,
	masterq=NULL,
	notify=NULL,
	now=NULL,
	N=NULL,
	o=NULL,
	P=NULL,
	p=NULL,
	pe=NULL,
	q=NULL,
	R=NULL,
	r=NULL,
	sc=NULL,
	shell=NULL,
	soft=NULL,
	sync=NULL,
	S=NULL,
	t=NULL,
	terse=NULL,
	u=NULL,
	v=NULL,
	verbose=NULL,
	verify=NULL,
	V=NULL,
	w=NULL,
	wd=NULL,
	...
){
	args <- names(formals(qsub))
	args <- args[sapply(args,function(x)!is.null(get(x)))]
	args <- args[sapply(args,function(x)is.character(get(x)))]
	args <- args[!args %in% c('permutation','hard','soft','command','...')]
	if(!is.null(permutation) & length(permutation)==length(args))args <- args[permutation]
	vals <-  sapply(args,function(x)get(x))
	soft <- args %in% soft
	hard <- args %in% hard
	if(length(args))args <- paste(sep='','-',args)	
	if(!is.null(soft))args[soft] <- paste('-soft',args[soft])
	if(!is.null(hard))args[hard] <- paste('-hard',args[hard])
	args <- sub('input_stream','i',args)
	args <- sub('at','@',args)
	names(vals) <- args
	vectors <- c(as.list(args),as.list(vals))
	vectors <- vectors[t(matrix(seq(length.out=length(vectors)),ncol=2))]
	string <- do.call(paste,vectors)
	result <- paste('qsub',string,command)
	result <- gsub('-[^ ]* NA','',result)
	result
}
#qsub('nm.pl',at='options',input_stream='stdin',hold_jid=c(NA,'run3'),permutation=c(1,3,2),hard='at',soft=c('input_stream','N'),N='misc')
#[1] "qsub -hard -@ options  -soft -i stdin -soft -N misc nm.pl"              
#[2] "qsub -hard -@ options -hold_jid run3 -soft -i stdin -soft -N misc nm.pl"


nmdir <- function(nmcom,pathsep='/',...)paste(rev(rev(strsplit(nmcom,pathsep)[[1]])[-(1:2)]),collapse=pathsep)#recover the directory from a fully qualifed path to a execution wrapper
nmcom <- function(nmdir,pathsep='/',...)paste(sep=pathsep,nmdir,'test',paste(rev(strsplit(nmdir,pathsep)[[1]])[[1]],'pl',sep='.'))
config <- function(nmdir,pathsep='/',...)paste(nmdir,'test','config.xml',sep=pathsep)#identify the configfile in a NONMEM installation
compiler <- function(config,pathsep='/',...){#identify the compiler as the first word in the nmtran instruction
	tree <- xmlParse(config)
	nmtran <- xmlValue(getNodeSet(tree,"//d:instruction[@id='nmtran']/text()",c(d='http://metruminstitute.org/nmqual/configuration'))[[1]])
	nmtran <- sub('^ *','',nmtran)
	comp <- strsplit(nmtran,' ')[[1]]
	rev(strsplit(comp,pathsep)[[1]])[[1]]
}
resource <- function(compiler,mappings=list(ifort=1),hard=TRUE,...){}#develop a value for -l, e.g. -l hard compile=1
nm.pl <- function(
	dir,
	infile,
	outfile=NULL,
	perl='perl',
	search=TRUE,
	checksum=TRUE,
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	...
){


}#a character vector of commandlines to run nm.pl.










































