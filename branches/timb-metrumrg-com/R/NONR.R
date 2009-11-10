`NONR` <-  
function (
	NMcom, 
	b, 
	ProjectDir=getwd(), 
	boot = FALSE,
	concurrent = grid,
	urgent = !boot,
	SGEflgs = "", 
	checkrunno = TRUE, 
	diag = TRUE, 
	fdata = TRUE, 
	epilog = NULL, 
	dvname = NULL, 
	logtrans = FALSE,
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99, 
	invisible=!is.null(udef),
	nochecksum = FALSE, 
	grid = boot, 
	nice=FALSE, 
	udef=NULL, 
	file=NULL,
	...
){
    if (win())  grid <- FALSE
    if (win())  concurrent <- FALSE
    b <- unique(b)
	    
    if (any(!file.exists(filename(ProjectDir, b, ".ctl")))) 
        stop("One or more control stream(s) missing.")
    for (i in b) {
        args <- list(
		NMcom = NMcom, 
		ProjectDir = ProjectDir, 
		b = i, 
		boot = boot,
		urgent = urgent,
		SGEflgs = SGEflgs, 
		checkrunno = checkrunno, 
		diag = diag, 
		fdata = fdata, 
		epilog = epilog, 
		dvname = dvname, 
		logtrans = logtrans, 
		grp = grp, 
		grpnames = grpnames, 
		cont.cov = cont.cov, 
		cat.cov = cat.cov, 
		par.list = par.list, 
		eta.list = eta.list, 
		missing = missing,
		invisible = invisible, 
		nochecksum = nochecksum, 
		grid = grid, 
		nice = nice,
		udef = udef, 
		file = file,
		...
	)
        if (concurrent){
            pid <- fork(NULL)
            if (pid == 0) {
                do.call("runNonmem", args)
                exit()
            }
        } else do.call('runNonmem', args)
    }
    message("NONR complete.")
}
nix <- function().Platform$OS.type == 'unix'
win <- function().Platform$OS.type == 'windows'
