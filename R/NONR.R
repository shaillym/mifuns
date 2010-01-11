`NONR` <-  
function (
	run, 
	command, 
	ProjectDir = getwd(), 
	boot = FALSE,
	concurrent = grid,
	urgent = !boot,
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
	invisible=udef,
	checksum = TRUE, 
	grid = boot, 
	nice= FALSE, 
	udef= FALSE, 
	file= NULL,
	compile = TRUE,
	execute = TRUE,
	split = grid & compile & execute,
	...
){
    if (win())  grid <- FALSE
    if (win())  concurrent <- FALSE
    run <- unique(run)
	    
    for (each in run) {
        args <- list(
		run = each, 
		command = command, 
		ProjectDir = ProjectDir, 
		boot = boot,
		urgent = urgent,
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
		checksum = checksum, 
		grid = grid, 
		nice = nice,
		udef = udef, 
		file = file,
		split = split,
		compile = compile,
		execute = execute,
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
