`NONR` <-  
function (
	NMcom, 
	b, 
	ProjectDir, 
	boot = 0, 
	SGEflgs = "", 
	checkrunno = TRUE, 
	diag = TRUE, 
	fdata = FALSE, 
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
	dosbox=TRUE,
	nochecksum = FALSE, 
	grid = FALSE, 
	nice=FALSE, 
	udef=NULL, 
	file=NULL,
	...
){
    Platform <- "Windows"
    if (.Platform$OS.type == "unix" & regexpr("apple", version$platform) > 1) Platform <- "Mac"
   
    if (Platform == "Windows") {
        concurrent <- FALSE
        boot <- 0
    }
    if (Platform == "Mac" & grid==FALSE) {
        concurrent <- FALSE
        boot <- 0
    }
    
    if (Platform == "Mac" & grid==TRUE) {
        concurrent <- TRUE
        #boot <- 0
    }
    if (any(!file.exists(filename(ProjectDir, b, ".ctl")))) 
        stop("One or more control stream(s) missing.")
    for (i in b) {
        run.args <- list(NMcom = NMcom, ProjectDir = ProjectDir, 
            b = i, boot = boot, concurrent = concurrent, Platform = Platform, 
            SGEflgs = SGEflgs, checkrunno = checkrunno, diag = diag, 
            fdata = fdata, epilog = epilog, dvname = dvname, 
            logtrans = logtrans, grp = grp, 
            grpnames = grpnames, cont.cov = cont.cov, cat.cov = cat.cov, 
            par.list = par.list, eta.list = eta.list, missing = missing,
            dosbox = dosbox, nochecksum = nochecksum, grid = grid, nice=nice,
            udef = udef, file=file,...)
        if (!concurrent) 
            do.call("runmsge", run.args)
        if (concurrent & boot %in% c(1,3)) 
            do.call("runmsge", run.args)
        if (concurrent & boot %in% c(0,2)) {
            pid <- fork(NULL)
            if (pid == 0) {
                do.call("runmsge", run.args)
                exit()
            }
        }
        message(paste("Run ", i, " complete.", sep = ""))
    }
    message("NONR complete.")
}

