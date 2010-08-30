.onAttach <-function(lib,pkg)
{
    ver <- as.character(
    	read.dcf(
		file.path(
			lib, 
			pkg, 
			"DESCRIPTION"
		), 
		"Version"
	)
    )
    cat("MIfuns", ver, "loaded\n")
}
