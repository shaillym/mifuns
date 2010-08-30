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
    print("MIfuns", ver, "loaded\n")
}
