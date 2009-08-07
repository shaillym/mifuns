epilog <- function(...){
#User code to plot structural/statistical diagnostic plots and/or 
#plots to evaluate covariate-parameter relationships and/or
#any plots user would like to create with their own code


# Important variables that are available for plot script use include:

# ProjectDir - Main project directory that contains NONMEM table files (directory referenced in call to NONR)
# cont.cov - if defined in NONR
# cat.cov - if defined in NONR
# par.list - if defined in NONR
# eta.list - if defined in NONR
# b  - ctl stream number 

#Both NONR (runmsge) and PLOTR can call epilog, and both supply everything needed
#for dataSynthesis().  dataSynthesis scavenges columns from the TAB file, the par.TAB
#file, and the underlying dataset, reporting just observation records that are
#not commented.  Limiting to one record per ID is appropriate for some types of plotting.
#Columns to scavenge are supplied by grp, cont.cov, cat.cov, par.list, and eta.list.
#All TAB file columns are returned, plus first instance of 'scavenge' columns from 
#either the parent data set or the par file (in that order) unless already present.

#dataSynthesis() returns precisely the dataset that is passed to the standard plots.
    data <- dataSynthesis(...)
        b,
        ProjectDir,
        dvname=dvname,
	logtrans=logtrans,
	grp=grp,
	grpnames=grpnames,
	cont.cov=cont.cov,
	cat.cov=cat.cov,
	par.list=par.list,
	eta.list=eta.list,
	missing=missing,
	...
    )
    numDV<-c("DV","LDV","AMT")  # vector of variable names that should be numeric in data
    # loop to change variables in numDV to numeric
    for(k in numDV) data[[k]]<-as.numeric(as.character(data[[k]]))
    subj<-data[!duplicated(data$ID),]

    # user supplied name of output pdf file
    pdf(file=paste(ProjectDir,"/TestPlots_",b,".pdf",sep=""))

    #The plotting code below can be as complicated or as simple as is required. 

    # plot of observed PK vs Time based on Nonmem data set
    plot(data$TIME, data$DV, main="PK obs", xlab="Time", ylab="PK measure")
     
    # histogram of ETA1 based on Nonmem table file (XXpar.TAB)       
    hist(subj$ETA1, main="Histogram of ETA1")

    dev.off()
}
