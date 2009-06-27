`doCov` <-
function (
	b,
	ProjectDir=getwd(),
	cont.cov=NULL, 
	cat.cov=NULL,
	par.list=NULL, 
	eta.list=NULL, 
	covfile=NULL, 
	tabfile=NULL,
	parfile=NULL,
	missing=-99,
	...
) 
{
    plots <- list()
    missing <- as.numeric(as.character(missing))
    nonmdir <- filename(ProjectDir, b)
    covplt = !!length(c(cat.cov,cont.cov))
    if (!covplt) message(paste('No covariates specified for run ', b, '.', sep = ''))
    if (!covplt) return(plots)
    if (is.null(tabfile)) tabfile <- getTabs(filename(ProjectDir, b, '.TAB'),b,ProjectDir)
    if (is.null(covfile)) covfile <- getCovs(getdname(filename(nonmdir,b,'.ctl'),nonmdir)
    if (is.null(parfile)) parfile <- getPars(filename(ProjectDir, b, 'par.TAB'))
    if (is.null(parfile)) return(plots)
    par.list <- intersect(par.list,names(parfile))
    eta.list <- intersect(eta.list,names(parfile))
    all <- union(par.list,eta.list)
    if(length(all))covfile <- stableMerge(covfile,parfile[, c("ID",all)])
    cont <- cont.cov
    cat <- cat.cov
    cont <- intersect(cont, names(covfile))
    cat <- intersect(cat, names(covfile))
    temp <- filename(nonmdir, NULL, 'cov.datat')
    write.table(
    	covfile,
	file=temp,
        sep = ",", 
        quote = FALSE, 
        row.names = FALSE, 
        col.names = TRUE, 
        append = FALSE, 
        na = "."
    )
    for(col in cont){
    	covfile[[col]] <- as.numeric(as.character(covfile[[col]]))
    	covfile[[col]][!is.na(covfile[[col]]) & covfile[[col]]==missing] <- NA
    }
    #Covariate SPLOM
    if (length(cont) >= 2)plots$covSplom <- splom(
    	covfile[, cont], 
    	panel = function(x, y) {
        	panel.splom(x, y)
            panel.lines(lowess(x,y))
        },
        main="Covariate Scatterplots",
        xlab="",
        pscales=0
    )
    #Cont vs cat bwpots
    if (length(cont) & length(cat)) {
        molten <- melt(covfile,measure.var=cont,id.var=cat)
        names(molten)[names(molten)=="variable"] <- "cont"
    	names(molten)[names(molten)=="value"] <- "y"
    	plasma <- melt(molten,measure.var=cat)
    	names(plasma)[names(plasma)=="variable"] <- "cat"
    	names(plasma)[names(plasma)=="value"] <- "x"
    	plots$contCat <- bwplot(
    		y ~ factor(x) | cont + cat,
    		plasma,
    		as.table=TRUE,
    		layout=c(2,2),
    		horizontal=FALSE,
    		ylab="continuous covariate",
    		xlab="categorical covariate",
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
    		main="Continuous Covariates vs. Categorical Covariates"
    	)
    }
    #ETA SPLOM
	if (length(eta.list) >= 2) {
        plots$etaSplom <- splom(
        	covfile[, eta.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.lines(lowess(x,y))
        	},
        	main="ETA Scatterplots",
        	xlab="",
        	pscales=0
        )
    }
    #Parmater SPLOM
    if (length(par.list) >= 2) {
        plots$paramSplom <- splom(
        	covfile[, par.list], 
        	panel = function(x, y) {
            	panel.splom(x, y)
            	panel.lines(lowess(x,y))
        	},
        	main="Parameter Scatterplots",
        	xlab="",
        	pscales=0
        )
    }
    #ETA Histograms
    if(length(eta.list)){
    	etas <- melt(covfile,measure.var=eta.list)
    	plots$etaHist <- histogram(
    		~ value | variable,
    		etas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Histograms of Etas",
    		breaks=NULL,
    		scales=list(relation="free")
    	)
	}
    #ETA Densityplots
    if(length(eta.list)){
    	etas <- melt(covfile,measure.var=eta.list)
    	plots$etaDens <- densityplot(
    		~ value | variable,
    		etas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Density of Etas",
    		scales=list(relation="free")
    	)
	}
    #ETA vs Categoricals
    if(length(cat) && length(eta.list)){
    	etas <- melt(covfile,measure.var=eta.list,id.var=cat)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCat <- bwplot(
    		delta ~ factor(value) | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Boxplots of Etas by Categorical Covariate",
    		horizontal=FALSE,
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...),
    		ylab="ETA",
    		xlab="categorical covariate level"
    	)
    }
    #ETAS vs. Continuous
    if (length(cont) && length(eta.list)) {
    	etas <- melt(covfile,measure.var=eta.list,id.var=cont)
    	names(etas)[names(etas)=="variable"] <- "eta"
    	names(etas)[names(etas)=="value"] <- "delta"
    	condEtas <- melt(id.var=c("eta","delta"),etas)
    	plots$etaCont <- xyplot(
    		delta ~ value | variable + eta,
    		condEtas,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="Etas vs. Continuous Covariates",
    		ylab="ETA",
    		xlab="continuous covariate",
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.lines(lowess(x,y),lty=2,col="red",...)
    		}
    	)
    }
    #CWRES
    if(length(union(cont,cat)))tabfile <- stableMerge(tabfile,covfile[,c("ID",union(cont,cat))])
    #CWRES vs. Categoricals
    if("CWRES" %in% names(tabfile) && length(cat)){
    	res <- melt(tabfile,id.var="CWRES",measure.var=cat)
    	plots$cwresCat <- bwplot(
    		CWRES ~ factor(value) | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Categorical Covariates",
    		xlab="categorical Covariate",
    		ylab="conditional weighted residuals",
    		scales=list(relation="free"),
    		prepanel=function(x,y,...)prepanel.default.bwplot(factor(x),y,...),
    		panel=function(x,y,...)panel.bwplot(factor(x),y,...)
    	)
    }
    #CWRES vs. Continuous
    if("CWRES" %in% names(tabfile) && length(cont)){
    	res <- melt(tabfile,id.var="CWRES",measure.var=cont)
    	plots$cwresCont <- xyplot(
    		CWRES ~ value | variable,
    		res,
    		as.table=TRUE,
    		layout=c(2,2),
    		main="CWRES vs. Continuous Covariates",
    		xlab="continuous covariate",
    		ylab="conditional weighted residuals",
    		scales=list(relation="free"),
    		panel=function(x,y,...){
    			panel.xyplot(x,y,...)
    			panel.abline(h=0)
    			panel.lines(lowess(x,y),lty=2,col="red",...)
    		}
    	)
    }
    if (file.exists(temp)) file.remove(temp)
    plots
}