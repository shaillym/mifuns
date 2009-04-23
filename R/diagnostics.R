`diagnostics` <-
function (grp, grpnames, ProjectDir, b, dataObs, dvname,  covplt) 
{
  plots <-list()
  data <- dataObs
  if (is.null(grp)) {
    data$plotrGroup <- "all"
    grp <- "plotrGroup"
  }
  grp <- intersect(grp,names(data))
  data$grpnames <- factor(
  	do.call(
  		paste,
  		c(
  			as.list(data[,grp,drop=FALSE]),
  			sep=", "
  		)
  	)
  )
  nlevs <- length(levels(data$grpnames))
  if(!is.null(grpnames))if(length(grpnames)==nlevs)levels(data$grpnames) <- grpnames
  if(!is.null(grpnames))if(length(grpnames)!=nlevs)warning(
  	paste(
  		"Run", 
  		b, 
  		"has",
  		nlevs,
  		"grouping levels but",
  		length(grpnames),
  		"grpnames (ignored)." 
  	)
  )
  observed <- melt(data,measure.var=intersect(c("PRED","IPRE"),names(data)),id.var=c("DV","grpnames"))
  observed$variable <- factor(
  	observed$variable,
  	levels=intersect(c("PRED","IPRE"),names(data)),
  	labels=c("population","individual")[c("PRED","IPRE") %in% names(data)]
  )
  resvar <- intersect(c("RES","WRES","CWRES"),names(data))
  resid <- intersect(c("PRED","TIME","grpnames","TAD"),names(data))
  res <- melt(data,measure.var=resvar,id.var=resid)
 groupSubtitle <- function(grp){
  	if(grp[[1]] == "plotrGroup")return(NULL)
  	paste("for",paste(grp,collapse=", "," data"))
  }
  #Observed vs. Predicted 
  for (group in levels(observed$grpnames)){
  plots[[paste('obsPred',group)]] <- xyplot(
  	DV ~ value |  variable, 
  	observed[observed$grpnames==group,],
  	as.table=TRUE,
  	aspect=1,
  	layout=c(2,2),
  	#xlim = with(data[data$grpames==group,],c(min(0, DV, PRED), max(0, DV, PRED))),
  	#ylim = with(data[data$grpnames==group,],c(min(0, DV, PRED), max(0, DV, PRED))),
  	ylab = paste("Observed", dvname), 
  	xlab = paste("Predicted", dvname),
  	panel= function(x,y,...){
  		panel.xyplot(x,y,...)
  		panel.abline(0,1)
  	},
  	main=paste("Model",b,"\nObserved vs. Predicted",groupSubtitle(group))
  )
  #Residuals vs. Predicted
   plots[[paste('resPred',group)]] <- xyplot(
  	value ~ PRED |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Predicted", dvname),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. Predicted",groupSubtitle(group))
  )
  #Residuals vs. Time
  plots[[paste('resTime',group)]] <- xyplot(
  	value ~ TIME |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. Time",groupSubtitle(group))
  )
  #Residuals vs. TAD
  if("TAD" %in% names(res))plots[[paste('resTad',group)]] <- xyplot(
  	value ~ TAD |  variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	ylab = "residuals", 
  	xlab = paste("Time (hr)"),
  	panel= function(x,y,lty=1,...){
  		panel.xyplot(x,y,lty=lty,...)
  		panel.abline(h=0,lty=lty,)
  		panel.loess(x,y,lty=2,...)
  	},
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nResiduals vs. TAD",groupSubtitle(group))
  )
  #QQ-Norm
  plots[[paste('resQ',group)]] <- qqmath(
  	~ value | variable, 
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	aspect=1,
  	ylab = "residuals", 
  	xlab = paste("theoretical quantiles"),
  	panel = function(x, ...) {
    	panel.qqmathline(x, ...)
    	panel.qqmath(x, ...)
    },
  	scales=list(y=list(relation="free")),
  	main=paste("Model",b,"\nNormal Q-Q Plot Residuals",groupSubtitle(group))
  )
  #QQ-Res
  if("CWRES" %in% names(data))plots[[paste('resCwresQ',group)]] <- qq(
  	variable ~ value ,
  	res[res$grpnames==group,],
  	as.table=TRUE,
  	layout=c(2,2),
  	aspect=1,
  	subset=variable %in% c("WRES","CWRES"),
  	panel = function(...) {
    	panel.qq(...)
    	panel.abline(0,1)
    },
  	main=paste("Model",b,"\nQ-Q Plot of CWRES vs. WRES",groupSubtitle(group))
  )
  #Residuals
  plotsRes <- bwplot(
  	value ~ variable,
  	res[res$grpnames==group,],
  	main="Boxplots of Residuals",
  	ylab="residuals"
  )
  }
  plots
}

