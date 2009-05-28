panel.densitystrip <- function(
	x,
	y,
	scale=0.5,
	horizontal=TRUE,
	groups=NULL,
	col='grey',
	border='black',
	...
){
	if (!any(is.finite(x) & is.finite(y))) return()
	x <- as.numeric(x)
	y <- as.numeric(y)
	if(horizontal)dat <- split(data.frame(a=x,b=y),y)
	else dat <- split(data.frame(a=y,b=x),x)
	dat <- lapply(
		dat,
		function(s,scale,horizontal,...){
			extra <- list(...)
			extra <- extra[names(extra) %in% names(formals(density.default))]
			b <- unique(s$b)
			a <- s$a
			d <- do.call(density,c(list(x=a),extra))
			x <- d$x
			y <- d$y
			y <- y/max(y,na.rm=TRUE)
			y <- y * scale
			y <- y + b
			if(horizontal)return(data.frame(x=x,y=y))
			else return(data.frame(x=y,y=x))
		},
		scale=scale,
		horizontal,
		...
	)
	lapply(dat,function(s,fcol='grey',...)with(s,panel.polygon(x,y,col=fcol,border=border,...)),...)
}

panel.ref <- function(x,y,col='grey90',horizontal,rlim,...){
        x <- as.numeric(x)
        y <- as.numeric(y)
	if(horizontal)panel.rect(xleft=rlim[1],ybottom=0,xright=rlim[2],ytop=max(y) + 1,border='transparent',col=col)
	else panel.rect(xleft=0,ybottom=rlim[1],xright=max(x) + 1, ytop=rlim[2],border='transparent',
	col=col)
}
panel.cuts <- function(
	x,
	y,
	cuts,
	horizontal=TRUE,
	groups=NULL,
	offset=-0.2,
	format=function(x)as.numeric(round(x/sum(x)*100)),
	include.range=TRUE,
	zero.rm=TRUE,
	cex=0.7,
	...
){
	if (!any(is.finite(x) & is.finite(y))) return()
	x <- as.numeric(x)
	y <- as.numeric(y)
	dat <- data.frame(x=x,y=y)
	if(!horizontal){
		z <- x
		x <- y
		y <- z
	}
	dat <- split(data.frame(x=x,y=y),y)
	cuts <- cuts[cuts >= min(x,na.rm=TRUE) & cuts <= max(x,na.rm=TRUE)]
	range <- range(x)
	if(include.range) cuts <- c(range(x),cuts)
	cuts <- sort(unique(cuts))
	midpoints <- (cuts[1:length(cuts)-1] + cuts[-1])/2
	dat <- lapply(
		dat,
		function(s,cuts,offset,format,zero.rm,midpoints,...){
			y <- unique(s$y)
			x <- s$x
			count <- bin(x,breaks=cuts,...)
			value <- format(count)
			if(zero.rm)value[value==0] <- NA
			value <- as.character(value)
			value[is.na(value)] <- ''
			y <- y + offset
			if(horizontal)return(data.frame(x=midpoints,y=y,value=value))
			else return(data.frame(y=midpoints,x=y,value=value))
		},
		cuts=cuts,
		offset=offset,
		horizontal=horizontal,
		format=format,
		zero.rm=zero.rm,
		midpoints=midpoints,
		...
	)
	lapply(dat,function(s,...)with(s,ltext(x,y,value,cex=cex,...)),...)
}

panel.covplot <- function(
	x,
	y,
	groups=NULL,
	ref=1,
	rlim=ref * c(0.75,1.25),
	cuts=ref * c(0.75,1,1.25),
	horizontal=TRUE,
	border='black',
	fill='grey',
	text='black',
	shade='grey90',
	col='white',
	...
){
	x <- as.numeric(x)
	y <- as.numeric(y)
	panel.ref(x,y,rlim=rlim,horizontal=horizontal,col=shade,...)
	if(!is.null(groups)){
		panel.superpose(x=x,y=y,groups=groups,horizontal=horizontal,panel.groups=panel.densitystrip,border=border,...)
		panel.superpose(x=x,y=y,groups=groups,cuts=cuts,horizontal=horizontal,col=text,panel.groups=panel.cuts,...)
	}
	else{
		panel.densitystrip(x,y,horizontal=horizontal,col=fill,border=border,...)
		panel.cuts(x,y,cuts=cuts,horizontal=horizontal,col=text,...)
	}
	if(horizontal)args <- list(v=cuts,col=col,...)else args<-list(h=cuts,col=col,...)
	do.call(panel.abline,args)
	if(horizontal)args <- list(v=ref,...)else args<-list(h=ref,...)
	do.call(panel.abline,args)
	
}


