`ComputePredictiveCheck` <-
function(NonmemData,SimulatedData,
                 SimpleFun =c("min","max","mean","median"),
                 CombFun = c("Tmin","Tmax","AUC"),
                 StatFun = c("min","max","mean","median","Q25","Q75"),
                 IDname="ID",TimeName="TIME",DVname="DV",CName="C",Ignore="C") {
    if(!is.element(IDname,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",IDname,"column"))
    if(!is.element(DVname,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",DVname,"column"))
    if(!is.element(TimeName,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",TimeName,"column"))
    if(!is.element(CName,names(NonmemData))) stop(paste(
                                    "NonmemData does not contain",CName,"column"))
    if(!is.element("MDV",names(NonmemData))) stop(paste(
                                    "NonmemData does not contain","MDV","column"))

    if(IDname != "ID") NonmemData$ID <- as.character(NonmemData[,IDname])
    if(DVname != "DV") NonmemData$DV <- NonmemData[,DVname]
    if(TimeName != "TIME") NonmemData$TIME <- NonmemData[,TimeName]
    if(CName != "C") NonmemData$C <- as.character(NonmemData[,CName])
    NonmemDataSet <- NonmemData[NonmemData$C != Ignore,]
    NonmemData$STUDY <- rep(0,length(NonmemData$ID))

    if(!is.element(DVname,names(SimulatedData))) stop(paste(
                                       "SimulatedData does not contain",DVname,"column"))
    if(DVname != "DV") SimulatedData$DV <- SimulatedData[,DVname]

    nrec <- length(NonmemData$ID)
    nsimrec <- length(SimulatedData$DV)
    nsim <- nsimrec/nrec
    if(nsim != round(nsim)) stop("Error: computed number of simulations is not an integer")

    message("Simulated data set contains data for ",nsim," simulated trials")
    message("The following individual parameters will be computed for each patient:")
    message(paste(c(SimpleFun,CombFun),collapse=" "))

    message("The following statistics will be computed for each study:")
    message(paste(StatFun,collapse=" "))

    message("Computation may take a long time; please, be patient")

    SimulatedData$ID <- as.character(rep(NonmemData$ID,times=nsim))
    SimulatedData$TIME <- rep(NonmemData$TIME,times=nsim)
    SimulatedData$STUDY <- rep(1:nsim,each=nrec)
    SimulatedData$ID <- paste(SimulatedData$STUDY,SimulatedData$ID,sep="-")
 

   NonmemData    <- NonmemData[NonmemData$MDV == 0,]
   SimulatedData <- SimulatedData[NonmemData$MDV == 0,]
   CombinedData <- rbind(NonmemData[,c("ID","STUDY","DV","TIME")],
                      SimulatedData[,c("ID","STUDY","DV","TIME")])
# compute PK parameters:
    Combined <- CombinedData[!duplicated(CombinedData$ID),c("ID","STUDY")]

    if(length(SimpleFun) > 0) for(i in 1:length(SimpleFun) ) {
       ftemp <- function(x){do.call(SimpleFun[i],list(x))}
 
       temp <- aggregate.data.frame(CombinedData[,"DV"], 
                                    by=CombinedData[,"ID",drop=FALSE],FUN=ftemp)
       names(temp) <- c("ID",SimpleFun[i])
       temp$ID <- as.character(temp$ID)
       Combined <- merge(Combined,temp)
       message("Individual parameter ", SimpleFun[i]," computed")

   }
    if(length(CombFun) > 0)for(i in 1:length(CombFun) ) {
       ftemp <- function(x){do.call(CombFun[i],list(data=x))}
 
       temp <- ftemp(CombinedData)
       names(temp) <- c("ID",CombFun[i])
       temp$ID <- as.character(temp$ID)
       Combined <- merge(Combined,temp)
       message("Individual parameter ", CombFun[i]," computed")
  }
 
   Stats <- list()
   for(i in 1:length(StatFun)) {
      ftemp <- function(x){do.call(StatFun[i],list(x))}
      Stats[[i]] <- aggregate.data.frame(Combined[,c(SimpleFun,CombFun)],
                                     by=Combined[,"STUDY",drop=FALSE],FUN=ftemp)
      names(Stats[[i]]) <- c("STUDY",SimpleFun,CombFun)
      message("Summary statistic ", StatFun[i]," computed")
   } 
   Combined <- Combined[order(Combined$STUDY,Combined$ID),]
   names(Stats) <-  StatFun
   message("Output is the list with two elements: Combined and Stats")
   message("Combined is the data frame that contains individual parameters")
   message("Stats is the list, a collection of data frames, one per statistics requested")
   message("Each data frame contains summaries of individual  parameters by  study")
   message("Study 0 refers to the observed data")

   return(list(Combined=Combined,Stats=Stats))                          
}
`PlotPredictiveCheck` <-
function(PCData,
                 Parameters=c("min","max","mean","median","Tmin","Tmax","AUC"),
                 Summaries = c("min","max","mean","median","Q25","Q75"),
                 plot.hist=TRUE,plot.qq=TRUE,DrugName = "DV",plotType="pdf",
                 FileTemplate="Plots"){
 if(plotType == "wmf") {
    plotDev <- win.metafile
    FileName <- paste(FileTemplate,"%03d.wmf",sep="")
 }
 if(plotType == "pdf") {
    plotDev <- pdf
    FileName <- paste(FileTemplate,".pdf",sep="")
 }
 if(plotType == "ps") {
    plotDev <- postscript
    FileName <- paste(FileTemplate,".ps",sep="")
 }
 if(plotType == "png") {
    plotDev <- png
    FileName <- paste(FileTemplate,"%03d.png",sep="")
 }
 if(plotType == "jpeg") {
    plotDev <- jpeg
    FileName <- paste(FileTemplate,"%03d.jpeg",sep="")
 }
 if(plotType == "bmp") {
    plotDev <- bmp
    FileName <- paste(FileTemplate,"%03d.bmp",sep="")
 }

 Combined <- PCData$Combined
 Stats <- PCData$Stats

 Parameters <- Parameters[is.element(Parameters,names(Combined[c(-1,-2)]))]
 Summaries <- Summaries[is.element(Summaries,names(Stats))]
 plotDev(FileName)
 if(plot.hist) for(ipar in Parameters) for(isum in Summaries) {
     parName  <- paste(isum,ipar,DrugName)
     Observed  <- Stats[isum][[1]][,ipar][Stats[isum][[1]][,"STUDY"] == 0]
     Simulated <- Stats[isum][[1]][,ipar][Stats[isum][[1]][,"STUDY"] != 0]
     p.est <- length(Simulated[Simulated >= Observed])/length(Simulated)
     p.est <- 2*min(p.est,1-p.est)
     if(all(Simulated == Simulated[1])) {
          plot(x=1,y=1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
          text(x=1,y=1.2,labels=paste("Parameter:",parName))
          text(x=1,y=1.0,labels=paste("All simulated values are equal to",Simulated[1]))
          text(x=1,y=0.8,labels=paste("Observed value is equal to",Observed))
     }else{
         hist(Simulated,probability=TRUE,main=paste(parName," observed = ",round(Observed,1),
               " (p = ",round(p.est,4),")",sep=""),xlab=paste("Simulated",parName))
         abline(v=Observed)
     }
   }

 if(plot.qq) for(ipar in Parameters) {
     qqi<-function(x)(qqplot(x,Combined[,ipar][Combined[,"STUDY"]==0],plot=FALSE))
     test<-tapply(Combined[,ipar], Combined[,"STUDY"], qqi)
     test.x <- NULL
     test.y <- NULL
     for(i in 2:length(test)) {
        test.x<-cbind(test.x,test[[i]]$x)
        test.y<-cbind(test.y,test[[1]]$y)
     }
  
     matplot(as.matrix(test.y),as.matrix(test.x), type="l", lty=1,
          xlab=paste("Observed",ipar),ylab=paste("Simulated", ipar)) 
     abline(0,1, lwd=2)

   }
 dev.off()
 message("Results are saved in the file(s) ",FileName)
 return(NULL)
}

