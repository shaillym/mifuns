###################################################
### chunk number 1: dir
###################################################
getwd()


###################################################
### chunk number 2: lib
###################################################
library(MIfuns)


###################################################
### chunk number 3: read
###################################################
dose <- read.csv('../data/ph1/source/dose.csv',na.strings='.',stringsAsFactors=FALSE)
pk   <- read.csv('../data/ph1/source/pk.csv',na.strings='.',stringsAsFactors=FALSE)
dem  <- read.csv('../data/ph1/source/dem.csv',na.strings='.',stringsAsFactors=FALSE)


###################################################
### chunk number 4: dose
###################################################
head(dose)
dose <- as.keyed(dose, key=c('SUBJ','HOUR'))
summary(dose)


###################################################
### chunk number 5: dem
###################################################
head(dem)
dem <- as.keyed(dem, key='SUBJ')
summary(dem)


###################################################
### chunk number 6: pk
###################################################
head(pk)
pk <- as.keyed(pk, key=c('SUBJ','HOUR'))
head(pk)
summary(pk)


###################################################
### chunk number 7: assemble
###################################################
phase1 <- 
	nm() + 
	aug(dose,SEQ=1,EVID=1) + 
	aug(pk,  SEQ=0,EVID=0) | 
	dem

summary(phase1)


###################################################
### chunk number 8: hide
###################################################
phase1 <- hide(phase1, where=predoseDv(phase1), why='predose')
summary(phase1)


###################################################
### chunk number 9: zerodv
###################################################
phase1 <- hide(phase1, where=zeroDv(phase1), why='zerodv')
summary(phase1)


###################################################
### chunk number 10: spec
###################################################
spec <- c('C','ID','TIME','SEQ','EVID','AMT','DV')
spec <- c(spec, setdiff(names(phase1),spec))
spec
phase1 <- phase1[,spec]
head(phase1)


###################################################
### chunk number 11: write
###################################################
write.nm(phase1,file='../data/ph1/derived/phase1.csv')


