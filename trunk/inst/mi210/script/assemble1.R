#This script assembles phase 1 data.

#Make sure you are in the script directory, where this files resides.
getwd()

#Load the MIfuns package.
library(MIfuns)

#Read dosing, pk, and demographic data.
dose <- read.csv('../data/original/phase1/dose.csv',na.strings='.',stringsAsFactors=FALSE)
pk   <- read.csv('../data/original/phase1/pk.csv',na.strings='.',stringsAsFactors=FALSE)
dem  <- read.csv('../data/original/phase1/dem.csv',na.strings='.',stringsAsFactors=FALSE)

#Groom the dose data
head(dose)
dose <- as.keyed(dose, key=c('SUBJ','HOUR'))
summary(dose)
#Looks okay.

#Groom the demographic data.
head(dem)
dem <- as.keyed(dem, key='SUBJ')
summary(dem)
#Looks okay.  Note that DOSE is a treatment group, not an actual dose.

#Groom the pk data.
head(pk)
pk <- as.keyed(pk, key=c('SUBJ','HOUR'))
head(pk)
summary(pk)
#Looks okay.  

#Combine these data sources into an NMTRAN-style data set.
#The function 'aug' adds columns on-the-fly.
#The function 'as.nm' sets up a chain reaction that makes sure the
# final result has properties of an NMTRAN data set as described in ?nm.

#Every source must specify DATETIME or HOUR.  All of ours specify HOUR.
#If HOUR is the same for two records, we want, e.g., pk samples to sort 
# before dose records (assumed predose).  SEQ controls the sort order 
# when times and subject identifiers match.

#The '+' operator means "outer join" or "full merge" when the arguments are "keyed" data.frames.
#The '|' operator means "left join" (merge, all.x=TRUE) when the arguments are "keyed" data.frames.

phase1 <- 
	nm() + 
	aug(dose,SEQ=1,EVID=1) + 
	aug(pk,  SEQ=0,EVID=0) | 
	dem

summary(phase1)

#Note 20 predose/zero DV.
# See ?zeroDv
#We comment-out these records.

phase1 <- hide(phase1, where=predoseDv(phase1), why='predose')
summary(phase1)

#We could rearrange columns for convenience and clarity.
spec <- c('C','ID','TIME','SEQ','EVID','AMT','DV')
spec <- c(spec, setdiff(names(phase1),spec))
phase1 <- phase1[,spec]
head(phase1)

#We create a file using write.nm to format NAs specially, etc.
write.nm(phase1,file='../data/derived/phase1.csv')




