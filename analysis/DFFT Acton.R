###DFFT anomalies###

setwd("~/Documents/work groups/RCN/resilience MARSS/Acton")

library(lmom)
library(CircStats)
library("discharge")
library(dataRetrieval)

#############
##Discharge data retrieval from USGS

# Sevenmile Ck at Camden OH
siteNumber<-"03272700"
SevmileInfo<-readNWISsite(siteNumber)
parameterCd<-"00060"
#Raw daily data:
rawDailyData<-readNWISdv(siteNumber,parameterCd,"","")

#parameter code for discharge (ft3/s): 00060
#Setting the start date to ""(no space) will prompt the program to ask for the earliest date, and setting the end date to "" (no space) will prompt for the latest available date.

names(rawDailyData)<-c("agency", "gauge", "Date",  "Qft3s", "code")

rawDailyData$Q_m3s <- rawDailyData[,4]/35.3146662127
  

##read in Q data from LTREB. L_Q_m3s is sum of Q across three streams
#Note: these records were too short to resolve the seasonal pattern
#Update: Beginning 2012-12-31, there are repeated observations on each date. DFFT expects mean daily Q. Fixed this.
#Confirm: Streams tend to dry in Sept/Oct/Nov?
Acton<-read.csv("ActonQ.csv")
Acton$Date<-as.Date(Acton$Date, format="%m/%d/%y")
names(Acton)<-c("Date", "FMQ", "LFQ", "MBQ", "L_Q_m3s")

tsplot<-ggplot( data = Acton, aes( Date, L_Q_m3s )) + 
  geom_line()
  #scale_x_date(limits = as.Date(c("2004-01-01","2005-01-01")))

#Calculate mean daily Q on LTREB-measured Q 
Acton.mdd<-aggregate(x = Acton[c("FMQ", "LFQ", "MBQ", "L_Q_m3s")],
                                FUN = mean,
                                by = list(Date = Acton$Date))

#unlist files to allow DFFT analysis on specific years
#list2env(Qlist ,.GlobalEnv)

###Run DFFT###
#input data file date and discharge only
#from USGS
Acton.Q<-rawDailyData[,c("Date", "Q_m3s")]

#time series plot
#Acton gauge has missing data 2000-2002
tsplot<-ggplot( data = Acton.Q, aes( Date, Q_m3s )) + 
  geom_line()
  #+scale_x_date(limits = as.Date(c("1994-01-01","1995-01-01")))

##Develop rating curve from USGS and LTREB-measured Q
#Align datasets by date
Acton.rating<- merge(x = Acton.Q, y = Acton[ , c("Date", "L_Q_m3s")], by = "Date", all.x=TRUE)

plot(Acton.rating$L_Q_m3s, Acton.rating$Q_m3s)
#poor relationship between USGS gauge and LTREB-measured inputs

#from LTREB
Acton<-Acton.mdd[,c("Date", "L_Q_m3s")]

#note next step doesn't work if dplyr is installed
detach("package:dplyr", unload=TRUE)
Acton.flows<-asStreamflow(Acton,start.date="1994-01-01",end.date="2015-02-04", river.name="Acton [LTREB] 1994-2015")
Acton.seas<-fourierAnalysis(Acton.flows)
summary(Acton.seas)
plot(Acton.seas)
Acton.seas$rms

pos.residuals<-subset(Acton.seas$signal, Acton.seas$signal$resid.sig>=0) # we subset positive anomalies on one side...
neg.residuals<-subset(Acton.seas$signal, Acton.seas$signal$resid.sig<=0) # ...and negative anomalies on another side

library(dplyr)
#compile daily anomalies
vars<-c("date", "discharge", "resid.sig")
neg.res<-neg.residuals[,vars]
pos.res<-pos.residuals[,vars]
Acton.resid<-rbind(neg.res, pos.res)
names(Acton.resid)<-c("Date", "Q", "resid.sig")
resids<-left_join(Acton, Acton.resid, by="Date")
names(resids)<-c("date", "Q_m3s", "Q", "dailyresid")
resids<-left_join(resids, neg.res, by = "date")
resids<-left_join(resids, pos.res, by = "date")
resids<-resids[,-c(3,5,7)]
names(resids)<-c("date", "Q_m3s", "dailyresid", "negresid", "posresid")

resids$negresid[is.na(resids$negresid)]<-0
resids$posresid[is.na(resids$posresid)]<-0

write.csv(resids, "ActonDFFTresid.csv")

ggplot( data = resids, aes( date, posresid )) + geom_line()
#scale_x_date(limits=as.Date(c('1994-01-01','2015-02-04')))+
#ylim(-1, 10000)

##########
###Summaries of anomalies###

##sum of weekly anomalies
#average anomaly, sum of weekly anomalies, max, min
library(zoo)

##Summary stats##
##piecewise approach to summarizin Q anomalies by moving windows##
sumstats <- function(x, na.rm=TRUE) {
  mn  <- mean(x)
  sm  <- sum(x)
  return(c(mn=mn, sm=sm))
}

#previous week, not including current day
Acton.smmnwk<- data.frame(rollapply(data = resids$dailyresid, 
                                     width = list(-1:-7), 
                                     FUN=sumstats,
                                     by.column=FALSE, 
                                     align="right",
                                     fill = NA, 
                                     na.rm = T))

Acton.lsamwk<- data.frame("lsam"=rollapply(data = resids$negresid, 
                                            width = list(-1:-7), 
                                            FUN=min,
                                            align="right",
                                            fill = NA, 
                                            na.rm = T))
Acton.hsamwk<- data.frame("hsam"=rollapply(data = resids$posresid, 
                                            width = list(-1:-7), 
                                            FUN=max,
                                            align="right",
                                            fill = NA, 
                                            na.rm = T))


##Preceding 3 months 
#Sampling is ~every 3 months, except winter. 
#LSAM & HSAM
Acton.smmnm<- data.frame(rollapply(data = resids$dailyresid, 
                                    width = list(-1:-90), 
                                    FUN = sumstats,
                                    by.column=FALSE,
                                    align="right",
                                    fill = NA, 
                                    na.rm = T))

Acton.lsamm<- data.frame("lsam"=rollapply(data = resids$negresid, 
                                           width = list(-1:-90), 
                                           FUN=min,
                                           align="right",
                                           fill = NA, 
                                           na.rm = T))

Acton.hsamm<- data.frame("hsam"=rollapply(data = resids$posresid, 
                                           width = list(-1:-90), 
                                           FUN=max,
                                           align="right",
                                           fill = NA, 
                                           na.rm = T))


#combine weekly, 3-monthly Q stats
ActonQ.cov<-data.frame("date"=resids$date, "Q_m3s"=resids$Q_m3s, "dailyanom"=resids$dailyresid, "mn_mQanom"=Acton.smmnm$mn, "sum_mQanom"=Acton.smmnm$sm, "mHSAM"=Acton.hsamm$hsam, "mLSAM"=Acton.lsamm$lsam, "mn_wkQanom"=Acton.smmnwk$mn, "sum_wkQanom"=Acton.smmnwk$sm, "wkHSAM"=Acton.hsamwk$hsam, "wkLSAM"=Acton.lsamwk$lsam)

write.csv(ActonQ.cov, "ActonQcov.csv")





####################################################################
#Net annual anomalies
#Not applying to Acton
#annual anomalies
auc.pos<-tapply(pos.residuals$resid.sig, pos.residuals$year, sum) # then pool positive anomalies by year 
auc.neg<-tapply(neg.residuals$resid.sig, neg.residuals$year, sum) # and negative anomalies also by year 
NAA<-auc.pos+auc.neg
year<-seq(1946,2017,1)
HudGr.NAA<-data.frame(cbind(year, NAA))
names(HudGr.NAA)<-c("year", "NAA")
plot(HudGr.NAA$year, HudGr.NAA$NAA)
NAA$year<-names(NAA)


