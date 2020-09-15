#Example script for function to find average values of metrics from discharge package. 



#Ethan Baruch

library(discharge)
library(waterData)
library(tidyverse)


####Import data, run DFFT####

#####Import flow from USGS and format for discharge
#Load 20 years of flow data for each site: Some DFFT functions don't work well with >20 years

#Import Agua Fria
AGUAFRIA<- importDVs("09512500", code="00060", stat = "00003", 
                     sdate = "1998-01-01", edate = "2018-12-31")
#format for discharge package: Date (YYY-MM-DD) in column1, CFS (val) in column 2
AGUAFRIA<-select(AGUAFRIA, Date=dates, CFS= val)

#Run DFFT
AGUAFRIA.flow<-asStreamflow(AGUAFRIA,river.name="Agua Fria")
AGUAFRIA.seas<-fourierAnalysis(AGUAFRIA.flow)
#plot(AGUAFRIA.seas)


###Import San Pedro
SANPEDRO<- importDVs("09471000", code="00060", stat = "00003", 
                     sdate = "1998-01-01", edate = "2018-12-31")
SANPEDRO<-select(SANPEDRO, Date=dates, CFS= val)

###San Pedro DFFT
SANPEDRO.flow<-asStreamflow(SANPEDRO,river.name="SAN PEDRO")
SANPEDRO.seas<-fourierAnalysis(SANPEDRO.flow)
#plot(SANPEDRO.seas)

#Repeat for all sites. Or can load data from saved files



#####Make DFFTavg function####
#Metrics to use: FPExt, HSAF, HSAM, LSAF, LSAM, NAA, rms.noise, rms.signal, snr, Zdays,
#Seasonal, HFsigma, LFsigma
#All metrics that are not origionally composite (such as HFsigma, SNR, etc.) are averaged over 20 years
#could save annual values of metrics (such as HSAM) by not averaging


DFFTavg<- function(X.flow, X.seas, SiteName){
  X.bl<-prepareBaseline(X.flow)
  #calculate FPext, HSAF, HSAM, LSAF, LSAM, NAA, HFsigma, and LFsigma
  X.FPExt<-getFPExt(X.bl$resid.sig, X.flow$data$year)
  X.HSAF<-getHSAF(X.bl$resid.sig, X.flow$data$year)
  X.HSAM<-getHSAM(X.bl$resid.sig, X.flow$data$year)%>%
    select(year, HSAM)
  X.LSAF<-getLSAF(X.bl$resid.sig, X.flow$data$year)
  X.LSAM<-getLSAM(X.bl$resid.sig, X.flow$data$year)%>%
    select(year, LSAM)
  X.NAA<-getNAA(X.bl$resid.sig, X.flow$data$year)
  # Calculate variance of raw discharge data
  X.CV<-(sd(X.flow$data$discharge) / mean(X.flow$data$discharge)) * 100
  # Calculate average annual discharge in m^3
  X.AvgYrQ<-group_by(as.data.frame(X.flow$data), year) %>% 
    # *86400 (sec) convert to daily discharge, *0.0283168 convert to m^3
    summarise(YrQ = sum(discharge*86400)* 0.0283168) %>% 
    summarise(AvgYrQ_m3 = mean(YrQ))
  X.HFsigma<-sigmaHighFlows(X.flow) #Use X.HFsigma$sigma.hfb
  X.LFsigma<-sigmaLowFlows(X.flow) #Use X.LFsigma$sigma.hfb
  X.Zdays<-X.seas$signal%>% #Average number of zero flow days per year
    group_by(year)%>%
    summarize(Zdays=sum(discharge <0.1))
  #Find average of each DFFT metric
  X.fftavg<- bind_cols(Site=SiteName, HSAM=mean(X.HSAM$HSAM, na.rm=T),LSAM=mean(X.LSAM$LSAM, na.rm=T),
                       HSAF=mean(X.HSAF$HSAF, na.rm=T), LSAF=mean(X.LSAF$LSAF, na.rm=T), FPExt=mean(X.FPExt$FPExt),
                       NAAmean=mean(X.NAA$NAA),NAAvar=var(X.NAA$NAA), HFsigma=mean(X.HFsigma$sigma.hfb), LFsigma=mean(X.LFsigma$sigma.lfb))
  X.fftavg<-bind_cols(X.fftavg, rms.signal=X.seas$rms$rms.signal,
                      rms.noise=X.seas$rms$rms.noise, snr=X.seas$rms$snr, CV=X.CV, AvgYrQ=X.AvgYrQ,
                      Zdays=mean(X.Zdays$Zdays, na.rm=T), Seasonal=X.seas$seasonal)
}



#Average DFFT metrics by site####
Sites.dfftavg<-bind_rows(DFFTavg(AGUAFRIA.flow, AGUAFRIA.seas, "Agua Fria"),
                        DFFTavg(SANPEDRO.flow, SANPEDRO.seas, "San Pedro"))
