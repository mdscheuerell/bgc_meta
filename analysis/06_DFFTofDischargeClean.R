library(tidyverse)
library(discharge)
library(tidyquant)

##########################################################################################
# FUN FOR pos, neg, and residual lags
##########################################################################################

DFFTrollAvgFUN <- function(DFFTl.i, RollingWHAT, window1, window2){
  DFFTl.iRA <- DFFTl.i %>% 
    # resid.sig window1
    tq_mutate(select = resid.sig,
              mutate_fun = rollapply,
              width = window1, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("resid.sig_",window1,"d")) %>% 
    # resid.sig window2
    tq_mutate(select = resid.sig,
              mutate_fun = rollapply,
              width = window2, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("resid.sig_",window2,"d")) %>% 
    # pos.res window1
    tq_mutate(select = pos.res,
              mutate_fun = rollapply,
              width = window1, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("pos.res_",window1,"d")) %>% 
    # pos.res window2
    tq_mutate(select = pos.res,
              mutate_fun = rollapply,
              width = window2, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("pos.res_",window2,"d")) %>% 
    #neg.res window1
    tq_mutate(select = neg.res,
              mutate_fun = rollapply,
              width = window1, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("neg.res_",window1,"d")) %>% 
    # net.sig window2
    tq_mutate(select = neg.res,
              mutate_fun = rollapply,
              width = window2, #window widths, rows not necessarily time!
              alight = "right", #right aligns with the end of the window,
              FUN = RollingWHAT, #just the fun, could use max, min, or something else,
              na.rm = TRUE,
              col_rename = paste0("neg.res_",window2,"d"))
}



##########################################################################################
# Load data
##########################################################################################
Qall <- read.csv("Qall.df.csv") 
Qall2 <- Qall %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"),
         Y = as.numeric(as.character(strftime(Date, format = "%Y")))) %>% 
  mutate_at(vars(SiteID, Site, discharge_units, studyarea, watershed), funs(as.factor(.))) %>% 
  filter(Y >= 1981 & Y <= 2005) %>% # subset years
  # remove SEF too many breaks
  filter(studyarea != "SEF") 

levels(Qall2$studyarea)

# quick check
pdf("06_Qts.pdf", height = 25, width = 5)
ggplot(Qall2, aes(y = discharge_Ls, x = Date)) +
  geom_point(color = "purple", size = 0.25) +
  facet_grid(SiteID ~., scales = "free_y")
dev.off()

# what's going on with HBEF_WS8 & HBEF_WS9? - only 0's and 1's
Qall2.HBEFWS8 <- Qall2 %>% 
  filter(SiteID == "HBEF_WS8")
Qall2.HBEFWS9 <- Qall2 %>% 
  filter(SiteID == "HBEF_WS9")

ggplot(Qall2.HBEFWS8, aes(y = discharge_Ls, x = Date)) +
  geom_point(color = "purple") 
ggplot(Qall2.HBEFWS9, aes(y = discharge_Ls, x = Date)) +
  geom_point(color = "purple") 

#What's going on with HJA_WS7?
# Missing data from 30 Sep 1987 to 1 Oct 1994
Qall2.HJA_WS7 <- Qall2 %>% 
  filter(SiteID == "HJA_WS7")
ggplot(Qall2.HJA_WS7, aes(y = discharge_Ls, x = Date)) +
  geom_point(color = "purple") 
Qall2.HJA_WS7.1990 <- Qall2 %>% 
  filter(SiteID == "HJA_WS7") %>% 
  mutate(Y = as.numeric(as.character(strftime(Date, format = "%Y")))) %>% 
  filter(Y == 1990)

##########################################################################################
# DFFT
##########################################################################################

# Prepare data
Qall3 <- Qall2 %>% 
  select(SiteID, Date, discharge_Ls) %>% 
  mutate(Date = as.Date(Date)) %>% 
  droplevels() 
# this should have 9131 daysfor each site - yup

# transform to list
Qall3l <- split(Qall3, Qall3$SiteID)



# Function that counts unknowns - certainly an easier way
CheckDatUN <- function(X.Q, SiteName){
  #for testing
  # X.Q = Qall3l
  # SiteName = "CWT_WS7"
  
  # prepare df
  X.Q_i <-  as.data.frame(X.Q[SiteName])
  names(X.Q_i) <- c("SiteID", "Date", "discharge_Ls")

    # get df without nas  
  X.Q_i_NoNaS <- X.Q_i %>% #select by site ID
    filter(is.na(discharge_Ls) == FALSE) #remove NA
  
  # determine min and max date without NAs
  X.Q_i_NoNaSminDate <- min(X.Q_i_NoNaS$Date)
  X.Q_i_NoNaSmaxDate <- max(X.Q_i_NoNaS$Date)
  
  # prepare final dataframe missing leading and trailing NAs
  X.Q_i2 <- X.Q_i %>% #select by site ID
    filter(Date >= X.Q_i_NoNaSminDate & Date <= X.Q_i_NoNaSmaxDate) %>%  #remove NA
    droplevels() %>% 
    select(Date, discharge_Ls) %>% 
    arrange(Date) %>% 
    filter(is.na(discharge_Ls))
  c(SiteName, dim(X.Q_i2)[1])
}

SiteNames <- as.vector(unique(as.character(Qall3$SiteID)))
CheckMat <- matrix(nrow = length(SiteNames), ncol = 2)
for(i in 1:length(SiteNames)){
  SiteNames.i <- SiteNames[i]
  Check.i <-  CheckDatUN(Qall3l, SiteNames.i)
  CheckMat[i,] <- Check.i
}

#just ELA_EIF has missing values between 81 and 05
CheckMat


# longest string of missing data is 20 days
Qall3 %>% 
  filter(SiteID == "ELA_EIF") %>% 
  filter(is.na(discharge_Ls))

ggplot(Qall3 %>% 
         filter(SiteID == "ELA_EIF"), aes(y = discharge_Ls, x = Date)) +
  geom_line()


##########################################################################################
# Remove ELA_EIF (bunch of NAs - could be ARIMA'd), 
# HBEF_WS8 (0 & 1's), HBEF_WS9 (0 & 1's), HJA_WS7 (missing >6yr) for now
##########################################################################################
SiteNamesRemoved <- c("ELA_EIF", "HBEF_WS8", "HBEF_WS9", "HJA_WS7")
SiteNamesNoNAs <- SiteNames[!(SiteNames %in% SiteNamesRemoved)]

# DFFT for each site
##########################################################################################
# function to create positive & negative residuals and potential other daily things you want.
##########################################################################################

DFFTdailyFUN <- function(X.Q, SiteName){
  #for testing
  # X.Q = Qall3l
  # SiteName = "CWT_WS7" #"CWT_WS18" has 1 na throws errors with fourier Analysis
  
  # prepare df
  X.Q_i <-  as.data.frame(X.Q[SiteName])
  names(X.Q_i) <- c("SiteID", "Date", "discharge_Ls")
  
  # get df without nas
  X.Q_i_NoNaS <- X.Q_i %>% #select by site ID
    filter(is.na(discharge_Ls) == FALSE) #remove NA
  
  # determine min and max date without NAs
  X.Q_i_NoNaSminDate <- min(X.Q_i_NoNaS$Date)
  X.Q_i_NoNaSmaxDate <- max(X.Q_i_NoNaS$Date)
  
  # prepare final dataframe missing leading and trailing NAs
  X.Q_i2 <- X.Q_i %>% #select by site ID
    filter(Date >= X.Q_i_NoNaSminDate & Date <= X.Q_i_NoNaSmaxDate) %>%  #remove NA
    droplevels() %>% 
    select(Date, discharge_Ls) %>% 
    arrange(Date)
  
  # use discharge package
  X.flow<-asStreamflow(X.Q_i2,river.name= SiteName, max.na=2) #,max.na=10 won't work
  X.seas<-fourierAnalysis(X.flow)
  
  #create final dataframe
  X.seasdf <- X.seas$signal %>% 
    mutate(pos.res = ifelse(resid.sig >= 0, resid.sig, 0),
           neg.res = ifelse(resid.sig <= 0, resid.sig, 0),
           SiteName = SiteName)
  X.seasdf
}

# agh!!!!!! can't get right ofrmat for SiteNames for mapply - sorry Tamara
# DFFTallSites <- mapply(DFFTdailyFUN, X.Q = Qall3l, SiteName = SiteNames2, SIMPLIFY = FALSE)

# For loop for DFFT
DFFTmat <- matrix(ncol = 14, nrow = dim(Qall3)[1])
DFFTmatl <- list(DFFTmat)
for(i in 1:length(SiteNamesNoNAs)){
  SiteNamesNoNAs.i <- SiteNamesNoNAs[i]
  DFFTmat.i <- DFFTdailyFUN(Qall3l,  SiteName = SiteNamesNoNAs.i)
  DFFTmatl[i] <- list(DFFTmat.i)
}

# transform to dataframe
DFFTdf <- do.call("rbind", DFFTmatl)


##########################################################################################
# "LOOP" pos, neg, and residual lags across df's
##########################################################################################
# "LOOP" across items in list using mapply
DFFTlagL <- mapply(DFFTrollAvgFUN, DFFTl.i = DFFTmatl, RollingWHAT = "mean", window1 = 14, window2 =365, SIMPLIFY = FALSE)
# convert to "long" tibble databframe
DFFTlag.tib <- do.call("rbind", DFFTlagL)
# you can see the stream names here
unique(DFFTlag.tib$SiteName)

# just take a look - looks good

pdf("06_DFFTlagPlot.pdf", height = 25, width = 5)
ggplot() +
  # geom_line(data = DFFTlag.tib, aes(y = discharge, x= date), color = "green") +
  geom_line(data = DFFTlag.tib, aes(y = resid.sig, x= date)) +
  geom_point(data = DFFTlag.tib, aes(y = resid.sig_14d, x= date), color = "blue", size = 0.1) +
  geom_point(data = DFFTlag.tib, aes(y = resid.sig_365d, x= date), color = "red", size = 0.1) +
  facet_grid(SiteName ~., scales = "free_y")
dev.off()

##########################################################################################
# Export data
##########################################################################################
write.csv(DFFTlag.tib, "06_DFFTofDischarge.csv")
