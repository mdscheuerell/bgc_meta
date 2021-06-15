##-------------------
## required packages
##-------------------
library(here)
library(readr)
library(tidyverse)
library(readxl)
library(GGally)
library(openxlsx)

# Common time period
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days


##-------------------
# Get data
##-------------------
BBWM <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                  "01_BBWMcomb.csv"))

DOR <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_DORcomb.csv"))

ELA <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_ELAcomb.csv"))

HBEF <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_HBEFcomb.csv"))

HJA <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_HJAcomb.csv"))

MEF <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_MEFcomb.csv"))

SEF <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_SEFcomb.csv"))

SLP <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_SLPcomb.csv"))

TLW <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                           "01_TLWcomb.csv"))

sol <- rbind(BBWM, DOR, ELA, HBEF, HJA, MEF, SEF, SLP, TLW) %>% 
          mutate_at(vars(Site, WS), funs(factor)) %>% 
          mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                 SiteWs = as.factor(paste0(Site,"_",WS))) %>% 
          # average to day to get rid of a couple replicate samples
          group_by(SiteWs, Date) %>% 
          summarise(across(Q_Ls:SO4_mgL, ~ mean(.x, na.rm = TRUE))) %>% 
          filter(Date >= CTstart & Date <= CTend) %>% 
          separate(SiteWs, sep =  "_", into = c("Site", "WS"), remove = FALSE) %>% 
          mutate(Site = as.factor(Site),
                 WS = as.factor(WS))
    
# still duplicates?
  dim(sol[duplicated(sol[,c("SiteWs", "Date")]) == TRUE,])

  
# Make conc plots
# all sites
  # NOTE: NOT ALL TDP VALUES ARE TDP SEE WATERSHED DATA NOTES SPREADSHEET
pdf(file.path(here::here("plots"),
             "RawConcPlotAllSites.pdf"), width = 25, height = 10)
ggplot(sol %>%
         rename(TDP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))), aes(y = conc, x = Date, color = Site)) +
          geom_point(size = 0.5) +
        facet_grid(solute ~ SiteWs, scales = "free_y")

dev.off()

# TLW
pdf(file.path(here::here("plots"),
              "RawConcPlotTLW.pdf"), width = 15, height = 10)
ggplot(sol %>%
         rename(TDP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "TLW"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y") 
dev.off()

# SLP
# SRP here is very limited, not sure if SRP or TDP
pdf(file.path(here::here("plots"),
              "RawConcPlotSLP.pdf"), width = 5, height = 10)
ggplot(sol %>%
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "SLP"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# MEF
pdf(file.path(here::here("plots"),
              "RawConcPlotMEF.pdf"), width = 10, height = 10)
ggplot(sol %>%
         rename(TDP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "MEF"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# HJA
pdf(file.path(here::here("plots"),
              "RawConcPlotHJA.pdf"), width = 10, height = 10)
ggplot(sol %>%
         rename(TDP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "HJA"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# HBEF
pdf(file.path(here::here("plots"),
              "RawConcPlotHBEF.pdf"), width = 20, height = 10)
ggplot(sol %>%
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "HBEF"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# ELA
pdf(file.path(here::here("plots"),
              "RawConcPlotELA.pdf"), width = 12, height = 10)
ggplot(sol %>%
         rename(TDP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "ELA"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# DOR
pdf(file.path(here::here("plots"),
              "RawConcPlotDOR.pdf"), width = 25, height = 10)
ggplot(sol %>%
         rename(TP_mgL = "SRP_mgL") %>% 
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "DOR"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()

# BBWM
pdf(file.path(here::here("plots"),
              "RawConcPlotBBWM.pdf"), width = 5, height = 10)
ggplot(sol %>%
         pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
         mutate(solute = fct_relevel(solute,
                                     c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))) %>% 
         filter(Site == "BBWM"), aes(y = conc, x = Date, color = Site)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~ SiteWs, scales = "free_y")
dev.off()


##-------------------
# Calculate FWMC
##-------------------



# using eq here: https://ncwqr.files.wordpress.com/2017/06/d-time-weighted-and-flow-weighted-mean-concentrations.pdf
# Campbell w/ hubbard brook infilled the conc values with means
# going to have to get sol in the right form
sol2 <- sol %>% 
  ungroup() %>% 
  # add Interval col
  mutate(Interval = as.numeric("NA")) %>% 
  # wide to long
  pivot_longer(cols = c(Ca_mgL:SO4_mgL), names_to = "solute", values_to = "mgL") %>% 
  mutate(SiteWsSol = paste0(SiteWs, "_", solute)) %>% 
  select(SiteWsSol, Date, Q_Ls, mgL, Interval) %>% 
  # need to remove NA's
  filter(!is.na(Q_Ls)) %>% 
  filter(!is.na(mgL)) %>% 
  # need to arrange
  arrange(SiteWsSol, Date)

# rename so I have backup
sol3 <- sol2

SiteWsSolnames <-  unique(sol2$SiteWsSol)

for(w in 1:length(SiteWsSolnames)){
  # w <- 1
  SiteWsSolnames_w <- SiteWsSolnames[w]
  sol2_w <- sol2[sol2$SiteWsSol == SiteWsSolnames_w,]
  sol2_wDates <- sol2_w$Date
  
  for(t in 2:length(sol2_wDates)){
    # t=2
    t_i <- sol2_wDates[t]
    t_im1 <- sol2_wDates[t-1]
    Interval_ti <- as.numeric(t_i - t_im1)
    sol2_w[t,]$Interval <- Interval_ti # this will be in days
  }
  # put back in new df
  sol3[sol3$SiteWsSol == SiteWsSolnames_w,]$Interval <-  sol2_w$Interval
}

# calculate components of FWMC
sol4 <- sol3 %>% 
  mutate(Interval = as.numeric(Interval)) %>% 
  separate(SiteWsSol, sep = "_", into = c("Site", "WS", "Solute", "conc")) %>% 
  # don't really need conc indicator since always mgL
  select(-conc) %>% 
  # make cols a factor
  mutate(across(c(Site, WS, Solute), factor)) %>% 
  mutate(SiteWs = as.factor(paste0(Site,"_", WS))) %>% 
  # calculate FWMC
  # There's a wide range in this interval 1-5649 days
  # <60 = 117653; <30 = 116448; <14 = 101975
  # dim(sol3[sol3$Interval <14,])
  # I'd rather do < 10
  mutate(FWMCtop = ifelse(Interval <= 30, 
                          mgL * Interval * (Q_Ls*60*60*24),
                          as.numeric("NA")), # convert Ls to L/d, as interval is in day
         FWMCbottom = ifelse(Interval <= 30,
                             Interval * (Q_Ls*60*60*24),
                             as.numeric("NA")),
  # this is only for daily stuff - summing for monthly
         FWMC = FWMCtop/FWMCbottom) %>% 
  # HJA is already FWMC - this also circumvents the interval
  mutate(FWMC = ifelse(Site == "HJA", mgL, FWMC))


pdf(file.path(here::here("plots"),
              "IntervalsBWconcSmp.pdf"), width = 10, height = 25)
ggplot(sol4, aes(y = log10(Interval), x = Date)) +
  geom_point()+
  facet_grid(SiteWs ~ Solute, scales = "free_y") +
  geom_hline(yintercept = log10(14), color = "red") +
  geom_hline(yintercept = log10(30), color = "green") 
dev.off()

ggplot(sol4, aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y")

hist(log10(sol4$Interval))


# summarize to monthly
# avg conc and and Q data
solM_Qconc <- sol4 %>% 
  # make Y and Month cols
  mutate(Y = strftime(Date, format = "%Y"),
         M = strftime(Date, format = "%m")) %>% 
  # sum to month
  group_by(SiteWs, Y, M, Solute) %>% 
  summarise(across(c(Q_Ls, mgL), ~mean(.x, na.rm = TRUE)))

# sum FWMC to monthly
solM_FWMC <- sol4 %>% 
  # make Y and Month cols
  mutate(Y = strftime(Date, format = "%Y"),
         M = strftime(Date, format = "%m")) %>% 
  # sum to month
  group_by(SiteWs, Y, M, Solute) %>% 
  summarise(across(c(FWMCtop, FWMCbottom), ~sum(.x, na.rm = TRUE))) %>% 
  mutate(FWMC = FWMCtop/FWMCbottom)
 
# combine conc, Q, and FWMC
solM <- solM_Qconc %>% 
  full_join(solM_FWMC, by = c("SiteWs", "Y", "M", "Solute")) %>% 
  mutate(Date = as.Date(paste0(Y,"-",M, "-01"), format = "%Y-%m-%d")) %>% 
  separate(SiteWs, sep = "_", into = c("Site", "WS"), remove = FALSE) %>% 
  # HJA conc = FWMC
  mutate(FWMC = ifelse(Site == "HJA", mgL, FWMC)) %>% 
  ungroup() %>% 
  select(-FWMCtop, -FWMCbottom,-Y,-M) %>% 
  mutate(Site = as.factor(Site),
         WS = as.factor(WS)) %>% 
  # remove SEF
  filter(Site != "SEF") %>% 
  # removing site/solute that are all (or close to all) NAs
  # very limited SRP data in SLP - remove
  filter(!(Site == "SLP" & Solute == "SRP")) %>% 
  # MEF NO3 and NH4 data unreliable
  filter(!(Site == "MEF" & Solute == "NO3")) %>% 
  filter(!(Site == "MEF" & Solute == "NH4")) %>% 
  #no SRP data
  filter(!(Site == "BBWM" & Solute == "SRP")) 
  # change SRP to TDP since that is target
  
  
  
   
ggplot(solM, aes(y = FWMC, x = mgL, color = Solute)) +
  geom_point() +
  facet_wrap(vars(Solute), scales = "free")

# export FWMC plots
# all site
pdf(file.path(here::here("plots"),
             "FWMCPlotAllSites.pdf"), width = 25, height = 10)
ggplot(solM, aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#BBWM
pdf(file.path(here::here("plots"),
              "FWMCPlotBBWM.pdf"), width = 5, height = 10)
ggplot(solM %>% 
         filter(Site == "BBWM"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#DOR
pdf(file.path(here::here("plots"),
              "FWMCPlotDOR.pdf"), width = 25, height = 10)
ggplot(solM %>% 
         mutate(Solute = recode(Solute, "SRP" = "TP")) %>% 
         filter(Site == "DOR"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#ELA
pdf(file.path(here::here("plots"),
              "FWMCPlotELA.pdf"), width = 15, height = 10)
ggplot(solM %>% 
         mutate(Solute = recode(Solute, "SRP" = "TDP")) %>% 
         filter(Site == "ELA"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#HBEF
pdf(file.path(here::here("plots"),
              "FWMCPlotHBEF.pdf"), width = 25, height = 10)
ggplot(solM %>% 
         filter(Site == "HBEF"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#HJA
pdf(file.path(here::here("plots"),
              "FWMCPlotHJA.pdf"), width = 10, height = 10)
ggplot(solM %>% 
         mutate(Solute = recode(Solute, "SRP" = "TDP")) %>% 
         filter(Site == "HJA"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#MEF
pdf(file.path(here::here("plots"),
              "FWMCPlotMEF.pdf"), width = 10, height = 10)
ggplot(solM %>% 
         mutate(Solute = recode(Solute, "SRP" = "TDP")) %>% 
         filter(Site == "MEF"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#SEF - no data
# pdf(file.path(here::here("data/plots"),
#               "FWMCPlotSEF.pdf"), width = 25, height = 10)
# ggplot(solM %>% 
#          filter(Site == "SEF"), aes(y = FWMC, x = Date, color = Site)) +
#   geom_point()+
#   facet_grid(Solute ~ SiteWs, scales = "free_y") +
#   ylab("Flow-weighted mean conc (mg solute/L)")
# dev.off()

#SLP
pdf(file.path(here::here("plots"),
              "FWMCPlotSLP.pdf"), width = 5, height = 10)
ggplot(solM %>% 
         filter(Site == "SLP"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

#TLW
pdf(file.path(here::here("plots"),
              "FWMCPlotTLW.pdf"), width = 15, height = 10)
ggplot(solM %>% 
         mutate(Solute = recode(Solute, "SRP" = "TDP")) %>% 
         filter(Site == "TLW"), aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

# export dataframe
write.csv(solM, file.path(here::here("data/JMHnewMungedDat"), 
                            "02_MonthlyQConcFWMCallSites.csv"))
###########
# make dataframe for MARs
##########
# create blank df with every month

TSxts <- as.character(seq(as.Date("1985-11-01"), length = 301, by = "months"))
# add in site_WS
SiteWs <- levels(solM$SiteWs)
TSxts2 <- rep(TSxts, times = length(SiteWs))
SiteWS2 <- rep(SiteWs, each = length(TSxts))
BlankTS <- as.data.frame(cbind(SiteWS2, TSxts2))
names(BlankTS) <-  c("SiteWs", "Date")



MARSdf <- solM %>% 
  ungroup() %>% 
  mutate(SiteWs = paste0(Site,"_",WS)) %>% 
  select(SiteWs, Date, Solute, FWMC) %>% 
  pivot_wider(names_from = "Solute", values_from = "FWMC")

MARSdf2 <- BlankTS %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  full_join(MARSdf, by = c("SiteWs", "Date")) %>% 
  separate(SiteWs, sep = "_", into= c("Site", "WS"), remove = FALSE) %>% 
  rename(Ca_fwmc_mgL = "Ca", DOC_fwmc_mgL = "DOC", NH4_fwmc_mgL = "NH4",
         NO3_fwmc_mgL = "NO3", SO4_fwmc_mgL = "SO4", 
         # renaming to TDP b/c this is target
         # exceptions: DOR = TP, HBEF = SRP
         TDP_fwmc_mgL = "SRP")

write.csv(MARSdf2, file.path(here::here("data/JMHnewMungedDat"), 
                   "02_Dat4Mars.csv"))


save.image(file.path(here::here("analysis"),
                     "01c_data_munging_bringing2getherRdat"))
# load(file.path(here::here("analysis"),
#                "01c_data_munging_bringing2getherRdat"))







