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
          filter(Date >= CTstart & Date <= CTend) 
    
# still duplicates?
  dim(sol[duplicated(sol[,c("SiteWs", "Date")]) == TRUE,])

# pdf(file.path(here::here("data/JMHnewMungedDat"),
#              "RawConcPlot.pdf"), width = 25, height = 10)
# ggplot(sol %>%
#          pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
#          mutate(solute = fct_relevel(solute,
#                                      c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL"))), aes(y = conc, x = Date, color = Site)) +
#           geom_point(size = 0.5) +
#         facet_grid(solute ~ SiteWs, scales = "free_y")
# 
# dev.off()


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
  separate(SiteWsSol, sep = "_", into = c("Site", "WS", "Solute")) %>% 
  # make cols a factor
  mutate(across(c(Site, WS, Solute), factor)) %>% 
  mutate(SiteWs = as.factor(paste0(Site,"_", WS))) %>% 
  # calculate FWMC
  # There's a wide range in this interval 1-5649 days
  # <60 = 117653; <30 = 116448; <14 = 101975
  # dim(sol3[sol3$Interval <14,])
  # Ecologically, I'd rather do < 10
  mutate(FWMCtop = ifelse(Interval <= 14, 
                          mgL * Interval * (Q_Ls*60*60*24),
                          as.numeric("NA")), # convert Ls to L/d, as interval is in day
         FWMCbottom = ifelse(Interval <= 14,
                             Interval * (Q_Ls*60*60*24),
                             as.numeric("NA")),
         FWMC = FWMCtop/FWMCbottom)



ggplot(sol4, aes(y = Interval, x = Date)) +
  geom_point()+
  facet_grid(SiteWs ~ Solute, scales = "free_y")

ggplot(sol4, aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y")

hist(log10(sol4$Interval))

solM <- sol4 %>% 
  # make Y and Month cols
  mutate(Y = strftime(Date, format = "%Y"),
         M = strftime(Date, format = "%m")) %>% 
  # sum to month
  group_by(SiteWs, Y, M, Solute) %>% 
  summarise(across(c(Q_Ls, mgL, FWMCtop, FWMCbottom), ~sum(.x, na.rm = TRUE))) %>% 
  # make date and calc FWMC
  mutate(Date = as.Date(paste0(Y,"-",M, "-01"), format = "%Y-%m-%d"),
         FWMC = FWMCtop/FWMCbottom) %>% 
  separate(SiteWs, sep = "_", into = c("Site", "WS"), remove = FALSE)


ggplot(solM, aes(y = FWMC, x = mgL, color = Solute)) +
  geom_point() +
  facet_wrap(vars(Solute), scales = "free")


pdf(file.path(here::here("data/JMHnewMungedDat"),
             "FWMCPlot.pdf"), width = 25, height = 10)
ggplot(solM, aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()


save.image(file.path(here::here("analysis"), 
                     "01c_data_munging_bringing2getherRdat"))
# load("01c_data_munging_bringing2getherRdat")







