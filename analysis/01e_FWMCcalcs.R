# JMH 24 Mar 2022
# calculate FWMC for all streams

# libraries
library(tidyverse)

# data
sol <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                          "01c_ProcessedConcDataAllSites.csv"), row.names = 1) %>% 
                mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))



# Calculate FWMC ----

## Prepare data ----
# using eq here: https://ncwqr.files.wordpress.com/2017/06/d-time-weighted-and-flow-weighted-mean-concentrations.pdf
# Campbell w/ hubbard brook infilled the conc values with means
# going to have to get sol in the right form
# HJA IS FWMC ALREADY
solFW <- sol %>% 
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

## Calc interval b/w solute smp ----
# This calculates the interval between samples
SiteWsSolnames <-  unique(solFW$SiteWsSol)

# duplicate for loop
solFW2 <- solFW

# loop to calc interval
for(w in 1:length(SiteWsSolnames)){
  # w <- 1
  SiteWsSolnames_w <- SiteWsSolnames[w]
  sol2_w <- solFW[solFW$SiteWsSol == SiteWsSolnames_w,]
  sol2_wDates <- sol2_w$Date
  
  for(t in 2:length(sol2_wDates)){
    # t=2
    t_i <- sol2_wDates[t]
    t_im1 <- sol2_wDates[t-1]
    Interval_ti <- as.numeric(t_i - t_im1)
    sol2_w[t,]$Interval <- Interval_ti # this will be in days
  }
  # put back in new df
  solFW2[solFW2$SiteWsSol == SiteWsSolnames_w,]$Interval <-  sol2_w$Interval
}

## calc components of FWMC ----
solFW3 <- solFW2 %>% 
  mutate(Interval = as.numeric(Interval)) %>% 
  separate(SiteWsSol, sep = "_", into = c("Site", "WS", "Solute", "conc")) %>% 
  # don't really need conc indicator since always mgL
  select(-conc) %>% 
  # make cols a factor
  mutate(across(c(Site, WS, Solute), factor)) %>% 
  mutate(SiteWs = as.factor(paste0(Site,"_", WS))) %>% 
  # calculate FWMC
  # There's a wide range in this interval 1-5649 days
  # <60 = 117653; <30 = 116448; <14 = 101975 <- numbers from the last round of this, but general point stands
  # dim(sol3[sol3$Interval <14,])
  # If Interval <= 30 then calc FWMC otherwise its an NA
  # I'd rather do < 10
  mutate(FWMCtop = ifelse(Interval <= 30, 
                          mgL * Interval * (Q_Ls*60*60*24),
                          as.numeric("NA")), # convert Ls to L/d, as interval is in day
         FWMCbottom = ifelse(Interval <= 30,
                             Interval * (Q_Ls*60*60*24),
                             as.numeric("NA")),
         # this is only for daily stuff - summing for monthly
         FWMC = FWMCtop/FWMCbottom) %>% 
  # HJA is already FWMC - this also circumvents that calculation
  mutate(FWMC = ifelse(Site == "HJA", mgL, FWMC))

## Plot intervals ----
pdf(file.path(here::here("plots"),
              "IntervalsBWconcSmp_20220324.pdf"), width = 10, height = 25)
ggplot(solFW3, aes(y = log10(Interval), x = Date)) +
  geom_point()+
  facet_grid(SiteWs ~ Solute, scales = "free_y") +
  geom_hline(yintercept = log10(14), color = "green") +
  geom_hline(yintercept = log10(30), color = "red") 
dev.off()

ggplot(solFW3, aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y")

hist(log10(solFW3$Interval))


# summarize 2 monthly ----
## Conc & Q ----
solM_Qconc <- solFW3 %>% 
  # make Y and Month cols
  mutate(Y = strftime(Date, format = "%Y"),
         M = strftime(Date, format = "%m")) %>% 
  # average to month
  group_by(SiteWs, Y, M, Solute) %>% 
  summarise(across(c(Q_Ls, mgL), ~mean(.x, na.rm = TRUE)))

## FWMC ----
solM_FWMC <- solFW3 %>% 
  # make Y and Month cols
  mutate(Y = strftime(Date, format = "%Y"),
         M = strftime(Date, format = "%m")) %>% 
  # sum to month
  group_by(SiteWs, Y, M, Solute) %>% 
  summarise(across(c(FWMCtop, FWMCbottom), ~sum(.x, na.rm = TRUE))) %>% 
  mutate(FWMC = FWMCtop/FWMCbottom)

## combine conc, Q, and FWMC ----
solM <- solM_Qconc %>% 
  full_join(solM_FWMC, by = c("SiteWs", "Y", "M", "Solute")) %>% 
  mutate(Date = as.Date(paste0(Y,"-",M, "-01"), format = "%Y-%m-%d")) %>% 
  separate(SiteWs, sep = "_", into = c("Site", "WS"), remove = FALSE) %>% 
  # HJA conc = FWMC, so we have to do this again
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
  filter(!(Site == "BBWM" & Solute == "SRP")) %>% 
  # too many NA's
  filter(!(Site == "SLP" & Solute == "NH4"))

# write.csv(solM, file.path(here::here("data/JMHnewMungedDat"), 
#                          "01e_ProcessedConcQFwmc_AllSites.csv"))
# save.image(file.path(here::here("analysis"), "01ee_FWMCcalcs_Rdat"))
