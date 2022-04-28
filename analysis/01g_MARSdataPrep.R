# JMH 24 Mar 22
# Prepare data for MARS analysis

# libaries ----
library(tidyverse)

# data ----
solM <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                          "01e_ProcessedConcQFwmc_AllSites.csv"), row.names = 1) %>% 
                mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))


# df for MARs ----

## blank df with no missing months ----
# Need to align data with a df with no missing months
# create blank df with every month
# 1985-11-01 to 2019-10-31 - 407 months and 30 days
TSxts <- as.character(seq(as.Date("1985-11-01"), length = 408, by = "months"))
# add in site_WS
SiteWs <- unique(solM$SiteWs)
TSxts2 <- rep(TSxts, times = length(SiteWs))
SiteWS2 <- rep(SiteWs, each = length(TSxts))
BlankTS <- as.data.frame(cbind(SiteWS2, TSxts2))
names(BlankTS) <-  c("SiteWs", "Date")


# cast data df wide
# use FWMC data
MARSdf <- solM %>% 
  ungroup() %>% 
  mutate(SiteWs = paste0(Site,"_",WS)) %>% 
  select(SiteWs, Date, Solute, FWMC) %>% 
  pivot_wider(names_from = "Solute", values_from = "FWMC")

# join month and data df's
MARSdf2 <- BlankTS %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  full_join(MARSdf, by = c("SiteWs", "Date")) %>% 
  separate(SiteWs, sep = "_", into= c("Site", "WS"), remove = FALSE) %>% 
  # terminate and end of last water year
  filter(Date <= as.POSIXct(paste0("2019-10-31", format = "%Y-%m-%d")))

## Plot MARS data ----
pdf(file.path(here::here("plots"),
              "MARSdataAllSites20220324.pdf"), width = 25, height = 10)
ggplot(MARSdf2 %>% 
         pivot_longer(cols = Ca:TDP, names_to = "Solute", values_to = "FWMC_mgL"), aes(y =  FWMC_mgL, x = Date, color = WS)) +
  geom_point()+
  geom_line()+
  scale_y_log10() +
  facet_grid(Solute ~ Site, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg element/L)")
dev.off()


# Missing data ----

## Notes on stuff that was removed ----
# REMOVED FOR DL ISSUES
# NH4 = WStoDropNH4 <-  c("BBWM_EB", "HBEF_WS6", "HJA_GSWS08", "HJA_GSWS09", "TLW_C32", "TLW_C35")
# NO3 = WStoDropNO3 <- "HBEF_WS6"
# TDP = WStoDropTDP <- c("HBEF_WS6", "HBEF_WS7", "HBEF_WS8", "HBEF_WS9", "SLP_W9")

# REMOVED FOR OTHER REASONS
# very limited SRP data in SLP - remove
# MEF NO3 and NH4 data unreliable
# BBWM SRP
# SLP NH4

## df of missing data ----
MARSmissingDataByWS <- MARSdf2 %>% 
  group_by(Site, WS) %>% 
  # turn on to check number of rows in each WS
  # summarise(across(Date, ~n()))
  # Calcs the % missing rows for each solute
  summarise(across(Ca:TDP, ~round(sum(is.na(.))/408*100,0)))   #there should be 408 rows for each WS

# export/save ----
# write.csv(MARSmissingDataByWS, file.path(here::here("data/JMHnewMungedDat"), "01g_NumberOfNAsInMARSdf.csv"))
# write.csv(MARSdf2, file.path(here::here("data/JMHnewMungedDat"), "01g_Dat4MARS_FWMCmgElementL.csv"))
# save.image(file.path(here::here("analysis"), "01gg_MARSdataPrep_Rdat"))
# load(file.path(here::here("analysis"), "01gg_MARSdataPrep_Rdat"))
