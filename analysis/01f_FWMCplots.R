# JMH, 24 Mar 2022
# FWMC plots

# libraries ----
library(tidyverse)

# data ----
solM <- read.csv(file.path(here::here("data/JMHnewMungedDat"), 
                      "01e_ProcessedConcQFwmc_AllSites.csv"), row.names = 1) %>% 
            mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))


# FWMC plots ----
## all site ----
pdf(file.path(here::here("plots"),
              "FWMCPlotAllSites_20220324.pdf"), width = 25, height = 10)
ggplot(solM, aes(y = FWMC, x = Date, color = Site)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## BBWM ----
pdf(file.path(here::here("plots"),
              "FWMCPlotBBWM_20220324.pdf"), width = 5, height = 10)
ggplot(solM %>% 
         filter(Site == "BBWM"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## DOR ----
pdf(file.path(here::here("plots"),
              "FWMCPlotDOR_20220324.pdf"), width = 25, height = 10)
ggplot(solM %>% 
         filter(Site == "DOR"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## ELA ----
pdf(file.path(here::here("plots"),
              "FWMCPlotELA_20220324.pdf"), width = 15, height = 10)
ggplot(solM %>% 
         filter(Site == "ELA"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## HBEF ----
pdf(file.path(here::here("plots"),
              "FWMCPlotHBEF_20220324.pdf"), width = 25, height = 10)
ggplot(solM %>% 
         filter(Site == "HBEF"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## HJA ----
pdf(file.path(here::here("plots"),
              "FWMCPlotHJA_20220324.pdf"), width = 10, height = 10)
ggplot(solM %>% 
         filter(Site == "HJA"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## MEF ----
pdf(file.path(here::here("plots"),
              "FWMCPlotMEF_20220324.pdf"), width = 10, height = 10)
ggplot(solM %>% 
         filter(Site == "MEF"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

##  SEF ----
# no data


## SLP ----
pdf(file.path(here::here("plots"),
              "FWMCPlotSLP_20220324.pdf"), width = 5, height = 10)
ggplot(solM %>% 
         filter(Site == "SLP"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

## TLW ----
pdf(file.path(here::here("plots"),
              "FWMCPlotTLW_20220324.pdf"), width = 15, height = 10)
ggplot(solM %>% 
         filter(Site == "TLW"), aes(y = FWMC, x = Date)) +
  geom_point()+
  facet_grid(Solute ~ SiteWs, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg solute/L)")
dev.off()

# notes on WS/solutes to drop ----
#BBWM: lots of forced values for NH4
# DOR: lots of forced values for SO4 - DROP SO4
# ELA looks fine
# HBEF - Lots of forced values for NH4 and NO3- might want to drop NH4 and TDP for WS7-9
# HBEF why do there seem to be two lines in NH4? Seems like the values for WS7-9 have 2 sig units. Also they seem to have forced a lot to zero.
# HJA- drop TDP
# MEF - fine
# TLW - fine


## export MARS data ----
write.csv(MARSdf2, file.path(here::here("data/JMHnewMungedDat"), 
                             "02_Dat4MARS_FWMCmgElementL.csv"))