# Plots of conc data for data vis & check
# JMH Mar 22

# Libraries ----
library(tidyverse)

# Data ----
  sol <- read.csv(file.path(here::here("03_generatedData"), 
                          "01c_ProcessedConcDataAllSites.csv"), row.names =1) %>% 
                  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))

# Conc plots ----
  ## all sites ----
  # NOTE: NOT ALL TDP VALUES ARE TDP SEE WATERSHED DATA NOTES SPREADSHEET
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotAllSites.pdf"), width = 25, height = 10)
  ggplot(sol %>%
           pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  
  dev.off()
  
  ## TLW ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotTLW.pdf"), width = 15, height = 10)
  ggplot(sol %>%
             pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "TLW"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y") 
  dev.off()
  
  ## SLP ----
  # DROPPED
  
  ## MEF ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotMEF.pdf"), width = 10, height = 10)
  ggplot(sol %>%
             pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "MEF"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  ## HJA ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotHJA.pdf"), width = 10, height = 10)
  ggplot(sol %>%
             pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "HJA"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  ## HBEF ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotHBEF.pdf"), width = 20, height = 10)
  ggplot(sol %>%
           pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "HBEF"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  ## ELA ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotELA.pdf"), width = 12, height = 10)
  ggplot(sol %>%
           pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "ELA"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  ## DOR ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotDOR.pdf"), width = 25, height = 10)
  ggplot(sol %>%
           pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "DOR"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  ## BBWM ----
  pdf(file.path(here::here("04_plots"),
                "01d_RawConcPlotBBWM.pdf"), width = 5, height = 10)
  ggplot(sol %>%
           pivot_longer(col = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>%
           mutate(solute = fct_relevel(solute,
                                       c("Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "TDP_mgL", "SO4_mgL"))) %>% 
           filter(Site == "BBWM"), aes(y = conc, x = Date, color = Site)) +
    geom_point(size = 0.5) +
    facet_grid(solute ~ SiteWs, scales = "free_y")
  dev.off()
  
  