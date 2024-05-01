# JMH 24 Mar 22
# Prepare data for MARS analysis

# libaries ----
library(tidyverse)

# data ----
solM <- read.csv(here::here("03_generatedData", "01e_ProcessedConcQFwmc_AllSites.csv"),
                 row.names = 1) %>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))


# df for MARSS ----

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

# Prep MARSS df ----
## cast data df wide ----
# use FWMC data
MARSdf <- solM %>% 
  ungroup() %>% 
  mutate(SiteWs = paste0(Site,"_",WS)) %>% 
  select(SiteWs, Date, Solute, FWMC) %>% 
  pivot_wider(names_from = "Solute", values_from = "FWMC")

## join month and data df's ----
MARSdf2 <- BlankTS %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  full_join(MARSdf, by = c("SiteWs", "Date")) %>% 
  separate(SiteWs, sep = "_", into= c("Site", "WS")) %>% 
  # terminate and end of last water year
  filter(Date <= as.POSIXct(paste0("2019-10-31", format = "%Y-%m-%d")))

## clean up colnames
colnames(MARSdf2)[1:3] <- c("site", "catchment", "date")

## create decimal water year ----
MARSdf2 <- MARSdf2 %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m")),
         dec_water_yr = case_when(
           month >= 10 ~ (year + 1) + (month - 10) / 12,
           month < 10 ~ year + (month + 2) / 12),
         .after = catchment) %>%
  select(!c(year, month, date))

## assign regions to sites ----
MARSdf2 <- MARSdf2 %>%
  mutate(region = case_when(
    site == "HJA" ~ "NW",
    site == "ELA" | site == "MEF" | site == "TLW" | site == "DOR" ~ "NF",
    site == "HBEF" | site == "SLP" ~ "NF",
    site == "BBWM" | site == "CWT" | site == "SEF" ~ "EF",
    site == "LEF" ~ "PR"
  ), .before = site) %>%
  arrange(region, site, catchment, dec_water_yr)


# Plot MARS data ----
pdf(file.path(here::here("04_plots"),
              "01g_MARSdataAllSites.pdf"), width = 25, height = 10)
ggplot(MARSdf2 %>% 
         pivot_longer(cols = Ca:TDP, names_to = "Solute", values_to = "FWMC_mgL"), aes(y =  FWMC_mgL, x = dec_water_yr, color = catchment)) +
  geom_point()+
  geom_line()+
  scale_y_log10() +
  facet_grid(Solute ~ site, scales = "free_y") +
  ylab("Flow-weighted mean conc (mg element/L)")
dev.off()


# Missing data ----

## Notes on stuff that was removed ----
# REMOVED FOR DL ISSUES
# NH4 = 
WStoDropNH4 <-  c("BBWM_EB", "HBEF_WS6", "HJA_GSWS08", "HJA_GSWS09", "TLW_C32", "TLW_C35")
# NO3 = 
WStoDropNO3 <- "HBEF_WS6"
# TDP = 
WStoDropTDP <- c("HBEF_WS6", "HBEF_WS7", "HBEF_WS8", "HBEF_WS9", "SLP_W9")

# REMOVED FOR OTHER REASONS
# very limited SRP data in SLP - remove
# MEF NO3 and NH4 data unreliable
# BBWM SRP
# SLP NH4

## df of missing data ----
MARSmissingDataByWS <- MARSdf2 %>% 
  group_by(site, catchment) %>% 
  # turn on to check number of rows in each WS
  # summarise(across(Date, ~n()))
  # Calcs the % missing rows for each solute
  summarise(across(Ca:TDP, ~round(sum(is.na(.))/408*100,0))) %>%     #there should be 408 rows for each WS
  # NA-out catchment * solute combos that were removed
  mutate(SiteCatch = paste0(site,"_", catchment),
         NH4 = ifelse(SiteCatch %in% WStoDropNH4, as.numeric("NA"), NH4),
         NO3 = ifelse(SiteCatch %in% WStoDropNO3, as.numeric("NA"), NO3),
         TDP = ifelse(SiteCatch %in% WStoDropTDP, as.numeric("NA"), TDP),
         TDP = ifelse(site == "SLP", as.numeric("NA"), TDP),
         NH4 = ifelse(site == "SLP", as.numeric("NA"), NH4),
         NH4 = ifelse(site == "MEF", as.numeric("NA"), NH4),
         NO3 = ifelse(site == "MEF", as.numeric("NA"), NO3),
         TDP = ifelse(site == "BBWM", as.numeric("NA"), TDP),
         TDP = ifelse(site == "MEF", as.numeric("NA"), TDP)) %>% 
  select(-SiteCatch)


# export/save ----
write.csv(MARSmissingDataByWS, file.path(here::here("03_generatedData"), "01g_PercentNAsInMARSdf.csv"))
write.csv(MARSdf2, here::here("03_generatedData", "01g_tbl_solutes_unmanaged_mon_v2.csv"),
          row.names = FALSE)


save.image(file.path(here::here("07_Rdat"), "01gg_MARSdataPrep_Rdat"))

