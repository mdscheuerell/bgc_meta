# updated: JMH 6 Oct. 21, 23 Mar 22 (new MEF)

# Libraries ----
  library(here)
  library(readr)
  library(tidyverse)
  library(readxl)


# Common time window ----
  CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
  # changed this to include more data, stopping at last full water year
  CTend <- as.POSIXct(paste0("2019-10-31", format = "%Y-%m-%d"))
  # 13,149 - updated 24 sept 21


# Get data ----
# Detection limit data ----
  # prepared by David and Tamara 
  # from info provided by data providers
  LOQ1 <- readxl::read_xlsx(file.path(here::here("01_data"), 
                                     "maximum detection limits.xlsx"),1, 
                           range = "A1:H12") 
  
  names(LOQ1) <- c("Site", "SiteCode", "DL_Ca_mgL", "DL_DOC_mgL", "DL_NH4_N_mgL", "DL_NO3_N_mgL", "DL_TP_TDP_PO4_P_mgL", "DL_SO4_S_mgL")
  
  # HBEF NO3 detection limit updated by J. Campbell
  LOQ1[LOQ1$SiteCode == "HBEF","DL_NO3_N_mgL"] <- as.character("0.013")
  # Sleepers NH4 detection limit updated by Serena Matt
  # BUT THEY'VE ALREADY IMPLEMENTED THIS (ALL VALUES WERE -0.028 WHICH I CHANGED TO + IN EARLIER SCRIPT)
  # Entering 0.029 here so that those 0.28 will be considered below the DL
  LOQ1[LOQ1$SiteCode == "SLP", "DL_NH4_N_mgL"] <- as.character("0.029") 
  
  LOQ <- LOQ1 %>% 
    mutate(across(DL_NH4_N_mgL:DL_TP_TDP_PO4_P_mgL, as.numeric)) %>% 
    # calculate value to replace with if < DL
    # RV = replacement value, which is 1/2 of the detection limit
    # DL = "Detection limit"
    mutate(RV_Ca_mgL = DL_Ca_mgL/2,
           RV_DOC_mgL = DL_DOC_mgL/2,
           RV_NH4_N_mgL = DL_NH4_N_mgL/2,
           RV_NO3_N_mgL = DL_NO3_N_mgL/2,
           RV_TP_TDP_PO4_P_mgL = DL_TP_TDP_PO4_P_mgL/2,
           RV_SO4_S_mgL = DL_SO4_S_mgL/2) %>% 
    select(-Site) %>% 
    #drop sites not used
    filter(!(SiteCode %in% c("COW", "LUQ", "SAN")))
  
# Chem & Q data ----
  BBWM <- read.csv(file.path(here::here("03_generatedData"), 
                    "01_BBWMcomb.csv"))
  
  DOR <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_DORcomb.csv"))
  
  ELA <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_ELAcomb.csv"))
  
  HBEF <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_HBEFcomb.csv"))
  
  HJA <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_HJAcomb.csv"))
  
  MEF <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_MEFcomb.csv"))
  
  SEF <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_SEFcomb.csv"))
  
  SLP <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_SLPcomb.csv"))
  
  TLW <- read.csv(file.path(here::here("03_generatedData"), 
                             "01_TLWcomb.csv"))

# combine solute data ----
  # What TDP really is: BBWM = N/A; DOR = TP; ELA = TDP; HBEF = SRP; HJA = TDP; MEF = TP (DA: little PP so TP = TDP)
  # SLP = N/A (no SRP/TDP data in window); TLW = TP (DA: little PP so TP = TDP)
  
  sol2 <- rbind(BBWM, DOR, ELA, HBEF, HJA, MEF, SEF, SLP, TLW) %>% 
            mutate_at(vars(Site, WS), funs(factor)) %>% 
            mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                   SiteWs = as.factor(paste0(Site,"_",WS))) %>% 
            # average to day to get rid of a couple replicate samples
            group_by(SiteWs, Date) %>% 
            summarise(across(Q_Ls:SO4_mgL, ~ mean(.x, na.rm = TRUE))) %>% 
            # truncate to focal dates, but at end only trimming to the end of water year 2020
            filter(Date >= CTstart & Date <= CTend) %>% 
            separate(SiteWs, sep =  "_", into = c("Site", "WS"), remove = FALSE) %>% 
            mutate(Site = as.factor(Site),
                   WS = as.factor(WS)) %>% 
            # need to drop SEF
            filter(Site != "SEF") %>% 
            rename(TDP_mgL = "SRP_mgL")
  
  sol1 <- sol2 %>% 
          # join with detection limit info
          left_join(LOQ, by = c("Site" = "SiteCode")) %>% 
          # if below DL then use DL/2
          mutate(Ca_LtDl = case_when(Ca_mgL < DL_Ca_mgL ~ "LT_DL",
                                     Ca_mgL >= DL_Ca_mgL ~ "GT_DL", 
                                     is.na(Ca_mgL) ~ "NA"),
                 DOC_LtDl = case_when(DOC_mgL < DL_DOC_mgL ~ "LT_DL",
                                      DOC_mgL >= DL_DOC_mgL ~ "GT_DL",
                                      is.na(DOC_mgL) ~ "NA"),
                 NH4_LtDl = case_when(NH4_mgL < DL_NH4_N_mgL ~ "LT_DL",
                                      NH4_mgL >= DL_NH4_N_mgL ~ "GT_DL",
                                      is.na(NH4_mgL) ~ "NA"),
                 NO3_LtDl = case_when(NO3_mgL < DL_NO3_N_mgL ~ "LT_DL",
                                      NO3_mgL >= DL_NO3_N_mgL ~ "GT_DL",
                                      is.na(NO3_mgL) ~ "NA"),
                 TDP_LtDl = case_when(TDP_mgL < DL_TP_TDP_PO4_P_mgL ~ "LT_DL",
                                      TDP_mgL >= DL_TP_TDP_PO4_P_mgL ~ "GT_DL",
                                      is.na(TDP_mgL) ~ "NA"),
                 SO4_LtDl = case_when(SO4_mgL < DL_SO4_S_mgL ~ "LT_DL", 
                                      SO4_mgL >= DL_SO4_S_mgL ~ "GT_DL",
                                      is.na(SO4_mgL) ~ "NA"),
           # if below DL set to replacement value (RV)
                 Ca_mgL.u = ifelse(Ca_mgL < DL_Ca_mgL, RV_Ca_mgL, Ca_mgL),
                 DOC_mgL.u = ifelse(DOC_mgL < DL_DOC_mgL, RV_DOC_mgL, DOC_mgL),
                 NH4_mgL.u = ifelse(NH4_mgL < DL_NH4_N_mgL, RV_NH4_N_mgL, NH4_mgL),
                 NO3_mgL.u = ifelse(NO3_mgL < DL_NO3_N_mgL, RV_NO3_N_mgL, NO3_mgL),
                 TDP_mgL.u = ifelse(TDP_mgL < DL_TP_TDP_PO4_P_mgL, RV_TP_TDP_PO4_P_mgL, TDP_mgL),
                 SO4_mgL.u = ifelse(SO4_mgL < DL_SO4_S_mgL, RV_SO4_S_mgL, SO4_mgL)) 

# Detection limit examination ----
  ## Prepare data ----
  sol1s <- rbind(sol1 %>% select(SiteWs:WS, LtDl = Ca_LtDl, mgL.u = Ca_mgL.u) %>% mutate(sol = "Ca"),
                 sol1 %>% select(SiteWs:WS, LtDl = DOC_LtDl, mgL.u = DOC_mgL.u) %>% mutate(sol = "DOC"),
                 sol1 %>% select(SiteWs:WS, LtDl = NH4_LtDl, mgL.u = NH4_mgL.u) %>% mutate(sol = "NH4"),
                 sol1 %>% select(SiteWs:WS, LtDl = NO3_LtDl, mgL.u = NO3_mgL.u) %>% mutate(sol = "NO3"),
                 sol1 %>% select(SiteWs:WS, LtDl = TDP_LtDl, mgL.u = TDP_mgL.u) %>% mutate(sol = "TDP"),
                 sol1 %>% select(SiteWs:WS, LtDl = SO4_LtDl, mgL.u = SO4_mgL.u) %>% mutate(sol = "SO4")) %>% 
            group_by(SiteWs, Site, WS, sol, LtDl) %>% 
            summarise(n = n()) %>% 
            pivot_wider(id_cols = c(SiteWs:sol), names_from = LtDl, values_from = n) %>% 
            mutate(LT_DL = ifelse(is.na(LT_DL), 0, LT_DL),
                   GT_DL = ifelse(is.na(GT_DL), 0, GT_DL),
                   PerBelowDL = LT_DL/(LT_DL + GT_DL) *100)
  
  

## Plots of DLs ----
  # NO3 for HBEF seems high - fixed error in units
  pdf(file.path(here::here("04_plots"),
                "01c_PercentBelowDL.pdf"), width = 25, height = 10)
  ggplot(sol1s, aes(y = PerBelowDL, x = SiteWs)) +
    geom_bar(stat = "identity")+
    facet_wrap(vars(sol)) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_hline(yintercept = 50) # this is our threshold for exclusion
  dev.off()
  

  ### Calcium ----
  summary(sol1$DL_Ca_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = Ca_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_Ca_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ### DOC ----
  summary(sol1$DL_DOC_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = DOC_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_DOC_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ### NH4 ----
  # matters a lot!
  summary(sol1$DL_NH4_N_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = NH4_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_NH4_N_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ### sleepers issues ----
  # ultimate dropped
  # Checking out sleepers flatline for NH4 - 
  # samples less than DL were converted to the -DL by data providers
  # I swtiched back to positive. Min here is 0.028, but our SLP DL is 0.004 
  # this has now been changed to 0.028, following conversations with data providers
  ggplot() +
    geom_point(data = sol1 %>% 
                 filter(SiteWs == "SLP_W9"), aes(y = NH4_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_NH4_N_mgL), color = "red")
  
  ggplot() +
    geom_point(data = sol1, aes(y = NH4_mgL.u, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_NH4_N_mgL), color = "red") +
    scale_y_log10()+
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ### NO3 ----
  # - also matters a lot
  summary(sol1$DL_NO3_N_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = NO3_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_NO3_N_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ggplot() +
    geom_point(data = sol1, aes(y = NO3_mgL.u, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_NO3_N_mgL), color = "red") +
    scale_y_log10()+
    facet_wrap(vars(SiteWs), scales = "free_y") 
  
  ### TDP ----
  # also matters a lot for HBEF and TLW
  summary(sol1$DL_TP_TDP_PO4_P_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = TDP_mgL, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_TP_TDP_PO4_P_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 

  ### SO4 ----
  # also matters a lot
  summary(sol1$DL_SO4_S_mgL)
  ggplot() +
    geom_point(data = sol1, aes(y = SO4_mgL.u, x = Date)) +
    geom_hline(data = sol1, aes(yintercept = DL_SO4_S_mgL), color = "red") +
    facet_wrap(vars(SiteWs), scales = "free_y") 
    
## still duplicates? ----
  dim(sol1[duplicated(sol1[,c("SiteWs", "Date")]) == TRUE,])
  
# Drop data we can't use ----
# Remove any datasets where > 50% of samples are > DL
  # NH4 = 
  WStoDropNH4 <-  c("BBWM_EB", "HBEF_WS6", "HJA_GSWS08", "HJA_GSWS09", "SLP_W9", "TLW_C32", "TLW_C35")
  # NO3 = 
  WStoDropNO3 <- "HBEF_WS6"
  # TDP = 
  WStoDropTDP <- c("HBEF_WS6", "HBEF_WS7", "HBEF_WS8", "HBEF_WS9", "SLP_W9")

  sol <-  sol1 %>% 
    select(SiteWs:Q_Ls, Ca_mgL.u:SO4_mgL.u) %>% 
    # remove u - values now adjusted for the site-specific DL
    rename(Ca_mgL = Ca_mgL.u, DOC_mgL = DOC_mgL.u, NH4_mgL = NH4_mgL.u, NO3_mgL = NO3_mgL.u, 
           TDP_mgL = TDP_mgL.u, SO4_mgL = SO4_mgL.u) %>% 
    mutate(NH4_mgL = ifelse(SiteWs %in% WStoDropNH4, as.numeric("NA"), NH4_mgL),
           NO3_mgL = ifelse(SiteWs == WStoDropNO3, as.numeric("NA"), NO3_mgL),
           TDP_mgL = ifelse(SiteWs %in% WStoDropTDP, as.numeric("NA"), TDP_mgL)) %>% 
    # remove some super high outliers
    mutate(TDP_mgL = ifelse(Site == "HJA" & TDP_mgL > 0.1, as.numeric("NA"), TDP_mgL)) %>% # removes 2 points
    mutate(NH4_mgL = ifelse(Site == "TLW" & NH4_mgL > 0.2, as.numeric("NA"), NH4_mgL)) # removes 1 point

 # export processed df ----
  write.csv(sol, file.path(here::here("03_generatedData"),
  "01c_ProcessedConcDataAllSites.csv"))

 save.image(file.path(here::here("07_Rdat"), "01cc_data_munging_bringing2gether.Rdata"))

