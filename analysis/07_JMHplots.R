library(tidyverse)
library(here)
library(MARSS)
library(ggpubr)


###########################
# Timeseries plots - using short timeseries
###########################

BlankTS <- seq.Date(from = as.Date("1984/10/01"), to = as.Date("2010/10/01"), "months")
  

# GET BEST MODELS
# THE ORDER OF THE MODELS IS: Ca, DOC, NH4, NO3N, TDP, SO4
# this is the "best" model for DOC, NH4, NO3, TDP
MarsSeasSiteState <- readRDS(file = file.path(here::here("analysis"), "fitted_seas_site_state_RW.rds"))
# this is the "best" Ca model
MarsSeasSiteStateB <- readRDS(file = file.path(here::here("analysis"), "fitted_seas_site_state_RW_b.rds"))
# This is the "best" model for SO4
SeasUniqState <- readRDS(file = file.path(here::here("analysis"), "fitted_seas_unique_states_RW.rds"))


Ca <- as.data.frame(t(MarsSeasSiteStateB[[1]]$states)) %>% 
    mutate(solute = "Ca") %>% 
    mutate(TimeNum = seq(1,313, by = 1)) %>%  # this is the # of rows in all of these files CAREFUL to change if needed
    mutate(DateTime = BlankTS)
Ca.se <- as.data.frame(t(MarsSeasSiteStateB[[1]]$states.se)) %>% 
    mutate(solute = "Ca") %>% 
    rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

DOC <- as.data.frame(t(MarsSeasSiteState[[2]]$states)) %>% 
  mutate(solute = "DOC") %>% 
  mutate(TimeNum = seq(1,313, by = 1)) %>% 
  mutate(DateTime = BlankTS)

DOC.se <- as.data.frame(t(MarsSeasSiteState[[2]]$states.se))%>% 
  mutate(solute = "DOC") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NH4N <- as.data.frame(t(MarsSeasSiteState[[3]]$states)) %>% 
  mutate(solute = "NH4N") %>% 
  mutate(TimeNum = seq(1,313, by = 1)) %>% 
  mutate(DateTime = BlankTS)

NH4N.se <- as.data.frame(t(MarsSeasSiteState[[3]]$states.se))%>% 
  mutate(solute = "NH4N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NO3N <- as.data.frame(t(MarsSeasSiteState[[4]]$states)) %>% 
  mutate(solute = "NO3N") %>% 
  mutate(TimeNum = seq(1,313, by = 1)) %>% 
  mutate(DateTime = BlankTS)

NO3N.se <- as.data.frame(t(MarsSeasSiteState[[4]]$states.se))%>% 
  mutate(solute = "NO3N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

#TDP data missing at BBWM, so filling in with NAs
TDP <- as.data.frame(t(MarsSeasSiteState[[5]]$states)) %>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  mutate(TimeNum = seq(1,313, by = 1)) %>% 
  mutate(DateTime = BlankTS)

TDP.se <- as.data.frame(t(MarsSeasSiteState[[5]]$states.se))%>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

# THIS IS NOT THE BEST MODEL FOR SO4 B/C IT WOULDN'T FIT IN DF
SO4S <- as.data.frame(t(MarsSeasSiteState[[6]]$states)) %>% 
  mutate(solute = "SO4S") %>% 
  mutate(TimeNum = seq(1,313, by = 1)) %>% 
  mutate(DateTime = BlankTS)

SO4S.se <- as.data.frame(t(MarsSeasSiteState[[6]]$states.se))%>% 
  mutate(solute = "SO4S") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

states0 <- rbind(Ca, DOC, NH4N, NO3N, TDP, SO4S) %>% 
  pivot_longer(cols = BBWM:HJA, names_to = "site", values_to = "state")%>% 
  mutate(SiteSolNum = paste0(site,"__",solute, "__",TimeNum))

#Check
# plot(states0$DateTime ~ states0$TimeNum)

states.se <- rbind(Ca.se, DOC.se, NH4N.se, NO3N.se, TDP.se, SO4S.se) %>% 
  pivot_longer(cols = BBWM__se:HJA__se, names_to = "site", values_to = "state.se") %>% 
  separate(site, into = c("site", "se"), sep = "__") %>% 
  mutate(SiteSolNum = paste0(site,"__",solute, "__",TimeNum))

states <- states0 %>% 
  full_join(states.se %>% 
              select(state.se, SiteSolNum), by = "SiteSolNum") %>% 
  mutate(site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")),
         DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d"))



# Make plot
pdf(file = file.path(here::here("plots"), "07p_NO3timeseriesPlot.pdf"), height = 6, width = 10)
ggplot(states %>% 
         filter(solute == "NO3N"), aes(y = state, x = DateTime)) +
  geom_line() +
  geom_ribbon(data = states%>% 
                filter(solute == "NO3N"), aes(ymin = state - state.se, ymax = state + state.se, x = DateTime), alpha = 0.25, color = "transparent", fill = "grey20") +
  facet_grid(site ~.) +
  theme_bw() +
  ylab(expression(paste(NO[3]^{"-"},"-N state (± 1 SE)"))) +
  xlab("Time") +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "5 years") +
  scale_y_continuous(breaks = c(-7.5, 0, 7.5), limits = c(-7.5,7.5)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1,"lines"))
dev.off()
  
###########################
# Bias plot
###########################
# load the MARS data
# I will only have a row for Ca at site states

MARSmodCoefs.Ca <- MARSSparamCIs(MarsSeasSiteStateB[[1]], method = "hessian", alpha = 0.05, nboot = 1000)

CaCoef <- as.data.frame(MARSmodCoefs.Ca$coef[2:8])
names(CaCoef) <- "Bias"
CaCoef$site <- rownames(CaCoef)

CaCoef2 <- CaCoef %>% 
  mutate(
         #still not sure this is right
          Bias_ug.L.mo = (exp(Bias)-1)*1000, #units mg/L/mo to ug/L/mo
          site = as.factor(site),
         site = fct_relevel(site, c("U.HJA", "U.ELA", "U.MEF", "U.TLW", "U.DOR", "U.HBEF", "U.BBWM"))) %>% 
  arrange(site)

# see below for bias plot code



###########################
# Seasonality Plots
###########################

# FUCTIONS TO BUILD SEASONAL DATASET
# https://stackoverflow.com/questions/24384179/how-to-determine-whether-two-variables-have-the-same-sign-in-r/24384436
SigFun <- function(a,b) {ifelse(a == 0 | b == 0,"FALSE",!xor(sign(a)+1,sign(b)+1))}

# for the models with site states
SeasDatFun.Site <- function(MarsMod, SitesList){
  # for testing
  # MarsMod <- MarsSeasSiteStateB[[1]]
  
  Modpar <- MARSSparamCIs(MarsMod, method = "hessian", alpha = 0.05, nboot = 1000)
  ModCcoefs <- data.frame(coefs = as.factor(as.character(row.names(Modpar$par$U))),
                          C = Modpar$par$U,
                          Lci = Modpar$par.lowCI$U,
                          Uci = Modpar$par.upCI$U) %>% 
    filter(str_detect(coefs, "seas")) %>% 
    mutate(coefs = str_replace_all(coefs, "[(]",""),
           coefs = str_replace_all(coefs, "[)]",""),
           coefs = str_replace_all(coefs, "C.","")) %>% 
    separate(coefs, into = c("sites", "seas"), sep = ",") %>% 
    mutate(sites = fct_relevel(sites, SitesList)) %>% 
    mutate(Sig = SigFun(Lci, Uci)) %>% 
    pivot_wider(id_cols = sites, names_from = seas, values_from = C:Sig) %>% 
    mutate(Sig = paste0(Sig_seas_1, "_", Sig_seas_2),
           Sig2 = ifelse(Sig == "FALSE_FALSE", "no", 
                         ifelse(Sig == "TRUE_FALSE" | Sig == "FALSE_TRUE", "one",
                                ifelse(Sig == "TRUE_TRUE", "both", "blah"))))
}

# for models with catchment (unique) states
SeasDatFun.Unique <- function(MarsMod, SitesList){
  # for testing
  # MarsMod <- SeasUniqState[[6]]
  # SitesList <- SitesList_Not4Tdp
  
  Modpar <- MARSSparamCIs(MarsMod, method = "hessian", alpha = 0.05, nboot = 1000)
  ModCcoefs <- data.frame(coefs = as.factor(as.character(row.names(Modpar$par$U))),
                          C = Modpar$par$U,
                          Lci = Modpar$par.lowCI$U,
                          Uci = Modpar$par.upCI$U) %>% 
    filter(str_detect(coefs, "seas")) %>% 
    mutate(coefs = str_replace_all(coefs, "[(]",""),
           coefs = str_replace_all(coefs, "[)]",""),
           coefs = str_replace_all(coefs, "X.","")) %>% 
    separate(coefs, into = c("sites", "seas"), sep = ",") %>% 
    separate(sites, into = c("region", "sites", "catchment"), sep = "_") %>% 
    mutate(sites = fct_relevel(sites, SitesList)) %>% 
    mutate(Sig = SigFun(Lci, Uci)) %>% 
    pivot_wider(id_cols = region:catchment, names_from = seas, values_from = C:Sig) %>% 
    mutate(Sig = paste0(Sig_seas_1, "_", Sig_seas_2),
           Sig2 = ifelse(Sig == "FALSE_FALSE", "no", 
                         ifelse(Sig == "TRUE_FALSE" | Sig == "FALSE_TRUE", "one",
                                ifelse(Sig == "TRUE_TRUE", "both", "blah"))))
}


# FUNCTIONS TO GENERATE TIME SERIES FOR PLOTING
# for sites
seasPlotFun.Site <- function(periodS, MarsDF, solute){
  # set up sin and cos matrix
  # periodS <- 12# TEST
  PeriodStart <- 1
  PeriodEnd <- periodS
  monthNum <- seq(PeriodStart, PeriodEnd, by = 1)
  #seas_1 is sin; seas_2 is cos
  cos.t <- cos(2 * pi * monthNum/periodS)
  sin.t <- sin(2 * pi * monthNum/periodS)
  #sin is seas_1, cos is seas_2
  c.Four <- rbind(sin.t, cos.t) # if these get switched in C output they have to be switched
  # MarsDF <- Seas.Ca # TEST
  coefs <- as.matrix(MarsDF[,c("C_seas_1", "C_seas_2")])
  
  # calculate seasonality
  seas.F <- coefs %*% c.Four
  seas.F.df <- as.data.frame(t(seas.F))
  colnames(seas.F.df) <- MarsDF$sites
  seas.F.df$month <- monthNum

  # Prepare and export data frame
  seas.F.df2 <- seas.F.df %>% 
    pivot_longer(cols = -month, names_to = "sites", values_to = "seas") %>% 
    left_join(MarsDF %>% 
                select(sites,Sig2), by = c("sites")) %>% 
    mutate(solute = solute)
  
  seas.F.df2
}

# For catchments
seasPlotFun.Unique <- function(periodS, MarsDF, solute){
  # set up sin and cos matrix
  periodS <- 12# TEST
  PeriodStart <- 1
  PeriodEnd <- periodS
  monthNum <- seq(PeriodStart, PeriodEnd, by = 1)
  #seas_1 is sin; seas_2 is cos
  cos.t <- cos(2 * pi * monthNum/periodS)
  sin.t <- sin(2 * pi * monthNum/periodS)
  #sin is seas_1, cos is seas_2
  c.Four <- rbind(sin.t, cos.t) # if these get switched in C output they have to be switched
  MarsDF <- Seas.SO4.Unique # TEST
  coefs <- as.matrix(MarsDF[,c("C_seas_1", "C_seas_2")])
  
  # calculate seasonality
  seas.F <- coefs %*% c.Four
  seas.F.df <- as.data.frame(t(seas.F))
  colnames(seas.F.df) <- MarsDF$catchment
  seas.F.df$month <- monthNum
  
  # Prepare and export data frame
  seas.F.df2 <- seas.F.df %>% 
    pivot_longer(cols = -month, names_to = "catchment", values_to = "seas") %>% 
    left_join(MarsDF %>% 
                select(catchment, sites,Sig2), by = c("catchment")) %>% 
    mutate(solute = solute)
  
  seas.F.df2
}


# PREPARE DATASETS
SitesList_Not4Tdp <- c("BBWM","HBEF","MEF","TLW","DOR","ELA","HJA")
SitesList_4Tdp <- c("HBEF","MEF","TLW","DOR","ELA","HJA")

Seas.Ca <- SeasDatFun.Site(MarsSeasSiteStateB[[1]], SitesList_Not4Tdp)
Seas.Ca.df <- seasPlotFun.Site(12, Seas.Ca, "Ca")

Seas.Doc <- SeasDatFun.Site(MarsSeasSiteState[[2]], SitesList_Not4Tdp)
Seas.Doc.df <- seasPlotFun.Site(12, Seas.Doc, "DOC")

Seas.NH4 <- SeasDatFun.Site(MarsSeasSiteState[[3]], SitesList_Not4Tdp)
Seas.NH4.df <- seasPlotFun.Site(12, Seas.NH4, "NH4")

Seas.NO3 <- SeasDatFun.Site(MarsSeasSiteState[[4]], SitesList_Not4Tdp)
Seas.NO3.df <- seasPlotFun.Site(12, Seas.NO3, "NO3")

Seas.TDP <- SeasDatFun.Site(MarsSeasSiteState[[5]], SitesList_4Tdp)
Seas.TDP.df <- seasPlotFun.Site(12, Seas.TDP, "TDP") 

# SO4: BEST MODEL UNIQUE SITES BEST HERE SO DOING IT BOTH WAYS
  # Sites
    Seas.SO4.Site <- SeasDatFun.Site(MarsSeasSiteState[[6]], SitesList_Not4Tdp)
    Seas.SO4.Site.df <- seasPlotFun.Site(12, Seas.SO4.Site, "SO4")

  # Catchments
    Seas.SO4.Unique <- SeasDatFun.Unique(SeasUniqState[[6]], SitesList_Not4Tdp)
    # NEED TO FIGURE OUT IF I CAN USE THIS
    Seas.SO4.Unique.df <- seasPlotFun.Unique(12, Seas.SO4.Unique, "SO4") 
    
  # join site and catchment DF
    Seas.So4.Both.df <- Seas.SO4.Unique.df %>% 
      full_join(Seas.SO4.Site.df, by = c("sites","solute", "month"), suffix = c("_catch","_sites"))
    
  # Compare site & catchment fits
  # checked BBWM has one catchment  - EB

  pdf(file = file.path(here::here("plots"), "07p_SeasCompOfSiteCatch_so4.pdf"), height = 10, width = 5)
    ggplot() +
      geom_line(data = Seas.So4.Both.df, 
                 aes(y = seas_catch, x = month, color = catchment))+
      geom_line(data = Seas.So4.Both.df, 
                aes(y = seas_sites, x = month)) +
      # scale_color_brewer(palette = "Set2")+
      facet_grid(sites ~.)
  dev.off()
      

# COMBINE ALL SITE MODELS

SeasDat <- rbind(Seas.Ca.df, Seas.Doc.df, Seas.NH4.df, Seas.NO3.df, Seas.TDP.df, Seas.SO4.Site.df) %>% 
            mutate(sites = fct_relevel(sites, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")),
                   Sig2 = fct_relevel(Sig2, c("no", "one", "both")),
                   solute = fct_relevel(solute, c("Ca", "DOC", "NH4", "NO3", "TDP", "SO4")), 
                   #corrected for water year
                   month2 = ifelse(month == "1", "10",
                              ifelse(month == "2", "11",
                                ifelse(month == "3", "12",
                                  ifelse(month == "4", "1",
                                    ifelse(month == "5", "2",
                                      ifelse(month == "6", "3",
                                        ifelse(month == "7", "4",
                                          ifelse(month == "8", "5",
                                            ifelse(month == "9", "6", 
                                              ifelse(month == "10", "7",
                                                ifelse(month == "11", "8",
                                                  ifelse(month == "12", "9", "blah")))))))))))),
                   month3 = as.numeric(month2),
                   # ugh don't know a better way to do this
                   DateIsh = as.POSIXct(paste0("01-",month3,"-2020"), format = "%d-%m-%Y"),
                   doy = as.POSIXct(paste0("01-",month3,"-2020"), format = "%j"),
                   monthName = strftime(paste0("01-",month3,"-2020"), format = "%b"),
                   solute2 = fct_recode(solute, "Calcium" = "Ca", "Dissolved organic C" = "DOC",
                                       "Ammonium" = "NH4", "Nitrate" = "NO3", "Total dissolved P" = "TDP",
                                       "Sulfate" = "SO4")) 
    
  
  
pdf(file = file.path(here::here("plots"), "07p_SeasBySolute.pdf"), height = 8, width = 10)
  ggplot(SeasDat, 
         aes(y = seas, x = DateIsh, color = sites, linetype = Sig2)) +
    geom_line(size = 1.25) +
    scale_color_brewer(palette = "Set2", name = "Sites")+
    scale_linetype_manual(values = c("dotted" ,"dashed", "solid"), name = "Significant coef") +
    scale_x_datetime(date_labels = "%b") +
    facet_wrap(vars(solute2), nrow = 3, ncol = 3) +
    xlab(NULL) +
    ylab("Seasonality") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title.y = element_text(size = 18),
      panel.grid.minor = element_blank()
    )
dev.off()


pdf(file = file.path(here::here("plots"), "07p_SeasBySite.pdf"), height = 8, width = 10)
  ggplot(SeasDat, 
         aes(y = seas, x = DateIsh, color = solute, linetype = Sig2)) +
    geom_line(size = 1.25) +
    scale_x_datetime(date_labels = "%b") +
    xlab(NULL) +
    scale_linetype_manual(values = c("dotted" ,"dashed", "solid")) +
    scale_color_brewer(palette = "Set2")+
    facet_wrap(vars(sites), nrow = 3, ncol = 3) +
    theme_bw()
dev.off()


# RAW TIMESERIES DATA
# Let's check one or two of these
library(lubridate)
df <- readr::read_csv(file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv")) %>% 
  # this is not correct because date is water not calender year, but probably within a couple months
  mutate(dec_water_yr2 = format(date_decimal(dec_water_yr), "%d-%m-%Y"),
         # Water year starts on 1 Oct.
         dec_water_yr3 = as.POSIXct(dec_water_yr2, format = "%d-%m-%Y") - (92*24*60*60),
         Y = as.numeric(strftime(dec_water_yr3, format = "%Y")),
         Yf = as.factor(as.character(Y)),
         M = strftime(dec_water_yr3, format = "%m"),
         doy = as.numeric(strftime(dec_water_yr3, format = "%j"))) 

# This looks roughly similar but note that the months are different between this ans seasonality
pdf(file = file.path(here::here("plots"), "07p_RawSeasonalityPlots_TDP.pdf"), height = 10, width = 10)
ggplot(df, aes(y = log(FWATDPmgL), x = doy, color = Yf)) +
  geom_point(size = 0.5, alpha = 0.5) +
  facet_wrap(vars(site,catchment)) +
  stat_smooth(se = FALSE, size = 0.5)
  # facet_grid(site ~ catchment)
dev.off()

save.image("07_JMHplots_Rdat")
load("07_JMHplots_Rdat")


df %>% 
  group_by(site) %>% 
  summarize_at(vars(FWACamgL:FWASO4SmgL), list(mean = mean), na.rm = TRUE)






#################################
# Junk for bias plot.spec.coherency
# MarDatFun <- function(MarMod){
#   MarDatCI0 <- MARSSparamCIs(MarMod, method = "hessian", alpha = 0.1, nboot = 1000)
#   MarDatCI <- as.data.frame(MarDatCI0$par$U) 
#   MarDatCI$params <- row.names(MarDatCI) 
#   MarDatCI$coefLCI <- as.vector(MarDatCI0$par.lowCI$U[,1])
#   MarDatCI$coefUCI <- as.vector(MarDatCI0$par.upCI$U[,1])
#   MarDatCI2 <- as.data.frame(MarDatCI)
#   names(MarDatCI2) <- c("coef", "params", "LCI", "UCI")
#   
#   MarDatCI3 <- MarDatCI2 %>% 
#     mutate(params = str_replace_all(params, "[(]",""),
#            params = str_replace_all(params, "[)]","")) %>% 
#     separate(params, into = c("IDs", "seas"), sep = ",") %>% 
#     separate(IDs, into = c("group", "site", "catchment"), sep = "_") %>% 
#     mutate(group = as.factor(group),
#            group = fct_relevel(group, "X.NW", "X.EF", "X.NF"), #what do these mean??
#            group = recode(group, "X.NW" = "Northwest", "X.EF" = "EastSomething", "X.NF" = "NorthSomething"),
#            site = as.factor(site),
#            site = fct_relevel(site, "BBWM","HBEF","MEF","TLW","DOR","ELA","HJA"))
#   MarDatCI3
# }
# 
# MarDatFunNoCatch <- function(MarMod){
#   MarDatCI0 <- MARSSparamCIs(MarMod, method = "hessian", alpha = 0.1, nboot = 1000)
#   MarDatCI <- as.data.frame(MarDatCI0$par$U) 
#   MarDatCI$params <- row.names(MarDatCI) 
#   MarDatCI$coefLCI <- as.vector(MarDatCI0$par.lowCI$U[,1])
#   MarDatCI$coefUCI <- as.vector(MarDatCI0$par.upCI$U[,1])
#   MarDatCI2 <- as.data.frame(MarDatCI)
#   names(MarDatCI2) <- c("coef", "params", "LCI", "UCI")
#   
#   MarDatCI3 <- MarDatCI2 %>% 
#     mutate(params = str_replace_all(params, "[(]",""),
#            params = str_replace_all(params, "[)]","")) %>% 
#     separate(params, into = c("sites", "seas"), sep = ",") %>% 
#     mutate(sites = fct_relevel(sites, "BBWM","HBEF","MEF","TLW","DOR","ELA","HJA"))
#   
#   MarDatCI3
# }
# 
# BmarsCa <- MarDatFunNoCatch(MARSmodB[[1]])
# 
# BmarsCa2 <- BmarsCa[]

# Uerrorbars <- aes(ymin = LCI, ymax = UCI)
# 
# BiasPlotFun <- function(MarsCoefsDF, Site4fun){
#   BiasPlot <- ggplot(MarsCoefsDF %>% 
#                        filter(is.na(seas)) %>% 
#                        mutate(blah = "one") %>% 
#                        filter(sites == Site4fun), aes(y = coef, x = blah)) +
#     geom_hline(yintercept = 0, color = "red") +
#     geom_pointrange(Uerrorbars) + 
#     geom_point(shape = 21, size = 4) +
#     facet_wrap(vars(sites), nrow = 1, ncol = 7, scales = "free_y") +
#     ylab(NULL) +
#     xlab(NULL) +
#     # ylab("Bias coefficient (± 95% CI)") +
#     # xlab("Site") +
#     theme_bw() +
#     theme(panel.background = element_rect(fill = "transparent"),
#           axis.title.y = element_text(size = 22),
#           axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           axis.text.y = element_text(size = 10))
#   BiasPlot
# }
# 
# BBWMbiasP <- BiasPlotFun(BmarsCa, "BBWM") +
#                 scale_y_continuous(limits = c(-0.003,0.003), breaks = c(-0.003, -0.0015, 0, 0.0015, 0.003))
# HBEFbiasP <- BiasPlotFun(BmarsCa, "HBEF") +
#   scale_y_continuous(limits = c(-0.0015,0.0015), breaks = c(-0.0015, -0.00075, 0, 0.00075, 0.0015))
# 
# MEFbiasP <- BiasPlotFun(BmarsCa, "MEF") +
#   scale_y_continuous(limits = c(-0.15,0.15), breaks = c(-0.15, -0.075, 0, 0.075, 0.15))
# TLWbiasP <- BiasPlotFun(BmarsCa, "TLW") +
#   scale_y_continuous(limits = c(-4e-4,4e-4), breaks = c(-4e-4, -2e-4, 0, 2e-4, 4e-4))
# DORbiasP <- BiasPlotFun(BmarsCa, "DOR") +
#   scale_y_continuous(limits = c(-0.05,0.05), breaks = c(-0.05, -0.025, 0, 0.025, 0.05))
# ELAbiasP <- BiasPlotFun(BmarsCa, "ELA") +
#   scale_y_continuous(limits = c(-0.08,0.08), breaks = c(-0.08, -0.04, 0, 0.04, 0.08))
# HJAbiasP <- BiasPlotFun(BmarsCa, "HJA") +
#   scale_y_continuous(limits = c(-0.08,0.08), breaks = c(-0.08, -0.04, 0, 0.04, 0.08))
# 
# CaBiasP <- ggarrange(BBWMbiasP, HBEFbiasP, MEFbiasP, TLWbiasP, DORbiasP, ELAbiasP, HJAbiasP, nrow = 1, ncol = 7)
# CaBiasP2 <- CaBiasP %>% 
#   annotate_figure(left = text_grob("Bias coefficient (± 95% CI)", rot = 90, size = 18))
# 
# 
# # pdf("~/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/IrenasPaper/bgc_meta/plots/07p_CaBiasPlot.pdf", height = 4, width = 10)
# CaBiasP2
# # dev.off()