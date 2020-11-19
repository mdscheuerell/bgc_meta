library(tidyverse)
library(here)
library(MARSS)
library(ggpubr)


###########################
# Timeseries plots - using short timeseries
###########################

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
    mutate(TimeNum = seq(1,313, by = 1)) # this is the # of rows in all of these files CAREFUL to change if needed

Ca.se <- as.data.frame(t(MarsSeasSiteStateB[[1]]$states.se)) %>% 
    mutate(solute = "Ca") %>% 
    rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

DOC <- as.data.frame(t(MarsSeasSiteState[[2]]$states)) %>% 
  mutate(solute = "DOC") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

DOC.se <- as.data.frame(t(MarsSeasSiteState[[2]]$states.se))%>% 
  mutate(solute = "DOC") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NH4N <- as.data.frame(t(MarsSeasSiteState[[3]]$states)) %>% 
  mutate(solute = "NH4N") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NH4N.se <- as.data.frame(t(MarsSeasSiteState[[3]]$states.se))%>% 
  mutate(solute = "NH4N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NO3N <- as.data.frame(t(MarsSeasSiteState[[4]]$states)) %>% 
  mutate(solute = "NO3N") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NO3N.se <- as.data.frame(t(MarsSeasSiteState[[4]]$states.se))%>% 
  mutate(solute = "NO3N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

#TDP data missing at BBWM, so filling in with NAs
TDP <- as.data.frame(t(MarsSeasSiteState[[5]]$states)) %>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

TDP.se <- as.data.frame(t(MarsSeasSiteState[[5]]$states.se))%>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

# THIS IS NOT THE BEST MODEL FOR SO4 B/C IT WOULDN'T FIT IN DF
SO4S <- as.data.frame(t(MarsSeasSiteState[[6]]$states)) %>% 
  mutate(solute = "SO4S") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

SO4S.se <- as.data.frame(t(MarsSeasSiteState[[6]]$states.se))%>% 
  mutate(solute = "SO4S") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

states0 <- rbind(Ca, DOC, NH4N, NO3N, TDP, SO4S) %>% 
  pivot_longer(cols = BBWM:HJA, names_to = "site", values_to = "state")%>% 
  mutate(SiteSolNum = paste0(site,"__",solute, "__",TimeNum))


states.se <- rbind(Ca.se, DOC.se, NH4N.se, NO3N.se, TDP.se, SO4S.se) %>% 
  pivot_longer(cols = BBWM__se:HJA__se, names_to = "site", values_to = "state.se") %>% 
  separate(site, into = c("site", "se"), sep = "__") %>% 
  mutate(SiteSolNum = paste0(site,"__",solute, "__",TimeNum))

states <- states0 %>% 
  full_join(states.se %>% 
              select(state.se, SiteSolNum), by = "SiteSolNum") %>% 
  mutate(site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")))


# Make plot
pdf(file = file.path(here::here("plots"), "07p_NO3timeseriesPlot.pdf"), height = 6, width = 10)
ggplot(states %>% 
         filter(solute == "NO3N"), aes(y = state, x = TimeNum)) +
  geom_line() +
  geom_ribbon(data = states%>% 
                filter(solute == "NO3N"), aes(ymin = state - state.se, ymax = state + state.se, x = TimeNum), alpha = 0.25, color = "transparent", fill = "grey20") +
  facet_grid(site ~.) +
  theme_bw() +
  ylab(expression(paste(NO[3],"-N state (± 1 SE)"))) +
  xlab("Time") +
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

# FUCTION TO BUILD SEASONAL DATASET
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


# FUNCTION TO GENERATE TIME SERIES FOR PLOTING
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

# BEST MODEL UNIQUE SITES BEST HERE SO DOING IT BOTH WAYS
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
  pdf(file = file.path(here::here("plots"), "07p_SeasCompOfSiteCatch.pdf"), height = 10, width = 5)
    ggplot() +
      geom_line(data = Seas.So4.Both.df, 
                 aes(y = seas_catch, x = month, color = catchment))+
      geom_line(data = Seas.So4.Both.df, 
                aes(y = seas_sites, x = month)) +
      facet_grid(sites ~.)
  dev.off()
      

# COMBINE ALL SITE MODELS

SeasDat <- rbind(Seas.Ca.df, Seas.Doc.df, Seas.NH4.df, Seas.NO3.df, Seas.TDP.df, Seas.SO4.Site.df) %>% 
            mutate(sites = fct_relevel(sites, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")),
                   Sig2 = fct_relevel(Sig2, c("no", "one", "both")),
                   solute = fct_relevel(solute, c("Ca", "DOC", "NH4", "NO3", "TDP", "SO4"))) 
    
  
  
pdf(file = file.path(here::here("plots"), "07p_SeasBySolute.pdf"), height = 10, width = 10)
  ggplot(SeasDat, 
         aes(y = seas, x = month, color = sites, linetype = Sig2)) +
    geom_line() +
    scale_linetype_manual(values = c("dotted" ,"dashed", "solid")) +
    facet_wrap(vars(solute), nrow = 3, ncol = 3) +
    theme_bw()
dev.off()


pdf(file = file.path(here::here("plots"), "07p_SeasBySite.pdf"), height = 10, width = 10)
  ggplot(SeasDat, 
         aes(y = seas, x = month, color = solute, linetype = Sig2)) +
    geom_line() +
    scale_linetype_manual(values = c("dotted" ,"dashed", "solid")) +
    facet_wrap(vars(sites), nrow = 3, ncol = 3) +
    theme_bw()
dev.off()


save.image("07_JMHplots_Rdat")







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