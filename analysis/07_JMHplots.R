library(tidyverse)
library(here)
library(MARSS)
library(ggpubr)

# Timeseries plots - short
MARSmod <- readRDS("~/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/IrenasPaper/bgc_meta/analysis/fitted_seas_site_state_RW.rds")

Ca <- as.data.frame(t(MARSmod[[1]]$states)) %>% 
    mutate(solute = "Ca") %>% 
    mutate(TimeNum = seq(1,313, by = 1)) # this is the # of rows in all of these files CAREFUL to change if needed

Ca.se <- as.data.frame(t(MARSmod[[1]]$states.se)) %>% 
    mutate(solute = "Ca") %>% 
    rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

DOC <- as.data.frame(t(MARSmod[[2]]$states)) %>% 
  mutate(solute = "DOC") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

DOC.se <- as.data.frame(t(MARSmod[[2]]$states.se))%>% 
  mutate(solute = "DOC") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NH4N <- as.data.frame(t(MARSmod[[3]]$states)) %>% 
  mutate(solute = "NH4N") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NH4N.se <- as.data.frame(t(MARSmod[[3]]$states.se))%>% 
  mutate(solute = "NH4N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NO3N <- as.data.frame(t(MARSmod[[4]]$states)) %>% 
  mutate(solute = "NO3N") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

NO3N.se <- as.data.frame(t(MARSmod[[4]]$states.se))%>% 
  mutate(solute = "NO3N") %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

#TDP data missing at BBWM, so filling in with NAs
TDP <- as.data.frame(t(MARSmod[[5]]$states)) %>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

TDP.se <- as.data.frame(t(MARSmod[[5]]$states.se))%>% 
  mutate(solute = "TDP",
         BBWM = as.numeric("NA")) %>% 
  select(BBWM, HBEF, MEF, TLW, DOR, ELA, HJA, solute) %>% 
  rename_with(~paste0(.,"__se"), BBWM:HJA) %>% 
  mutate(TimeNum = seq(1,313, by = 1))

SO4S <- as.data.frame(t(MARSmod[[6]]$states)) %>% 
  mutate(solute = "SO4S") %>% 
  mutate(TimeNum = seq(1,313, by = 1))

SO4S.se <- as.data.frame(t(MARSmod[[6]]$states.se))%>% 
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
              select(state.se, SiteSolNum), by = "SiteSolNum") 


pdf("~/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/IrenasPaper/bgc_meta/plots/07p_NO3timeseriesPlot.pdf", height = 6, width = 10)
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
  

# Bias plot
# load the MARS data
MARSmodB <- readRDS("~/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/IrenasPaper/bgc_meta/analysis/fitted_seas_site_state_RW_b.rds")

# use this model <-  fitted_seas_site_state_RW
# I will only have a row for Ca at site states

# function to get CIs
# Just for testing function: 
MarMod <- MARSmodB[[1]]


MarDatFun <- function(MarMod){
  MarDatCI0 <- MARSSparamCIs(MarMod, method = "hessian", alpha = 0.1, nboot = 1000)
  MarDatCI <- as.data.frame(MarDatCI0$par$U) 
  MarDatCI$params <- row.names(MarDatCI) 
  MarDatCI$coefLCI <- as.vector(MarDatCI0$par.lowCI$U[,1])
  MarDatCI$coefUCI <- as.vector(MarDatCI0$par.upCI$U[,1])
  MarDatCI2 <- as.data.frame(MarDatCI)
  names(MarDatCI2) <- c("coef", "params", "LCI", "UCI")
  
  MarDatCI3 <- MarDatCI2 %>% 
    mutate(params = str_replace_all(params, "[(]",""),
           params = str_replace_all(params, "[)]","")) %>% 
    separate(params, into = c("IDs", "seas"), sep = ",") %>% 
    separate(IDs, into = c("group", "site", "catchment"), sep = "_") %>% 
    mutate(group = as.factor(group),
           group = fct_relevel(group, "X.NW", "X.EF", "X.NF"), #what do these mean??
           group = recode(group, "X.NW" = "Northwest", "X.EF" = "EastSomething", "X.NF" = "NorthSomething"),
           site = as.factor(site),
           site = fct_relevel(site, "BBWM","HBEF","MEF","TLW","DOR","ELA","HJA"))
  MarDatCI3
}

MarDatFunNoCatch <- function(MarMod){
  MarDatCI0 <- MARSSparamCIs(MarMod, method = "hessian", alpha = 0.1, nboot = 1000)
  MarDatCI <- as.data.frame(MarDatCI0$par$U) 
  MarDatCI$params <- row.names(MarDatCI) 
  MarDatCI$coefLCI <- as.vector(MarDatCI0$par.lowCI$U[,1])
  MarDatCI$coefUCI <- as.vector(MarDatCI0$par.upCI$U[,1])
  MarDatCI2 <- as.data.frame(MarDatCI)
  names(MarDatCI2) <- c("coef", "params", "LCI", "UCI")
  
  MarDatCI3 <- MarDatCI2 %>% 
    mutate(params = str_replace_all(params, "[(]",""),
           params = str_replace_all(params, "[)]","")) %>% 
    separate(params, into = c("sites", "seas"), sep = ",") %>% 
    mutate(sites = fct_relevel(sites, "BBWM","HBEF","MEF","TLW","DOR","ELA","HJA"))
  
  MarDatCI3
}

BmarsCa <- MarDatFunNoCatch(MARSmodB[[1]])


Uerrorbars <- aes(ymin = LCI, ymax = UCI)

BiasPlotFun <- function(MarsCoefsDF, Site4fun){
  BiasPlot <- ggplot(MarsCoefsDF %>% 
                       filter(is.na(seas)) %>% 
                       mutate(blah = "one") %>% 
                       filter(sites == Site4fun), aes(y = coef, x = blah)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange(Uerrorbars) + 
    geom_point(shape = 21, size = 4) +
    facet_wrap(vars(sites), nrow = 1, ncol = 7, scales = "free_y") +
    ylab(NULL) +
    xlab(NULL) +
    # ylab("Bias coefficient (± 95% CI)") +
    # xlab("Site") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          axis.title.y = element_text(size = 22),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10))
  BiasPlot
}

BBWMbiasP <- BiasPlotFun(BmarsCa, "BBWM") +
                scale_y_continuous(limits = c(-0.003,0.003), breaks = c(-0.003, -0.0015, 0, 0.0015, 0.003))
HBEFbiasP <- BiasPlotFun(BmarsCa, "HBEF") +
  scale_y_continuous(limits = c(-0.0015,0.0015), breaks = c(-0.0015, -0.00075, 0, 0.00075, 0.0015))

MEFbiasP <- BiasPlotFun(BmarsCa, "MEF") +
  scale_y_continuous(limits = c(-0.15,0.15), breaks = c(-0.15, -0.075, 0, 0.075, 0.15))
TLWbiasP <- BiasPlotFun(BmarsCa, "TLW") +
  scale_y_continuous(limits = c(-4e-4,4e-4), breaks = c(-4e-4, -2e-4, 0, 2e-4, 4e-4))
DORbiasP <- BiasPlotFun(BmarsCa, "DOR") +
  scale_y_continuous(limits = c(-0.05,0.05), breaks = c(-0.05, -0.025, 0, 0.025, 0.05))
ELAbiasP <- BiasPlotFun(BmarsCa, "ELA") +
  scale_y_continuous(limits = c(-0.08,0.08), breaks = c(-0.08, -0.04, 0, 0.04, 0.08))
HJAbiasP <- BiasPlotFun(BmarsCa, "HJA") +
  scale_y_continuous(limits = c(-0.08,0.08), breaks = c(-0.08, -0.04, 0, 0.04, 0.08))

CaBiasP <- ggarrange(BBWMbiasP, HBEFbiasP, MEFbiasP, TLWbiasP, DORbiasP, ELAbiasP, HJAbiasP, nrow = 1, ncol = 7)
CaBiasP2 <- CaBiasP %>% 
  annotate_figure(left = text_grob("Bias coefficient (± 95% CI)", rot = 90, size = 18))


pdf("~/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/IrenasPaper/bgc_meta/plots/07p_CaBiasPlot.pdf", height = 4, width = 10)
CaBiasP2
dev.off()
