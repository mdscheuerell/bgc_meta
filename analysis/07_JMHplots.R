library(tidyverse)
library(here)
library(MARSS)
library(ggpubr)



# Timeseries plots ----
TimeSeriesLength <- 408
BlankTS.01 <- as.data.frame(seq(1,TimeSeriesLength, by = 1))
BlankTS.0 <- seq.Date(from = as.Date("1986/11/01"), to = as.Date("2020/10/31"), "months")
BlankTS <- cbind(BlankTS.01, BlankTS.0)
names(BlankTS) <- c("TimeNum", "Date")

# original dataframe ----
df <- readr::read_csv(here::here("data", "tbl_solutes_unmanaged_mon_v2.csv"))

# GET MARSS MODELS ----
# THE ORDER OF THE MODELS IS: "Ca"  "DOC" "NO3" "SO4" "NH4" "TDP"
# this is unique states model with seasonality and bias
MarsSeasSiteState <- readRDS(file = file.path(here::here("analysis"), "fitted_seas_unique_states_RW_b.rds"))

# Bias bootstraps
biasBS <- readRDS(here::here("Unity", "mod_set_site_RW_b_BiasTerms.rds"))


## Calcium ----
Ca <- as.data.frame(t(MarsSeasSiteState[[1]]$states)) %>% 
    # this is the # of rows in all of these files CAREFUL to change if needed
    mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>%  
    pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "Ca")

Ca.se <- as.data.frame(t(MarsSeasSiteState[[1]]$states.se)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>%  
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "Ca.se")

## DOC ----
DOC <- as.data.frame(t(MarsSeasSiteState[[2]]$states)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "DOC")

DOC.se <- as.data.frame(t(MarsSeasSiteState[[2]]$states.se))%>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1))%>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "DOC.se")

## Nitrate -N
NO3N <- as.data.frame(t(MarsSeasSiteState[[3]]$states)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "NO3N")

NO3N.se <- as.data.frame(t(MarsSeasSiteState[[3]]$states.se))%>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "NO3N.se")

## Sulfate ----
SO4 <- as.data.frame(t(MarsSeasSiteState[[4]]$states)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "SO4")


SO4.se <- as.data.frame(t(MarsSeasSiteState[[4]]$states.se))%>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1))%>% 
  pivot_longer(cols = X.EF_BBWM_EB:X.NW_HJA_GSWS09, names_to = "WA", values_to = "SO4.se")

## NH4-N
NH4N <- as.data.frame(t(MarsSeasSiteState[[5]]$states)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.NF_DOR_HP3:X.NF_TLW_C38, names_to = "WA", values_to = "NH4N")

NH4N.se <- as.data.frame(t(MarsSeasSiteState[[5]]$states.se)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1))%>% 
  pivot_longer(cols = X.NF_DOR_HP3:X.NF_TLW_C38, names_to = "WA", values_to = "NH4N.se")

## TDP
TDP <- as.data.frame(t(MarsSeasSiteState[[6]]$states)) %>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1)) %>% 
  pivot_longer(cols = X.NF_DOR_HP3:X.NW_HJA_GSWS09, names_to = "WA", values_to = "TDP")


TDP.se <- as.data.frame(t(MarsSeasSiteState[[6]]$states.se))%>% 
  mutate(TimeNum = seq(1,TimeSeriesLength, by = 1))%>% 
  pivot_longer(cols = X.NF_DOR_HP3:X.NW_HJA_GSWS09, names_to = "WA", values_to = "TDP.se")


## Bring dataframes together ----
# "Ca"  "DOC" "NO3" "SO4" "NH4" "TDP"
states0.s <- Ca %>% 
  full_join(DOC, by = c("TimeNum", "WA"))%>% 
  full_join(NO3N, by = c("TimeNum", "WA"))%>% 
  full_join(SO4, by = c("TimeNum", "WA"))%>% 
  full_join(NH4N, by = c("TimeNum", "WA"))%>% 
  full_join(TDP, by = c("TimeNum", "WA")) %>% 
  pivot_longer(cols = Ca:TDP, names_to =  "solute", values_to = "states") 
  


states0.se <-   Ca.se %>% 
  full_join(DOC.se, by = c("TimeNum", "WA"))%>% 
  full_join(NO3N.se, by = c("TimeNum", "WA"))%>% 
  full_join(SO4.se, by = c("TimeNum", "WA"))%>% 
  full_join(NH4N.se, by = c("TimeNum", "WA"))%>% 
  full_join(TDP.se, by = c("TimeNum", "WA")) %>% 
  pivot_longer(cols = Ca.se:TDP.se, names_to =  "solute", values_to = "states.se") %>% 
  mutate_at("solute", str_replace, ".se","")

states <- states0.s %>% 
          full_join(states0.se, by = c("TimeNum", "WA", "solute")) %>% 
          separate(WA, sep = "_", into= c("region", "site", "watershed")) %>% 
          mutate_at("region", str_replace, "X.", "") %>% 
          mutate_at(c("region", "site", "watershed"), factor)  %>% 
          full_join(BlankTS, by = "TimeNum") %>% 
          mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))

# site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")),
# , 
# watershed = fct_relevel(watershed,
#                         c("GSWS08", "GSWS09",
#                           "EIF", "NEIF", "NWIF",
#                           "S2", "S5",
#                           "C32", "C35", "C38",
#                           "HP3", "HP3A", "HP4", "HP5", "HP6", "HP6A",
#                           "WS6", "WS7", "WS8", "WS9",
#                           "EB"))

SiteList <- states %>% select(site, watershed) %>% distinct()

states %>% select(site, watershed, states, solute) %>% filter(is.na(states)) %>% distinct()


# Make plot
SoluteList <- unique(states$solute)
# SoluteYaxis <- list(expression(paste(NO[3]^{"-"},"-N state (± 1 SE)")),
#                     expression(paste(NH[4]^{"+1"},"-N state (± 1 SE)")),
#                     expression(paste(NO[3]^{"-2"},"-N state (± 1 SE)")),
#                     expression(paste(NH[4]^{"+3"},"-N state (± 1 SE)")),
#                     expression(paste(NO[3]^{"-4"},"-N state (± 1 SE)")),
#                     expression(paste(NH[4]^{"+5"},"-N state (± 1 SE)")))

pdf(file = file.path(here::here("plots"), "MARSS_StatePlots_20220922.pdf"), paper = "letter")
for(i in 1:length(SoluteList)){
  SoluteList_i <- SoluteList[i]
  # SoluteList_i <- SoluteList[6]
  
  TestPlot_i <- ggplot() +
    geom_line( data = states %>% 
                 filter(solute == SoluteList_i), aes(y = states, x = Date, color = watershed)) +
    geom_ribbon(data = states %>% 
                  filter(solute == SoluteList_i), aes(ymin = states - states.se, 
                                                      ymax = states + states.se, 
                                                      x = Date, fill= watershed), 
                alpha = 0.25, color = "transparent") +
    facet_grid(site ~., scales = "free_y") +
    theme_bw() +
    ylab(SoluteList_i) +
    xlab("Time") +
    scale_x_datetime(date_labels = "%Y", date_breaks = "5 years") +
    # scale_y_continuous(breaks = c(-7.5, 0, 7.5), limits = c(-7.5,7.5)) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1,"lines"))
  
  print(TestPlot_i)
  
}
dev.off()


# Bias plot ----

# create table of bias estimates (+/- CI) ----
tmp <- list()
for(i in 1:length(SoluteList)) {
  bias_ID <- rownames(biasBS[[i]]$par$U)
  tmp$solute <- rep(SoluteList[i], length(bias_ID))
  tmp$site <- bias_ID
  tmp$bias <- biasBS[[i]]$par$U
  tmp$loCI <- biasBS[[i]]$par.lowCI$U
  tmp$upCI <- biasBS[[i]]$par.upCI$U
  if(i == 1) {
    tbl_fit_bootstrap <- data.frame(tmp)
  } else {
    tbl_fit_bootstrap <- rbind(tbl_fit_bootstrap, data.frame(tmp))
  }
}
tbl_fit_bootstrap[, -c(1:2)] <- signif(tbl_fit_bootstrap[, -c(1:2)], 3)
rownames(tbl_fit_bootstrap) <- NULL

# cleans up seas coefs df
tbl_fit_seas_bs <- tbl_fit_bootstrap %>% 
                    filter(grepl("seas", site)) %>% 
                    separate(site, sep = ",", into = c("site", "seas")) %>% 
                    separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
                    mutate_at("region", str_remove, "X.") %>%
                    mutate_at("seas", str_remove, "[)]") %>% 
                    mutate_at("region", str_remove, "[())]") %>% 
                    rename("coef" = "bias")

# cleans up bias df
tbl_fit_bias_bs <- tbl_fit_bootstrap %>% 
                    filter(!grepl("seas", site)) %>% 
                    separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
                    mutate_at("region", str_remove, "X.") %>%
                    #units percent decline/mo to percent decline/yr
                    mutate(
                      #still not sure this is right
                      U_perChange_y = (exp(bias)-1)*12*100, 
                      U_perChange_y_lowCI = (exp(loCI)-1)*12*100,
                      U_perChange_y_upCI = (exp(upCI)-1)*12*100) %>% 
                    mutate_at(c("region", "site", "watershed"), factor)  %>%
                    mutate(site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM", "SLP")),
                         watershed = fct_relevel(watershed,
                                                 c("GSWS08", "GSWS09",
                                                   "EIF", "NEIF", "NWIF",
                                                   "S2", "S5",
                                                   "C32", "C35", "C38",
                                                   "HP3", "HP3A", "HP4", "HP5", "HP6", "HP6A",
                                                   "WS6", "WS7", "WS8", "WS9",
                                                   "EB",
                                                   "W9"))) %>% 
                      mutate(Sig = ifelse((loCI > 0 & upCI > 0) | (loCI <0 & upCI < 0),
                                          "Sig", "NS"),
                             Sig = fct_relevel(Sig, "Sig", "NS"))



# png(file = file.path(here::here("plots"), "MARSS_BiasPlots.png"), units="in", width= 8, height=6, res=300)
ggplot() +
        geom_hline(yintercept = 0) +
        geom_pointrange(data = tbl_fit_bias_bs, aes(y = U_perChange_y, x = watershed, fill = site,
                                                     ymin = U_perChange_y_lowCI,
                                                     ymax = U_perChange_y_upCI),
                                                    shape = 21) +
        # scale_shape_manual(values = c(21, 23)) +
        facet_grid(solute ~., scales = "free_y") +
        theme(axis.text.x = element_text(angle = 90)) +
        ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
        geom_text(data = tbl_fit_bias_bs[tbl_fit_bias_bs$Sig == "Sig",], 
                  aes(y = U_perChange_y_upCI + 10, x = watershed, label = "*"), size = 8, fontface = "bold")
# dev.off()


ggplot() +
        geom_hline(yintercept = 0) +
        geom_pointrange(data = tbl_fit_bias_bs %>% 
                                filter(Sig == "Sig") %>% 
                                mutate(S_WS = paste0(site," ", watershed)), 
                                aes(y = U_perChange_y, x = solute, fill = site,
                                                    ymin = U_perChange_y_lowCI,
                                                    ymax = U_perChange_y_upCI),
                        shape = 21, position = "jitter") +
        ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) 



#### STOPPED HERE ####

# Seasonality Plots ----


# FUCTIONS TO BUILD SEASONAL DATASET
# https://stackoverflow.com/questions/24384179/how-to-determine-whether-two-variables-have-the-same-sign-in-r/24384436
SigFun <- function(a,b) {ifelse(a == 0 | b == 0,"FALSE",!xor(sign(a)+1,sign(b)+1))}

# for the models with site states
SeasDatFun.Site <- function(MarsMod, SitesList){
  # for testing
  # MarsMod <- MarsSeasSiteState[[1]]
  
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
  # MarsMod <- MarsSeasSiteState[[6]]
  # SitesList <- as.vector(SitesList_TDP)
  
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
  # periodS <- 12# TEST
  PeriodStart <- 1
  PeriodEnd <- periodS
  monthNum <- seq(PeriodStart, PeriodEnd, by = 1)
  #seas_1 is sin; seas_2 is cos
  cos.t <- cos(2 * pi * monthNum/periodS)
  sin.t <- sin(2 * pi * monthNum/periodS)
  #sin is seas_1, cos is seas_2
  c.Four <- rbind(sin.t, cos.t) # if these get switched in C output they have to be switched
  # MarsDF <- Seas.SO4.Unique # TEST
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
# SitesList_Not4Tdp <- levels(states$site)
SitesList_Ca <- as.vector(unique(states[states$solute == "Ca" & !is.na(states$states),]$site))
SitesList_DOC <- as.vector(unique(states[states$solute == "DOC" & !is.na(states$states),]$site))
SitesList_NH4 <- as.vector(unique(states[states$solute == "NH4N" & !is.na(states$states),]$site))
SitesList_NO3 <- as.vector( unique(states[states$solute == "NO3N" & !is.na(states$states),]$site))
SitesList_SO4 <- as.vector(unique(states[states$solute == "SO4" & !is.na(states$states),]$site))
SitesList_TDP <- as.vector(unique(states[states$solute == "TDP" & !is.na(states$states),]$site))
  # c("BBWM","HBEF","MEF","TLW","DOR","ELA","HJA")


# for the ones with hessian issues I need to write a different function that uses
# MARSSparamCIs() with method = "parametric"
# That will take forever

# Order of models in MARSS model list
# "Ca"  "DOC" "NO3" "SO4" "NH4" "TDP"

Seas.Ca <- SeasDatFun.Unique(MarsSeasSiteState[[1]], SitesList_Ca)
Seas.Ca.df <- seasPlotFun.Unique(12, Seas.Ca, "Ca")

Seas.Doc <- SeasDatFun.Unique(MarsSeasSiteState[[2]], SitesList_DOC)
Seas.Doc.df <- seasPlotFun.Unique(12, Seas.Doc, "DOC")

Seas.NO3 <- SeasDatFun.Unique(MarsSeasSiteState[[3]], SitesList_NO3) #hessian issues
Seas.NO3.df <- seasPlotFun.Unique(12, Seas.NO3, "NO3")

Seas.SO4 <- SeasDatFun.Unique(MarsSeasSiteState[[4]], SitesList_SO4)
Seas.SO4.df <- seasPlotFun.Unique(12, Seas.SO4, "SO4")

Seas.NH4 <- SeasDatFun.Unique(MarsSeasSiteState[[5]], SitesList_NH4) #hessian issues
Seas.NH4.df <- seasPlotFun.Unique(12, Seas.NH4, "NH4") 

Seas.TDP <- SeasDatFun.Unique(MarsSeasSiteState[[6]], SitesList_TDP) #hessian issues
Seas.TDP.df <- seasPlotFun.Unique(12, Seas.TDP, "TDP")

# join site and catchment DF
    Seas.So4.Both.df <- Seas.SO4.Unique.df %>% 
      full_join(Seas.SO4.Site.df, by = c("sites","solute", "month"), suffix = c("_catch","_sites"))
    
  # Compare site & catchment fits
  # checked BBWM has one catchment  - EB

  # pdf(file = file.path(here::here("plots"), "07p_SeasCompOfSiteCatch_so4.pdf"), height = 10, width = 5)
  #   ggplot() +
  #     geom_line(data = Seas.So4.Both.df, 
  #                aes(y = seas_catch, x = month, color = catchment))+
  #     geom_line(data = Seas.So4.Both.df, 
  #               aes(y = seas_sites, x = month)) +
  #     # scale_color_brewer(palette = "Set2")+
  #     facet_grid(sites ~.)
  # dev.off()
      

# COMBINE ALL SITE MODELS

SeasDat <- rbind(Seas.Ca.df, Seas.Doc.df, Seas.NH4.df, Seas.NO3.df, Seas.TDP.df, Seas.SO4.df) %>% 
            mutate(sites = fct_relevel(sites, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")),
                   Sig2 = fct_recode(Sig2, "Hessian issues" = "blah"),
                   Sig2 = fct_relevel(Sig2, c("Hessian issues", "no", "one", "both")),
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
         aes(y = seas, x = DateIsh, color = catchment, linetype = Sig2)) +
    geom_line(size = 1.25) +
    # scale_color_brewer(palette = "Set2", name = "Sites")+
    scale_linetype_manual(values = c("dotted" ,"dashed", "longdash", "solid"), name = "Significant coef") +
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


# THIS DOESN'T REALLY WORK - TOO MANY CATCHMENTS
# pdf(file = file.path(here::here("plots"), "07p_SeasBySite.pdf"), height = 8, width = 10)
#   ggplot(SeasDat, 
#          aes(y = seas, x = DateIsh, color = solute, linetype = Sig2)) +
#     geom_line(size = 1.25) +
#     scale_x_datetime(date_labels = "%b") +
#     xlab(NULL) +
#     scale_linetype_manual(values = c("dotted" ,"dashed", "longdash", "solid")) +
#     scale_color_brewer(palette = "Set2")+
#     facet_wrap(vars(sites), nrow = 3, ncol = 3) +
#     theme_bw()
# dev.off()




#STOPPED HERE
# save.image("analysis/07_JMHplots_Rdat")


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
load("Analysis/07_JMHplots_Rdat")


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