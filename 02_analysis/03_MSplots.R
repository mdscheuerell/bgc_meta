# plot results of MARSS models
# JMH Oct 22
# TK Jun 23
# JMH Jun 23

# libraries ----
library(tidyverse)
library(here)
library(MARSS)
library(ggpubr)
library(lubridate)
library(viridis)

# Timeseries plots ----
TimeSeriesLength <- 408
BlankTS.01 <- as.data.frame(seq(1,TimeSeriesLength, by = 1))
BlankTS.0 <- seq.Date(from = as.Date("1986/11/01"), to = as.Date("2020/10/31"), "months")
BlankTS <- cbind(BlankTS.01, BlankTS.0)
names(BlankTS) <- c("TimeNum", "Date")

# original dataframe ----
yr_first <- 1987 #state estimates start on 11 Oct. 2026
yr_last <- 2020
df <- readr::read_csv(here::here("03_generatedData", "01g_tbl_solutes_unmanaged_mon_v2.csv")) %>%
  filter(dec_water_yr >= yr_first & dec_water_yr <= yr_last)

# Get MARSS models ----
# THE ORDER OF THE MODELS IS: "Ca"  "DOC" "NO3" "SO4" "NH4" "TDP"
# this is unique states model with seasonality and bias
MarsSeasSiteState <- readRDS(file = file.path(here::here("06_ModelRDSFiles"), "02a_fitted_seas_unique_states_RW_b.rds"))

# Model coefs ----
# run in 04_model_fitting_BiasTerms_1000bootstraps.R
biasBS <- readRDS(file = file.path(here::here("06_ModelRDSFiles"), "mod_set_site_RW_b_BiasTerms_1000.rds"))
  

# Assemble states ----
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
          mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"),
                 watershed = fct_relevel(watershed,
                                         # BBWM
                                         "EB", 
                                         # DOR
                                         "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                         # ELA
                                         "EIF",    "NEIF",   "NWIF",
                                         # HBEF
                                         "WS6",    "WS7",    "WS8",    "WS9",
                                         # MEF
                                         "S2",     "S5",     
                                         # SLP
                                         "W9",
                                         # TLW
                                         "C32",    "C35",    "C38",
                                         # HJA
                                         "GSWS08", "GSWS09"),
                 site = fct_relevel(site, "BBWM", "DOR",  "ELA",  "HBEF", "MEF",  "SLP",  "TLW", "HJA")) %>% 
          # making catchment/watershed consistent across df's
          rename(catchment = "watershed")

SiteList <- states %>% select(site, catchment) %>% distinct()

# List of sites with no state data
SitesSol_Deleted <- states %>% 
  select(site, catchment, states, solute) %>% 
  filter(is.na(states)) %>% 
  distinct() %>% 
  mutate(SiteCatchSol = paste0(site, "_", catchment, "_", solute)) %>% 
  select(SiteCatchSol)

# these have state predictions, but there was no data - remove
StatePredNoDat <- as.data.frame(c("TLW_C32_NH4N", "TLW_C35_NH4N", "HBEF_WS6_NH4N", "HBEF_WS6_NO3N"))
names(StatePredNoDat) <- c("SiteCatchSol")
SitesSol_Deleted <- rbind(SitesSol_Deleted,StatePredNoDat)

# remove catchments-solutes with no data
states2 <- states %>% 
            mutate(SiteCatchSol = paste0(site, "_", catchment, "_", solute)) %>% 
            filter(!(SiteCatchSol %in% SitesSol_Deleted$SiteCatchSol)) %>% 
            # clip last year of predictions
            filter(Date < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"))

# NOTE-this doesn't change the number of rows, because it doesn't delete any catchments, only Na's are missing solutes            
df2 <- df %>% 
        pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
        mutate(SiteCatchSol = paste0(site, "_", catchment, "_", solute)) %>% 
        filter(!(SiteCatchSol %in% SitesSol_Deleted$SiteCatchSol)) %>% 
        pivot_wider(id_cols = region:dec_water_yr, names_from = solute, values_from = FWMC)%>% 
        # convert to posix.ct then adjust from water years (start oct) to normal years
        mutate(pdt = lubridate::date_decimal(dec_water_yr)-91*24*60*60) %>% 
  mutate(watershed = fct_relevel(catchment,
                                 # BBWM
                                 "EB", 
                                 # DOR
                                 "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                 # ELA
                                 "EIF",    "NEIF",   "NWIF",
                                 # HBEF
                                 "WS6",    "WS7",    "WS8",    "WS9",
                                 # MEF
                                 "S2",     "S5",     
                                 # SLP
                                 "W9",
                                 # TLW
                                 "C32",    "C35",    "C38",
                                 # HJA
                                 "GSWS08", "GSWS09"),
         site = fct_relevel(site, "BBWM", "DOR",  "ELA",  "HBEF", "MEF",  "SLP",  "TLW", "HJA")) 

# States plot ----
# prepare raw data for comparison

# table of distinct catchments and sites
DistinctCatchments <- states %>% select(site, catchment) %>% distinct()

# list of solutes
SoluteList <- unique(states$solute)

# Global color palette for figs
cols1 <- c(
  "#fde725", # yellow-BBWM
  "#05d5f5", "#6eb5db", "#aad6f0", "#0367a1", "#0505f7", "#084ec7", # blue-DOR
  "#7f039e", "#8507fa", "#7b54a1",   # purple-ELA
  "#1e5920", "#5ec962", "#029e07", "#07f0b2", # green-HBEF
  "#35528b", "#c392f0", # lavender-MEF
  "#21918c", # teal-SLP
  "#f51505", "#f0079a", "#f095ce", # red-TLW
  "orange red", "#f5965f" # orange-HJA         
)

# Fancy labels with units
State_fancyYaxisLabels <- c(expression(paste("Scaled volume-weighted ", Ca^'2+', " concentration")),
                           expression(paste("Scaled volume-weighted DOC concentration")),
                           expression(paste("Scaled volume-weighted ", NO[3]^'-', "-N", " concentration")),
                           expression(paste("Scaled volume-weighted ",SO[4]^'2-', " concentration")),
                           expression(paste("Scaled volume-weighted ",NH[4]^'+', "-N concentration")),
                           expression(paste("Scaled volume-weighted TDP concentration")))


## States timeseries (Fig 4, S1-5) ----
for(i in 1:length(SoluteList)){
  SoluteList_i <- SoluteList[i]
# SoluteList_i <- SoluteList[6]
State_fancyYaxisLabels_i <- State_fancyYaxisLabels[i]
  
  TestPlot_i <- ggplot() +
    geom_point(data = df2 %>% 
                 # log data
                 mutate(across(Ca:TDP,log)) %>%
                 # need to center each timeseries individually
                 group_by(catchment) %>% 
                 # center
                 mutate(across(Ca:TDP, scale, scale = FALSE)) %>% 
                 rename(NO3N = NO3, NH4N = NH4) %>% 
                 select(site, catchment, pdt, !!SoluteList_i) %>% 
                 # drop the sites with no data for this solute, in conj w state2 filters
                 filter(!is.na(.data[[SoluteList_i]])) %>% 
                 droplevels(), 
               aes(y = .data[[SoluteList_i]], x = pdt), color = "grey", shape = 1) +
    geom_line( data = states2 %>%
                 filter(solute == SoluteList_i) %>% 
                 # drop the sites with no data for this solute, in conj w df2 filters
                 filter(!is.na(!!SoluteList_i)) %>% 
                 droplevels(), aes(y = states, x = Date, color = catchment)) +
    geom_ribbon(data = states2 %>%
                  filter(solute == SoluteList_i) %>% 
                  # drop the sites with no data for this solute, in conj w df2 filters
                  filter(!is.na(!!SoluteList_i)) %>% 
                  droplevels(), aes(ymin = states - states.se,
                                                      ymax = states + states.se,
                                                      x = Date, fill= catchment),
                alpha = 0.25, color = "transparent") +
    scale_color_manual(values = cols1, name = "Catchment") +
    scale_fill_manual(values = cols1, name = "Catchment") +
    facet_grid(site ~., scales = "free_y") +
    theme_bw() +
    # this needs to be fixed once stack exchange comes online
    ylab(State_fancyYaxisLabels_i) +
    xlab(NULL) +
    scale_x_datetime(date_labels = "%Y", date_breaks = "5 years") + 
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1,"lines"))
  
  ggsave(TestPlot_i, path = "04_plots", file = paste0("03_MARSSStatePlots_",SoluteList_i,".png"), width = 11, height = 9, units = "in")
}



# Bias plots ----

## create table of bias estimates (+/- CI) ----
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


## cleans up df ----
tbl_fit_bias_bs <- tbl_fit_bootstrap %>% 
                    # remove seasonality coefs
                    filter(!grepl("seas", site)) %>% 
                    separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
                    mutate_at("region", str_remove, "X.") %>%
                    #units percent decline/mo to percent decline/yr
                    mutate(
                      # Convert coef to % change/year
                      U_perChange_y = (exp(bias)-1)*12*100, 
                      U_perChange_y_lowCI = (exp(loCI)-1)*12*100,
                      U_perChange_y_upCI = (exp(upCI)-1)*12*100) %>% 
                    mutate_at(c("region", "site", "watershed"), factor)  %>%
                    mutate(site = fct_relevel(site, c("BBWM", "DOR",  "ELA",  "HBEF", "MEF",  "SLP",  "TLW", "HJA")),
                           watershed = fct_relevel(watershed,
                                                   # BBWM
                                                   "EB", 
                                                   # DOR
                                                   "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                                   # ELA
                                                   "EIF",    "NEIF",   "NWIF",
                                                   # HBEF
                                                   "WS6",    "WS7",    "WS8",    "WS9",
                                                   # MEF
                                                   "S2",     "S5",     
                                                   # SLP
                                                   "W9",
                                                   # TLW
                                                   "C32",    "C35",    "C38",
                                                   # HJA
                                                   "GSWS08", "GSWS09")) %>% 
                      # Label sig if CI's don't overlap zero
                      mutate(Sig = ifelse((loCI > 0 & upCI > 0) | (loCI <0 & upCI < 0),
                                          "Sig", "NS"),
                             Sig = fct_relevel(Sig, "Sig", "NS"))


##  plots ----
### All watersheds/solutes ----
png(file = file.path(here::here("04_plots"), "03b_BiasPlots_All.png"), units="in", width= 8, height=6, res=300)
ggplot() +
        geom_hline(yintercept = 0) +
        geom_pointrange(data = tbl_fit_bias_bs, aes(y = U_perChange_y, x = watershed, fill = site,
                                                     ymin = U_perChange_y_lowCI,
                                                     ymax = U_perChange_y_upCI),
                                                    shape = 21) +
        facet_grid(solute ~ ., scales = "free_y") +
        theme(axis.text.x = element_text(angle = 90)) +
        ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
        geom_text(data = tbl_fit_bias_bs[tbl_fit_bias_bs$Sig == "Sig",], 
                  aes(y = U_perChange_y_upCI + 10, x = watershed, label = "*"), size = 8, fontface = "bold")
dev.off()


### FIG 6 - sig bias est ----
bialsPlotColors <- c(# BBWM
                      "#fde725",
                      # DOR
                      "#0505f7",
                      # ELA
                      "#7f039e",
                      # HBEF
                      "#029e07",
                      # MEF
                      "#c392f0",
                      # SLP
                      "#21918c",
                      # TLW
                      "#f51505",
                      # HJA
                      "orange red")

# number of ts for each solute, entered into x-axis title in fig below
tbl_fit_bias_bs %>% 
  select(watershed, solute, bias) %>% 
  pivot_wider(id_cols = c(watershed), names_from = "solute", values_from = "bias") %>% 
  summarise(across(Ca:TDP, ~sum(!is.na(.))))


bias.pl <- ggplot() +
              geom_hline(yintercept = 0) +
              geom_pointrange(data = tbl_fit_bias_bs %>% 
                    filter(Sig == "Sig") %>% 
                    mutate(S_WS = paste0(site," ", watershed)), 
                      aes(y = U_perChange_y, x = solute, fill = site,
                          ymin = U_perChange_y_lowCI,
                          ymax = U_perChange_y_upCI),
                      shape = 21, size = 1.5, position = position_jitter(w = 0.2)) +
               scale_fill_manual(values = bialsPlotColors, name = "Site") +
              scale_x_discrete(labels = c(expression(atop(Ca^{"2+"}," (22)")),
                                          expression(atop("DOC", "(22)")), 
                                          expression(atop(NH[4]^{"+"}," (16)")), 
                                          expression(atop(NO[3]^{"-"}," (20)")),
                                          expression(atop(SO[4]^{"2-"}," (22)")))) +
              ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
              theme_bw() +
              theme(legend.position = "right",
                legend.background = element_rect(fill = NA, color = NA),
                legend.text = element_text(size = 28),
                legend.title = element_text(size = 28),
                panel.grid = element_blank(),
                panel.border = element_rect(color = "black", linewidth = 2),
                plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
                axis.text = element_text(size = 30),
                axis.title.y = element_text(size = 30),
                axis.text.x = element_text(vjust = 0.5),
                axis.title.x = element_blank())

ggsave(bias.pl, path = "04_plots", file = "03b_Bias_SigOnly_Fig6.pdf", width = 11, height = 8, units = "in")


# Seasonality ----
## cleans up seas coefs df ----
tbl_fit_seas_bs <- tbl_fit_bootstrap %>% 
  filter(grepl("seas", site)) %>% 
  separate(site, sep = ",", into = c("site", "seas")) %>% 
  separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
  mutate_at("region", str_remove, "X.") %>%
  mutate_at("seas", str_remove, "[)]") %>% 
  mutate_at("region", str_remove, "[())]") %>% 
  rename("coef" = "bias") %>% 
  mutate(Sig = ifelse((loCI > 0 & upCI > 0) | (loCI <0 & upCI < 0),
                      "Sig", "NS"),
         Sig = fct_relevel(Sig, "Sig", "NS"))


# Function for seasonality 
SigFun <- function(a,b) {ifelse(a == 0 | b == 0,"FALSE",!xor(sign(a)+1,sign(b)+1))}

## For catchments ----
seasPlotFun.Unique <- function(periodS, MarsDF, solute_i){
  # For testing function only
  # periodS <- 12 # TEST
  # solute_i  <-  "Ca" # TEST
  
  MarsDF <- tbl_fit_seas_bs %>%
    filter(solute == solute_i) %>%
    pivot_wider(id_cols = solute:watershed, names_from = seas, values_from = coef:Sig)

  PeriodStart <- 1
  PeriodEnd <- periodS
  monthNum <- seq(PeriodStart, PeriodEnd, by = 1)
  
  #seas_1 is sin; seas_2 is cos
  cos.t <- cos(2 * pi * monthNum/periodS)
  sin.t <- sin(2 * pi * monthNum/periodS)
  
  #sin is seas_1, cos is seas_2
  c.Four <- rbind(sin.t, cos.t) 
  
  # MarsDF <- Seas.SO4.Unique # For TEST
  coefs <- as.matrix(MarsDF[,c("coef_seas_1", "coef_seas_2")])
  
  # calculate seasonality
  seas.F <- coefs %*% c.Four
  seas.F.df <- as.data.frame(t(seas.F))
  colnames(seas.F.df) <- MarsDF$watershed
  seas.F.df$month <- monthNum
  
  # Prepare and export data frame
  seas.F.df2 <- seas.F.df %>% 
    pivot_longer(cols = -month, names_to = "watershed", values_to = "seas") %>% 
    left_join(MarsDF %>% 
                select(solute:Sig_seas_2), by = c("watershed")) %>% 
    mutate(solute = solute)
  
  seas.F.df2
}


## Prepare df ----
SitesList_Ca <- as.vector(unique(states[states$solute == "Ca" & !is.na(states$states),]$site))
SitesList_DOC <- as.vector(unique(states[states$solute == "DOC" & !is.na(states$states),]$site))
SitesList_NH4 <- as.vector(unique(states[states$solute == "NH4N" & !is.na(states$states),]$site))
SitesList_NO3 <- as.vector( unique(states[states$solute == "NO3N" & !is.na(states$states),]$site))
SitesList_SO4 <- as.vector(unique(states[states$solute == "SO4" & !is.na(states$states),]$site))
SitesList_TDP <- as.vector(unique(states[states$solute == "TDP" & !is.na(states$states),]$site))
  

Seas.Ca.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "Ca")
Seas.Doc.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "DOC")
Seas.NO3.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "NO3N")
Seas.SO4.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "SO4")
Seas.NH4.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "NH4N") 
Seas.TDP.df <- seasPlotFun.Unique(12, tbl_fit_seas_bs, "TDP")


SeasDat <- rbind(Seas.Ca.df, Seas.Doc.df, Seas.NH4.df, Seas.NO3.df, Seas.TDP.df, Seas.SO4.df) %>% 
  mutate(site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM", "SLP")),
         Sig2 = ifelse(Sig_seas_1 == "Sig" & Sig_seas_2 == "Sig", "TwoSig",
                       ifelse((Sig_seas_1 == "Sig" & Sig_seas_2 != "Sig") |
                                (Sig_seas_1 != "Sig" & Sig_seas_2 == "Sig"),"OneSig", "NS")),
         Sig2 = fct_relevel(Sig2, c("TwoSig", "OneSig", "NS")),
         solute = fct_relevel(solute, c("Ca", "DOC", "NH4N", "NO3N", "TDP", "SO4")), 
         # convert from water year to calendar year
         monthCal = as.numeric(ifelse(month <= 3, month + 9, 
                                      ifelse(month > 3, month-3,"NA"))),
         solute2 = fct_recode(solute, "Calcium" = "Ca", "DOC" = "DOC",
                              "Ammonium" = "NH4N", "Nitrate" = "NO3N", "TDP" = "TDP",
                              "Sulfate" = "SO4")) 

# this has the same patterns as raw data
df2_seas <- df2 %>%
  mutate(dec_water_yrC = as.character(dec_water_yr)) %>%
  # errors generated here, but don't cause issues
  separate(dec_water_yrC, into = c("Y","dec")) %>% 
  # log data
  mutate(month = as.numeric(strftime(pdt, format = "%m"))) %>% 
  mutate(across(Ca:TDP,log)) %>%
  # need to center each timeseries individually
  group_by(catchment) %>%
  # center
  mutate(across(Ca:TDP, scale, scale = FALSE)) %>%
  rename(NO3N = NO3, NH4N = NH4) %>%
  pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC_log_scaled") 

## Plots ----

### All seasonality fits ----
# not used in MS
pdf(file = file.path(here::here("04_plots"), "03c_Seas_all_NotUsedInMS.pdf"), height = 40, width = 30)
ggplot() +
  geom_line(data = SeasDat %>% 
              rename(catchment = "watershed"),
            aes(y = seas, x = month, color = catchment, linetype = Sig2), linewidth = 1.5) +
  geom_point(data =  df2_seas,
             aes(y = FWMC_log_scaled, x = month, color = watershed), shape = 1, size = 0.5, alpha = 0.5) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Significant coef") +
  # facet_grid(watershed ~ solute, scale = "free_y") +
  facet_wrap(vars(catchment, solute), scale = "free_y") +
  xlab(NULL) +
  ylab("Seasonality") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 18),
    panel.grid.minor = element_blank())
dev.off()



## Seasonality by solute: presentation fig ----
colsseasNO3 <- c(
  "#fde725", # yellow-BBWM
  "#05d5f5", "#6eb5db", "#aad6f0", "#0367a1", "#0505f7", "#084ec7", # blue-DOR
  "#7f039e", "#8507fa", "#7b54a1",   # purple-ELA
  "#1e5920", "#5ec962", "#029e07", "#07f0b2", # green-HBEF
  # "#35528b", "#c392f0", # lavender-MEF
  "#21918c", # teal-SLP
  "#f51505", "#f0079a", "#f095ce", # red-TLW
  "orange red", "#f5965f" # orange-HJA         
)


### Nitrate x site ----
# not used in MS
seas.NO3.pl <- SeasDat %>% 
                rename(catchment = "watershed") %>% 
                filter(solute2 == "Nitrate") %>%
                mutate(catchment = fct_relevel(catchment,
                                               # BBWM
                                               "EB", 
                                               # DOR
                                               "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                               # ELA
                                               "EIF",    "NEIF",   "NWIF",
                                               # HBEF
                                               "WS6",    "WS7",    "WS8",    "WS9",
                                               # MEF - NO NO3 DATA FOR MEF
                                               # "S2",     "S5",
                                               # SLP
                                               "W9",
                                               # TLW
                                               "C32",    "C35",    "C38",
                                               # HJA
                                               "GSWS08", "GSWS09")) %>% 
                           ggplot(aes(y = seas, x = month, color = catchment, linetype = Sig2)) +
                              geom_line(linewidth = 1.25) +
                              scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Significant coef") +
                              scale_color_manual(values = colsseasNO3) +
                              xlab("month") +
                              ylab(expression("seasonality of"~NO[3]^{"-"})) +
                              scale_x_continuous(limits = c(1, 12), expand = c(0, 0)) +
                              guides(linetype = "none") +
                              theme_bw() +
                              theme(legend.position = "right",
                                legend.background = element_rect(fill = NA, color = NA),
                                legend.text = element_text(size = 28),
                                legend.title = element_text(size = 30),
                                panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", linewidth = 2),
                                plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
                                axis.text = element_text(size = 30),
                                axis.title.x = element_text(size = 30),
                                axis.title.y = element_text(size = 30))

ggsave(seas.NO3.pl, path = "04_plots", file = "03c_Seas_NO3bySite_NotUsed.pdf", width = 11, height = 8, units = "in")

### Fig 5 ----
# all sites x solute
seas.sol.pl <- SeasDat %>% 
              rename(catchment = "watershed") %>% 
              mutate(catchment = fct_relevel(catchment,
                                             # BBWM
                                             "EB", 
                                             # DOR
                                             "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                             # ELA
                                             "EIF",    "NEIF",   "NWIF",
                                             # HBEF
                                             "WS6",    "WS7",    "WS8",    "WS9",
                                             # MEF - NO NO3 DATA FOR MEF
                                             "S2",     "S5",
                                             # SLP
                                             "W9",
                                             # TLW
                                             "C32",    "C35",    "C38",
                                             # HJA
                                             "GSWS08", "GSWS09")) %>% 
                ggplot(aes(y = seas, x = month, color = catchment, linetype = Sig2)) +
                  geom_line(linewidth = 1.25) +
                  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = "Significant coef") +
                  scale_color_manual(values = cols1) +
                  facet_wrap(vars(solute2), nrow = 3, ncol = 3) +
                  xlab("Month") +
                  ylab("Seasonality") +
                  scale_x_continuous(limits = c(1, 12), expand = c(0, 0),
                                     breaks = c(1,4,8, 12)) +
                  theme_bw() +
                  theme(legend.position = "right",
                    legend.text = element_text(size = 24),
                    legend.title = element_text(size = 24),
                    panel.grid = element_blank(),
                    panel.border = element_rect(color = "black", linewidth = 2),
                    plot.margin = unit(c(t = 0.5, r = 1.5, b = 0.5, l = 0.5), "cm"),
                    axis.text = element_text(size = 30),
                    axis.title.y = element_text(size = 30),
                    axis.title.x = element_text(size = 30),
                    strip.background = element_blank(),
                    strip.text = element_text(size = 34),
                    panel.spacing.x = unit(30,"pt"))

ggsave(seas.sol.pl, path = "04_plots", file = "03c_Seas_SiteBySolute_Fig5.pdf", width = 17, height = 10, units = "in")


# Save image ----
# save.image("07_Rdat/03_MSplots.Rdata")
# load("07_Rdat/03_MSplots.Rdata")
