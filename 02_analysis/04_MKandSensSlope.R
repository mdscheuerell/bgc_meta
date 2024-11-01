# mann kendal and sens slopes
# JMH Nov 2022
# JMH JUN 2023
# JMH Nov 2023

# libraries
library(tidyverse)
library(trend)
library(EnvStats)
library(here)
library(broom)

# Data used in MARSS models ----
df <- readr::read_csv(here::here("03_generatedData", "01g_tbl_solutes_unmanaged_mon_v2.csv")) %>% 
        pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
        # have to remove NAs
        filter(!is.na(FWMC)) %>% 
        # log and center on mean
        group_by(catchment, solute) %>% 
        mutate(FWMC = log(FWMC),
               FWMC = scale(FWMC, scale = FALSE))

# MARSS Model coefs ----
# run in 02b_model_fitting_BiasTerms_1000bootstraps.R
biasBS <- readRDS(file = file.path(here::here("06_ModelRDSFiles"), "mod_set_site_RW_b_BiasTerms_1000.rds"))

# Normal MK & Sens ----
# tested a few to make sure the code below was working
# mk.test(df[df$catchment == "EB" & df$solute == "Ca",]$FWMC)

df_mk <- df %>% 
          group_by(catchment, solute) %>% 
          nest() %>% 
          mutate(mk_pval = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$p.value),
                 mk_n = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$parameter),
                 mk_stat = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$statistic),
                 s_slope = map(data, ~trend::sens.slope(.x$FWMC)$estimates),
                 s_L95 = map(data, ~trend::sens.slope(.x$FWMC)$conf.int[1]),
                 s_U95 = map(data, ~trend::sens.slope(.x$FWMC)$conf.int[2])) %>% 
          unnest(c(mk_pval, mk_n, mk_stat, s_slope, s_L95, s_U95)) %>% 
          select(-data)



## Bias plot ----

### create table of bias estimates (+/- CI) ----

SoluteList <- c("Ca", "DOC", "NO3N", "SO4", "NH4N", "TDP")

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


### cleans up df ----
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
  # Label sig if CI's don't overlap zero
  mutate(Sig = ifelse((loCI > 0 & upCI > 0) | (loCI <0 & upCI < 0),
                      "Sig", "NS"),
         Sig = fct_relevel(Sig, "Sig", "NS"))
     
# combine
df_mk_c <- df_mk %>% 
              mutate(Sig = ifelse(mk_pval < 0.05, "Sig", "NS"),
                     s_slope = s_slope*12*100,
                     s_L95 = s_L95*12*100,
                     s_U95 = s_U95*12*100) %>% 
              select(solute, watershed = catchment, Bias = s_slope, Bias_L95 = s_L95, Bias_U95 = s_U95, pval = mk_pval, Sig) %>% 
              mutate(analysis = "MKandSens",
                     solute = ifelse(solute == "NH4","NH4N",
                                  ifelse(solute == "NO3", "NO3N", solute)))

tbl_fit_bias_bs_c <- tbl_fit_bias_bs %>% 
                          mutate(pval = as.numeric("NA"),
                                 analysis = "MARSS") %>% 
                        select(solute, watershed, Bias = U_perChange_y, Bias_L95 = U_perChange_y_lowCI, 
                               Bias_U95 = U_perChange_y_upCI, pval, Sig, analysis)


MARSSmkTab <- rbind(df_mk_c, tbl_fit_bias_bs_c)


# Seasonal MK & Sen ----
## Import data used in MARSS models 
df.seas <- readr::read_csv(here::here("03_generatedData", "01g_tbl_solutes_unmanaged_mon_v2.csv")) 
  
## Make regular time series for compatibility with Mann-Kendall function
## Log & center FWMC 
df.seas <- df.seas %>% mutate(date = as.Date(round_date(date_decimal(dec_water_yr), "month"), format = "%Y-%m")) %>%
                       group_by(site, catchment) %>%
                       complete(date = seq.Date(from = as.Date("1986-01-01"), to = as.Date("2020-12-01"), by = "month")) %>%
                       mutate(year = as.numeric(format(date, "%Y"))) %>%
                       mutate(month = as.numeric(format(date, "%m"))) %>%
                       pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
                       ungroup() %>%
                       # log and center on mean by solute
                       group_by(solute) %>%
                       mutate(FWMC = c(scale(log(FWMC), scale = FALSE)))
                  

## Seasonal MK all ----                                       

## Remove sites*solutes lacking data
df.seas.rm <- df.seas %>% filter(!(solute == "TDP" & site %in% c("BBWM", "HJA", "MEF", "SLP"))) %>%
                          filter(!(solute == "NH4" & site %in% c("BBWM", "HJA", "MEF", "SLP"))) %>%
                          filter(!(solute == "NO3" & site %in% c("MEF"))) %>%
                          filter(!(solute == "NH4" & catchment %in% c("C32", "C35", "WS6"))) %>%
                          filter(!(solute == "NO3" & catchment %in% c("WS6"))) %>%
                          filter(!(solute == "TDP" & catchment %in% c("WS6", "WS7", "WS8", "WS9"))) %>% 
                          # Sites/solutes lacks data (n <= 3) in dropped months
                          filter(!(catchment == "EIF" & solute %in% c("Ca", "DOC", "NH4", "SO4", "TDP") & month %in% c(4,5))) %>% 
                          filter(!(catchment == "EIF" & solute %in% c("NO3") & month %in% c(3,4,5))) %>% 
                          filter(!(catchment == "NEIF" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(3,4,5)))  %>% 
                          filter(!(catchment == "NWIF" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(3,4,5))) %>% 
                          filter(!(catchment == "S2" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(4,5))) %>% 
                          filter(!(catchment == "S5" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(5,6))) 
  
## Summarize number of time series dropped from seasonal Kenddall due to missing data
n_ts <- df.seas %>% group_by(catchment, solute) %>%
                    summarize(n_comb = n_distinct(unlist(across(where(is.character))))) %>%
                    ungroup() %>%
                    nrow()

n_ts.rm <- df.seas.rm %>% group_by(catchment, solute) %>%
                          summarize(n_comb = n_distinct(unlist(across(where(is.character))))) %>%
                          ungroup() %>%
                          nrow()

## h.test object doesn't play with tidy - need to rerun models for each stat
# by default this uses the continuity correction: correct = TRUE
df_seasmk <- df.seas.rm %>% 
  group_by(catchment, solute) %>% 
  nest() %>% 
  mutate(mk_hetstat = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                           data = .,
                                                           alternative = "two.sided",
                                                           correct = TRUE, #continuity correction is on
                                                           ci.slope = TRUE,
                                                           conf.level = 0.95, 
                                                           independent.obs = FALSE)$statistic[1])) %>%
  mutate(mk_trendzstat = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                           data = .,
                                                           alternative = "two.sided",
                                                           correct = TRUE, #continuity correction is on
                                                           ci.slope = TRUE,
                                                           conf.level = 0.95, 
                                                           independent.obs = FALSE)$statistic[2])) %>%
  mutate(mk_het_pval = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                              data = .,
                                                              alternative = "two.sided",
                                                            correct = TRUE, #continuity correction is on
                                                              ci.slope = TRUE,
                                                              conf.level = 0.95, 
                                                            independent.obs = FALSE)$p.value[1])) %>%
  mutate(mk_pval = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                            data = .,
                                                            alternative = "two.sided",
                                                        correct = TRUE, #continuity correction is on
                                                            ci.slope = TRUE,
                                                            conf.level = 0.95, 
                                                        independent.obs = FALSE)$p.value[2])) %>%
  mutate(mk_df = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                        data = .,
                                                        alternative = "two.sided",
                                                      correct = TRUE, #continuity correction is on
                                                        ci.slope = TRUE,
                                                        conf.level = 0.95, 
                                                      independent.obs = FALSE)$parameter)) %>%
  mutate(sen_sl = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                      data = .,
                                                      alternative = "two.sided",
                                                      correct = TRUE, #continuity correction is on
                                                      ci.slope = TRUE,
                                                      conf.level = 0.95, 
                                                      independent.obs = FALSE)$estimate[["slope"]])) %>%
  mutate(sen_L95 = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                      data = .,
                                                      alternative = "two.sided",
                                                      correct = TRUE, #continuity correction is on
                                                      ci.slope = TRUE,
                                                      conf.level = 0.95, 
                                                      independent.obs = FALSE)$interval$limits[[1]])) %>%
  mutate(sen_U95 = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                        data = .,
                                                        alternative = "two.sided",
                                                        correct = TRUE, #continuity correction is on
                                                        ci.slope = TRUE,
                                                        conf.level = 0.95, 
                                                        independent.obs = FALSE)$interval$limits[[2]])) %>%
         
  unnest(c(mk_hetstat, mk_trendzstat, mk_het_pval, mk_pval, mk_df, sen_sl, sen_L95, sen_U95)) %>% 
  select(-data)

# combine
df_seasmk_c <- df_seasmk %>% mutate(Sig = ifelse(mk_pval < 0.05, "Sig", "NS"),
                                    SeasDifSlopeSig = ifelse(mk_het_pval < 0.05, "Y", "N"),
                                    s_slope = sen_sl*100,
                                    s_L95 = sen_L95*100,
                                    s_U95 = sen_U95*100) %>% 
                              # remove cases where tau is sig - monthly slopes are pos & neg
                              # this removes 28 site x solute combos
                              filter(mk_het_pval > 0.05) %>% 
                             select(solute, watershed = catchment, Bias = s_slope, Bias_L95 = s_L95, Bias_U95 = s_U95, pval = mk_pval, Sig) %>% 
                             mutate(analysis = "SeasMKandSens",
                                    solute = ifelse(solute == "NH4","NH4N",
                                    ifelse(solute == "NO3", "NO3N", solute))) 

df_seasmk_c_WsigHet <- df_seasmk %>% mutate(Sig = ifelse(mk_pval < 0.05, "Sig", "NS"),
                                             SeasDifSlopeSig = ifelse(mk_het_pval < 0.05, "Y", "N"),
                                    s_slope = sen_sl*100,
                                    s_L95 = sen_L95*100,
                                    s_U95 = sen_U95*100) %>% 
                              select(solute, watershed = catchment, Bias = s_slope, Bias_L95 = s_L95, Bias_U95 = s_U95, 
                                     pval = mk_pval, Sig, mk_het_pval, SeasDifSlopeSig) %>% 
                              mutate(analysis = "SeasMKandSens",
                                     solute = ifelse(solute == "NH4","NH4N",
                                                     ifelse(solute == "NO3", "NO3N", solute))) 

tbl_fit_bias_bs_c <- tbl_fit_bias_bs %>% 
  mutate(pval = as.numeric("NA"),
         analysis = "MARSS") %>% 
  select(solute, watershed, Bias = U_perChange_y, Bias_L95 = U_perChange_y_lowCI, 
         Bias_U95 = U_perChange_y_upCI, pval, Sig, analysis)


MARSSseasmkTab <- rbind(df_seasmk_c, tbl_fit_bias_bs_c)
MARSSseasmkTab_WsigTaus <- rbind(df_seasmk_c_WsigHet, tbl_fit_bias_bs_c)

ggplot(MARSSseasmkTab %>% 
         ungroup() %>% 
         group_by(analysis) %>% 
         mutate(ID = as.factor(paste0(solute,"_", watershed)),
                ID = fct_reorder(ID, Bias)), aes(y = Bias, x = ID, shape = analysis, color = Sig)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(analysis))


## FIG 7 ----
Fig7df <- MARSSseasmkTab %>%
                            ungroup() %>% 
                            mutate(watershed = as.factor(watershed),
                                   watershed = fct_reorder(watershed, Bias)) %>% 
                            mutate(watershed = fct_relevel(watershed,
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
                            mutate(Sig = case_when(Sig == "Sig" ~ "S", Sig == "NS" ~ "NS"),
                                   analysis = case_when(analysis == "MARSS" ~ "MARSS",
                                   analysis == "SeasMKandSens" ~ "Sens slope"),
                                   solute2 = fct_recode(solute, "Calcium" = "Ca", "DOC" = "DOC",
                              "Ammonium" = "NH4N", "Nitrate" = "NO3N", "TDP" = "TDP",
                              "Sulfate" = "SO4")) 


  # Fig 7 ----
  Fig7df2 <- Fig7df %>% 
    mutate(analysis = ifelse(analysis == "Sens slope", "SS", analysis)) %>% 
    pivot_wider(id_cols = c(solute,watershed, solute2), names_from = analysis, values_from = Bias:Sig) %>% 
    mutate(significance = ifelse(Sig_SS == "S" & Sig_MARSS == "NS", "Kendall",
                              ifelse(Sig_SS == "NS" & Sig_MARSS == "S", "MARSS",
                                  ifelse(Sig_SS == "S" & Sig_MARSS == "S", "both", "none")))) %>%
    filter(!is.na(Sig_SS)) %>% 
    # censure error bars b/w -15 and 15
    mutate(Bias_L95_SS = ifelse(Bias_L95_SS < -15, -15, Bias_L95_SS),
           Bias_L95_MARSS = ifelse(Bias_L95_MARSS < -15, -15, Bias_L95_MARSS),
           Bias_U95_SS = ifelse(Bias_U95_SS > 15, 15, Bias_U95_SS),
           Bias_U95_MARSS = ifelse(Bias_U95_MARSS > 15, 15, Bias_U95_MARSS)) %>% 
    droplevels() 
    
  Fig7ScatterColors <- c("red", "pink", "blue", "white")
  
Fig7 <- 
  ggplot(Fig7df2, aes(y = Bias_SS, x = Bias_MARSS, fill = significance)) +
    geom_errorbar(data = Fig7df2, aes(y = Bias_SS, x = Bias_MARSS, ymin = Bias_L95_SS, ymax = Bias_U95_SS), color = "grey") +
    geom_errorbarh(data = Fig7df2, aes(y = Bias_SS, xmin = Bias_L95_MARSS, xmax = Bias_U95_MARSS), color = "grey") +
    geom_point(shape = 21, size = 3) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~ solute2, nrow = 3, scales = "free") +
    xlab(expression(paste("MARSS bias (%"~y^{"-1"}*")"))) +
    ylab(expression(paste("Kendall seasonal trend (%"~y^{"-1"}*")"))) +
    #xlim(-15,10) +
    #ylim(-15,10) +
    scale_fill_manual(values = Fig7ScatterColors) +
    theme_bw() +
    theme(legend.position = "right",
          legend.background = element_rect(fill = NA, color = NA),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 22, face = "bold"),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 2),
          plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 24, vjust = 0.5, hjust = 1, angle = 90),
          axis.title.x = element_text(size = 30),
          strip.background = element_blank(),
          strip.text = element_text(size = 30))

ggsave(Fig7, path = "04_plots", file = "04_SeasKendalBiasVMarssBiasPlot_Fig7.png", units = "in", height = 13, width = 10)

## export table ----
MARSSseasmkTab_2 <- MARSSseasmkTab_WsigTaus %>% 
  pivot_wider(id_cols = c("solute", "watershed"), names_from = analysis, values_from = Bias:SeasDifSlopeSig) %>% 
  select(solute, watershed, Bias_MARSS, Bias_L95_MARSS, Bias_U95_MARSS, Sig_MARSS,
         Bias_SeasMKandSens, Bias_L95_SeasMKandSens, Bias_U95_SeasMKandSens, Sig_SeasMKandSens, 
         Taupval_MKandSens = pval_SeasMKandSens,
         mk_het_pval_SeasMKandSens) %>% 
  arrange(solute, watershed)

write.csv(MARSSseasmkTab_2, "05_Tables/04_MARSS_seasSensSlops_BiasTable.csv", row.names = FALSE)

## Compare Kendall & MK significance tests
# heterogeneous Kendall trends by month
n_het <- MARSSseasmkTab_2 %>% filter(mk_het_pval_SeasMKandSens <= 0.05) %>% nrow()
n_tot <- MARSSseasmkTab_2 %>% filter(!is.na(mk_het_pval_SeasMKandSens)) %>% nrow()

tot <- MARSSseasmkTab_2 %>%
  filter(Bias_MARSS != 0) %>%
  nrow()

MARSSsig <- MARSSseasmkTab_2 %>% filter(Bias_MARSS != 0) %>%
                                 filter(Sig_MARSS == "Sig" & Sig_SeasMKandSens == "NS") 


Kensig <- MARSSseasmkTab_2 %>% filter(mk_het_pval_SeasMKandSens > 0.05) %>%
                               filter(Bias_MARSS != 0) %>%
                               filter(Sig_MARSS == "NS" & Sig_SeasMKandSens == "Sig") 

bothsig <- MARSSseasmkTab_2 %>% filter(mk_het_pval_SeasMKandSens > 0.05) %>%
                                filter(Bias_MARSS != 0) %>%
                                filter(Sig_MARSS == "Sig" & Sig_SeasMKandSens == "Sig") %>%
                                mutate(slcomp = Bias_MARSS/Bias_SeasMKandSens)


# save/load ----
# save.image("07_Rdat/04_MKandSensSlope.Rdata")
# load("analysis/08_MKandSensSlope_Rdat")
