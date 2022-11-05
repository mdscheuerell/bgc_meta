# mann kendal and sens slopes
# JMH Nov 2022

# libraries
library(tidyverse)
library(trend)

# Data used in MARSS models
df <- readr::read_csv(here::here("data", "tbl_solutes_unmanaged_mon_v2.csv")) %>% 
        pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
        # have to remove NAs
        filter(!is.na(FWMC)) %>% 
        # log and center on mean
        group_by(catchment, solute) %>% 
        mutate(FWMC = log(FWMC),
               FWMC = scale(FWMC, scale = FALSE))

# Mann-Kendall & sens slope
# tested a few to make sure the code below was workign
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

# Model coefs ----
# run in 04_model_fitting_BiasTerms_1000bootstraps.R
biasBS <- readRDS(file = file.path(here::here("analysis"), "mod_set_site_RW_b_BiasTerms_1000.rds"))

# Bias plot ----

## create table of bias estimates (+/- CI) ----

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


## cleans up df ----
tbl_fit_bias_bs <- tbl_fit_bootstrap %>% 
  # remove seasonality coefs
  filter(!grepl("seas", site)) %>% 
  separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
  mutate_at("region", str_remove, "X.") %>%
  #units percent decline/mo to percent decline/yr
  mutate(
    # Convert coef to % change/year
    # GET MARK TO CHECK
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

ggplot(MARSSmkTab, aes(y = Bias, x = analysis, color = Sig)) +
  geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) +
  facet_grid(solute ~ watershed, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(MARSSmkTab %>% 
         ungroup() %>% 
         group_by(analysis) %>% 
         mutate(ID = as.factor(paste0(solute,"_", watershed)),
                ID = fct_reorder(ID, Bias)), aes(y = Bias, x = ID, shape = analysis, color = Sig)) +
        geom_point()+
        theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(analysis), scales = "free_y")
        # geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) 

pdf(file = file.path(here::here("plots"), "MARSS_Sens_BiasPlots_All.pdf"))
for(i in 1:length(SoluteList)){
  Solute_i = SoluteList[i]
  BiasPlot_i <- ggplot(MARSSmkTab %>%
           filter(solute == Solute_i) %>% 
           ungroup() %>% 
           mutate(watershed = as.factor(watershed),
                  watershed = fct_reorder(watershed, Bias)), aes(y = Bias, x = watershed, shape = analysis, color = Sig)) +
            geom_point()+
            # geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_color_manual(values = c("black", "blue")) +
            # theme_bw() +
            ggtitle(Solute_i)
  print(BiasPlot_i)
}
dev.off()

save.image("08_MKandSensSlope_Rdat")
