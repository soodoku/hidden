#   
#  Table 7: MC Vs. Prob.
#

# Set Working dir 
setwd(basedir) 
setwd("hidden")

# Load libaries
library(plyr)
library(broom)
library(lme4)
library(lmerTest) #masks lmer; Satterthwaite approximation

# Source Recode Files
source("scripts/01_arep_recode.R")
source("scripts/02_srep_recode.R")
source("scripts/04_mturk_recode.R")

## AREP 
## -------

arep_conf       <- subset(arep, correct.or.conf == "percent")
arep_clsd       <- subset(arep, correct.or.conf == "regular")

conf_know_qs    <- c("rg.rg", "sb.sb", "future.increase", "upper.class")
clsd_qs         <- paste0(conf_know_qs, ".c") 

conf_know_labs  <- c("Robert Gates", "Stephen Breyer", "Future Increase", "Upper Income")

arep_conf_list  <- lapply(arep_conf[, conf_know_qs], function(x) c(mean_10 = mean(x == 1), 
                                                                  conf10_se = se_prop(mean(x == 1), nrow(arep_conf)), 
                                                                  mean_8 = mean(x > .8), 
                                                                  conf8_se = se_prop(mean(x > .8), nrow(arep_conf))))
arep_conf_df    <- ldply(arep_conf_list, rbind)
arep_conf_df    <- cbind(arep_conf_df, ldply(lapply(arep_clsd[, clsd_qs],  function(x) c(closed = mean(x == 1), 
                                                                                         closed_se = se_prop(mean(x == 1), nrow(arep_clsd))))))
# Guessing corrected
arep_rg_gc  <- with(arep_clsd, stnd_cor_sum(rg[rg != "Don't Know"] == "Robert Gates", 4))
arep_sb_gc  <- with(arep_clsd, stnd_cor_sum(sb[sb != "Couldn't Say"] == "Stephen Breyer", 4))
arep_ac1_gc <- with(arep_clsd, stnd_cor_sum(healthbill[healthbill != "Couldn't Say"] == "Limits future increases in payments to Medicare providers", 4))
arep_ac2_gc <- with(arep_clsd, stnd_cor_sum(str_detect(healthcare.pk1[healthcare.pk1 != "Couldn't say"], "Increases the Medicare payroll"), 4))

arep_gc     <- c(arep_rg_gc, arep_sb_gc, arep_ac1_gc, arep_ac2_gc)/nrow(arep_clsd)

# Combine
arep_conf_df    <- cbind(survey = "AREP", labs = conf_know_labs, arep_conf_df, gc = arep_gc)

## SREP
## --------

srep_conf       <- subset(srep, correct.or.conf == "percent")
srep_clsd       <- subset(srep, correct.or.conf == "regular")

srep_conf_list  <- lapply(srep_conf[, conf_know_qs], function(x) c(mean_10 = mean(x == 1), 
                                                                   conf10_se = se_prop(mean(x == 1), nrow(arep_conf)), 
                                                                   mean_8 = mean(x > .8), 
                                                                   conf8_se = se_prop(mean(x > .8), nrow(srep_conf))))
srep_conf_df    <- ldply(srep_conf_list, rbind)
srep_conf_df    <- cbind(srep_conf_df, ldply(lapply(srep_clsd[, clsd_qs],  function(x) c(closed = mean(x == 1), 
                                                                                         closed_se = se_prop(mean(x == 1), nrow(srep_clsd)))))) 
# Guessing corrected
srep_rg_gc  <- with(srep_clsd, stnd_cor_sum(rg[rg != "Don't Know"] == "Robert Gates", 4))
srep_sb_gc  <- with(srep_clsd, stnd_cor_sum(sb[sb != "Couldn't Say"] == "Stephen Breyer", 4))
srep_ac1_gc <- with(srep_clsd, stnd_cor_sum(healthbill[healthbill != "Couldn't Say"] == "Limits future increases in payments to Medicare providers", 4))
srep_ac2_gc <- with(srep_clsd, stnd_cor_sum(str_detect(healthcare.pk1[healthcare.pk1 != "Couldn't say"], "Increases the Medicare payroll"), 4))

srep_gc     <- c(srep_rg_gc, srep_sb_gc, srep_ac1_gc, srep_ac2_gc)/nrow(srep_clsd)

# Combine
srep_conf_df    <- cbind(survey = "SREP", labs = conf_know_labs, srep_conf_df, gc = srep_gc)

## pee-values
# With correct
arep_rg_p   <- tidy(prop.test(c(sum(arep_clsd$rg.rg.c),  sum(arep_conf$rg.rg == 1)),   n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_sb_p   <- tidy(prop.test(c(sum(arep_clsd$sb.sb.c),  sum(arep_conf$sb.sb == 1)),  n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_fi_p   <- tidy(prop.test(c(sum(arep_clsd$future.increase.c),  sum(arep_conf$future.increase == 1)),    n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_uc_p   <- tidy(prop.test(c(sum(arep_clsd$upper.class.c),      sum(arep_conf$upper.class == 1)),    n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
srep_rg_p  <- tidy(prop.test(c(sum(srep_clsd$rg.rg.c),  sum(srep_conf$rg.rg == 1)),   n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_sb_p  <- tidy(prop.test(c(sum(srep_clsd$sb.sb.c),  sum(srep_conf$sb.sb == 1)),   n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_fi_p  <- tidy(prop.test(c(sum(srep_clsd$future.increase.c),  sum(srep_conf$future.increase == 1)),     n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_uc_p  <- tidy(prop.test(c(sum(srep_clsd$upper.class.c), sum(srep_conf$upper.class == 1)),    n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value

c_peas   <- c(arep_rg_p, arep_sb_p, arep_fi_p, arep_uc_p, srep_rg_p, srep_sb_p, srep_fi_p, srep_uc_p)

## pee-values
# With correct
arep_rg_gp  <- tidy(prop.test(c(arep_rg_gc,   sum(arep_conf$rg.rg == 1)),  n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_sb_gp  <- tidy(prop.test(c(arep_sb_gc,   sum(arep_conf$sb.sb == 1)),  n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_fi_gp  <- tidy(prop.test(c(arep_ac1_gc,  sum(arep_conf$future.increase == 1)), n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
arep_uc_gp  <- tidy(prop.test(c(arep_ac2_gc,  sum(arep_conf$upper.class == 1)),    n = c(nrow(arep_clsd), nrow(arep_conf))))$p.value
srep_rg_gp  <- tidy(prop.test(c(srep_rg_gc,   sum(srep_conf$rg.rg == 1)),   n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_sb_gp  <- tidy(prop.test(c(srep_sb_gc,   sum(srep_conf$sb.sb == 1)),   n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_fi_gp  <- tidy(prop.test(c(srep_ac1_gc,  sum(srep_conf$future.increase == 1)), n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value
srep_uc_gp  <- tidy(prop.test(c(srep_ac2_gc,  sum(srep_conf$upper.class == 1)),    n = c(nrow(srep_clsd), nrow(srep_conf))))$p.value

gc_peas     <- c(arep_rg_gp, arep_sb_gp, arep_fi_gp, arep_uc_gp, srep_rg_gp, srep_sb_gp, srep_fi_gp, srep_uc_gp)

# Combine AREP and SREP
res_conf_clsd   <- rbind(arep_conf_df, srep_conf_df)

# Diff. cols
res_conf_clsd$diff_c_10   <-  res_conf_clsd$closed - res_conf_clsd$mean_10
res_conf_clsd$diff_gc_10  <-  res_conf_clsd$gc - res_conf_clsd$mean_10

# p-vals
res_conf_clsd[, c("c_p", "gc_p")] <- cbind(c_peas, gc_peas)

## Averages
res_conf_clsd[nrow(res_conf_clsd) + 1, c(4, 6, 9, 12, 14, 15)]  <- colMeans(res_conf_clsd[1:4, c(4, 6, 9, 12, 14, 15)] )
res_conf_clsd[nrow(res_conf_clsd) + 1, c(4, 6, 9, 12, 14, 15)]  <- colMeans(res_conf_clsd[5:8, c(4, 6, 9, 12, 14, 15)] )

write.csv(res_conf_clsd, file = "res/tab7_arep_srep_conf_mc.csv", row.names = F)

# Ns
nrow(arep_clsd)
nrow(arep_conf)
nrow(srep_clsd)
nrow(srep_conf)

## MTurk
## ----------
# no gg2 as we screwed up 

# Convenient subsets
mturk_clsd  <- subset(mturk, rg_test == 'closed')
mturk_scale <- subset(mturk, rg_test == 'scale')

# Var. names
stems_1      <- c("aca", "aca2", "gg", "dt") 
stems_2a     <- c("ftc_am", "ftc_mmc", "fvc_am", "fvc_mmc")
stems_2b     <- c("fts_am", "fts_mmc", "fvs_am", "fvs_mmc")

# Labels
labs        <- c("ACA", "ACA2", "Global Warming", "Immigration Order", "Merkel, Text", "McConnell, Text", "Merkel, Photo",
                 "McConnell, Photo", "Avg.")

clsd_1      <- sapply(mturk_clsd[, stems_1], function(x) mean(x))
clsd_2      <- sapply(mturk[, paste0(stems_2a, "_c")], function(x) mean(x))
closed      <- c(clsd_1, clsd_2)

mean_10_1   <- sapply(mturk_scale[, paste0(stems_1, "_10")], function(x) mean(x))
mean_10_2   <- sapply(mturk[, paste0(stems_2b, "_10")], function(x) mean(x))

# Guessing corrected
mturk_aca_gc  <- with(mturk_clsd, stnd_cor_sum(rgc_o_aca[!str_detect(rgc_o_aca, "know") & rgc_o_aca != ""] == "Increase the Medicare payroll tax for upper-income Americans", 4) + stnd_cor_sum(rgc_c_aca[!str_detect(rgc_c_aca, "know") & rgc_c_aca != ""] == "Increase the Medicare payroll tax for upper-income Americans", 4))

mturk_aca2_gc <- with(mturk_clsd, stnd_cor_sum(rgc_o_aca2[!str_detect(rgc_o_aca2, "know") & rgc_o_aca2 != ""] == "Limit future increases in payments to Medicare providers", 4) + stnd_cor_sum(rgc_c_aca2[!str_detect(rgc_c_aca2, "know") & rgc_c_aca2 != ""] == "Limit future increases in payments to Medicare providers", 4))

mturk_gg_gc   <- with(mturk_clsd, stnd_cor_sum(rgc_o_gg[!str_detect(rgc_o_gg, "know") & rgc_o_gg != ""] == "A cause of rising sea levels", 4) + stnd_cor_sum(rgc_c_gg[!str_detect(rgc_c_gg, "know") & rgc_c_gg != ""] == "A cause of rising sea levels", 4))

mturk_dt_gc   <- with(mturk_clsd, stnd_cor_sum(rgc_o_dt[!str_detect(rgc_o_dt, "know") & rgc_o_dt != ""] == "Temporarily ban immigrants from several majority-Muslim countries", 4) + stnd_cor_sum(rgc_c_dt[!str_detect(rgc_c_dt, "know") & rgc_c_aca != ""] == "Temporarily ban immigrants from several majority-Muslim countries", 4))

mturk_gc_1     <- c(mturk_aca_gc, mturk_aca2_gc, mturk_gg_gc, mturk_dt_gc)/nrow(mturk_clsd)

mturk_tam_gc   <- with(mturk, stnd_cor_sum(ftc_am[!str_detect(ftc_am, "know") & ftc_am != ""] == "Chancellor of Germany", 4))
mturk_tmm_gc   <- with(mturk, stnd_cor_sum(ftc_mmc[!str_detect(ftc_mmc, "know") & ftc_mmc != ""] == "U.S. Senate Majority Leader", 4))
mturk_vam_gc   <- with(mturk, stnd_cor_sum(fvc_am[!str_detect(fvc_am, "know") & fvc_am != ""] == "Chancellor of Germany", 4))
mturk_vmm_gc   <- with(mturk, stnd_cor_sum(fvc_mmc[!str_detect(fvc_mmc, "know") & fvc_mmc != ""] == "U.S. Senate Majority Leader", 4))

mturk_gc_2     <- c(mturk_tam_gc, mturk_tmm_gc, mturk_vam_gc, mturk_vmm_gc)/nrow(mturk)
mturk_gc       <- c(mturk_gc_1, mturk_gc_2)

# Prop.test
# With correct
aca_p  <- tidy(prop.test(c(sum(mturk_clsd$aca),  sum(mturk_scale$aca_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
aca2_p <- tidy(prop.test(c(sum(mturk_clsd$aca2), sum(mturk_scale$aca2_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
gg_p   <- tidy(prop.test(c(sum(mturk_clsd$gg),   sum(mturk_scale$gg_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
dt_p   <- tidy(prop.test(c(sum(mturk_clsd$dt),   sum(mturk_scale$dt_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
amt_p  <- tidy(prop.test(c(sum(mturk$ftc_am_c),  sum(mturk$fts_am_10)), n = rep(nrow(mturk), 2)))$p.value
mmt_p  <- tidy(prop.test(c(sum(mturk$ftc_mmc_c), sum(mturk$fts_mmc_10)), n = rep(nrow(mturk), 2)))$p.value
amv_p  <- tidy(prop.test(c(sum(mturk$fvc_am_c),  sum(mturk$fvs_am_10)), n = rep(nrow(mturk), 2)))$p.value
mmv_p  <- tidy(prop.test(c(sum(mturk$fvc_mmc_c), sum(mturk$fvs_mmc_10)), n = rep(nrow(mturk), 2)))$p.value

peas_1 <- c(aca_p, aca2_p, gg_p, dt_p, amt_p, mmt_p, amv_p, mmv_p)

# With Guessing corrected
aca_p  <- tidy(prop.test(c(mturk_aca_gc,  sum(mturk_scale$aca_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
aca2_p <- tidy(prop.test(c(mturk_aca2_gc, sum(mturk_scale$aca2_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
gg_p   <- tidy(prop.test(c(mturk_gg_gc,   sum(mturk_scale$gg_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
dt_p   <- tidy(prop.test(c(mturk_dt_gc ,  sum(mturk_scale$dt_10)), n = c(nrow(mturk_clsd), nrow(mturk_scale))))$p.value
amt_p  <- tidy(prop.test(c(mturk_tam_gc,  sum(mturk$fts_am_10)), n = rep(nrow(mturk), 2)))$p.value
mmt_p  <- tidy(prop.test(c(mturk_tmm_gc,  sum(mturk$fts_mmc_10)), n = rep(nrow(mturk), 2)))$p.value
amv_p  <- tidy(prop.test(c(mturk_vam_gc,  sum(mturk$fvs_am_10)), n = rep(nrow(mturk), 2)))$p.value
mmv_p  <- tidy(prop.test(c(mturk_vmm_gc,  sum(mturk$fvs_mmc_10)), n = rep(nrow(mturk), 2)))$p.value

peas_2   <- c(aca_p, aca2_p, gg_p, dt_p, amt_p, mmt_p, amv_p, mmv_p)

# Combine
res_conf_clsd <- data.frame(closed, mean_10, gc = mturk_gc, p_c_10 = peas_1, p_gc_10 = peas_2)

# Diff. cols
res_conf_clsd$diff_c_10   <-  res_conf_clsd$closed - res_conf_clsd$mean_10
res_conf_clsd$diff_gc_10  <-  res_conf_clsd$gc - res_conf_clsd$mean_10

## Averages
res_conf_clsd[nrow(res_conf_clsd) + 1, ]  <- colMeans(res_conf_clsd)

# Labels
res_conf_clsd$labs <- labs

# Write out to CSV 
write.csv(res_conf_clsd, file = "res/tab7_mturk_conf_mc.csv", row.names = F)

nrow(mturk)
