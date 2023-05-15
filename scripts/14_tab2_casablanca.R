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

# MC
round(table(arep_clsd$healthbill)/nrow(arep_clsd), 3) # ACA 1
round(table(arep_clsd$healthcare.pk1)/nrow(arep_clsd), 3) # ACA 2

# = 10
# ACA 1: death panels, medicare, cuts.benefits, future.increases
# ACA 2: illegal, single.payer, mammograms, upper.class

misinfo_stems <- c("death.panel", "medicare", "cuts.benefits", "illegal", "single.payer", "mammograms")
misinfo_10    <- sapply(arep_conf[, misinfo_stems], function(x) round(mean(x == 1), 3))
misinfo_0     <- sapply(arep_conf[, c("future.increase", "upper.class")], function(x) round(mean(x == 1), 3))

misinfo_8     <- sapply(arep_conf[, misinfo_stems], function(x) round(mean(x > .8), 3))
misinfo_2     <- sapply(arep_conf[, c("future.increase", "upper.class")], function(x) round(mean(x < .2), 3))

arep_misinfo  <- cbind(c(misinfo_10, misinfo_0), c(misinfo_8, misinfo_2))

## SREP
## --------

srep_conf       <- subset(srep, correct.or.conf == "percent")
srep_clsd       <- subset(srep, correct.or.conf == "regular")

# MC
round(table(srep_clsd$healthbill)/nrow(srep_clsd), 3) # ACA 1
round(table(srep_clsd$healthcare.pk1)/nrow(srep_clsd), 3) # ACA 2

# = 10, 0
misinfo_10    <- sapply(srep_conf[, misinfo_stems], function(x) round(mean(x == 1), 3))
misinfo_0     <- sapply(srep_conf[, c("future.increase", "upper.class")], function(x) round(mean(x == 1), 3))

# > 8, < 2
misinfo_8     <- sapply(srep_conf[, misinfo_stems], function(x) round(mean(x > .8), 3))
misinfo_2     <- sapply(srep_conf[, c("future.increase", "upper.class")], function(x) round(mean(x < .2), 3))

srep_misinfo  <- cbind(c(misinfo_10, misinfo_0), c(misinfo_8, misinfo_2))

## MTurk
## ----------
# no gg2 as we screwed up 

# Convenient subsets
mturk_clsd  <- subset(mturk, rg_test == 'closed' & rgc_o_aca != "")
mturk_scale <- subset(mturk, rg_test == 'scale')

# Get proportions of MC options
round(table(mturk_clsd$rgc_o_aca)/nrow(mturk_clsd), 3)
round(table(mturk_clsd$rgc_o_aca2)/nrow(mturk_clsd), 3)
round(table(mturk_clsd$rgc_o_gg)/nrow(mturk_clsd), 3)
round(table(mturk_clsd$rgc_o_gg2)/nrow(mturk_clsd), 3)
round(table(mturk_clsd$rgc_o_dt)/nrow(mturk_clsd), 3)

# Get proportions of MC 
# ACA 1: Illegal, Replace pvt. w/ single payer, increase payroll tax, reimburse mammograms
# ACA 2: Death Panel, public option, limit increase, cut benefits to medicare
# GG 1: respiratory problems, lung cancer, damage ozone, cause sea level rise
# GG 2: unconnected to burning gas, produced more by clean coal, produced by nuclear, reduced by trees
# DT 1: deports illegals, strips imm. of green cards, strips muslim maj. green cards, bans imm. from muslim maj.

misinfo_stems <- paste0("rg_s_", c("aca_1", "aca_2", "aca_4", 
	                               "aca2_1", "aca2_2", "aca2_4", 
	                               "gg_1", "gg_2", "gg_3",
	                               "gg2_1", "gg2_2", "gg2_3",
	                               "dt_1", "dt_2", "dt_3"))
cor_stems     <-  paste0("rg_s_", c("aca_3", "aca2_3", "gg_4", "gg2_4", "dt_4"))

# = 10, 0
misinfo_10    <- sapply(mturk_scale[, misinfo_stems], function(x) round(mean(as.numeric(x) == 10), 3))
misinfo_0     <- sapply(mturk_scale[, cor_stems], function(x) round(mean(as.numeric(x) == 10), 3))

# > 8, < 2
misinfo_8     <- sapply(mturk_scale[, misinfo_stems], function(x) round(mean(as.numeric(x) > 8), 3))
misinfo_2     <- sapply(mturk_scale[, cor_stems], function(x) round(mean(as.numeric(x) < 2), 3))

mturk_misinfo  <- cbind(c(misinfo_10, misinfo_0), c(misinfo_8, misinfo_2))

# Split by PID
# ------------------

# Mturk
# -----

# = 10, 0
misinfo_10    <- lapply(mturk_scale[, misinfo_stems],   function(x) t(table(mturk_scale$pid, as.numeric(x) == 10)[1:3,2]/table(mturk_scale$pid)[1:3]))
misinfo_0     <- lapply(mturk_scale[, cor_stems], function(x) t(table(mturk_scale$pid, as.numeric(x) == 0)[1:3,2]/table(mturk_scale$pid)[1:3]))

# > 8, < 2
misinfo_8     <- lapply(mturk_scale[, misinfo_stems], function(x) t(table(mturk_scale$pid, as.numeric(x) > 8)[1:3,2]/table(mturk_scale$pid)[1:3]))
misinfo_2     <- lapply(mturk_scale[, cor_stems], function(x) t(table(mturk_scale$pid, as.numeric(x) < 2)[1:3,2]/table(mturk_scale$pid)[1:3]))

mturk_10s <- plyr::ldply(c(misinfo_10, misinfo_0), rbind)
mturk_8s  <- plyr::ldply(c(misinfo_8, misinfo_2), rbind)

write.csv(mturk_10s, file = "casablanca.csv", row.names = F)

# Averages
# ----------
mturk_clsd$misinfo <- with(mturk_clsd, rowMeans(cbind(rgc_o_aca == "Provide coverage for people who are currently in the country illegally", rgc_o_aca2 == "Create government panels to make end-of-life decisions for people on Medicare",
	)))

mturk_scale$misinfo_10 <- with(mturk_scale, rowMeans(cbind(rg_s_aca_1, rg_s_aca_2, rg_s_aca_4, rg_s_aca2_1, rg_s_aca2_2, rg_s_aca2_4, rg_s_gg2_1, rg_s_dt_1, rg_s_dt_2, rg_s_dt_3) == 10))

mturk_scale$misinfo_8 <- with(mturk_scale, rowMeans(cbind(rg_s_aca_1, rg_s_aca_2, rg_s_aca_4, rg_s_aca2_1, rg_s_aca2_2, rg_s_aca2_4, rg_s_gg2_1, rg_s_dt_1, rg_s_dt_2, rg_s_dt_3) > 8))

# Correlations
cor(mturk_scale$misinfo_10,  as.numeric(mturk_scale$pid_strength))
cor(mturk_scale$misinfo_8,   as.numeric(mturk_scale$pid_strength))
cor(mturk_clsd$misinfo,  as.numeric(mturk_clsd$pid_strength))

cor(mturk_scale$misinfo_10,  as.numeric(mturk_scale$vote_1))
cor(mturk_scale$misinfo_8,   as.numeric(mturk_scale$vote_1))
cor(mturk_clsd$misinfo,      as.numeric(mturk_scale$vote_1))

cor(mturk_scale$misinfo_10, as.numeric(car::recode(mturk_scale$pid_strength, "0 = 10; 1 = 9; 2 = 8; 3 = 7; 4 = 6")))
cor(mturk_scale$misinfo_8, as.numeric(car::recode(mturk_scale$pid_strength, "0 = 10; 1 = 9; 2 = 8; 3 = 7; 4 = 6")))
