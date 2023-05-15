#   
#  Table 6: DK-Discouraging vs. DK-Encouraging
#

# Set Working dir 
setwd(basedir) 
setwd("hidden")

# Load libaries
library(reshape2)
library(broom)
library(lme4)
library(lmerTest) #masks lmer; Satterthwaite approximation

# Source Recode Files
source("scripts/01_arep_recode.R")
source("scripts/02_srep_recode.R")

# creates problems with t.test
detach(package:mosaic)

# Obama not asked in AREP
dk_labs <- c("Reagan Budget", "Clinton Budget", "Bush Budget", "Obama Budget")
dk_qs   <- c("rgn", "ctn", "bsh", "obm")  
survey  <- c(rep(c("AREP", "SREP"), each = 3), "SREP")

list_res_arep  <- lapply(arep[, dk_qs[1:3]], function(x) tidy(t.test(x ~ arep$dkencouraging.dk.discouraging == 'Branch A')))
res_dk_arep    <- cbind(survey = "AREP", dk_labs = dk_labs[1:3],  do.call(rbind, list_res_arep))

list_res_srep  <- lapply(srep[, dk_qs], function(x) tidy(t.test(x ~ srep$dkencouraging.dk.discouraging == 'Branch A')))
res_dk_srep    <- cbind(survey = "SREP", dk_labs, do.call(rbind, list_res_srep))

res_dk_fin        <- rbind(res_dk_arep, res_dk_srep)[, c(1:5, 7)]
names(res_dk_fin) <- c("survey", "labs", "diff", "dis", "enc", "p.value")
res_dk_fin        <- cbind(survey, labs=c(dk_labs[1:3], dk_labs), res_dk_fin)

# Guessing corrected
arep_enc <- subset(arep, dkencouraging.dk.discouraging == 'Branch A')
arep_dsc <- subset(arep, dkencouraging.dk.discouraging != 'Branch A')

srep_enc <- subset(srep, dkencouraging.dk.discouraging == 'Branch A')
srep_dsc <- subset(srep, dkencouraging.dk.discouraging != 'Branch A')

arep_enc_rgn_gc <- stnd_cor_sum(arep_enc$reagan[arep_enc$reagan != "Don't Know"] == "Increased", 3)/nrow(arep_enc)
arep_enc_ctn_gc <- stnd_cor_sum(arep_enc$clinton[arep_enc$clinton != "Don't Know"] == "Decreased", 3)/nrow(arep_enc)
arep_enc_bsh_gc <- stnd_cor_sum(arep_enc$wbush[arep_enc$wbush != "Don't Know"] == "Increased", 3)/nrow(arep_enc)

arep_dsc_rgn_gc <- stnd_cor_sum(arep_dsc$reagan2[arep_dsc$reagan2 != "Don't Know"] == "Increased", 3)/nrow(arep_dsc)
arep_dsc_ctn_gc <- stnd_cor_sum(arep_dsc$clinton2[arep_dsc$clinton2 != "Don't Know"] == "Decreased", 3)/nrow(arep_dsc)
arep_dsc_bsh_gc <- stnd_cor_sum(arep_dsc$wbush2[arep_dsc$wbush2 != "Don't Know"] == "Increased", 3)/nrow(arep_dsc)

srep_enc_rgn_gc <- stnd_cor_sum(srep_enc$reagan[srep_enc$reagan != "Don't Know"] == "Increased", 3)/nrow(srep_enc)
srep_enc_ctn_gc <- stnd_cor_sum(srep_enc$clinton[srep_enc$clinton != "Don't Know"] == "Decreased", 3)/nrow(srep_enc)
srep_enc_bsh_gc <- stnd_cor_sum(srep_enc$wbush[srep_enc$wbush != "Don't Know"] == "Increased", 3)/nrow(srep_enc)
srep_enc_obm_gc <- stnd_cor_sum(srep_enc$obama[srep_enc$obama != "Don't Know"] == "Increased", 3)/nrow(srep_enc)

srep_dsc_rgn_gc <- stnd_cor_sum(srep_dsc$reagan2[srep_dsc$reagan2 != "Don't Know"] == "Increased", 3)/nrow(srep_dsc)
srep_dsc_ctn_gc <- stnd_cor_sum(srep_dsc$clinton2[srep_dsc$clinton2 != "Don't Know"] == "Decreased", 3)/nrow(srep_dsc)
srep_dsc_bsh_gc <- stnd_cor_sum(srep_dsc$wbush2[srep_dsc$wbush2 != "Don't Know"] == "Increased", 3)/nrow(srep_dsc)
srep_dsc_obm_gc <- stnd_cor_sum(srep_dsc$obama2[srep_dsc$obama2 != "Don't Know"] == "Increased", 3)/nrow(srep_dsc)

# P-Value for Difference 
arep_rr <- matrix(c(stnd_cor_sum(arep_enc$reagan[arep_enc$reagan != "Don't Know"] == "Increased", 3),
                    stnd_cor_sum(arep_dsc$reagan2[arep_dsc$reagan2 != "Don't Know"] == "Increased", 3),
                    nrow(arep_enc) - stnd_cor_sum(arep_enc$reagan[arep_enc$reagan != "Don't Know"] == "Increased", 3),
                    nrow(arep_dsc)-stnd_cor_sum(arep_dsc$reagan2[arep_dsc$reagan2 != "Don't Know"] == "Increased", 3)), ncol=2)

arep_rr <- tidy(stats::prop.test(arep_rr))
#arep_rr

arep_bc <- matrix(c(stnd_cor_sum(arep_enc$clinton[arep_enc$clinton != "Don't Know"] == "Decreased", 3),
                    stnd_cor_sum(arep_dsc$clinton2[arep_dsc$clinton2 != "Don't Know"] == "Decreased", 3),
                    nrow(arep_enc) - stnd_cor_sum(arep_enc$clinton[arep_enc$clinton != "Don't Know"] == "Decreased", 3),
                    nrow(arep_dsc)-stnd_cor_sum(arep_dsc$clinton2[arep_dsc$clinton2 != "Don't Know"] == "Decreased", 3)), ncol=2)

arep_bc <- tidy(stats::prop.test(arep_bc))
#arep_bc

arep_gb <- matrix(c(stnd_cor_sum(arep_enc$wbush[arep_enc$wbush != "Don't Know"] == "Increased", 3),
                    stnd_cor_sum(arep_dsc$wbush2[arep_dsc$wbush2 != "Don't Know"] == "Increased", 3),
                    nrow(arep_enc) - stnd_cor_sum(arep_enc$wbush[arep_enc$wbush != "Don't Know"] == "Increased", 3),
                    nrow(arep_dsc) - stnd_cor_sum(arep_dsc$wbush2[arep_dsc$wbush2 != "Don't Know"] == "Increased", 3)), ncol=2)
arep_gb <- tidy(stats::prop.test(arep_gb))
#arep_gb

arep_diff_gc <- rbind(arep_rr, arep_bc, arep_gb)[,c(4)]

srep_rr <- matrix(c(stnd_cor_sum(srep_enc$reagan[srep_enc$reagan != "Don't Know"] == "Increased", 3),
                    stnd_cor_sum(srep_dsc$reagan2[srep_dsc$reagan2 != "Don't Know"] == "Increased", 3),
                    nrow(srep_enc) - stnd_cor_sum(srep_enc$reagan[srep_enc$reagan != "Don't Know"] == "Increased", 3),
                    nrow(srep_dsc)-stnd_cor_sum(srep_dsc$reagan2[srep_dsc$reagan2 != "Don't Know"] == "Increased", 3)), ncol=2)

srep_rr <- tidy(stats::prop.test(srep_rr))
#srep_rr

srep_bc <- matrix(c(stnd_cor_sum(srep_enc$clinton[srep_enc$clinton != "Don't Know"] == "Decreased", 3),
                    stnd_cor_sum(srep_dsc$clinton2[srep_dsc$clinton2 != "Don't Know"] == "Decreased", 3),
                    nrow(srep_enc) - stnd_cor_sum(srep_enc$clinton[srep_enc$clinton != "Don't Know"] == "Decreased", 3),
                    nrow(srep_dsc)-stnd_cor_sum(srep_dsc$clinton2[srep_dsc$clinton2 != "Don't Know"] == "Decreased", 3)), ncol=2)

srep_bc <- tidy(stats::prop.test(srep_bc))
#srep_bc

srep_gb <- matrix(c(stnd_cor_sum(srep_enc$wbush[srep_enc$wbush != "Don't Know"] == "Increased", 3),
                    stnd_cor_sum(srep_dsc$wbush2[srep_dsc$wbush2 != "Don't Know"] == "Increased", 3),
                    nrow(srep_enc) - stnd_cor_sum(srep_enc$wbush[srep_enc$wbush != "Don't Know"] == "Increased", 3),
                    nrow(srep_dsc) - stnd_cor_sum(srep_dsc$wbush2[srep_dsc$wbush2 != "Don't Know"] == "Increased", 3)), ncol=2)
srep_gb <- tidy(stats::prop.test(srep_gb))
#srep_gb

srep_bo <- matrix(c(stnd_cor_sum(srep_enc$obama[srep_enc$obama != "Don't Know"] == "Increased", 3),
                    stnd_cor_sum(srep_dsc$obama2[srep_dsc$obama2 != "Don't Know"] == "Increased", 3),
                    nrow(srep_enc) - stnd_cor_sum(srep_enc$obama[srep_enc$obama != "Don't Know"] == "Increased", 3),
                    nrow(srep_dsc) - stnd_cor_sum(srep_dsc$obama2[srep_dsc$obama2 != "Don't Know"] == "Increased", 3)), ncol=2)
srep_bo <- tidy(stats::prop.test(srep_bo))
#srep_bo

srep_diff_gc <- rbind(srep_rr, srep_bc, srep_gb, srep_bo)[,c(4)]
diffpvalue   <- rbind(arep_diff_gc,srep_diff_gc)


# Combine things
res_dk_fin$enc_gc  <- c(arep_enc_rgn_gc, arep_enc_ctn_gc, arep_enc_bsh_gc,
	                    srep_enc_rgn_gc, srep_enc_ctn_gc, srep_enc_bsh_gc, srep_enc_obm_gc)
res_dk_fin$dsc_gc  <- c(arep_dsc_rgn_gc, arep_dsc_ctn_gc, arep_dsc_bsh_gc,
	                    srep_dsc_rgn_gc, srep_dsc_ctn_gc, srep_dsc_bsh_gc, srep_dsc_obm_gc)
# Differences with GC
res_dk_fin$diff_dsc_enc_gc <- res_dk_fin$dsc_gc - res_dk_fin$enc_gc

res_dk_fin <- cbind(res_dk_fin,diffpvalue)

res_dk_fin <- res_dk_fin[,c(1,2,3,4,7,9,6,10,5,8,11,12)]

# Averages
res_dk_fin[nrow(res_dk_fin) + 1, 5:ncol(res_dk_fin)] <- colMeans(res_dk_fin[1:3, 5:ncol(res_dk_fin)])
res_dk_fin[nrow(res_dk_fin) + 1, 5:ncol(res_dk_fin)] <- colMeans(res_dk_fin[4:7, 5:ncol(res_dk_fin)])

write.csv(res_dk_fin, file = "res/tab6_arep_srep_dk_enc_disc.csv", row.names = F)
