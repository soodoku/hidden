#   
#  Table 4: MC Probe for OE DKs
# 
 
# Set Working dir  
setwd(basedir) 
setwd("hidden") 

# Load libaries 
library(reshape2)
library(broom)
library(lme4)
library(lmerTest) #masks lmer; Satterthwaite approximation
library(tidyverse)
library(goji)

tab0f <- function(pkitem, dkprompt){
  # test: pkitem <- naes04[!is.na(naes04$o2ldk), ]$cutperm; dkprompt <-naes04[!is.na(naes04$o2ldk), ]$o2ldk
  submeans <- tapply(pkitem,  dkprompt,   FUN = mean, na.rm = T) 
  subns    <- tapply(rep(1, length(pkitem)),    dkprompt,   FUN = sum, na.rm = T) 
  aggmeans <- c(mean(sum(pkitem[dkprompt == 0])/length(pkitem)),  mean(pkitem))
  c(submeans[1], subns[1], submeans[2], subns[2], aggmeans)
} 

#````````````````````````==============================================
# Tab 2: NAES 2004: Hidden Knowledge on the Closed-Ended Questions  ~~
#=====================~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("data/naes/naes04.Rdata")

tab2        <- data.frame(name=1:4, pcor.unprompt=NA,  n.unprompt=NA,  pcor.prompt=NA, n.prompt=NA, agg.unmprompt=NA, agg.prompt=NA,
                      diff=NA, dks=NA, dkprobe=NA, n=NA, wave=NA)
tab2$name   <- c("perm. taxcuts", "kerry on cuts", "over 200,000", "repeal")

# Hidden Knowledge (open-ended)
tab2[1,2:7] <- with(naes04[!is.na(naes04$cutdk), ], tab0f(cutperm,  cutdk))
tab2[2,2:7] <- with(naes04[!is.na(naes04$kerdk), ], tab0f(kerry,    kerdk))
tab2[3,2:7] <- with(naes04[!is.na(naes04$o2ldk), ], tab0f(over2l,   o2ldk))
tab2[4,2:7] <- with(naes04[!is.na(naes04$repdk), ], tab0f(repeal,   repdk))
tab2[,8]    <- tab2$agg.prompt - tab2$agg.unmprompt
tab2[,11]   <- tab2$n.prompt + tab2$n.unprompt
tab2[,9]    <- tab2$n.prompt/tab2$n
tab2

## Getting the DK responses to the answers. 02ld and repdk are tricky since they exist 5 times. Only the first has explicit dks
# table(naes04$ccb29_1, useNA = 'always')
# table(naes04$ccb29_2, useNA = 'always')
# table(naes04$ccb29_3, useNA = 'always')
# table(naes04$ccb29_4, useNA = 'always')
# table(naes04$ccb29_5, useNA = 'always')
# 
# table(naes04$ccb31_1, useNA = 'always')
# table(naes04$ccb31_2, useNA = 'always')
# table(naes04$ccb31_3, useNA = 'always')
# table(naes04$ccb31_4, useNA = 'always')
# table(naes04$ccb31_5, useNA = 'always') 
# table(naes04$ccb31_6, useNA = 'always')

naes04_probe <-
  naes04 %>%
  select(ccb25, ccb27, ccb29_1, ccb31_1, cutdk, kerdk, o2ldk, repdk) %>%
  mutate(cut_probe_dk    = ifelse(ccb25 > 4, 1, 0),
          kerry_probe_dk = ifelse(ccb27 > 4, 1, 0), 
          o2ld_probe_dk  = ifelse(ccb29_1 > 10, 1, 0),
          repe_probe_dk  = ifelse(ccb31_1 > 10, 1, 0)) 

dkprobe11 <-
  naes04_probe %>%
  filter(cutdk == 1 & cut_probe_dk == 1) %>%
  count(cut_probe_dk)

dkprobe12 <-
  naes04_probe %>%
  filter(kerdk == 1 & kerry_probe_dk == 1) %>%
  count(kerry_probe_dk)

dkprobe13 <-
  naes04_probe %>%
  filter(o2ldk == 1 & o2ld_probe_dk == 1) %>%
  count(o2ld_probe_dk)

dkprobe14 <-
  naes04_probe %>%
  filter(repdk == 1 & repe_probe_dk == 1) %>%
  count(repe_probe_dk)

dkprobe <- as.data.frame(rbind(dkprobe11[2], dkprobe12[2], dkprobe13[2], dkprobe14[2]))

tab2[1:4, 10] <-  (dkprobe /tab2$n)

tab2$wave <- rbind("7/8/04-10/24/04", "4/19/04-10/25/04", "1/30/04-10/25/04", "1/16/04-2/17/04")

write.csv(tab2, file="res/tab2.csv")

rm(tab2, dkprobe11, dkprobe12, dkprobe13, dkprobe14, naes04, naes04_probe, dkprobe)



 
#````````````````````````==============================================
# Tab 6: NAES 2008: Hidden Knowledge on Open/Closed-Ended Questions  ~~
#=====================~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("data/naes/naes08h.rdata")
load("data/naes/naes08.rdata")
pkq <- read.csv("data/naes/pkq.csv", header=T)
#pkq$dkprobe <- paste(unlist(strsplit(pkq[,1], "r")), "ar", sep="")
pkq$dkprobe <- sub("r$", "a", pkq$var)
# change from level with numeric values to character
pkq$var <- as.character(pkq$var)

tab6    <- data.frame(name=1:44, topic=1:44, type=NA, pcor.unprompt=NA,  n.unprompt=NA,  pcor.prompt=NA, n.prompt=NA, agg.unprompt=NA, agg.prompt=NA)

for(i in 1:nrow(pkq)){
  tab6$name    <- pkq[,3]
  tab6$topic   <- pkq[,4]
  tab6$type    <- pkq[,5]
  sub          <- naes08[!is.na(naes08[,pkq[i,7]]),]
  tab6[i, 4:9] <- tab0f(nona(sub[,pkq[i,2]]==1),    sub[,pkq[i,7]])
}


# Calculating a couple of values
tab6$dk      <- tab6$n.prompt/(tab6$n.prompt + tab6$n.unprompt)
tab6$n       <- tab6$n.prompt + tab6$n.unprompt
tab6$dkprobe <- (tab6$n.prompt - (tab6$n.prompt * tab6$pcor.prompt))/tab6$n
tab6$n.cop   <- tab6$n.prompt * tab6$pcor.prompt
tab6$n.coup  <- tab6$n.unprompt * tab6$pcor.unprompt
tab6$c.un    <- tab6$n.unprompt * tab6$pcor.unprompt
tab6$c.pr    <- tab6$n.prompt * tab6$pcor.prompt
tab6$unpro   <- tab6$n - tab6$c.un 
tab6$total   <- tab6$n - tab6$c.un - tab6$c.pr
tab6$agg.unprompt   <- (tab6$n.unprompt * tab6$pcor.unprompt)/tab6$n
tab6$agg.prompt     <- (tab6$n.prompt * tab6$pcor.prompt + tab6$n.unprompt * tab6$pcor.unprompt)/tab6$n
tab6$diff           <- tab6$agg.prompt- tab6$agg.unprompt 


# Export table 
tab6_done <- data.frame(name=1:44, cbf=1:44, cap=1:44, diff=1:44, dk=1:44, dkap=1:44, n=1:44, wave=1:44)
tab6_done$name <- tab6$name
tab6_done$cbf  <- round(tab6$agg.unprompt, digits = 3)
tab6_done$cap  <- round(tab6$agg.prompt, digits = 3)
tab6_done$diff <- round(tab6$diff, digits = 3)
tab6_done$dk   <- round(tab6$dk, digits = 3)
tab6_done$dkap <- round(tab6$dkprobe, digits = 3)
tab6_done$n    <- tab6$n
tab6_done$wave <- pkq$dates
tab6_done$wave <- gsub(" \xd0 ", "-", tab6_done$wave)

write.csv(tab6_done, file="res/tab6.csv")







#### --- OLD 
# ANES Recode
source("scripts/03_anes_recode.R")

tab0f <- function(pkitem, dkprompt){
# test: pkitem <- naes04[!is.na(naes04$o2ldk), ]$cutperm; dkprompt <-naes04[!is.na(naes04$o2ldk), ]$o2ldk
  submeans <- tapply(pkitem,  dkprompt,   FUN = mean, na.rm = T) 
  subns    <- tapply(rep(1, length(pkitem)),    dkprompt,   FUN = sum, na.rm = T) 
  aggmeans <- c(mean(sum(pkitem[dkprompt == 0])/length(pkitem)),  mean(pkitem))
  c(submeans[1], subns[1], submeans[2], subns[2], aggmeans)
}

## ANES 2000
## --------------

load("data/anes/nes00.Rdata")

# Post
nes00 <- subset(nes00, prepost)

# Only people who got the experimental treatment
nes00d <- subset(nes00, dkprobe == 1)

tab4       <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab4$name  <- c("lott", "rehnquist", "blair", "reno")

# Hidden Knowledge (open-ended)
tab4[1, 2:7] <- with(nes00d[!is.na(nes00d$lotdk), ], tab0f(lott,      lotdk))
tab4[2, 2:7] <- with(nes00d[!is.na(nes00d$rehdk), ], tab0f(rehnquist, rehdk))
tab4[3, 2:7] <- with(nes00d[!is.na(nes00d$bladk), ], tab0f(blair,     bladk))
tab4[4, 2:7] <- with(nes00d[!is.na(nes00d$rendk), ], tab0f(reno,      rendk))

write.csv(tab4,  file = "res/tab4_anes_2000.csv", row.names = F)

## ANES 2004
## -------------

load("data/anes/nes04.Rdata")

# Post
nes04 <- subset(nes04, nes04$prepost == 1)

tab3    <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab3$name     <- c("hastert", "cheney", "blair", "rehnquist")

# Hidden Knowledge (open-ended)
tab3[1, 2:7] <- tab0f(nes04$know_hastert,     nes04$hasdk)
tab3[2, 2:7] <- tab0f(nes04$know_cheney,      nes04$chedk)
tab3[3, 2:7] <- tab0f(nes04$know_blair,       nes04$bladk)
tab3[4, 2:7] <- tab0f(nes04$know_rehnquist,   nes04$rehdk)

write.csv(tab3,  file = "res/tab4_anes_2004.csv", row.names = F)

## ANES 2008  
## ------------

# Load data
load("data/anes/nes08h.Rdata")

# Post 
nes08 <- subset(nes08, !nes08$prepost)

tab0      <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab0$name <- c("pelosi", "cheney", "roberts", "brown")

# Hidden Knowledge (open-ended)
tab0[1,2:7] <- tab0f(nes08$pelosi,  nes08$peldk)
tab0[2,2:7] <- tab0f(nes08$cheney,  nes08$chedk)
tab0[3,2:7] <- tab0f(nes08$roberts, nes08$robdk)
tab0[4,2:7] <- tab0f(nes08$brown,   nes08$brodk)

write.csv(tab0, file = "res/tab4_anes_2008.csv", row.names = F)
