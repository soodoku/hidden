# 
# Table 4: MC Probe for OE DKs
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

## AREP 
## -------------

# Create a vector of names of knowledge questions (col names)
know_labs <- c("Angela Merkel, Photo", "Angela Merkel, Text", "Mitch McConnell, Photo", 
               "Mitch McConnell, Text", "Average")
#survey    <- rep(c("AREP", "SREP"), each = 4)

arep_vis <- subset(arep, photo.nonphoto.pk == 'photo')
arep_txt <- subset(arep, photo.nonphoto.pk == 'nonphoto')


table(arep_vis$am2c, useNA = "always")

#ttest for p-value
arep_am_vis  <- with(arep_vis,  tidy(t.test(nona(am2c | am21c), nona(am2c))))
arep_am_txt  <- with(arep_txt,  tidy(t.test(nona(am1c | am11c), nona(am1c))))
arep_mm_vis  <- with(arep_vis,  tidy(t.test(nona(mm2c | mm21c), nona(mm2c))))
arep_mm_txt  <- with(arep_txt,  tidy(t.test(nona(mm1c | mm11c), nona(mm1c))))

# Proportion who didn't respond to Open-ended
arep_oe_dk   <- c(mean(is.na(arep_vis$am2c)), mean(is.na(arep_txt$am1c)), 
                  mean(is.na(arep_vis$mm2c)), mean(is.na(arep_txt$mm1c)))

arep_mc_dk   <- c(mean(arep_vis$am21 == "Don't Know", na.rm = T), # mean(arep_vis$am21[is.na(arep_vis$am2)]=="Don't Know")
                  mean(arep_txt$am11 == "Don't Know", na.rm = T),
                  mean(arep_vis$mm21 == "Don't Know", na.rm = T),
                  mean(arep_txt$mm11 == "Don't Know", na.rm = T))

# Guessing corrected MC
am_txt_gc      <-  with (arep_txt, ( sum(am1c, na.rm = T) + 
                      stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4)))/
                  nrow(arep_txt)

am_vis_gc      <-  with (arep_vis, ( sum(am2c, na.rm = T) + 
                      stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4)))/
                  nrow(arep_vis)

mm_txt_gc      <-  with (arep_txt, ( sum(mm1c, na.rm = T) + 
                      stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4))) /
                  nrow(arep_txt)

mm_vis_gc      <-  with (arep_vis, ( sum(mm2c, na.rm = T) + 
                      stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4))) /
                  nrow(arep_vis)

oe_mc_gc     <- c(am_vis_gc, am_txt_gc, mm_vis_gc, mm_txt_gc)


# p-value for [OE+MC/GC – OE] - USING prop.test! -- Verify with Gaurav
am_vis <- matrix(c(sum(nona(arep_vis$am2c)),
                    sum(with(arep_vis, (sum(am2c, na.rm = T) + stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4)))),
                    (nrow(arep_vis)-sum(nona(arep_vis$am2c))),
                    nrow(arep_vis)-sum(with(arep_vis, ( sum(am2c, na.rm = T) + stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4))))), ncol=2)
am_vis <- tidy(stats::prop.test(am_vis))

am_text <- matrix(c(sum(nona(arep_txt$am1c)),
                   sum(with(arep_txt, (sum(am1c, na.rm = T) + stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4)))),
                   (nrow(arep_txt)-sum(nona(arep_txt$am1c))),
                   nrow(arep_txt)-sum(with(arep_txt, ( sum(am1c, na.rm = T) + stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4))))), ncol=2)
am_text <- tidy(stats::prop.test(am_text))

mm_vis <- matrix(c(sum(nona(arep_vis$mm2c)),
                   sum(with(arep_vis, (sum(mm2c, na.rm = T) + stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4)))),
                   (nrow(arep_vis)-sum(nona(arep_vis$mm2c))),
                   nrow(arep_vis)-sum(with(arep_vis, ( sum(mm2c, na.rm = T) + stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4))))), ncol=2)
mm_vis <- tidy(stats::prop.test(mm_vis))

mm_text <- matrix(c(sum(nona(arep_txt$mm1c)),
                    sum(with(arep_txt, (sum(mm1c, na.rm = T) + stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4)))),
                    (nrow(arep_txt)-sum(nona(arep_txt$mm1c))),
                    nrow(arep_txt)-sum(with(arep_txt, ( sum(mm1c, na.rm = T) + stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4))))), ncol=2)
mm_text <- tidy(stats::prop.test(mm_text))

oecgcoe <- rbind(am_vis,am_text,mm_vis,mm_text)[,c(4)]
rm(am_vis, am_text, mm_vis, mm_text)

arep_table3  <- rbind(arep_am_vis, arep_am_txt, arep_mm_vis, arep_mm_txt)[, c(3, 2, 1, 5)]
arep_table3  <- cbind(arep_table3, oe_dk = arep_oe_dk, mc_dk = arep_mc_dk, oe_mc_gc = oe_mc_gc)
arep_table3$gc_diff <-  arep_table3$oe_mc_gc - arep_table3$estimate2
arep_table3  <- cbind(arep_table3, oecgcoe)
arep_table3 <- arep_table3[,c(1,5,2,6,7,3,4,8,9)]
rm(oecgcoe)

names(arep_table3)   <- c("open", "oe_dk", "closed", "mc_dk", "oe_mc_gc", "diff", "p.value", "gc_diff", "p.value")
arep_table3[nrow(arep_table3) + 1, ] <- colMeans(arep_table3)

arep_table3            <- cbind(know_labs, arep_table3)

write.csv(arep_table3, file = "res/arep_fugitive_open_probe_closed.csv", row.names = F)

# Ns
nrow(arep_vis)
nrow(arep_txt)

## SREP 
## -------------

srep_vis <- subset(srep, photo.nonphoto.pk == 'photo')
srep_txt <- subset(srep, photo.nonphoto.pk == 'nonphoto')

srep_am_vis  <- with(srep_vis,  tidy(t.test(nona(am2c | am21c), nona(am2c))))
srep_am_txt  <- with(srep_txt,  tidy(t.test(nona(am1c | am11c), nona(am1c))))
srep_mm_vis  <- with(srep_vis,  tidy(t.test(nona(mm2c | mm21c), nona(mm2c))))
srep_mm_txt  <- with(srep_txt,  tidy(t.test(nona(mm1c | mm11c), nona(mm1c))))

# Proportion who didn't respond to Open-ended
srep_oe_dk   <- c(mean(is.na(srep_vis$am2c)), mean(is.na(srep_txt$am1c)), 
                  mean(is.na(srep_vis$mm2c)), mean(is.na(srep_txt$mm1c)))

srep_mc_dk   <- c(mean(srep_vis$am21 == "Don't Know", na.rm = T), # mean(srep_vis$am21[is.na(srep_vis$am2)]=="Don't Know")
                  mean(srep_txt$am11 == "Don't Know", na.rm = T),
                  mean(srep_vis$mm21 == "Don't Know", na.rm = T),
                  mean(srep_txt$mm11 == "Don't Know", na.rm = T))
 
# Guessing corrected MC
am_txt_gc      <-  with (srep_txt, ( sum(am1c, na.rm = T) + 
                      stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4))) /
                  nrow(srep_txt)

am_vis_gc      <-  with (srep_vis, ( sum(am2c, na.rm = T) + 
                      stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4))) /
                  nrow(srep_vis)

mm_txt_gc      <-  with (srep_txt, ( sum(mm1c, na.rm = T) + 
                      stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4))) /
                  nrow(srep_txt)

mm_vis_gc      <-  with (srep_vis, ( sum(mm2c, na.rm = T) + 
                      stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4))) /
                  nrow(srep_vis)

oe_mc_gc     <- c(am_vis_gc, am_txt_gc, mm_vis_gc, mm_txt_gc)

srep_table3  <- rbind(srep_am_vis, srep_am_txt, srep_mm_vis, srep_mm_txt)[, c(3, 2, 1, 5)]
srep_table3  <- cbind(srep_table3, oe_dk = srep_oe_dk, mc_dk = srep_mc_dk, oe_mc_gc = oe_mc_gc)

srep_table3$gc_diff <-  srep_table3$oe_mc_gc - srep_table3$estimate2


# p-value for [OE+MC/GC – OE] - USING prop.test!
am_vis <- matrix(c(sum(nona(srep_vis$am2c)),
                   sum(with(srep_vis, (sum(am2c, na.rm = T) + stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4)))),
                   (nrow(srep_vis)-sum(nona(srep_vis$am2c))),
                   nrow(srep_vis)-sum(with(srep_vis, ( sum(am2c, na.rm = T) + stnd_cor_sum(am21[is.na(am2) & am21 != "Don't Know"] == "Chancellor of Germany", 4))))), ncol=2)

am_vis 
am_vis <- tidy(stats::prop.test(am_vis))

am_text <- matrix(c(sum(nona(srep_txt$am1c)),
                    sum(with(srep_txt, (sum(am1c, na.rm = T) + stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4)))),
                    (nrow(srep_txt)-sum(nona(srep_txt$am1c))),
                    nrow(srep_txt)-sum(with(srep_txt, ( sum(am1c, na.rm = T) + stnd_cor_sum(am11[is.na(am1) & am11 != "Don't Know"] == "Chancellor of Germany", 4))))), ncol=2)
am_text
am_text <- tidy(stats::prop.test(am_text))

mm_vis <- matrix(c(sum(nona(srep_vis$mm2c)),
                   sum(with(srep_vis, (sum(mm2c, na.rm = T) + stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4)))),
                   (nrow(srep_vis)-sum(nona(srep_vis$mm2c))),
                   nrow(srep_vis)-sum(with(srep_vis, ( sum(mm2c, na.rm = T) + stnd_cor_sum(mm21[is.na(mm2) & mm21 != "Don't Know"] == "Senate Minority Leader", 4))))), ncol=2)
mm_vis <- tidy(stats::prop.test(mm_vis))

mm_text <- matrix(c(sum(nona(srep_txt$mm1c)),
                    sum(with(srep_txt, (sum(mm1c, na.rm = T) + stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4)))),
                    (nrow(srep_txt)-sum(nona(srep_txt$mm1c))),
                    nrow(srep_txt)-sum(with(srep_txt, ( sum(mm1c, na.rm = T) + stnd_cor_sum(mm11[is.na(mm1) & mm11 != "Don't Know"] == "Senate Minority Leader", 4))))), ncol=2)
mm_text <- tidy(stats::prop.test(mm_text))

oecgcoe <- rbind(am_vis,am_text,mm_vis,mm_text)[,c(4)]
rm(am_vis, am_text, mm_vis, mm_text)
 
srep_table3  <- cbind(srep_table3, oecgcoe)
srep_table3 <- srep_table3[,c(1,5,2,6,7,3,4,8,9)]
rm(oecgcoe)


names(srep_table3)    <- c("open", "oe_dk", "closed", "mc_dk", "oe_mc_gc", "diff", "p.value", "gc_diff", "p.value")
srep_table3[nrow(srep_table3) + 1, ] <- colMeans(srep_table3)
srep_table3            <- cbind(know_labs, srep_table3)

write.csv(srep_table3, file = "res/srep_fugitive_open_probe_closed.csv", row.names = F)

# Ns 
nrow(srep_vis)
nrow(srep_txt)
