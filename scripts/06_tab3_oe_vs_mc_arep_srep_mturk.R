#
# Table 3: MC vs. OE 
# Fugitive Knowledge: MC vs. OE 
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
source("scripts/04_mturk_recode.R")

# Detach mosaic
detach(package:mosaic)

## AREP and SREP
## ---------------

labs   <- rep(c("WW 2 Casualties - USSR", "Female Mortality - Heart"), 2)
survey <- rep(c("AREP", "SREP"), each = 2)

tt_arep_ww2  <- tidy(t.test(ww2_cas_oc ~ open.closed, arep))
tt_arep_fd   <- tidy(t.test(fd_oc ~ open.closed, arep))
tt_srep_ww2  <- tidy(t.test(ww2_cas_oc ~ open.closed, srep))
tt_srep_fd   <- tidy(t.test(fd_oc ~ open.closed, srep))

tab_2_fin            <- rbind(tt_arep_ww2, tt_arep_fd, tt_srep_ww2, tt_srep_fd)[, c(2, 3, 1, 5)]
names(tab_2_fin)     <- c("closed", "open", "diff", "p.value")

# append labels
tab_2_fin            <- cbind(survey, labs, tab_2_fin)

# AREP Average
arep_long <- reshape2::melt(arep[, c("ww2_cas_oc", "fd_oc", "userid", "open.closed")], id.vars = c("userid", "open.closed"))

m1 <- lmer(value ~ open.closed + (1 | userid), data = arep_long, REML = FALSE)
tab_2_fin[(nrow(tab_2_fin) + 1), ] <- c("AREP", "Avg.", colMeans(tab_2_fin[1:2, 3:4]), coef(summary(m1))[2, c(1, 5)])

# SREP Average
srep_long <- reshape2::melt(srep[, c("ww2_cas_oc", "fd_oc", "userid", "open.closed")], id.vars = c("userid", "open.closed"))

m1 <- lmer(value ~ open.closed + (1 | userid), data = srep_long, REML = FALSE)
tab_2_fin[(nrow(tab_2_fin) + 1), ] <- c("SREP", "Avg.", colMeans(sapply(tab_2_fin[3:4, 3:4], as.numeric)), coef(summary(m1))[2, c(1, 5)])

# Write out
write.csv(tab_2_fin, file="res/arep_srep_closed_open.csv", row.names = F)

# Ns
table(arep$open.closed)
table(srep$open.closed)

## MTurk
## ---------------

labs      <- c("McConnel - Text", "Merkel - Text", "McConnel - Photo", "Merkel - Photo")

mmc_text  <- with(mturk, tidy(t.test(mmc_photo_oc ~ probe_test)))
mmc_photo <- with(mturk, tidy(t.test(mmc_text_oc ~ probe_test)))
am_text   <- with(mturk, tidy(t.test(am_photo_oc ~ probe_test)))
am_photo  <- with(mturk, tidy(t.test(am_text_oc ~ probe_test)))

tab_2m_fin            <- rbind(mmc_text, mmc_photo, am_text, am_photo)[, c(2, 3, 1, 5)]
names(tab_2m_fin)     <- c("closed", "open", "diff", "p.value")
tab_2m_fin            <- cbind(labs, tab_2m_fin)

# MTurk Average
mturk_long <- reshape2::melt(mturk[, c("mmc_photo_oc", "mmc_text_oc", "am_photo_oc", "am_text_oc", "ResponseId", "probe_test")], 
	                         id.vars = c("ResponseId", "probe_test"))

m1 <- lmer(value ~ I(probe_test =='closed') + (1 | ResponseId), data = mturk_long, REML = FALSE)
tab_2m_fin[(nrow(tab_2m_fin) + 1), ] <- c("AREP", colMeans(tab_2m_fin[, 2:3]), coef(summary(m1))[2, c(1, 5)])

write.csv(tab_2m_fin, file="res/mturk_closed_open.csv", row.names=F)

# Ns
table(mturk$probe_test)
