# 
# Table 2: Text Vs. Visual
# Identifying Job or Position Held by Public Figures Described by Name or Photo
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
 
## AREP 
## ---------------

know_qs   <- paste0(c("nsc", "jnc", "hrc", "am1c", "mm1c"), "_pnp")
know_labs <- c("Nicholas Sarkozy", "Janet Napolitano", "Harry Reid", "Angela Merkel", "Mitch McConnell")

list_res_arep      <- lapply(arep[, know_qs], function(x) tidy(t.test(nona(x) ~ photo.nonphoto.pk == 'photo', arep)))
res_arep_vis_txt   <- do.call(rbind, list_res_arep)

res_arep_vis_txt_fin        <- res_arep_vis_txt[, c(1:3, 5)]

# Calculate the average
res_arep_vis_txt_fin[nrow(res_arep_vis_txt_fin) + 1, ] <- colMeans(res_arep_vis_txt_fin)

# Calculate the p-value of the average diff.
# Melt the data.frame
arep_long <- melt(arep[, c(know_qs, "userid", "photo.nonphoto.pk")], id.vars = c("userid", "photo.nonphoto.pk"))
# lmer it
m1 <- lmer(nona(value) ~ photo.nonphoto.pk + (1 | userid), data = arep_long, REML = FALSE)
res_arep_vis_txt_fin[nrow(res_arep_vis_txt_fin), "p.value"] <- coef(summary(m1))[2, 5]

names(res_arep_vis_txt_fin) <- c("Diff", "Text", "Photo", "p.value")
res_arep_vis_txt_fin        <- res_arep_vis_txt_fin[, c("Text", "Photo", "Diff", "p.value")]
res_arep_vis_txt_fin        <- cbind(labs = c(know_labs, "Avg."), res_arep_vis_txt_fin)

write.csv(res_arep_vis_txt_fin, file = "res/arep_vis_txt.csv", row.names = F)

table(arep$photo.nonphoto.pk) # Ns

## SREP  
## ---------------

know_qs   <- paste0(c("nsc", "jnc", "hrc", "am1c", "mm1c"), "_pnp")
know_labs <- c("Nicholas Sarkozy", "Janet Napolitano", "Harry Reid", "Angela Merkel", "Mitch McConnell")

list_res_srep      <- lapply(srep[, know_qs], function(x) tidy(t.test(nona(x) ~ photo.nonphoto.pk == 'photo', srep)))
res_srep_vis_txt   <- do.call(rbind, list_res_srep)

# Calculate the average
res_srep_vis_txt_fin    <- res_srep_vis_txt[, c(1:3, 5)]
res_srep_vis_txt_fin[nrow(res_srep_vis_txt_fin)+1, ] <- colMeans(res_srep_vis_txt_fin)

# Calculate the p-value of the average diff.
# Melt the data.frame
srep_long <- melt(srep[, c(know_qs, "userid", "photo.nonphoto.pk")], id.vars = c("userid", "photo.nonphoto.pk"))
# lmer it
m1 <- lmer(nona(value) ~ photo.nonphoto.pk + (1 | userid), data = srep_long, REML = FALSE)
res_srep_vis_txt_fin[nrow(res_srep_vis_txt_fin), "p.value"] <- coef(summary(m1))[2, 5]

names(res_srep_vis_txt_fin) <- c("Diff", "Text", "Photo", "p.value")
res_srep_vis_txt_fin        <- res_srep_vis_txt_fin[, c("Text", "Photo", "Diff", "p.value")]
res_srep_vis_txt_fin        <- cbind(labs = c(know_labs, "Avg."), res_srep_vis_txt_fin)

write.csv(res_srep_vis_txt_fin, file="res/srep_vis_txt.csv", row.names = F)

table(srep$photo.nonphoto.pk) # Ns

## MTurk
## ---------------

vis_txt  <- paste0(c("mcconnell", "schumer", "merkel", "putin", "roberts", "pelosi"), "_vis_txt")
vis_labs <- c("Mitch McConnell", "Chuck Schumer", "Angela Merkel", "Vladimir Putin", "John Roberts", "Nancy Pelosi")

# For each question, do a t.test with cheat_no_cheat as variable to subset the vector
list_res          <- lapply(mturk[,vis_txt], function(x) tidy(t.test(x ~ visual_test == 'photo', mturk)))
res_vis_txt       <- do.call(rbind, list_res)

# Only 
res_vis_txt_fin        <- res_vis_txt[ , c(1:3, 5)]
res_vis_txt_fin[nrow(res_vis_txt_fin) + 1, ] <- colMeans(res_vis_txt_fin)

# Calculate the p-value of the average diff.
# Melt the data.frame
mturk_long <- melt(mturk[, c(vis_txt, "ResponseId", "visual_test")], id.vars = c("ResponseId", "visual_test"))
# lmer it
m1 <- lmer(nona(value) ~ visual_test + (1 | ResponseId), data = mturk_long, REML = FALSE)
res_vis_txt_fin[nrow(res_vis_txt_fin), "p.value"] <- coef(summary(m1))[2, 5]

names(res_vis_txt_fin) <- c("Diff", "Text", "Photo", "p.value")
res_vis_txt_fin        <- res_vis_txt_fin[, c("Text", "Photo", "Diff", "p.value")]
res_vis_txt_fin        <- cbind(labs = c(vis_labs, "Avg."), res_vis_txt_fin)
res_vis_txt_fin        <- as.data.frame(lapply(res_vis_txt_fin, nolead0s))

# Write out to CSV 
write.csv(res_vis_txt_fin, file="res/mturk_vis_txt.csv", row.names = F)

table(mturk$visual_test) # Ns
