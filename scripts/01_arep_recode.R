#
#  AREP Recode 
# 

# Set dir
setwd(basedir)
setwd("hidden")

# Load libraries
library(goji)
library(stringr)
library(dplyr)
library(mosaic)

# Load functions
source("scripts/00_common_func.R")

# Read in the data
arep        <- read.csv("data/arep/arep.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors=FALSE)

# Subset AREP
# -------------------

# 1. Limit to user_ids with only FY prefix as those data belong to real people not experimenters
arep <- subset(arep, str_detect(arep$userid, "FY"))
# 2. Investigate this later --- just one duplicate but investigate which row to keep
arep <- subset(arep, !duplicated(arep$userid))
# 3. country is mispelled. Subset to users in the US --- maybe.  
names(arep)[names(arep)=="county"] <- "country"
#arep <- subset(arep, arep$country == "United States")
# 4. Only completes --- 1 person didn't complete
arep <- subset(arep, !is.na(arep$photo.nonphoto))  

# Add Demographic/Panel Data
# ----------------------------

dem <- read.csv("data/arep/arep_dem.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors=FALSE)
# Keep userids with FY upfront
dem <- subset(dem, str_detect(dem$arep.id, "FY"))
# Nuke duplicates
dem <- subset(dem, !duplicated(dem$arep.id))
# Merge 
arep <- merge(arep, dem, by.x = "userid", by.y = "arep.id", all.x=T, all.y = F)

# Photo/Text Identification Items
# --------------------------------------------

arep$hrc    <- str_detect(arep$hr, regex("semate|senat|sente", ignore_case = TRUE)) & 
               str_detect(arep$hr, regex("leader|chief|leder|head|president|chair|whip|nev", ignore_case = TRUE))

arep$nsc    <- str_detect(arep$ns, regex("france|fren", ignore_case = TRUE)) & 
               str_detect(arep$ns, regex("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister", ignore_case = TRUE))

arep$jnc    <- str_detect(arep$jn, regex("homeland|security|dhs", ignore_case = TRUE)) &  
               str_detect(arep$jn, regex("director|sec|head", ignore_case = TRUE))

arep$hr2c   <- str_detect(arep$hr2, regex("semate|senat|sente", ignore_case = TRUE)) & 
               str_detect(arep$hr2, regex("leader|chief|leder|head|president|chair|whip|nev", ignore_case = TRUE))

arep$ns2c   <- str_detect(arep$ns2, regex("france|fren", ignore_case = TRUE)) &  
               str_detect(arep$ns2, regex("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister|prince minister|prseident", ignore_case = TRUE))

arep$jn2c   <- str_detect(arep$jn2, regex("homeland|security|dhs", ignore_case = TRUE)) &  
               str_detect(arep$jn2, regex("director|sec|head", ignore_case = TRUE))

# Open followed by Closed (Still Photo Versus Text) 
# Photo
arep$am1c   <- str_detect(arep$am1, regex("germ|greman", ignore_case = TRUE)) &  
               str_detect(arep$am1, regex("leader|pres|chancellor|top dog|chanc|kansler|pm|premier|prime|head", ignore_case = TRUE))

# A small minority of people offer 'Don't Know' as an open-ended response
arep$am1c_dk <- is.na(arep$am1) | str_detect(arep$am1, regex("^\\?$|don't know|I do not know|No idea|Not sure|not a clue|dunno|dont know", ignore_case = TRUE))

arep$am11c  <- nona(arep$am11 == "Chancellor of Germany")

arep$mm1c   <- str_detect(arep$mm1, regex("semate|senat|sente", ignore_case = TRUE)) & 
               str_detect(arep$mm1, regex("minority leader|minority whip|minority spkr|monority|head of repubs|republican head|republican leader|head republican|kentucky|ky", ignore_case = TRUE))

# A small minority of people offer 'Don't Know' as an open-ended response
arep$mm1c_dk <- is.na(arep$mm1) | str_detect(arep$mm1, regex("^\\?$|don't know|I do not know|No idea|Not sure|not a clue|dunno|dont know", ignore_case = TRUE))

arep$mm11c  <- nona(arep$mm11 == "Senate Minority Leader")

# Text Q
arep$am2c   <- str_detect(arep$am2, regex("germ|greman", ignore_case = TRUE)) &  
               str_detect(arep$am2, regex("leader|pres|chancellor|top dog|chanc|kansler|pm|premier|prime|head", ignore_case = TRUE))

# A small minority of people offer 'Don't Know' as an open-ended response
arep$am2c_dk <- is.na(arep$am2) | str_detect(arep$am2, regex("^\\?\\?$|don't know|I do not know|No idea|Not sure|not a clue|dunno|dont know|oh lord cannot put name to face right now", ignore_case = TRUE))

arep$am21c  <- nona(arep$am21 == "Chancellor of Germany")

arep$mm2c   <- str_detect(arep$mm2, regex("semate|senat|sente", ignore_case = TRUE)) &  
               str_detect(arep$mm2, regex("minority leader|minority whip|minority spkr|monority|head of repubs|republican head|republican leader|head republican|kentucky|ky", ignore_case = TRUE))

# A small minority of people offer 'Don't Know' as an open-ended response
arep$mm2c_dk <- is.na(arep$mm2) | str_detect(arep$mm2, regex("^\\?\\?$|^\\?$|don't know|I do not know|No idea|Not sure|not a clue|dunno|dont know", ignore_case = TRUE))

arep$mm21c  <- nona(arep$mm21 == "Senate Minority Leader")

# Combine Photo/Text Qs. Randomizing vars = photo.nonphoto.pk
arep$nsc_pnp     <-  with(arep, ifelse(photo.nonphoto.pk == "photo", ns2c, nsc))
arep$jnc_pnp     <-  with(arep, ifelse(photo.nonphoto.pk == "photo", jn2c, jnc))
arep$hrc_pnp     <-  with(arep, ifelse(photo.nonphoto.pk == "photo", hr2c, hrc))
arep$am1c_pnp    <-  with(arep, ifelse(photo.nonphoto.pk == "photo", am2c, am1c))
arep$am111c_pnp  <-  with(arep, ifelse(photo.nonphoto.pk == "photo", (am2c | am21c), (am1c | am11c))) # Open plus closed
arep$mm1c_pnp    <-  with(arep, ifelse(photo.nonphoto.pk == "photo", mm2c, mm1c))
arep$mm111c_pnp  <-  with(arep, ifelse(photo.nonphoto.pk == "photo", (mm2c | mm21c), (mm1c | mm11c)))

# Open Closed
arep$ww2.cas.c    <- nona(arep$ww2.cas == "[Erstwhile] Soviet Union")
arep$fem.death.c  <- nona(arep$fem.death == "Heart Disease")
arep$ww2.cas2.c   <- nona(str_detect(arep$ww2.cas2,   regex("russia|ussr|soviet", ignore_case = TRUE)))
arep$fem.death2.c <- nona(str_detect(arep$fem.death2, regex("heart|card|hypertension", ignore_case = TRUE)))

# Combine Open and Closed. Randomizing var = open.closed 
arep$ww2_cas_oc  <- with(arep, ifelse(open.closed == "closed", ww2.cas.c, ww2.cas2.c))
arep$fd_oc       <- with(arep, ifelse(open.closed == "closed", fem.death.c, fem.death2.c))

# Confidence Versus Closed Ended
# ------------------------------------

## Closed-Ended
arep$rg.rg.c <- nona(arep$rg == "Robert Gates")
arep$sb.sb.c <- nona(arep$sb == "Stephen Breyer")

arep$future.increase.c    <- nona(arep$healthbill=="Limits future increases in payments to Medicare providers")
arep$upper.class.c        <- nona(grepl("Increases the Medicare payroll", arep$healthcare.pk1))

# Confidence Measures - Preprocessing -  greater than 10 is divided by 10 and NA is converted to 0
# Stems with rg = bob gates, stems with sb = stephen breyer", 2 ACA items 
conf_qs         <- c("rg.rg", "rg.ad", "rg.jc", "rg.ak", "sb.sb", "sb.jb", "sb.ks", "sb.tv", 
                     "future.increase", "death.panel", "medicare", "cuts.benefits", 
                     "upper.class", "illegal", "single.payer", "mammograms")

arep[, conf_qs] <- sapply(arep[, conf_qs], conf_10)

# Most confident about the right ans. 
arep$rg.rg.cf            <- with(arep, most_conf(rg.rg, rg.ad, rg.jc, rg.ak))
arep$sb.sb.cf            <- with(arep, most_conf(sb.sb, sb.jb, sb.ks, sb.tv))
arep$future.increase.cf  <- with(arep, most_conf(future.increase, death.panel, medicare, cuts.benefits))
arep$upper.class.cf      <- with(arep, most_conf(upper.class, illegal, single.payer, mammograms))

# DK Encouraging vs. Discouraging 
# arep$dkencouraging.dk.discouraging is the randomizing variable 
# ------------------------------------

arep$rgn <- with(arep, ifelse(dkencouraging.dk.discouraging == "Branch A", reagan == "Increased", reagan2 == "Increased"))
arep$ctn <- with(arep, ifelse(dkencouraging.dk.discouraging == "Branch A", clinton == "Decreased", clinton2 == "Decreased"))
arep$bsh <- with(arep, ifelse(dkencouraging.dk.discouraging == "Branch A", wbush == "Increased", wbush2 == "Increased"))
# Never asked obama:  arep$alobm <- with(arep, ifelse(!is.na(obama), obama, obama2)) 

# Closed-Ended
# Steagall Act, Closed-Ended
arep$steagall.c  <- nona(arep$steagall=="Bill Clinton")

arep$deport.c    <- nona(arep$deport == "Increased") 

# AZ Immigration Law
arep$imm.c       <- nona(arep$imm == "Can ask people they suspect of being illegal immigrants for their papers only when stopping them for other reasons") 

## Open-ended
arep$tax.100k.r <- as.numeric(sub("%", "", arep$tax.100k))
# Roughly what percentage of the U.S. federal budget would you say goes toward
# welfare?
arep$welfare.r <- as.numeric(sub("%", "", arep$welfare))
# Roughly what percentage of Blacks in the U.S. would you say are on welfare?
arep$blk.welfare.r <- as.numeric(sub("%", "", arep$blk.welfare))

# Media
# -------------------------------------------- 
# Conservative Media news.wsj, cable.fox, radio.ms, radio.rl
arep$con.media <- with(arep, rowMeans(cbind(!is.na(news.wsj), !is.na(cable.fox), !is.na(radio.ms), !is.na(radio.rl))))

# news.nyt, news.wp, news.usa, news.other, news.other.s tv.cbs, tv.fox, tv.pbs,
# tv.nbc, tv.other, tv.abc, tv.others cable.fox, cable.msnbc, cable.cspan,
# cable.bbc, cable.other, cable.cnn, cable.others radio.npr, radio.rl, radio.ms,
# radio.other, radio.others web.huff, web.drudge, web.portal, web.other,
# web.other.s tv.days attention
arep$attention.r <- recode(arep$attention, 'A great deal' = 1, 'A lot' = .75, 'A moderate amount' = .50, 'A little' = .25, 'Not at all' = 0)

# Political Variables
# --------------------------------

# Ideology 
arep$libcon.surv <- recode(arep$ideology, "Very liberal" = 1, "Liberal" = 2, "Slightly liberal" = 3, "Moderate" = 4,
                           "Slightly conservative" = 5, "Conservative" = 6, "Very Conservative" = 7, .default = NA_real_)

# Lib/Con Dummies
arep$con <- recode(arep$ideology, "Very conservative" = 1, "Conservative" = 1, "Slightly conservative" = 1, .default = 0)
arep$lib <- recode(arep$ideology, "Very liberal" = 1, "Liberal" = 1, "Slightly liberal" = 1, .default = 0)

# PID
arep$pid <- NA
temp <- with(arep, paste(pid1, pid2.r, pid2.d, pid.other, pid2.other))
arep$pid[temp == "Democrat NA Strong NA NA"] <- 1
arep$pid[temp == "Democrat NA Not very strong NA NA NA"] <- 2
arep$pid[temp == "Independent NA NA NA Closer to Democratic Party"] <- 3
arep$pid[temp == "Independent NA NA NA Equally close to both"] <- 4
arep$pid[temp == "Independent NA NA NA Closer to Republican Party"] <- 5
arep$pid[temp == "Republican Not very strong NA NA NA"] <- 6
arep$pid[temp == "Republican Strong NA NA NA"] <- 7

arep$rd <- recode(arep$pid, "1" = "dem", "2" = "dem", "3" = "dem",
                  "5" = "rep", "6" = "rep", "7" = "rep", .default = NA_character_)

# Coding dummies for socio_dem comparison 
arep$democrat     <- recode(arep$pid, `1` = 1, `2` = 1, `3` = 1, .default = 0)
arep$republican   <- recode(arep$pid, `5` = 1, `6` = 1, `7` = 1, .default = 0)
arep$independent  <- recode(arep$pid, `4` = 1, .default = 0)

# Political Interest
arep$pol.interest <- zero1(arep$pol.interest, 0, 10)

# Political participation 
# vote.2010 contribute1 contact attend 

# def.party def.other

# Policy Attitudes
# Randomizing var = policy.placement
# DKs coded as NA 
arep$healthcare.supp <- with(arep, as_num(ifelse(!is.na(healthcare.law), healthcare.law, healthcare.law2)))
arep$imm.supp        <- with(arep, as_num(ifelse(!is.na(imm.law), imm.law, imm.law2)))

# Thermometers
# -------------------------

arep$therm.br  <- zero1(as_num(arep$therm.b),  0, 100)
arep$therm.wr  <- zero1(arep$therm.w,  0, 100)
arep$therm.rr  <- zero1(arep$therm.r,  0, 100)
arep$therm.dr  <- zero1(arep$therm.d,  0, 100)
arep$therm.hr  <- zero1(arep$therm.h,  0, 100)
arep$therm.lr  <- zero1(arep$therm.l,  0, 100)
arep$therm.ilr <- zero1(arep$therm.il, 0, 100)

# Female
# --------------
arep$female <- as.numeric(arep$gender == "Female")

# Education 
# -------------
arep$college     <- recode(arep$educ, "Four year college degree (BA or BS)" = 1, .default = 0)
arep$postg       <- recode(arep$educ, "Masters degree (MA or MS)" = 1, "PhD MD MBA Law degree" = 1, .default = 0)

# Age 
# AREP/SREP based on 2010
# ------------------
arep <- mutate(arep, agegroup = as.character(derivedFactor(
                                "65+ years old" = (year < 1946),
                                "45-64 years old" = (year > 1945 & year < 1966),
                                "30-44 years old" = (year > 1965 & year < 1981),
                                "18-29 years old" = (year > 1980 & year < 1993),
                                .default = NA)))

arep$age18 <- recode(arep$agegroup, "18-29 years old"  = 1, .default = 0)
arep$age30 <- recode(arep$agegroup, "30-44 years old"  = 1, .default = 0)
arep$age45 <- recode(arep$agegroup, "45-64 years old"  = 1, .default = 0)
arep$age65 <- recode(arep$agegroup, "65+ years old"  = 1, .default = 0)

# Race 
# -------------------
arep$ra <- recode(arep$ethnic, "#NAME?"  = "Other/Mixed", "American Indian Native American" = "Other/Mixed", "Asian or Pacific Islander" = "Asian",
                      "Black or African American" = "Black", "Latino or Hispanic" = "Hispanic/Latino", "Unknown" = "Other/Mixed", "White Caucasian" = "White")

arep$asian     <- recode(arep$ra, "Asian" = 1, .default = 0)
arep$black     <- recode(arep$ra, "Black" = 1, .default = 0)
arep$other     <- recode(arep$ra, "Other/Mixed" = 1, .default = 0)  
arep$white     <- recode(arep$ra, "White" = 1, .default = 0)
arep$hisla     <- recode(arep$ra, "Hispanic/Latino" = 1, .default = 0)

# Personality
# -------------------------

re7 <- "7=1;6=2;5=3;4=4;3=5;2=6;1=7"

# Authoritarianism 
arep$curi.r <- zero1(arep$curiosity, 1, 7)
arep$cons.r <- zero1(arep$considerate, 1, 7)
arep$inde.r <- zero1(arep$independence, 1, 7)
arep$obed.r <- zero1(car::recode(arep$obedience, re7), 1, 7)
cor(cbind(arep$curi.r, arep$cons.r, arep$inde.r, arep$obed.r), use = "na.or.complete")
arep$auth <- rowMeans(cbind(arep$curi.r, arep$cons.r, arep$inde.r, arep$obed.r), na.rm = T)

# arep$structure arep$certain arep$oneside arep$changeview arep$confident

