#
# SREP Recode 
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
srep  <- read.csv("data/srep/srep.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors=FALSE)

# No duplicated User IDs and User IDs must have the word srep in them
srep  <- subset(srep, grepl("srep", srep$userid)) 
srep  <- subset(srep, !duplicated(srep$userid))

# Merge Panel Data
dem   <- read.csv("data/srep/srep_dem.csv", header = TRUE, na.strings = c("","NA"), stringsAsFactors=FALSE)
dem   <- subset(dem, grepl("srep", dem$srep.id))
dem   <- subset(dem, !duplicated(dem$srep.id))
srep  <- merge(srep, dem, by.x="userid", by.y="srep.id", all.x=T, all.y=F)

# Only completed surveys
srep <- subset(srep, !is.na(srep$photo.nonphoto)  & srep$status=="Complete") 

# Political Knowledge
#----------------------------

# Photo/Text 

srep$hrc    <- str_detect(srep$hr, regex("senate|sente|senator|harry", ignore_case=T)) & 
			   str_detect(srep$hr, regex("leader|reid|chief|leder|head|president|chair|whip|nevada", ignore_case=T))

srep$nsc    <- str_detect(srep$ns, regex("france|french", ignore_case = TRUE)) &  
			   str_detect(srep$ns, regex("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister", ignore_case = TRUE))

srep$jnc    <- str_detect(srep$jn, regex("homeland|homelenad|security|dhs", ignore_case = TRUE)) & 
			   str_detect(srep$jn, regex("director|sec|head", ignore_case = TRUE))

srep$hr2c   <- str_detect(srep$hr2, regex("senate|sente|senator|harry|herry", ignore_case = TRUE)) &  
			   str_detect(srep$hr2, regex("reid|reed|leader|chief|leder|head|president|chair|whip", ignore_case = TRUE))

srep$jn2c   <- str_detect(srep$jn2, regex("homeland|security|dhs|janet", ignore_case = TRUE)) &  
			   str_detect(srep$jn2, regex("napolitano|napoletano|napalatano|napalitano|director|sec|head", ignore_case = TRUE))

srep$ns2c   <- str_detect(srep$ns2, regex("france|french", ignore_case = TRUE)) &  
			   str_detect(srep$ns2, regex("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister|prince minister|prseident", ignore_case = TRUE))

# Open followed by Closed (Still Photo Versus Text)
# Text Q
srep$am1c   <- str_detect(srep$am1, regex("german|greman|germay", ignore_case = TRUE)) & 
			   str_detect(srep$am1, regex("leader|pres|chancellor|top dog|chanceller|chancelor|chancelier|kansler|pm|premier|prime minister|head", ignore_case = TRUE))

srep$am11c  <- nona(srep$am11=="Chancellor of Germany")

srep$mm1c   <- str_detect(srep$mm1, regex("senate|sente|seantor|senator", ignore_case = TRUE)) &  
			   str_detect(srep$mm1, regex("minority leader|head of repubs|monority|gop-ky|kentucky|rep leader|republican head|republican leader|head republican", ignore_case = TRUE))

srep$mm11c  <- nona(srep$mm11=="Senate Minority Leader")

# Photo Q
srep$am2c   <- str_detect(srep$am2, regex("german|greman|germay", ignore_case = TRUE)) &  
			   str_detect(srep$am2, regex("leader|pres|chancellor|top dog|canceller|chanceller|chancelor|chancelier|kansler|pm|premier|prime minister|head", ignore_case = TRUE))

srep$am21c  <- nona(srep$am21=="Chancellor of Germany")

srep$mm2c   <- str_detect(srep$mm2, regex("senate|sente|senator", ignore_case = TRUE)) &  
			   str_detect(srep$mm2, regex("minority leader|head of repubs|monority|gop-ky|kentucky|republican head|republican leader|head republican", ignore_case = TRUE))

srep$mm21c  <- nona(srep$mm21=="Senate Minority Leader")

# Combine Photo/Text 
srep$nsc_pnp     <-  with(srep, ifelse(photo.nonphoto.pk=="photo", ns2c, nsc))
srep$jnc_pnp     <-  with(srep, ifelse(photo.nonphoto.pk=="photo", jn2c, jnc))
srep$hrc_pnp     <-  with(srep, ifelse(photo.nonphoto.pk=="photo", hr2c, hrc))
srep$am1c_pnp    <-  with(srep, ifelse(photo.nonphoto.pk=="photo", am2c, am1c))
srep$am111c_pnp  <-  with(srep, ifelse(photo.nonphoto.pk=="photo", (am2c | am21c), (am1c | am11c))) # Open plus closed
srep$mm1c_pnp    <-  with(srep, ifelse(photo.nonphoto.pk=="photo", mm2c, mm1c))
srep$mm111c_pnp  <-  with(srep, ifelse(photo.nonphoto.pk=="photo", (mm2c | mm21c), (mm1c | mm11c)))

# Open Closed
srep$ww2.cas.c    <- nona(srep$ww2.cas=="[Erstwhile] Soviet Union")
srep$fem.death.c  <- nona(srep$fem.death=="Heart Disease")
srep$ww2.cas2.c   <- nona(str_detect(srep$ww2.cas2,   regex("russia|ussr|soviet|rushia", ignore_case = TRUE)))
srep$fem.death2.c <- nona(str_detect(srep$fem.death2, regex("heart|card|hypertension", ignore_case = TRUE)))
srep$fem.death.m  <- nona(srep$fem.death=="Breast Cancer")
srep$fem.death2.m <- nona(str_detect(srep$fem.death2, regex("breast", ignore_case = TRUE)))

# Combine Open and Closed 
srep$ww2_cas_oc  <- with(srep, ifelse(open.closed=="open", ww2.cas2.c, ww2.cas.c))
srep$fd_oc       <- with(srep, ifelse(open.closed=="open", fem.death2.c, fem.death.c))

# DK Coding
srep_vis <- subset(srep, photo.nonphoto.pk=='photo')
srep_txt <- subset(srep, photo.nonphoto.pk=='nonphoto')

# Angela Merkel open photo - Number of DK is given by TRUE
srep_vis$am2_dk <- str_detect(srep_vis$am2, regex("don\\'t know|dont know|no idea|not a clue|no clue|\\?|\\?\\?|dunno|oh lord cannot put name to face right now", ignore_case = TRUE))
srep_vis$am2_dk[is.na(srep_vis$am2_dk)] <- TRUE
# Follow up question - Number of DK is given by TRUE
srep_vis$am21_dk[srep_vis$am21 == "Don't Know"] <- TRUE

# Angela Merkel open text - Number of DK is given by TRUE
srep_txt$am1_dk <- str_detect(srep_txt$am1, regex("don\\'t know|no idea|i do not know", ignore_case = TRUE))
srep_txt$am1_dk[is.na(srep_txt$am1_dk)] <- TRUE
# Follow up question - Number of DK is given by TRUE
srep_txt$am11_dk[srep_txt$am11 == "Don't Know"] <- TRUE

# Mitch McConnell open photo
srep_vis$mmc_dk <- str_detect(srep_vis$mm2, regex("don\\'t know|dont know|no idea|not a clue|no clue|\\?|\\?\\?|dunno|oh lord cannot put name to face right now|Not sure", ignore_case = TRUE))
srep_vis$mmc_dk[is.na(srep_vis$mmc_dk)] <- TRUE
# Follow up question - Number of DK is given by TRUE
srep_vis$mm21_dk[srep_vis$mm21 == "Don't Know"] <- TRUE

# Mitch McConnell open text
srep_txt$mmc_dk <- str_detect(srep_txt$mm1, regex("don\\'t know|no idea|i do not know| \\?|Not sure|again - I have lived overseas for 10 years.  I believe he is an American politician", ignore_case = TRUE))
srep_txt$mmc_dk[is.na(srep_txt$mmc_dk)] <- TRUE
# Follow up question - Number of DK is given by TRUE
srep_txt$mm11_dk[srep_txt$mm11 == "Don't Know"] <- TRUE

# Confidence Vs. Closed Ended 

# Closed-Ended
srep$rg.rg.c <- nona(srep$rg== "Robert Gates")
srep$sb.sb.c <- nona(srep$sb== "Stephen Breyer")

srep$future.increase.c 	<- nona(srep$healthbill== "Limits future increases in payments to Medicare providers")
srep$upper.class.c 	    <- nona(grepl("Increases the Medicare payroll", srep$healthcare.pk1))

srep$mammograms.c 	<- str_detect(srep$healthcare.pk1, regex("mammograms", ignore_case = TRUE))
srep$illegal.c    	<- str_detect(srep$healthcare.pk1, regex("illegal", ignore_case = TRUE))
srep$single.payer.c <- str_detect(srep$healthcare.pk1, regex("single", ignore_case = TRUE))

# Confidence Measures - Preprocessing -  greater than 10 is divided by 10 and NA is converted to 0
# Stems with rg = bob gates, stems with sb = stephen breyer
conf_qs     <- c("rg.rg", "rg.ad", "rg.jc", "rg.ak", "sb.sb", "sb.jb", "sb.ks", "sb.tv", 
                "future.increase", "death.panel", "medicare", "cuts.benefits", 
                "upper.class", "illegal", "single.payer", "mammograms")

srep[, conf_qs] <- sapply(srep[, conf_qs], conf_10)

# Most confident about the right ans. 
srep$rg.rg.cf 			 <- with(srep, most_conf(rg.rg, rg.ad, rg.jc, rg.ak))
srep$sb.sb.cf 			 <- with(srep, most_conf(sb.sb, sb.jb,sb.ks, sb.tv))
srep$future.increase.cf  <- with(srep, most_conf(future.increase, death.panel, medicare, cuts.benefits))
srep$upper.class.cf      <- with(srep, most_conf(upper.class, illegal, single.payer, mammograms))

srep$illegal.cf          <- with(srep, most_conf(illegal, upper.class, single.payer, mammograms))
srep$single.payer.cf     <- with(srep, most_conf( single.payer,upper.class, illegal, mammograms))
srep$mammograms.cf       <- with(srep, most_conf(mammograms, upper.class, illegal, single.payer))

# Media
# --------------------------

# Conservative Media
# news.wsj, cable.fox, radio.ms, radio.rl
srep$con.media <- with(srep, rowMeans(cbind(!is.na(news.wsj), !is.na(cable.fox), !is.na(radio.ms), !is.na(radio.rl))))

#news.nyt, news.wp, news.usa, news.other, news.other.s
#tv.cbs, tv.fox, tv.pbs, tv.nbc, tv.other, tv.abc, tv.others
#cable.fox, cable.msnbc, cable.cspan, cable.bbc, cable.other, cable.cnn, cable.others
#radio.npr, radio.rl, radio.ms, radio.other, radio.others
#web.huff, web.drudge, web.portal, web.other, web.other.s tv.days
srep$attention.r <- car::recode(srep$attention, "'A great deal'=1; 'A lot'=.75; 'A moderate amount'=.50;'A little'=.25;'Not at all'=0")

# Misinformation
# Closed-Ended
# Steagall Act, Closed-Ended
srep$steagall.c  <- nona(srep$steagall=="Bill Clinton")

srep$rgn <- with(srep, ifelse(dkencouraging.dk.discouraging=="Branch A", reagan=="Increased",  reagan2=="Increased"))
srep$ctn <- with(srep, ifelse(dkencouraging.dk.discouraging=="Branch A", clinton=="Decreased", clinton2=="Decreased"))
srep$bsh <- with(srep, ifelse(dkencouraging.dk.discouraging=="Branch A", wbush=="Increased",   wbush2=="Increased")) 
srep$obm <- with(srep, ifelse(dkencouraging.dk.discouraging=="Branch A", obama=="Increased",   obama2=="Increased")) 

## Immigration Related 
# Deport under Obama
srep$deport.c <- nona(srep$deport == "Increased") 

# AZ Immigration Law
srep$imm.c    <- nona(srep$imm == "Can ask people they suspect of being illegal immigrants for their papers only when stopping them for other reasons") 

# NOTE: I checked the deport.sure variable and it does not have any observations. 

## Open-Ended
srep$tax.100k.r     <- as.numeric(sub("%", "", srep$tax.100k))
#Roughly what percentage of the U.S. federal budget would you say goes toward welfare?
srep$welfare.r      <- as.numeric(sub("%", "", srep$welfare))
#Roughly what percentage of Blacks in the U.S. would you say are on welfare?
srep$blk.welfare.r  <- as.numeric(sub("%", "", srep$blk.welfare))

# Political Variables
# ------------------------

# Ideology: Don't Know coded as NA
srep$libcon.surv <- recode(srep$ideology, "Very liberal"=1, "Liberal"=2, "Slightly liberal"=3, "Moderate"=4,
                           "Slightly conservative"=5, "Conservative"=6, "Very Conservative"=7, .default = NA_real_)

srep$con <- recode(srep$ideology, "Very conservative" = 1, "Conservative" = 1, "Slightly conservative" = 1, .default = 0)
srep$lib <- recode(srep$ideology, "Very liberal" = 1, "Liberal" = 1, "Slightly liberal" = 1, .default = 0)

# PID
srep$pid <- NA
temp <- with(srep, paste(pid1, pid2.r, pid2.d, pid.other, pid2.other))
srep$pid[temp=="Democrat NA Strong NA NA"] <- 1
srep$pid[temp=="Democrat NA Not very strong NA NA NA"] <- 2
srep$pid[temp=="Independent NA NA NA Closer to Democratic Party"] <- 3
srep$pid[temp=="Independent NA NA NA Equally close to both"] <- 4
srep$pid[temp=="Independent NA NA NA Closer to Republican Party"] <- 5
srep$pid[temp=="Republican Not very strong NA NA NA"] <- 6
srep$pid[temp=="Republican Strong NA NA NA"] <- 7

srep$rd <- recode(srep$pid, "1" = "dem", "2" = "dem", "3" = "dem",
                  "5" = "rep", "6" = "rep", "7" = "rep", .default = NA_character_)

# Dummy coding for socio-dem comparison 
srep$democrat     <- recode(srep$pid, `1` = 1, `2` = 1, `3` = 1, .default = 0)
srep$republican   <- recode(srep$pid, `5` = 1, `6` = 1, `7` = 1, .default = 0)
srep$independent  <- recode(srep$pid, `4` = 1, .default = 0)

# Political Interest 
srep$pol.interest <- zero1(srep$pol.interest)

# Political participation
# vote.2010 contribute1 contact attend

#def.party
#def.other

# Policy Attitudes
# Randomizing var = policy.placement
# ------------------------------- 
srep$healthcare.supp <- with(srep, as_num(ifelse(!is.na(srep$healthcare.law2), srep$healthcare.law2, srep$healthcare.law)))
srep$imm.supp        <- with(srep, as_num(ifelse(!is.na(srep$imm.law2), srep$imm.law2, srep$imm.law)))


# Thermometer Scales
# ------------------------------- 
srep$therm.br  <- zero1(srep$therm.b, 0, 100)
srep$therm.wr  <- zero1(srep$therm.w, 0, 100)
srep$therm.rr  <- zero1(srep$therm.r, 0, 100)
srep$therm.dr  <- zero1(srep$therm.d, 0, 100)
srep$therm.hr  <- zero1(srep$therm.h, 0, 100)
srep$therm.lr  <- zero1(srep$therm.l, 0, 100)
srep$therm.ilr <- zero1(srep$therm.il, 0, 100)

# Gender
# ------------------------
srep$female <- as.numeric(srep$sex=='Female')

# Education 
# ------------------
srep$somecollege <- recode(srep$educ, "Associate's degree" = 1, .default = 0)
srep$college     <- recode(srep$educ, "Four year college degree (BA or BS)" = 1, .default = 0)
srep$hs          <- recode(srep$educ, "High school diploma/degree" = 1, .default = 0)
srep$other       <- recode(srep$educ, "Other" = 1, .default = 0)
srep$postg       <- recode(srep$educ, "Master's degree (MA or MS)" = 1, "PhD MD MBA Law degree" = 1, .default = 0)

# Age 
# AREP/SREP based on 2010
# -----------------

srep <- mutate(srep, agegroup = as.character(derivedFactor(
                                "65+ years old" = (year < 1946),
                                "45-64 years old" = (year > 1945 & year < 1966),
                                "30-44 years old" = (year > 1965 & year < 1981),
                                "18-29 years old" = (year > 1980 & year < 1993),
                                .default = NA)))

srep$age18 <- recode(srep$agegroup, "18-29 years old"  = 1, .default = 0)
srep$age30 <- recode(srep$agegroup, "30-44 years old"  = 1, .default = 0)
srep$age45 <- recode(srep$agegroup, "45-64 years old"  = 1, .default = 0)
srep$age65 <- recode(srep$agegroup, "65+ years old"  = 1, .default = 0)

# Race 
# -------------

srep$ra <- recode(srep$ethnic, "#NAME?"  = "Other/Mixed", "American Indian Native American" = "Other/Mixed", "Asian or Pacific Islander" = "Asian",
                      "Black or African American" = "Black", "Latino or Hispanic" = "Hispanic/Latino", "Unknown" = "Other/Mixed", "White Caucasian" = "White")

srep$asian     <- recode(srep$ra, "Asian" = 1, .default = 0)
srep$black     <- recode(srep$ra, "Black" = 1, .default = 0)
srep$other     <- recode(srep$ra, "Other/Mixed" = 1, .default = 0)  
srep$white     <- recode(srep$ra, "White" = 1, .default = 0)
srep$hisla     <- recode(srep$ra, "Hispanic/Latino" = 1, .default = 0)

# Personality
# -------------------
re7 <- "7=1;6=2;5=3;4=4;3=5;2=6;1=7"
# structure, certain, oneside, changeview, srep$confident

srep$curi.r <- zero1(srep$curiosity, 1, 7)
srep$cons.r <- zero1(srep$considerate, 1, 7)
srep$inde.r <- zero1(srep$independence, 1, 7)
srep$obed.r <- zero1(car::recode(srep$obedience, re7), 1,7)
cor(cbind(srep$curi.r, srep$cons.r, srep$inde.r, srep$obed.r), use="na.or.complete")
srep$auth   <- rowMeans(cbind(srep$curi.r, srep$cons.r, srep$inde.r, srep$obed.r), na.rm=T)

