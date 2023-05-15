##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      NES 2008 ##
##Data: Feb 17, 2012 Version   
##Last Edited: 7.26.13           
##   Gaurav Sood## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
setwd(basedir)

# Sourcing Common Functions
source("func/func.R")
source("func/polr.func.R")
# Open-ended text matching
source("func/match.R")
source("func/mes.func.R")

# Merging etc.
# *****************************************************
## Get NES08 data
nes08 <- foreign::read.spss("data/nes08/anes_timeseries_2008.por", use.value.labels = F, to.data.frame=T, 
use.missings = FALSE)
names(nes08) <- tolower(names(nes08))

## Get PSU and stratum info.
stratum <- read.csv("data/nes08/2008psu_stratum.csv")

# Merge NES08 with Stratum and PSU Info.
nes08 <- merge(nes08, stratum, by="v080001")
names(nes08)[names(nes08) %in% c("v081205.y", "v081206.y")] = c("psu", "stratum") 
save(nes08, file="data/nes08/nes08.Rdata")

# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #

##Load Merged Data
load("data/nes08/nes08.Rdata")

##Admin Variables 
nes08$year <- nes08$surveyyr <- 2008

## Weight
names(nes08)[names(nes08) %in% c("v080101", "v080102")] = c("preweight", "postweight")
nes08$weight <- nes08$postweight

## Sociodem
socdem <- c("v083218x", "v083215x", "v083248x", "v083249", "v081204", "v081101")
names(nes08)[match(socdem, names(nes08))] <- c("educ", "age", "hhincome", "income", "census", "sex")

nes08$female <- 1*(nes08$sex==2)
nes08$age    <- car::recode(nes08$age, "c(-8,-9)=NA")
nes08$educ   <- car::recode(nes08$educ, "c(0,1,2,3)='1 HS or Below'; c(4,5)='2 Some College'; 6='3 College'; 7='4 More than college'; c(-9, -8)=NA")

# South/Non-South
nes08$census.south  <- (nes08$census==3)*1
# Kuklinski South includes AL, Arkansas, FL, GA, LA, MS, NC, SC, TX, and VA.
# No KY, MO, TN
nes08$south <- car::recode(nes08$v081201b, "c(1, 5, 12, 13, 21, 22, 28, 29, 37, 45, 47, 48, 51 )=1; else=0")

#Interviewer reported race
names(nes08)[names(nes08)=="v081102"] = "irace" 
nes08$iwhite <- car::recode(nes08$irace,"1=1; c(-4,-9) =NA; else= 0")
nes08$iblack <- car::recode(nes08$irace,"2=1; c(-4,-9) =NA; else= 0")
nes08$inonlatino <- 1*(!(car::recode(nes08$v081103, "c(-4,-9) =NA")) ==1)

#white, to qualify as this the respondent had to respond '50' to V083251a but not '1' to either V083255 or V083256
#V083255 or V083256 
#Respondent self-reported race
nes08$white1     <- 1*(car::recode(nes08$v083251a, "c(-8,-9) =NA") == 50)
nes08$nonlatino1 <- 1*(!(car::recode(nes08$v083255, "-1=NA") == 1 | car::recode(nes08$v083256, "c(-8,-99)=NA")==1 ))
nes08$black1     <- 1*(car::recode(nes08$v083251a, "c(-8,-9) =NA") == 10)
nes08$asian1     <- 1*(car::recode(nes08$v083251a, "c(-8,-9) =NA") == 20)

# Race 5 Category
nes08$race5 <- NA
nes08$race5[nona(nes08$white1) ==1 & nona(nes08$nonlatino1) ==1] <- "White"  # Non-Latino White
nes08$race5[nona(nes08$black1) ==1 & nona(nes08$nonlatino1) ==1] <- "Black"  # Non-Latino Black
nes08$race5[!is.na(nes08$nonlatino1) & nes08$nonlatino1 ==0]  <- "Latino" # Latino
nes08$race5[nona(nes08$asian1) ==1 & nona(nes08$nonlatino1) ==1] <- "Asian"  # Non-Latino Asian
nes08$race5[!is.na(nes08$v083251a) & is.na(nes08$race5)]  <- "Others" # Other

# Income
nes08$hhincome <- car::recode(nes08$hhincome, "c(-8, -9)=NA")

# Interviewer observed urbanicity 
nes08$iurban <- car::recode(nes08$v082025, "-4=NA; 1='Rural Farm'; 2='Rural Town';
3='Suburban';4='Urban Res';5='Retail Urban';
6='Comm Urban';7='Other'")
# Household size
nes08$hhsize <- car::recode(nes08$v081107, "c(-9,-4)=NA")

# Marital Status
# V083216a   Y2a. [VERSION M] Marital status                                M
# 1.Married 2.Widowed 3.Divorced 4.Separated 5.Never married
#V083216b   Y2b. [VERSION N] Marital status                                N
#1.Married 2.Divorced 3.Separated 4.Widowed 5.Never married
# V083216x    Y2x. SUMMARY: Marital status
# 1. Married 2. Divorced 3. Separated 4. Widowed 5. Never married 6. Partnered, not married {VOL}
nes08$marital <- car::recode(nes08$v083216x, "1='Married';2='Divorced';3='Separated';4='Widowed';5='Never married';6='Partner';else=NA")

# Employed or not
#1. Working Now (initial status) --to Y16a
#2. Temporarily Laid Off (initial status)  --to Y16a
#4. Unemployed (initial status)  --to Y9
#5. Retired (initial status)  --to Y8a
#6. Permanently Disabled (initial status)  --to Y9
#7. Homemaker (initial status)  --to Y7a
#8. Student (initial status)  --to Y7a
nes08$employment <-car::recode(nes08$v083222, "1='Working Now';2='Temporarily Laid off';4='Unemployed';5='Retired';6='Disabled';7='Homemaker';8='Student';else=NA")

# Religion
# V083185a    Xx1a. SUMMARY: R denomination (final)
# 083185b    Xx1b. SUMMARY: R major religion group (final)
# nes08$v085251b
#0. No religious identification; none (5 in X3)  [50]
#1. Christian, Protestant  [01-16]
#2. Christian, Catholic  [18]
#3. Christian, Orthodox  [19]
#4. Jewish  [21]
#7. Other  [17,20,22-25,26,30]
#8. Atheist or Agnostic (volunteered in X4)  [41-42]

nes08$religion <- car::recode(nes08$v083185b, "0='No identification';1='Protestant';2='Catholic';3='Orthodox';4='Jewish';7='Other';8='Atheist';else=NA")

## Vote

#V085036A C1a. R vote turnout [OLD] [4 - Sure Voter]
#V085036D C1b3. R vote turnout [NEW] [2,3,4,5]
#V085036X C1x. SUMMARY: R VOTE TURNOUT [OLD and NEW][1 Voted]
#V085044 C6. Did R vote for candidate for President
#V085046A C6c1. NONVOTER: Who did R prefer for President
#V085044A C6a. For whom did R vote for President

nes08$voteobama  <- car::recode(nes08$v085044a, "1 =1; c(3,7) =0; else =NA")
nes08$votemccain <- car::recode(nes08$v085044a, "3 =1; c(1,7) =0; else =NA")
nes08$voted      <- 1*(car::recode(nes08$v085036x, "-2=NA") ==1)

## Interviewer 
#V082251  Pre Interviewer ID

#V084251 PostIWR.1. Interviewer ID: Pre-election IW 100.0%
#V084252 PostIWR.2. Interviewer gender: Pre-election IW 100.0%
#V084253 PostIWR.3. Interviewer education: Pre-election IW 100.0%
#V084254 PostIWR.4. Interviewer race: Pre-election IW 100.0%
#V084255 PostIWR.5. Interviewer ethnicity: Pre-election IW 100.0%
#V084256 PostIWR.6. Interviewer languages: Pre-election IW 100.0%
#V084257 PostIWR.7. Interviewer years experience: Pre-election IW 100.0%
#V084258 PostIWR.8. Interviewer age group: Pre-election IW 100.0%

#Pre Interviewer
#v082253 <- interviewered
#V082258 <- interviewerage

#V082254  Pre Interviewer race (white, non white)
nes08$whtiwr   <- 1*(car::recode(nes08$v082254, "-4=NA")==1)
#v082255  Pre Interviewer race (hispanic, non hispanic)
nes08$hispiwr  <- 1*(car::recode(nes08$v082255, "-4=NA")==1)
nes08$whtnhiwr <- nes08$whtiwr & !nes08$hispiwr
nes08$blkiwr   <- !nes08$whtnhiwr & !nes08$hispiwr

# Partisanship
nes08$pid7   <- car::recode(nes08$v083098x, "0 = '0 str dem'; 1 = '1 weak dem'; 2='2 lean dem'; 3='3 ind'; 4 = '4 lean rep'; 5='5 weak rep'; 6 ='6 str rep'; else=NA")
nes08$pid3   <- car::recode(nes08$v083098x, "c(0,1,2)='dem'; 3='ind';c(4,5,6)='rep'; else=NA")
nes08$rep    <- car::recode(nes08$v083098x, "c(0,1,2,3)=0; c(4,5,6)=1; else=NA")
nes08$dem    <- car::recode(nes08$v083098x, "c(0,1,2)=1; c(3,4,5,6)=0; else=NA") 
nes08$rd     <- car::recode(nes08$v083098x, "c(0,1,2)=0; c(4,5,6)=1;   else=NA") 

# Ideology
nes08$libcon3  <- car::recode(nes08$v083069, "c(1,2,3)='lib'; 4='mod';c(5,6,7)='con'; else=NA")

# Party Therm. and Like/Dislike
# ********************************
# Party Feeling Therm
nes08$r.therm <- zero1(car::recode(nes08$v083044b, "c(-9, -8, -6)=NA"))
nes08$d.therm <- zero1(car::recode(nes08$v083044a, "c(-9, -8, -6)=NA"))
nes08$r.min.d.therm <- nes08$r.therm - nes08$d.therm

# Democratic Party Like Dislike (0 Strongly Dislike, 10 Strongly Like)
nes08$d.like <- zero1(car::recode(nes08$v085187a, "c(-2,-7,-8,-9)=NA"))
nes08$r.like <- zero1(car::recode(nes08$v085187b, "c(-2,-7,-8,-9)=NA"))

nes08$in.like   <- with(nes08, out(d.like, r.like, !is.na(dem) & dem==1, !is.na(rep) & rep==1))
nes08$out.like  <- with(nes08, out(d.like, r.like, !is.na(rep) & rep==1, !is.na(dem) & dem==1))
nes08$diff.like <- with(nes08, out(d.like - r.like, r.like - d.like, !is.na(dem) & dem==1, !is.na(rep) & rep==1))

# ~~~~~~~~~~~~~~~~~~~~~~
#  Misinformation
# ~~~~~~~~~~~~~~~~~~~~~~ ++
nes08$m.unemp  <- car::recode(nes08$v083087, "5='wrse'; 1='btr'; 3='same'; -8='dk'; else=NA")
nes08$m.unempr <- car::recode(nes08$m.unemp, "'wrse'='cor';c('same','btr')='incor'")
nes08$m.unemps   <- car::recode(nes08$m.unemp, "'btr'='incor';'wrse'='cor'")

nes08$m.infl  <- car::recode(nes08$v083089, "5='wrse'; 1='btr'; 3='same'; -8='dk'; else=NA")
nes08$m.inflr<- car::recode(nes08$m.infl, "'wrse'='cor';c('same','btr')='incor'")
nes08$m.infls   <- car::recode(nes08$m.infl, "'wrse'='cor';'btr'='incor'")

nes08$m.rich  <- car::recode(nes08$v085080, "3='smlr'; 1='lrgr'; 5='same'; -8='dk'; else=NA")
nes08$m.richr<- car::recode(nes08$m.rich, "'lrgr'='cor';c('same','smlr')='incor'")
nes08$m.richs   <- car::recode(nes08$m.rich, "'lrgr'='cor';'smlr'='incor'")

# Knowledge Measures, post-election
# ***********************************
# Party in Power
nes08$house<- nona(car::recode(nes08$v085066, "1=1; c(-9, -8, -2)=NA"))
nes08$senate<- nona(car::recode(nes08$v085067, "1=1; c(-9, -8, -2)=NA"))

openpk <- read.csv("data/nes08/know.open.end.08.csv")
names(openpk) <- c("id", "pel","pela","chen","chena","bro","broa", "rob", "roba")

nes08 <- merge(nes08, openpk, by.x="v080001", by.y="id", all.x=T, all.y=F)

# Post-election
nes08$pelosi <- nona(pk(clean(nes08$pel), c("speaker", "spaeker", "spesker", "speeker",  "spaeaker", "skeaker", "spealker", "leader", "spoker", "head"), c("house", "congress")))
# partial pk responses: congresswoman, representative, california, democrat
nes08$cheney <- nona(pk(clean(nes08$chen), c("vice", "vp", "vicde"), c("")))
# partial pk responses: republican, defense, military
nes08$brown <- nona(pk(clean(nes08$bro), c("prime", "head"), c("uk", "england", "britain", "brittian", "british", "english", "united")))
# no real partial pk responses
nes08$roberts<- nona(pk(clean(nes08$rob), c("supreme","surprme", "surpreme", "high", "sc"), c("justice", "judge", "cj", "")))
# judge, couple of people get that he is on supreme court

# NES yet to release the following data --
#nes08$pelosi<- nes08$v085120 #1)Nancy Pelosi (/a)
#nes08$cheney<- nes08$v0851212#Dick Cheney  (085121/a)
#nes08$brown<- nes08$v0851223#Gordon Brown (085122/a)
#nes08$roberts<- nes08$v0851234#John Roberts (085123/a)

# DK Probe used or not
nes08$peldk <- car::recode(nes08$v085120a, "1=1; 5=0; else=NA")
nes08$chedk <- car::recode(nes08$v085121a, "1=1; 5=0; else=NA")
nes08$brodk <- car::recode(nes08$v085122a, "1=1; 5=0; else=NA")
nes08$robdk <- car::recode(nes08$v085123a, "1=1; 5=0; else=NA")

# Abortion
# V085089a
# Presidential Lib/Con Placement (Pre-election)
nes08$ob.lib  <- nona(car::recode(nes08$v083070a, "1:3=1;c(-9, -8)=NA"))
nes08$mc.lib  <- nona(car::recode(nes08$v083070b, "5:7=1;c(-9, -8)=NA"))
nes08$dem.lib <- nona(car::recode(nes08$v083071a, "1:3=1;c(-9, -8)=NA"))
nes08$rep.lib <- nona(car::recode(nes08$v083070b, "5:7=1;c(-9, -8)=NA"))

nes08$preknow  <- with(nes08, rowMeans(cbind(pelosi, cheney, brown, roberts)))
nes08$postknow <- with(nes08, rowMeans(cbind(house, senate, ob.lib, mc.lib, dem.lib, rep.lib)))

# Calculate reliability
pksubs <- with(nes08, data.frame(house, senate, pelosi, cheney, brown, roberts, ob.lib, mc.lib, dem.lib, rep.lib))
ltm::cronbach.alpha(pksubs, standardized = FALSE, CI = FALSE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

nes08$know <- with(nes08, rowMeans(cbind(house, senate, pelosi, cheney, brown, roberts, ob.lib, mc.lib, dem.lib, rep.lib)))

misinfo.cols <- c("weight", "m.unemp", "m.unempr", "m.unemps", "m.infl", "m.inflr", "m.infls", "m.rich", "m.richr", 
  "m.richs","know", "pid7", "pid3", "libcon3", "r.therm", "d.therm", "r.min.d.therm", "female", "educ")
#nes08 <- subset(nes08, select=misinfo.cols)
#write.csv(nes08, file="misinformation/data/nes08.csv", row.names = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~
#  Racial Stereotypes
# ~~~~~~~~~~~~~~~~~~~~~~ ++

##Admin Variables
names(nes08)[names(nes08) %in% c("v084401", "v084405y", "v084406c")]  = c("rand", "randb", "randw")  

## Post Feeling Thermometers; No Feeling Thermometer in Pre
nes08[, c("feelbr", "feelwr", "feelhr", "feelar")] <- sapply(nes08[,c("v085064y", "v085065c", "v085065a", "v085064v")], function(x) zero1(car::recode(x, "c(-2,-6,-8,-9)=NA")))

nes08$feelwminb <- nes08$feelwr  - nes08$feelbr
nes08$feelwmina <- nes08$feelwr  - nes08$feelar
nes08$feelwminh <- nes08$feelwr  - nes08$feelhr
nes08$feelhmina <- nes08$feelhr  - nes08$feelar

# In Score
nes08$infeel <- NA
nes08$infeel[!is.na(nes08$race5) & (nes08$race5)=='White']  <- nes08$feelwr[!is.na(nes08$race5) & (nes08$race5)=='White']
nes08$infeel[!is.na(nes08$race5) & (nes08$race5)=='Black']  <- nes08$feelbr[!is.na(nes08$race5) & (nes08$race5)=='Black']
nes08$infeel[!is.na(nes08$race5) & (nes08$race5)=='Latino'] <- nes08$feelhr[!is.na(nes08$race5) & (nes08$race5)=='Latino']
nes08$infeel[!is.na(nes08$race5) & (nes08$race5)=='Asian']  <- nes08$feelwr[!is.na(nes08$race5) & (nes08$race5)=='Asian']

## Candidate Feeling Thermometers
##**********************************

## Post-Election 
nes08$mccainthr <- zero1(car::recode(nes08$v085063c, "c(-2, -6, -8,-9)=NA"))
nes08$obamathr  <- zero1(car::recode(nes08$v085063b, "c(-2, -6, -8,-9)=NA"))
#McCain feeling thermometer minus Obama feeling thermometer 
nes08$feelthdr  <- zero1(nes08$mccainthr - nes08$obamathr, -1, 1)

## Pre-Election
nes08$premccainthr <- zero1(car::recode(nes08$v083037b, "c(-2, -6, -8,-9)=NA"))
nes08$preobamathr  <- zero1(car::recode(nes08$v083037a, "c(-2, -6, -8,-9)=NA"))
nes08$prefeelthdr  <- zero1((nes08$premccainthr -nes08$preobamathr), -1, 1) 

## Stereotype Measures
## *****************************

#hardworking V085174a, V085174b, V085174c (hispanics), V085174d
#intelligent V085175a V085175b V085175c V085175d (asian)
#Pre #hardworking V083207a V083207b V083207c V083207d
#intelligent V083208a V083208b V083208c V083208d

e7r <- "1=7;7=1;6=2;2=6;3=5;5=3;4=4;else=NA"

## Pre-Election
tolist   <- paste("pre", c("hardwr", "hardbr", "hardhr", "hardar", "intelliwr", "intellibr", "intellihr", "intelliar"), sep="")
fromlist <- nes08[,c("v083207a", "v083207b", "v083207c", "v083207d", "v083208a", "v083208b", "v083208c", "v083208d")]
nes08[, tolist] <- sapply(fromlist, function(x) zero1(car::recode(x, e7r),1,7))

##Post-Election
tolist   <- c("hardwr", "hardbr", "hardhr", "hardar", "intelliwr", "intellibr", "intellihr", "intelliar")
fromlist <- nes08[,c("v085174a", "v085174b", "v085174c", "v085174d", "v085175a", "v085175b", "v085175c", "v085175d")]
nes08[, tolist] <- sapply(fromlist, function(x) zero1(car::recode(x, e7r),1,7))

# 3 cat
nes08$hardwr3cat <- car::recode(nes08$hardwr, "0:.49=0; .5=1; .5000001:1=2")
nes08$hardbr3cat <- car::recode(nes08$hardbr, "0:.49=0; .5=1; .5000001:1=2")
nes08$prehardwr3cat <- car::recode(nes08$prehardwr, "0:.49=0; .5=1; .5000001:1=2")
nes08$prehardbr3cat <- car::recode(nes08$prehardbr, "0:.49=0; .5=1; .5000001:1=2")

nes08$intelwr3cat     <- car::recode(nes08$intelliwr, "0:.49=0; .5=1; .5000001:1=2")
nes08$intelbr3cat     <- car::recode(nes08$intellibr, "0:.49=0; .5=1; .5000001:1=2")
nes08$preintelwr3cat  <- car::recode(nes08$preintelliwr, "0:.49=0; .5=1; .5000001:1=2")
nes08$preintelbr3cat  <- car::recode(nes08$preintellibr, "0:.49=0; .5=1; .5000001:1=2")

# In Score
nes08$inhard <- NA
nes08$inhard[!is.na(nes08$race5) & (nes08$race5)=='White']  <- nes08$prehardwr[!is.na(nes08$race5) & (nes08$race5)=='White']
nes08$inhard[!is.na(nes08$race5) & (nes08$race5)=='Black']  <- nes08$prehardbr[!is.na(nes08$race5) & (nes08$race5)=='Black']
nes08$inhard[!is.na(nes08$race5) & (nes08$race5)=='Latino'] <- nes08$prehardhr[!is.na(nes08$race5) & (nes08$race5)=='Latino']
nes08$inhard[!is.na(nes08$race5) & (nes08$race5)=='Asian']  <- nes08$prehardar[!is.na(nes08$race5) & (nes08$race5)=='Asian']

nes08$inintel <- NA
nes08$inintel[!is.na(nes08$race5) & (nes08$race5)=='White']  <- nes08$preintelliwr[!is.na(nes08$race5) & (nes08$race5)=='White']
nes08$inintel[!is.na(nes08$race5) & (nes08$race5)=='Black']  <- nes08$preintellibr[!is.na(nes08$race5) & (nes08$race5)=='Black']
nes08$inintel[!is.na(nes08$race5) & (nes08$race5)=='Latino'] <- nes08$preintellihr[!is.na(nes08$race5) & (nes08$race5)=='Latino']
nes08$inintel[!is.na(nes08$race5) & (nes08$race5)=='Asian']  <- nes08$preintelliar[!is.na(nes08$race5) & (nes08$race5)=='Asian']

# Diff. Score
##############

# Pre-Election
nes08$dprehardr  <- with(nes08, zero1((prehardwr- prehardbr), -1, 1))
nes08$dpreintelr <- with(nes08, zero1((preintelliwr - preintellibr), -1, 1))

nes08$dprehardhr  <- with(nes08, zero1((prehardwr- prehardhr), -1, 1))
nes08$dpreintelhr <- with(nes08, zero1((preintelliwr- preintellihr), -1, 1))

nes08$dprehardar  <- with(nes08, zero1((prehardwr- prehardar), -1, 1))
nes08$dpreintelar <- with(nes08, zero1((preintelliwr- preintelliar), -1, 1))

nes08$dprehard3cat  <- car::recode(nes08$dprehardr,  "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dpreintel3cat <- car::recode(nes08$dpreintelr, "lo:.49=0; .5=1; .5000001:hi=2")

nes08$dprehardh3cat  <- car::recode(nes08$dprehardhr,  "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dpreintelh3cat <- car::recode(nes08$dpreintelhr, "lo:.49=0; .5=1; .5000001:hi=2")

# Asian - Hispanic
nes08$dprehard.ahr  <- with(nes08, zero1((prehardar- prehardhr), -1, 1))
nes08$dpreintel.ahr <- with(nes08, zero1((preintelliar - preintellihr), -1, 1))

# Asian - Black
nes08$dprehard.abr <- with(nes08, zero1((prehardar- prehardbr), -1, 1))
nes08$dpreintel.abr <- with(nes08, zero1((preintelliar- preintellibr), -1, 1))

# Hispanic - Black
nes08$dprehard.hbr  <- with(nes08, zero1((prehardhr- prehardbr), -1, 1))
nes08$dpreintel.hbr <- with(nes08, zero1((preintellihr- preintellibr), -1, 1))


# Post-Election
nes08$dhardr   <- with(nes08, zero1((hardwr- hardbr), -1, 1))
nes08$dintelr  <- with(nes08, zero1((intelliwr- intellibr), -1, 1))
nes08$dhardhr  <- with(nes08, zero1((hardwr- hardhr), -1, 1))
nes08$dintelhr <- with(nes08, zero1((intelliwr- intellihr), -1, 1))
nes08$dhardar  <- with(nes08, zero1((hardwr- hardar), -1, 1))
nes08$dintelar <- with(nes08, zero1((intelliwr- intelliar), -1, 1))

nes08$dhard3cat  <- car::recode(nes08$dhardr, "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dintel3cat <- car::recode(nes08$dintelr, "lo:.49=0; .5=1; .5000001:hi=2")

nes08$dhardh3cat  <- car::recode(nes08$dhardhr, "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dintelh3cat <- car::recode(nes08$dintelhr, "lo:.49=0; .5=1; .5000001:hi=2")

# Greater Than
# Pre-Election
tolist   <-  paste("dgpre", c("hardr", "intelr", "hardhr", "intelhr", "hardar", "intelar"), sep="")
fromlist <-  paste("dpre",  c("hardr", "intelr", "hardhr", "intelhr", "hardar", "intelar"), sep="")
nes08[, tolist] <- sapply(nes08[,fromlist], function(x) (x > .5)*1)

# Post-Election
tolist   <-  paste("dg", c("hardr", "intelr", "hardhr", "intelhr", "hardar", "intelar"), sep="")
fromlist <-  paste("d",  c("hardr", "intelr", "hardhr", "intelhr", "hardar", "intelar"), sep="")
nes08[, tolist] <- sapply(nes08[,fromlist], function(x) (x > .5)*1)

# Rank Order Hardwork
hard    <- c("hardwr", "hardbr", "hardhr", "hardar")
intel   <- c("intelliwr", "intellibr", "intellihr", "intelliar")
hardrank  <- t(apply(nes08[,hard],1,rank))
intelrank <- t(apply(nes08[,intel],1,rank))

# Blacks
nes08$dbprehard3cat  <- car::recode(zero1((nes08$prehardbr  - nes08$prehardwr), -1,1), "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dbpreintel3cat <- car::recode(zero1((nes08$preintellibr - nes08$preintelliwr),-1,1), "lo:.49=0; .5=1; .5000001:hi=2")

# Pre-Election
nes08$dbgprehardr   <- with(nes08, (prehardbr  - prehardwr) > 0)*1
nes08$dbgpreintelr  <- with(nes08, (preintellibr - preintelliwr) > 0)*1
nes08$dbgprehardhr  <- with(nes08, (prehardbr - prehardhr) > 0)*1
nes08$dbgpreintelhr <- with(nes08, (preintellibr - preintellihr) > 0)*1
nes08$dbgprehardar  <- with(nes08, (prehardbr - prehardar) > 0)*1
nes08$dbgpreintelar <- with(nes08, (preintellibr - preintelliar) > 0)*1

nes08$dbhard3cat  <- car::recode(zero1((nes08$hardbr - nes08$hardwr), -1,1), "lo:.49=0; .5=1; .5000001:hi=2")
nes08$dbintel3cat <- car::recode(zero1((nes08$intellibr - nes08$intelliwr), -1,1), "lo:.49=0; .5=1; .5000001:hi=2")

# Post-Election
nes08$dbghardr   <- with(nes08, (hardbr - hardwr ) > 0)*1
nes08$dbgintelr  <- with(nes08, (intellibr - intelliwr )> 0)*1
nes08$dbghardhr  <- with(nes08, (hardbr - hardhr)> 0)*1
nes08$dbgintelhr <- with(nes08, (intellibr - intellihr)> 0)*1
nes08$dbghardar  <- with(nes08, (hardbr - hardar)> 0)*1
nes08$dbgintelar <- with(nes08, (intellibr - intelliar)> 0)*1

# Equal To
# Pre-Election
nes08$deprehardr  <- (nes08$dprehardr == .5)*1
nes08$depreintelr <- (nes08$dpreintelr == .5)*1

# Post-Election
nes08$dehardr  <- (nes08$dhardr == .5)*1
nes08$deintelr <- (nes08$dintelr == .5)*1

# AMP (1 = Pleasant)
amp.black.from <- paste0("v0853", c(11 + 0:23))
amp.black.to   <- paste0("amp.black", 1:24)

amp.white.from <- paste0("v0853", c(35 + 0:23))
amp.white.to   <- paste0("amp.white", 1:24)

nes08[, amp.black.to] <- sapply(nes08[,amp.black.from], function(x) car::recode(x, "c(-5,-4,-2)=NA"))
nes08[, amp.white.to] <- sapply(nes08[,amp.white.from], function(x) car::recode(x, "c(-5,-4,-2)=NA"))

nes08$amp <- rowMeans(nes08[,amp.white.to] - nes08[,amp.black.to], na.rm=T)

#Till election date
#V082003a (start), V082003b (end)

# PRE-POST
nes08$prepost <- as.numeric(nes08$postweight ==0)

# Local News 
nes08$localearly <- nes08$v083020a # Days/Week Early Afternoon
nes08$locallate  <- nes08$v083020b # Local TV news late eve
nes08$localatt  <- car::recode(nes08$v083020c, "1=1;2=.75;3=.50;4=.25;5=.00") # Attention to Local

nes08$local <- rowMeans(cbind(nes08$localearly, nes08$localate), na.rm=T)*nes08$localatt

# Thermometers
# ************************************

# 085064a  - Hispanics# 085064b  - Christian Fundamentalist
# 085064c  - Catholics# 085064d  - Feminists# 085064e  - Fed Gov. in Wash# 085064f  - Jews
# 085064g  - Liberals# 085064h  - Middle Class People# 085064j  - Lab Unions# 085064k  - Poor people
# 085064m  - Military# 085064n  - Big Business# 085064p  - People on welfare# 085064q  - Conservatives
# 085064r  - Working class# 085064s  - Environmentalists# 085064t  - US Supreme Court# 085064u  - Gay and Lesbians
# 085064v  - Asian Americans# 085064w  - Congress# 085064y  - Blacks# 085064z  - Southerners
# 085065a  - Illegal Imm.# 085065b  - Rich people# 085065c  - Whites# 085065d  - Israel
# 085065e  - Muslims# 085065f  - Hindus# 085065g  - Christians# 085065h  - Atheists

therm.to <- paste0("therm_", c("hispanics", "chrisf", "catholics", "feminists", "fedgov", "jews", "liberals", "middleclass", "unions", "poor", "military",
"bigb", "welfare", "conservatives","workingclass","enviros","sc", "gays", "asians", "congress", "blacks", "southerners", 
"illegal", "rich", "whites", "israel", "muslims", "hindus", "christians", "atheists"))

## Post Feeling Thermometers; No Feeling Thermometer in Pre
nes08[, therm.to] <- sapply(nes08[,c(paste0("v085064", c(letters[1:8], "j", "k", "m", "n", letters[16:23], "y", "z")), paste0("v085065", letters[1:8]))], function(x) zero1(car::recode(x, "c(-2,-6,-8,-9)=NA"), 0, 100))

# Authoritarianism
# N1a. Child trait more important: independence(1) or respect (5)
nes08$independence <- car::recode(nes08$v085158, "c(-2,-8,-9,7)=NA;1=0;3=.5;5=1")

# N1b. Child trait more important: curiosity (1) or good manners (5)
nes08$curiosity <- car::recode(nes08$v085159, "c(-2,-8,-9,7)=NA;1=0;3=.5;5=1")

# N1c. Child trait more important: obedience(1) or self-reliance (5)
nes08$selfreliance <- car::recode(nes08$v085160, "c(-2,-8,-9,7)=NA;1=1;3=.5;5=0")

# N1d. Child trait more important: considerate (1) or well-behaved (5)
nes08$considerate <- car::recode(nes08$v085161, "c(-2,-8,-9,7)=NA;1=0;3=.5;5=1")

# with(nes08, cor(cbind(independence, curiosity, selfreliance, considerate), use="na.or.complete"))

# Auth. Scale
nes08$authoritarian <- with(nes08, rowMeans(cbind(independence, curiosity, selfreliance, considerate), na.rm=T))

# Save File
save(nes08, file="stereotypes/data/nes08.Rdata")
save(nes08, file="hidden/data/nes08h.Rdata")
# Recoded File for NES 08
save(nes08, file="data/nes08/nes08r.Rdata")


if(FALSE){
nes08r <- nes08[,c("v085174a", "v085174b", "v085174c", "v085174d", "v085175a", "v085175b", "v085175c", "v085175d", 
"v083207a", "v083207b", "v083207c", "v083207d", "v083208a",  "v083208b", "v083208c", "v083208d" , "v080101", 
"v080102", "v081101", "v081102", "v084401", "v084405y", "v084406c", "v084414a", "v085115", "v084405y", 
"v084405p", "v084405v", "v085065c", "v085064y", "v081103", "v083251a", "v083255",  "v083256")]
}
#save(nes08r, file="gss_anes/data/nes08r.rdata", ascii=TRUE)

##    Polarization  ##
## ************************* ##

# Battleground and the state: 
#nes08$battleground <- car::recode(nes08$v081201b, "c('8', '12', '19', '26', '27', '37', '35', '33', '32', '39', '42', '54', '54')='Battleground'; else='Non-Battleground'")
nes08$battleground <- ifelse(nes08$v081201a %in% c('CO', 'FL', 'IA', 'MI', 'MN', 'NC', 'NM', 'NH', 'NV', 'OH', 'PA', 'WI', 'WV'), 'Battleground', 'Non-Battleground')


# Other Feeling therms from post
#V085064b    # Christian Fundamentalists
#V085064c    # Catholics
#V085064d# Feminists
#V085064f# Jews
#V085064n# Big Business
#V085064p# People on welfare
#V085064s # Environmentalists
#V085064u# Gay men and women
#V085064v# Asian Americans
#V085064a # Hispanics
#V085064y # Blacks

# Catholic/Protestant
nes08$cathprot  <- car::recode(as.integer(nes08$v083185b), "7=1; 6=0; c(1,2,3,4)=NA; else=NA")

# Rep. Party
nes08$v083044br <- car::recode(nes08$v083044b, "c(-2,-6,-8, -9)=NA")
# Dem. Party
nes08$v083044ar <- car::recode(nes08$v083044a, "c(-2,-6,-8, -9)=NA")

nes08$in.therm   <- with(nes08, out(v083044ar, v083044br, !is.na(dem) & dem==1, !is.na(rep) & rep==1))
nes08$out.therm  <- with(nes08, out(v083044ar, v083044br, !is.na(rep) & rep==1, !is.na(dem) & dem==1))
nes08$diff.therm <- with(nes08, out(v083044ar - v083044br, v083044br - v083044ar, !is.na(dem) & dem==1, !is.na(rep) & rep==1))

# Cath Therm
nes08$v085064cr <- car::recode(nes08$v085064c,"c(-2,-6,-8, -9)=NA")

# Blacks Therm
nes08$v085064yr <- car::recode(nes08$v085064y, "c(-2,-6,-8, -9)=NA")

# Big Business
nes08$v085064nr <- car::recode(nes08$v085064n, "c(-2,-8,-9,-6)=NA")

# Gay 
nes08$v085064ur <- car::recode(nes08$v085064u, "c(-2,-8,-9,-6)=NA")

# Welfare
nes08$v085064pr <- car::recode(nes08$v085064p, "c(-2,-8,-9,-6)=NA")

polar.cols <- c("weight", "rep","dem","rd", "d.like", "r.like", "in.like", "diff.like", "out.like", 
"in.therm", "out.therm", "diff.therm", "v085064nr", "v083044br", "v085064cr", "v085064ur",
"v085064pr", "battleground", "educ", "age", "female", "census")
nes08p <- subset(nes08, select=polar.cols)
save(nes08p, file="polar/data/nes08p.Rdata")

# PRE 
# V083079c, V083080c  Public officials don't care what people think
# V083079d, V083080d  People like me don't have any say in what govt does
# V045203             M3a. How much attention does govt pay to what people think
# V083079a, V083080a   Complicated
# V083079b, V083080b   Good Understanding

# V085150              M1d. How many in government are crooked
# V085148              M1b. Govt run by a few big interests or for benefit of all
# V085147A, V085147B   M1a1. How often trust govt in Wash to do what is right [OLD] 

# POST
# V085153a, V085153B, V085151c   Public officials don't care what people think
# V085151d, V085152dPeople like me don't have any say in what govt does
# V085154     M3b. Elections make govt pay attention [VERSION NEW]
# V085151a, V085152a     Too Complicated
# V085152b            Good Understanding

nes08$precomplex <- NA
nes08$precomplex[!is.na(nes08$v083079a)] <- nes08$v083079a[!is.na(nes08$v083079a)]
nes08$precomplex[!is.na(nes08$v083080a)] <- nes08$v083080a[!is.na(nes08$v083080a)]

nes08$precomplex1 <- NA
nes08$precomplex1[!is.na(nes08$v083079b)] <- nes08$v083079b[!is.na(nes08$v083079b)]
nes08$precomplex1[!is.na(nes08$v083080b)] <- nes08$v083080b[!is.na(nes08$v083080b)]
nes08$precomplex1r <- car::recode(nes08$precomplex1, l5r)

nes08$preoff <- NA
nes08$preoff[!is.na(nes08$v083079c)] <- nes08$v083079c[!is.na(nes08$v083079c)]
nes08$preoff[!is.na(nes08$v083080c)] <- nes08$v083080c[!is.na(nes08$v083080c)]

nes08$prenosay <- NA
nes08$prenosay[!is.na(nes08$v083079d)] <- nes08$v083079d[!is.na(nes08$v083079d)]
nes08$prenosay[!is.na(nes08$v083080d)] <- (car::recode(nes08$v083080d, l5r))[!is.na(nes08$v083080d)]

nes08$postcomplex <- NA
nes08$postcomplex[!is.na(nes08$v085151a)] <- nes08$v085151a[!is.na(nes08$v085151a)]
nes08$postcomplex[!is.na(nes08$v085152a)] <- nes08$v085152a[!is.na(nes08$v085152a)]

nes08$postcomplex1 <- NA
nes08$postcomplex1[!is.na(nes08$v085151b)] <- nes08$v085151b[!is.na(nes08$v085151b)]
nes08$postcomplex1[!is.na(nes08$v085152b)] <- nes08$v085152b[!is.na(nes08$v085152b)]
nes08$postcomplex1r <- car::recode(nes08$postcomplex1, l5r)

nes08$postoff <- NA
nes08$postoff[!is.na(nes08$v085151c)] <- nes08$v085151c[!is.na(nes08$v085151c)]
nes08$postoff[!is.na(nes08$v085152c)] <- nes08$v085152c[!is.na(nes08$v085152c)]

nes08$postnosay <- NA
nes08$postnosay[!is.na(nes08$v085151d)] <- nes08$v085151d[!is.na(nes08$v085151d)]
nes08$postnosay[!is.na(nes08$v085152d)] <- nes08$v085151d[!is.na(nes08$v085152d)]
#nes08$postnosay[!is.na(nes08$v085152d)] <- (recode(nes08$v085152d, l5r))[!is.na(nes08$v085152d)]

nes08$v085154r <- car::recode(nes08$v085154, l5r)

with(nes08, cor(cbind(zero1(postcomplex),zero1(postoff), zero1(postnosay), zero1(v085154r)), use="na.or.complete"))
nes08$post    <- rowMeans(cbind(zero1(nes08$postoff),zero1(nes08$postnosay)))
nes08$pre     <- with(nes08, rowMeans(cbind(zero1(precomplex), zero1(postoff), zero1(prenosay))))
nes08$prepost <- (nes08$post - nes08$pre)
# Not including zero1(nes08$v085154r)
nes08$postit <- rowMeans(cbind(zero1(nes08$postcomplex),zero1(nes08$preoff),zero1(nes08$postnosay)))

polar.cols <- c("surveyyr", "female","age","race","educ","weight","pre","post","prepost","postit",
"rep","dem","rd", "d.like", "r.like", "in.like", "diff.like", "out.like")
nes08p <- subset(nes08,select=polar.cols)
save(nes08p, file="polar/data/nes08p.Rdata")
