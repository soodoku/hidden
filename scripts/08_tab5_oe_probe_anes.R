#   
#  Table 5: DK Probe for OE Responses 
#  ANES 2000, 2004, and 2008

# Set Working dir 
setwd(basedir) 
setwd("hidden")

# Load libaries
library(goji)
library(broom)
library(dplyr)

tab0f <- function(pkitem, dkprompt){
  # test: pkitem <- naes04[!is.na(naes04$o2ldk), ]$cutperm; dkprompt <-naes04[!is.na(naes04$o2ldk), ]$o2ldk
  submeans <- tapply(pkitem,  dkprompt,   FUN = mean, na.rm = T) 
  subns    <- tapply(rep(1, length(pkitem)),    dkprompt,   FUN = sum, na.rm = T) 
  aggmeans <- c(mean(sum(pkitem[dkprompt == 0])/length(pkitem)),  mean(pkitem))
  c(submeans[1], subns[1], submeans[2], subns[2], aggmeans)
}

# ANES Recode
# source("scripts/03_anes_recode.R")

## ANES 2000
## --------------

load("data/anes/nes00.Rdata")

# Post
nes00 <- subset(nes00, prepost == 1)

# Only people who got the experimental treatment
nes00d <- subset(nes00, dkprobe == 1)

tab5_1    <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab5_1$name     <- c("lott", "rehnquist", "blair", "reno")
 
# Hidden Knowledge (open-ended)
tab5_1[1, 2:7] <- with(nes00d[!is.na(nes00d$lotdk), ], tab0f(lott,      lotdk))
tab5_1[2, 2:7] <- with(nes00d[!is.na(nes00d$rehdk), ], tab0f(rehnquist, rehdk))
tab5_1[3, 2:7] <- with(nes00d[!is.na(nes00d$bladk), ], tab0f(blair,     bladk))
tab5_1[4, 2:7] <- with(nes00d[!is.na(nes00d$rendk), ], tab0f(reno,      rendk))
tab5_1$dk      <- tab5_1$n.prompt/(tab5_1$n.prompt + tab5_1$n.unprompt)
tab5_1$n       <- (tab5_1$n.prompt + tab5_1$n.unprompt)

## Experimental below so i can trace the steps
dkprobe22 <- 
  nes00d %>%
  filter(v001448 == 1 & v001446b > 7) %>%
  mutate(v001446b = ifelse(v001446b > 7, 8, 0)) %>%
  count(v001446b)

dkprobe23 <- 
  nes00d %>%
  filter(v001451 == 1 & v001449b > 7) %>%
  mutate(v001449b = ifelse(v001449b > 7, 8, 0)) %>%
  count(v001449b) 

dkprobe24 <- 
  nes00d %>%
  filter(v001454 == 1 & v001452b > 7) %>%
  mutate(v001452b = ifelse(v001452b > 7, 8, 0)) %>%
  count(v001452b) 

dkprobe25 <- 
  nes00d %>%
  filter(v001457 == 1 & v001455b > 7) %>%
  mutate(v001455b = ifelse(v001455b > 7, 8, 0)) %>%
  count(v001455b) 

dkprobe <- as.data.frame(rbind(dkprobe22[2], dkprobe23[2], dkprobe24[2], dkprobe25[2]))
tab5_1$dkprobe <- NA
tab5_1[1:4, 10] <-  (dkprobe /tab5_1$n)
tab5_1 <- tab5_1[, c(1, 2, 3, 4, 5, 6, 7, 9, 10, 8)]
 
lott <- matrix(c(tab5_1$pcor.unprompt[1] * tab5_1$n.unprompt[1],
                     tab5_1$pcor.prompt[1]   * tab5_1$n.prompt[1]  +  tab5_1$pcor.unprompt[1] * tab5_1$n.unprompt[1],
                   ((tab5_1$n.unprompt[1]    + tab5_1$n.prompt[1]) - (tab5_1$pcor.unprompt[1] * tab5_1$n.unprompt[1])),
                   ((tab5_1$n.unprompt[1]    + tab5_1$n.prompt[1]) - (tab5_1$pcor.prompt[1]   * tab5_1$n.prompt[1]
                                                               + tab5_1$pcor.unprompt[1]  * tab5_1$n.unprompt[1]))), ncol = 2)
lott <- tidy(stats::prop.test(lott))

rehn <- matrix(c(tab5_1$pcor.unprompt[2] * tab5_1$n.unprompt[2],
                     tab5_1$pcor.prompt[2]   * tab5_1$n.prompt[2]  +  tab5_1$pcor.unprompt[2] * tab5_1$n.unprompt[2],
                     ((tab5_1$n.unprompt[2]    + tab5_1$n.prompt[2]) - (tab5_1$pcor.unprompt[2] * tab5_1$n.unprompt[2])),
                     ((tab5_1$n.unprompt[2]    + tab5_1$n.prompt[2]) - (tab5_1$pcor.prompt[2]   * tab5_1$n.prompt[2]
                                                                    + tab5_1$pcor.unprompt[2]  * tab5_1$n.unprompt[2]))),ncol=2)
rehn <- tidy(stats::prop.test(rehn))


blair <- matrix(c(tab5_1$pcor.unprompt[3]*tab5_1$n.unprompt[3],
                      tab5_1$pcor.prompt[3]*tab5_1$n.prompt[3]+ tab5_1$pcor.unprompt[3]*tab5_1$n.unprompt[3],
                      ((tab5_1$n.unprompt[3] + tab5_1$n.prompt[3])-(tab5_1$pcor.unprompt[3]*tab5_1$n.unprompt[3])),
                      ((tab5_1$n.unprompt[3] + tab5_1$n.prompt[3])-(tab5_1$pcor.prompt[3]*tab5_1$n.prompt[3]+ tab5_1$pcor.unprompt[3]*tab5_1$n.unprompt[3]))), ncol=2)

blair<- tidy(stats::prop.test(blair))

reno <- matrix(c(tab5_1$pcor.unprompt[4]   * tab5_1$n.unprompt[4],
                    tab5_1$pcor.prompt[4]  * tab5_1$n.prompt[4]  +  tab5_1$pcor.unprompt[4] * tab5_1$n.unprompt[4],
                    ((tab5_1$n.unprompt[4] + tab5_1$n.prompt[4]) - (tab5_1$pcor.unprompt[4] * tab5_1$n.unprompt[4])),
                    ((tab5_1$n.unprompt[4] + tab5_1$n.prompt[4]) - (tab5_1$pcor.prompt[4]   * tab5_1$n.prompt[4]
                                                                 + tab5_1$pcor.unprompt[4]  * tab5_1$n.unprompt[4]))),ncol=2)
reno <- tidy(stats::prop.test(reno))

tab5_11        <- rbind(lott, rehn, blair, reno)
tab5_11$oemcoe <- tab5_11$estimate2 - tab5_11$estimate1
tab5_11        <- tab5_11[, c(1, 2, 10, 4)]
tab5_1         <- cbind(tab5_11, tab5_1[,8:10])

rm(tab5_11, lott, rehn, blair, reno) 

## ANES 2004
## -------------

load("data/anes/nes04.Rdata")

# Post
nes04 <- subset(nes04, nes04$prepost == 1)

tab5_2    <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab5_2$name     <- c("hastert", "cheney", "blair", "rehnquist")

# Hidden Knowledge (open-ended)
tab5_2[1, 2:7] <- tab0f(nes04$know_hastert,     nes04$hasdk)
tab5_2[2, 2:7] <- tab0f(nes04$know_cheney,      nes04$chedk)
tab5_2[3, 2:7] <- tab0f(nes04$know_blair,       nes04$bladk)
tab5_2[4, 2:7] <- tab0f(nes04$know_rehnquist,   nes04$rehdk)
tab5_2$n       <- (tab5_2$n.prompt + tab5_2$n.unprompt)
tab5_2$dk      <- tab5_2$n.prompt/(tab5_2$n.prompt + tab5_2$n.unprompt)

## Experimental below so i can trace the steps
dkprobe22<- 
  nes04 %>%
  filter(hasdk == 1 & v045162 > 7) %>%
  count(v045162)

dkprobe23<- 
  nes04 %>%
  filter(chedk == 1 & v045163 > 7) %>%
  count(v045163) 

dkprobe24<- 
  nes04 %>%
  filter(bladk == 1 & v045164 > 7) %>%
  count(v045164) 

dkprobe25<- 
  nes04 %>%
  filter(rehdk == 1 & v045165 > 7) %>%
  count(v045165)   

dkprobe <- as.data.frame(rbind(dkprobe22[2],dkprobe23[2],dkprobe24[2],dkprobe25[2]))
tab5_2$dkprobe <- NA
tab5_2[1:4,10] <-  (dkprobe /tab5_2$n)
tab5_2 <- tab5_2[, c(1, 2, 3,4,5,6,7,9,10,8)]

rm(dkprobe, dkprobe22,dkprobe23,dkprobe24,dkprobe25)

hast <- matrix(c(tab5_2$pcor.unprompt[1] * tab5_2$n.unprompt[1],
                 tab5_2$pcor.prompt[1]   * tab5_2$n.prompt[1]  +  tab5_2$pcor.unprompt[1] * tab5_2$n.unprompt[1],
                 ((tab5_2$n.unprompt[1]    + tab5_2$n.prompt[1]) - (tab5_2$pcor.unprompt[1] * tab5_2$n.unprompt[1])),
                 ((tab5_2$n.unprompt[1]    + tab5_2$n.prompt[1]) - (tab5_2$pcor.prompt[1]   * tab5_2$n.prompt[1]
                                                                    + tab5_2$pcor.unprompt[1]  * tab5_2$n.unprompt[1]))),ncol=2)
hast <- tidy(stats::prop.test(hast))

chen  <- matrix(c(tab5_2$pcor.unprompt[2] * tab5_2$n.unprompt[2],
                 tab5_2$pcor.prompt[2]   * tab5_2$n.prompt[2]  +  tab5_2$pcor.unprompt[2] * tab5_2$n.unprompt[2],
                 ((tab5_2$n.unprompt[2]    + tab5_2$n.prompt[2]) - (tab5_2$pcor.unprompt[2] * tab5_2$n.unprompt[2])),
                 ((tab5_2$n.unprompt[2]    + tab5_2$n.prompt[2]) - (tab5_2$pcor.prompt[2]   * tab5_2$n.prompt[2]
                                                                    + tab5_2$pcor.unprompt[2]  * tab5_2$n.unprompt[2]))),ncol=2)
chen  <- tidy(stats::prop.test(chen ))

 

blair <- matrix(c(tab5_2$pcor.unprompt[3]*tab5_2$n.unprompt[3],
                  tab5_2$pcor.prompt[3]*tab5_2$n.prompt[3]+ tab5_2$pcor.unprompt[3]*tab5_2$n.unprompt[3],
                  ((tab5_2$n.unprompt[3] + tab5_2$n.prompt[3])-(tab5_2$pcor.unprompt[3]*tab5_2$n.unprompt[3])),
                  ((tab5_2$n.unprompt[3] + tab5_2$n.prompt[3])-(tab5_2$pcor.prompt[3]*tab5_2$n.prompt[3]+ tab5_2$pcor.unprompt[3]*tab5_2$n.unprompt[3]))), ncol=2)

blair<- tidy(stats::prop.test(blair))

rehn <- matrix(c(tab5_2$pcor.unprompt[4] * tab5_2$n.unprompt[4],
                 tab5_2$pcor.prompt[4]   * tab5_2$n.prompt[4]  +  tab5_2$pcor.unprompt[4] * tab5_2$n.unprompt[4],
                 ((tab5_2$n.unprompt[4]    + tab5_2$n.prompt[4]) - (tab5_2$pcor.unprompt[4] * tab5_2$n.unprompt[4])),
                 ((tab5_2$n.unprompt[4]    + tab5_2$n.prompt[4]) - (tab5_2$pcor.prompt[4]   * tab5_2$n.prompt[4]
                                                                    + tab5_2$pcor.unprompt[4]  * tab5_2$n.unprompt[4]))),ncol=2)
rehn <- tidy(stats::prop.test(rehn))

tab5_21 <- rbind(hast, chen, blair, rehn)
tab5_21$oemcoe <- tab5_21$estimate2 - tab5_21$estimate1
tab5_21 <- tab5_21[, c(1, 2, 10, 4)]
tab5_2 <- cbind(tab5_21, tab5_2[,8:10])

rm(tab5_21, hast, chen, blair, rehn)

tab5_2 

#write.csv(tab3,  file = "res/tab5_1_anes_2004.csv", row.names = F)

## ANES 2008  
## ------------

# Load data
load("data/anes/nes08h.Rdata")

# Post 
nes08 <- subset(nes08, !nes08$prepost)

tab5_3    <- data.frame(name = 1:4, pcor.unprompt = NA,  n.unprompt = NA,  pcor.prompt = NA, n.prompt = NA, agg.unmprompt = NA, agg.prompt = NA)
tab5_3$name     <- c("pelosi", "cheney", "roberts", "brown")


# Hidden Knowledge (open-ended)
tab5_3 [1, 2:7] <- tab0f(nes08$pelosi,  nes08$peldk)
tab5_3 [2, 2:7] <- tab0f(nes08$cheney,  nes08$chedk)
tab5_3 [3, 2:7] <- tab0f(nes08$roberts, nes08$robdk)
tab5_3 [4, 2:7] <- tab0f(nes08$brown,   nes08$brodk)
tab5_3$n       <- (tab5_3$n.prompt + tab5_3$n.unprompt)
tab5_3$dk      <- tab5_3$n.prompt/(tab5_3$n.prompt + tab5_3$n.unprompt)

## Experimental below so i can trace the steps
openpk <- read.csv("data/anes/know.open.end.08.csv")
names(openpk) <- c("id", "pel","pela","chen","chena","bro","broa", "rob", "roba")

openpk$pelosi_dk    <- str_detect(openpk$pel, regex("dk|don't know|dont know|no|DON'T REALLY KNOW|none|doesn't know|i don't i'm guess|
                                                    has no clue|never heard|does not know|won't know|notknow|do not know|not sure|
                                                    not really sure|no idea|can not recall|nothing comes to mind|nothing comes to her mind|
                                                    cant think of her job|dosent know|no|none",
                                                    ignore_case = TRUE)) 

openpk$cheney_dk    <- str_detect(openpk$chen, regex("dont know|dont rem|dont' know|forgot|don't know|has no idea|does not know|
                                                     have no idea|cant remember|i'm guessing|no guess|heard of it|not sure|
                                                     did not know|can't think|can'tell you the job|sont know|dk what he does",
                                                     ignore_case = TRUE))

openpk$brown_dk     <- str_detect(openpk$bro, regex("don't know|dont know|no guess|don't recog|don't recognize|don't remember|
                                                    don\"t know|donot know|do not know|do'nt know|do'not know|dont know|never heard|
                                                      never  heard|no idea|i don't kn ow|not sure|don';t know|don't have a guess|
                                                    don't have a guess|don't even know who|don't have aclue|DON'T REALLY KNOW|
                                                    just a guess|haven't heard|have no clue|have not heard|has no idea|
                                                      DON'T REALLY KNOW|DK|dont have any|dont know|dont no|dont recognize that name|
                                                    dont rember|dont rembember|dont want to|dont' know|dont want to|dot know|
                                                      drawing a blank|escapes me|have no cllue|have no ideal|have no quess|
                                                      never heard|dont have a guess|cnat recall|can't even think|heard oh him|
                                                    head of|dont have an ideal|don;'t know|DOESN'T KNOW|cant tell you|can't think|
                                                      can't remmeber|dont have an answer|not for sure|donot know|dont have a guess|
                                                    can't rememe|can not answer|not recognize|does not have a guess|
                                                      DOES NOT EVEN KNOW|my mind just went blank|not familiar|idont know|don'tknow|
                                                    don't think|dont have one|dont even know the name|know who|i have no ideaer|
                                                      not too familiar|WHO IS GORDON BROWN|no|none",
                                                     ignore_case = TRUE))

openpk$roberts_dk   <- str_detect(openpk$rob, regex("don't know|dont know|NOT SURE|DONT RECOGNIZE|not remembering|not heard|
                                                    don't know|Don't NO|DK|don't recognize|don't reg|don't remember|
                                                    don't want to guess|don'y know|don\"t know|done't know|donot know|do not know|
                                                    none|donotknow|have no ideal|dont have a guess|dont have a guess|
                                                    dont have an answer|dont kno|no clue|dont no|dont no him|dont recognices|
                                                    dont regoniz|dont remember|dont remembers|drew a blank|no idea|no guess|
                                                    not at all|no|not sure|can't give you an educated guess|never heard|not guess|
                                                    can't remember|cannot recall|cant think|couldn't tell you|DON'TKNOW ANYONE|
                                                    do'nt know|do't|don't have any idea|don't no|don't recognize|don't remember|don't who|no guess|don'yt know|dont know|dont remember|dontknow|no clue|have not heard|heard his name|HEARD THE NAME|drew a blank|no idea|n't know|not going to guess|would not know|wouldnt know|cant even guess|not  familiar|not familiar|HAVE NOT HEARD|DOES NOT KNOW|know idea|don't keep track|never heard|never hrd of|no se tampoco|no se|no sure|no|nope|not a clue|not familiar",
                                                    ignore_case = TRUE))  

nes08 <- merge(nes08, openpk, by.x="v080001", by.y="id", all.x=T, all.y=F)

dkprobe22<- 
   nes08 %>%
   filter(peldk == 1 & pelosi_dk == TRUE) %>%
   count(pelosi_dk)

dkprobe23<- 
  nes08 %>%
   filter(chedk == 1 & cheney_dk == TRUE) %>%
   count(cheney_dk) 
 
dkprobe24<- 
   nes08 %>%
   filter(robdk == 1 & roberts_dk == TRUE) %>%
   count(roberts_dk) 
 
dkprobe25<- 
   nes08 %>%
   filter(brodk == 1 & brown_dk == TRUE) %>%
   count(brown_dk)   
 
dkprobe <- as.data.frame(rbind(dkprobe22[2],dkprobe23[2],dkprobe24[2],dkprobe25[2]))
tab5_3$dkprobe <- NA
tab5_3[1:4,10] <-  (dkprobe /tab5_3$n)
tab5_3 <- tab5_3[, c(1, 2, 3,4,5,6,7,9,10,8)]
tab5_3 
rm(dkprobe, dkprobe22,dkprobe23,dkprobe24,dkprobe25)

pel <- matrix(c(tab5_3$pcor.unprompt[1] * tab5_3$n.unprompt[1],
                 tab5_3$pcor.prompt[1]   * tab5_3$n.prompt[1]  +  tab5_3$pcor.unprompt[1] * tab5_3$n.unprompt[1],
                 ((tab5_3$n.unprompt[1]    + tab5_3$n.prompt[1]) - (tab5_3$pcor.unprompt[1] * tab5_3$n.unprompt[1])),
                 ((tab5_3$n.unprompt[1]    + tab5_3$n.prompt[1]) - (tab5_3$pcor.prompt[1]   * tab5_3$n.prompt[1]
                                                                    + tab5_3$pcor.unprompt[1]  * tab5_3$n.unprompt[1]))),ncol=2)
pel <- tidy(stats::prop.test(pel))

chen  <- matrix(c(tab5_3$pcor.unprompt[2] * tab5_3$n.unprompt[2],
                  tab5_3$pcor.prompt[2]   * tab5_3$n.prompt[2]  +  tab5_3$pcor.unprompt[2] * tab5_3$n.unprompt[2],
                  ((tab5_3$n.unprompt[2]    + tab5_3$n.prompt[2]) - (tab5_3$pcor.unprompt[2] * tab5_3$n.unprompt[2])),
                  ((tab5_3$n.unprompt[2]    + tab5_3$n.prompt[2]) - (tab5_3$pcor.prompt[2]   * tab5_3$n.prompt[2]
                                                                     + tab5_3$pcor.unprompt[2]  * tab5_3$n.unprompt[2]))),ncol=2)
chen  <- tidy(stats::prop.test(chen))


rob <- matrix(c(tab5_3$pcor.unprompt[3]*tab5_3$n.unprompt[3],
                  tab5_3$pcor.prompt[3]*tab5_3$n.prompt[3]+ tab5_3$pcor.unprompt[3]*tab5_3$n.unprompt[3],
                  ((tab5_3$n.unprompt[3] + tab5_3$n.prompt[3])-(tab5_3$pcor.unprompt[3]*tab5_3$n.unprompt[3])),
                  ((tab5_3$n.unprompt[3] + tab5_3$n.prompt[3])-(tab5_3$pcor.prompt[3]*tab5_3$n.prompt[3]+ tab5_3$pcor.unprompt[3]*tab5_3$n.unprompt[3]))), ncol=2)

rob <- tidy(stats::prop.test(rob))

bro <- matrix(c(tab5_3$pcor.unprompt[4] * tab5_3$n.unprompt[4],
                 tab5_3$pcor.prompt[4]   * tab5_3$n.prompt[4]  +  tab5_3$pcor.unprompt[4] * tab5_3$n.unprompt[4],
                 ((tab5_3$n.unprompt[4]    + tab5_3$n.prompt[4]) - (tab5_3$pcor.unprompt[4] * tab5_3$n.unprompt[4])),
                 ((tab5_3$n.unprompt[4]    + tab5_3$n.prompt[4]) - (tab5_3$pcor.prompt[4]   * tab5_3$n.prompt[4]
                                                                    + tab5_3$pcor.unprompt[4]  * tab5_3$n.unprompt[4]))),ncol=2)
bro <- tidy(stats::prop.test(bro))

tab5_31 <- rbind(pel, chen, rob, bro)
tab5_31$oemcoe <- tab5_31$estimate2 - tab5_31$estimate1
tab5_31 <- tab5_31[, c(1, 2, 10, 4)]
tab5_3 <- cbind(tab5_31, tab5_3[,8:10])

## Combine everything
tab5 <- rbind(tab5_1, tab5_2, tab5_3)

tab5$name  <- c("NES00 - lott", "NES00 - rehnquist", "NES00 - blair", "NES00 - reno", 
                "NES04 - hastert", "NES04 - cheney", "NES04 - blair", "NES04 - rehnquist",
                "NES08 - pelosi", "NES08 - cheney", "NES08 - roberts", "NES08 - brown")

tab5
#write.csv(tab0, file = "res/tab5_1_anes_2008.csv", row.names = F)
