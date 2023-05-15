#
# Comparing Demographics
# 

# Set dir
setwd(basedir)
setwd("hidden")

# Load libraries
library(goji)
library(dplyr)
library(mosaic)

# Load functions
source("scripts/00_common_func.R")

# Loading the data
source("scripts/01_arep_recode.R")
source("scripts/02_srep_recode.R")
source("scripts/03_anes_recode.R")
source("scripts/04_mturk_recode.R")

### Recode

# Female
arep$female  <- recode(arep$gender, "Female"  = 1, .default = 0)
srep$female  <- recode(srep$gender, "Female"  = 1, .default = 0)
mturk$female <- recode(mturk$gender, "Female"  = 1, .default = 0)

# Race/Ethnicity
# Investigating Lations
## Important: In table note that question was asked differently for arep/srep and mturk 
## srep/arep: white, latino, black, asian, other/mixed 
## mturk: latino, not latino, white, black, asian, other/mixed

mturk$con  <- mturk$lib         <- NA
arep$hs    <- arep$somecollege  <- NA

# Analysis
socio_dem   <- c("democrat", "republican", "independent", "con", "lib", "hs", "somecollege", "college", "postg",
	              "age18", "age30", "age45", "age65", "female", "white", "black", "hisla", "asian", "other")

arepdem  <- sapply(arep[,  socio_dem], function(x) mean(x, na.rm = T))
srepdem  <- sapply(srep[,  socio_dem], function(x) mean(x, na.rm = T))
mturkdem <- sapply(mturk[, socio_dem], function(x) mean(x, na.rm = T))

# Census Data
acs2010 <- c(.46, .39, .14, .39, .26, .44, .30, .16, .09, .17, .20, .26, .13, .51, .74, .13, .05, .16, .08)
acs2015 <- c(.42, .43, .15, .42, .30, .41, .31, .17, .10, .17, .20, .26, .14, .51, .74, .13, .05, .17, .08)

demographics <- data.frame(arep_demos, arepdem, srepdem, mturkdem, acs2010, acs2015)

write.csv(demographics, file = "res/demographics.csv", row.names = F)

