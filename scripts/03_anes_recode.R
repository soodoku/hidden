#
# ANES Recode 
# 

# Set dir
setwd(basedir)
setwd("hidden")

# Load libraries
library(rio)
library(dplyr)
library(mosaic)
library(weights)
library(goji)

# Load data 
anes2012 <- import("data/anes/anes2012.txt")
anes2016 <- import("data/anes/anes2016.txt")

# PID 2012
anes2012 <- mutate(anes2012, rd = as.character(derivedFactor(
        "Democrat"    = ((pid_x == 1) | (pid_x == 2) | (pid_x == 3)),
        "Republican"  = ((pid_x == 5) | (pid_x == 6) | (pid_x == 7)),
        "Independent" = ( pid_x == 4),
        .default = NA_real_)))

# Calculating the means 
weighted.mean(nona(anes2012$rd == "Democrat"), w = anes2012$weight_full) # 0.46
weighted.mean(nona(anes2012$rd == "Republican"), w = anes2012$weight_full) # 0.39
weighted.mean(nona(anes2012$rd == "Independent"), w = anes2012$weight_full) # 0.14

# PID 2016
# Recode identifiers and leaners 
anes2016 <- mutate(anes2016, rd = as.character(derivedFactor(
        "Democrat"    = ((V161158x  == 1) | (V161158x == 2) | (V161158x == 3)),
        "Republican"  = ((V161158x  == 5) | (V161158x == 6) | (V161158x == 7)),
        "Independent" =  (V161158x  == 4),
        .default = NA_real_)))

# Calculating the means 
weighted.mean(nona(anes2016$rd == "Democrat"), w = anes2016$V160101) # 0.42
weighted.mean(nona(anes2016$rd == "Republican"), w = anes2016$V160101) # 0.43
weighted.mean(nona(anes2016$rd == "Independent"), w = anes2016$V160101) # 0.15


#Liberal/#Conservative

anes2012 <- mutate(anes2012, ideology = as.character(derivedFactor(
        "Liberal"       = (libcpre_self  == 1) | (libcpre_self  == 2) | (libcpre_self  == 3)|  (libcpre_self == 5 & libcpre_choose == 1) | (libcpre_self == -2 & libcpre_choose == 1) | (libcpre_self == -8 & libcpre_choose == 1),
        "Conservative"  = (libcpre_self  == 5) | (libcpre_self  == 6) | (libcpre_self  == 7) | (libcpre_self == 5 & libcpre_choose == 2) | (libcpre_self == -2 & libcpre_choose == 2) | (libcpre_self == -8 & libcpre_choose == 2),
        .default = NA_real_)))

weighted.mean(nona(anes2012$ideology == "Liberal"), w = anes2012$weight_full) # 0.26
weighted.mean(nona(anes2012$ideology == "Conservative"), w = anes2012$weight_full) # 0.39

anes2016 <- mutate(anes2016, ideology = as.character(derivedFactor(
        "Liberal"       = (V161126 == 1) | (V161126 == 2) | (V161126 == 3) | (V161126 == -8 & V161127 == 1) | (V161126 == 4 & V161127 == 1) | (V161126 == 99 & V161127 == 1),
        "Conservative"  = (V161126 == 5) | (V161126 == 6) | (V161126 == 7)| (V161126 == -8 & V161127 == 2) | (V161126 == 4 & V161127 == 2) | (V161126 == 99 & V161127 == 2),
        .default = NA_real_)))

weighted.mean(nona(anes2016$ideology == "Liberal"), w = anes2016$V160101) # 0.30
weighted.mean(nona(anes2016$ideology == "Conservative"), w = anes2016$V160101) # 0.42
