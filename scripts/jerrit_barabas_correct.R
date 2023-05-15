#####################################################################################
##
##    File Name:        jerrit_barabs_correct.R
##    Date:             2019-03-31
##    Author:           Daniel Weitzel
##    Purpose:          Calculating % correct by JB
##    Date Used:        
##    Data Used:        
##    Output File:      (none)
##    Data Output:      
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting working directory
setwd(basedir)
setwd("hidden/data/jerit_barabas/")

## Libraries
library(rio)
library(tidyverse)

## Import data
jb2012_df <- import("JeritBarabas_JOP_ReplicationFiles/JeritBarabas_JOP_Data/PB1.dta")
#jb2012_df <- import("JeritBarabas_JOP_ReplicationFiles/JeritBarabas_JOP_Data/PB2.dta")
#jb2012_df <- import("JeritBarabas_JOP_ReplicationFiles/JeritBarabas_JOP_Data/PB3.dta")   ## All PB# files produce the same result 
#jb2012_df <- import("JeritBarabas_JOP_ReplicationFiles/JeritBarabas_JOP_Data/PB4.dta")
jb2014_df <- import("/Users/dw28396/Dropbox/hidden/data/jerit_barabas/Quadrants/Data/Smasterimp_3-24.dta")

## Jerit and Barabas 2012
## Calculate the mean and stadnard deviation per survey question 
## From their do file 
## IEnumb=survey identifer (n=43, 1 to 45, skips 4 and 13)
## IEqid=unique question identifier (n=205, skips questions 23 and 24 [items asked only of Democrats] and 58, 59, 60, 61, and 62 [asked only of teens and without partisanship assessed])
## masterid=unique identifer for individual

jb2012_df <- 
  jb2012_df %>% 
  select(IEqid, know) %>% 
  group_by(IEqid) %>% 
  summarize(sum = mean(know, na.rm=TRUE))

## Calculate mean, std dev, and variance 
mean(jb2012_df$sum)
sd(jb2012_df$sum)
var(jb2012_df$sum)

## Jerit and Barabas 2014 
jb2014_items <- 
  jb2014_df %>% 
  select(QID, percor, answerchoices) %>% 
  group_by(QID, percor, answerchoices) %>% 
  unique() %>% ungroup() 
export(jb2014_items, "jb2014_items.csv")

jb2014_df %>% 
  select(QID, percor) %>% 
  group_by(QID, percor) %>% 
  unique() %>% ungroup() %>% 
  summarize(pc_mean = mean(percor, na.rm=TRUE),
            pc_sd   = sd(percor, na.rm = TRUE),
            pc_var  = var(percor, na.rm = TRUE))

    