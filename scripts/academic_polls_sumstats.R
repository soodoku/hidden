#####################################################################################
##
##    File Name:        academic_polls_sumstats.R
##    Date:             2019-01-08
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Summarize the categories in the academic poll data 
##    Date Used:        2019-01-08
##    Data Used:        anes_2012_clean.xls, anes_2016_clean.xls
##    Output File:      substance_sumstats.csv
##    Data Output:      (none)
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting wotking directory
setwd(basedir)
setwd("hidden/data/academic_polls")

## Packages
library(rio)
library(tidyverse)

## Load the data 
anes12 <- import("anes_2012/anes_2012_clean.xls")
anes16 <- import("anes_2016/anes2016_clean.xlsx")


## Summarizing 2012
names(anes12)

substance12 <- 
  anes12 %>% 
  select(Substance) %>% 
  group_by(Substance) %>% 
  tally() %>% 
  mutate(freq = round(n/sum(n), digits = 3),
         Substance = ifelse(Substance == "Econ", "Economy", Substance)) %>% 
  rename(n12 = n, 
         freq12 = freq)

## Summarizing 2016
substance16 <- 
  anes16 %>% 
  select(Substance) %>% 
  group_by(Substance) %>% 
  tally() %>% 
  mutate(freq = round(n/sum(n), digits = 3) )%>% 
  add_row(Substance = "Candidates") %>% 
  rename(n16 = n, 
         freq16 = freq)

substance <- left_join(substance12, substance16)

export(substance, "substance_sumstats.csv")
