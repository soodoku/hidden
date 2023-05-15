#####################################################################################
##
##    File Name:        academic_results.R
##    Date:             2018-06-15
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Make Table 1 based on academic polls
##    Date Used:        2018-07-28
##    Output File:      (none)
##    Data Output:      (none)
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting working directory
setwd(basedir)  
setwd("hidden/data/academic_polls")

## Load the libraries
library(rio)
library(tidyverse)

## Load and combine the data 
anes12 <- import("anes_2012/anes_2012_clean.xls")
anes16 <- import("anes_2016/anes2016_clean.xlsx")
unemployment <- import("unemployment.csv")

## Combine the ANES waves
data <- rbind(anes12, anes16)

## Fixes
# All questions have a constructed coding that is either correct, incorrect, or DK
# Change Placement questions to the multiple choice category
data$`Open/MC/Placement/Other Closed`[data$`Open/MC/Placement/Other Closed` == "Placement"] <- "MC"

## Explicit DK/Not sure
table(data$`Explicit DK option?`)

## DK
#  0

## Not sure
#  0

## DK Probe
table(data$`DK Probe`)
data$`DK Probe`[data$`DK Probe` == "WELL, WHAT'S YOUR BEST GUESS?"] <- 1

# For both surveys
data$`DK Probe` <- as.numeric(data$`DK Probe`)
mean(data$`DK Probe`)

# For 2012 (viewer observations because recognition questions were asked in different ways in 2016)
data %>%
  filter(Year == 2012) %>%
  group_by(`DK Probe`) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

# For 2016
data %>%
  filter(Year == 2016) %>%
  group_by(`DK Probe`) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

## Substantive Response encouraging wording
# Overview
table(data$`Text of disguise (As far as you know, would you say, etc.)`)

# Data prep
data <- 
  data %>% 
  mutate(dyt = str_detect(`Text of disguise (As far as you know, would you say, etc.)`, regex("do you think", ignore_case = TRUE)),
         dyt = ifelse(dyt == TRUE, 1,0),
         dyt = ifelse(is.na(dyt), 0, dyt),
         wipo = str_detect(`Text of disguise (As far as you know, would you say, etc.)`, regex("personal opinion", ignore_case = TRUE)),
         wipo = ifelse(wipo == TRUE, 1,0),
         wipo = ifelse(is.na(wipo), 0, wipo),
         wwys = str_detect(`Text of disguise (As far as you know, would you say, etc.)`, regex("would you say", ignore_case = TRUE)),
         wwys = ifelse(wwys == TRUE, 1,0),
         wwys = ifelse(is.na(wwys), 0, wwys),
         srew = ifelse(dyt == 1, 1, ifelse(wipo == 1, 1, ifelse(wwys == 1, 1, 0))))

## SREW - Substantive Response encouraging wording
mean(data$srew) 

# For 2012
data %>%
  filter(Year == 2012) %>%
  group_by(srew) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


# For 2016
data %>%
  filter(Year == 2016) %>%
  group_by(srew) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


## DYT, WWYS, and WIPO
## DYT
mean(data$dyt)    

# For 2012
data %>%
  filter(Year == 2012) %>%
  group_by(dyt) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


# For 2016
data %>%
  filter(Year == 2016) %>%
  group_by(dyt) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


## WWYS
mean(data$wwys)

# For 2012
data %>%
  filter(Year == 2012) %>%
  group_by(wwys) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

# For 2016
data %>%
  filter(Year == 2016) %>%
  group_by(wwys) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

## WIPO 
mean(data$wipo)

# For 2012
data %>%
  filter(Year == 2012) %>%
  group_by(wipo) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

# For 2016
data %>%
  filter(Year == 2016) %>%
  group_by(wipo) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


## Number of substantive response options
# Response options in the survey
data %>%
  group_by(`Number of options (if closed)`) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


# Constructed response options
data %>%
  group_by(`Number of options constructed`) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

# 2012 and 2016
data %>%
  group_by(Year,`Number of options constructed`) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))


## DKE preamble
table(data$`DKE preamble or not`)
data$`DKE preamble or not`[data$`DKE preamble or not` == "Where would you place YOURSELF on this scale, or haven't you thought much about this?"] <- 1
data$`DKE preamble or not`[is.na(data$`DKE preamble or not`)] <- 0
 
# Combined
data %>%
  mutate(dke_dummy = as.numeric(ifelse(is.na(`DKE preamble or not`), 0, `DKE preamble or not`))) %>%
  group_by(dke_dummy) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

# For 2012 and 2016
data %>%
  mutate(dke_dummy = as.numeric(ifelse(is.na(`DKE preamble or not`), 0, `DKE preamble or not`))) %>%
  group_by(Year, dke_dummy) %>%
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

## DKD preamble
table(data$`DKD preamble or not`)
