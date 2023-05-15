#####################################################################################
##
##    File Name:        prior_knowledge.R
##    Date:             2019-04-17
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Calculating the percent correct in Prior's Visual Knowledge
##    Date Used:        2019-04-18
##    Data Used:        upd2kn2008.dta
##    Output File:      (none)
##    Data Output:      (none)
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting working directory
setwd(basedir)
setwd("hidden/data")

## Libraries
library(rio)
library(tidyverse)

## Load data 
df_prior08 <- import("Markus_Prior_KN_Data/KnowledgeNetworksSurvey2008/data/upd2kn2008.dta")
df_prior03 <- import("Markus_Prior_KN_Data/KnowledgeNetworksSurvey2003/data/kn2003.survey.dta")

names(df_prior08)

## Prior 2008
## Reduce to the knowledge items to items with DK, and generate DK treatment variable
df_prior08 <- 
  df_prior08 %>% 
  select(rp, rf, ends_with("wDK")) %>% 
  rename(dk = rp,
         mode = rf) %>% 
  mutate(dk = ifelse(dk == 1, "explicit", 
                     ifelse(dk == 2, "not explicit", NA)),
         mode = ifelse(mode==1, "verbal", 
                       ifelse(mode==2,"visual",
                              ifelse(mode==3, "verbal and visual", NA))))

## Transform from wide to long
df_prior08_long <- 
  df_prior08 %>% 
  gather(know_item, response,ends_with("wDK"))

## Generate a couple of count variables and the percentage variable per survey question
df_prior08_long <-
  df_prior08_long %>% 
  group_by(dk, know_item, response) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  mutate(total_n = sum(n),
         response_label = ifelse(response == 1, "correct",
                                 ifelse(response == 0, "incorrect", "dk"))) %>% 
  group_by(dk, know_item) %>% 
  mutate(question_n = sum(n),
         percent = round(n/question_n,4)) %>% 
  ungroup()

## Get the mean value per dk treatment and response option (correct, incorrect, dk)
dk_summaries_08 <-
  df_prior08_long %>% 
  select(dk, response_label, percent) %>% 
  group_by(dk, response_label) %>% 
  summarize(mean = mean(percent, na.rm = TRUE))

dk_summaries_08


## Include the visual knowledge treatment 
## Transform from wide to long
df_prior08_long_vis <- 
  df_prior08 %>% 
  gather(know_item, response,ends_with("wDK"))

## Generate a couple of count variables and the percentage variable per survey question
df_prior08_long_vis <-
  df_prior08_long_vis %>% 
  group_by(dk, mode, know_item, response) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  mutate(total_n = sum(n),
         response_label = ifelse(response == 1, "correct",
                                 ifelse(response == 0, "incorrect", "dk"))) %>% 
  group_by(dk, mode, know_item) %>% 
  mutate(question_n = sum(n),
         percent = round(n/question_n,4)) %>% 
  ungroup()

## Get the mean value per dk treatment and response option (correct, incorrect, dk)
dk_summaries_08_vis <-
  df_prior08_long_vis %>% 
  select(dk, mode, response_label, percent) %>% 
  group_by(dk, mode, response_label) %>% 
  summarize(mean = mean(percent, na.rm = TRUE))

dk_summaries_08_vis

rm(df_prior08, df_prior08_long, df_prior08_long_vis, dk_summaries_08, dk_summaries_08_vis)

## Prior 2003
## This data has two sets of variables that are relevant for us. Columns starting with kn are dummies indicating whether a 
## response was correct (1) or false/dk (0). Columns starting with ms indicate whether something was a DK (1) or not (0)
## Below I generate a knowledge variable that is 1 for correct, 0 for incorrect, adn -1 for DK (like in the Prior 08 data). 

## getting all the knowledge items and preparing them for a merge with the DK items
df_prior03_kn <- 
  df_prior03 %>% 
  select(starts_with("kn"), id, w2r_vis) %>%
  gather(know_item, correct, starts_with("kn")) %>% 
  separate(know_item, into = c("wave", "item"), sep="_") %>% 
  separate(wave, into = c("drop", "wave"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  select(-drop) %>% 
  rename(mode_w2 = w2r_vis) %>% 
  mutate(mode_w2 = ifelse(mode_w2 == 0, "names",
                          ifelse(mode_w2 == 1, "photo", NA)))

## getting all the DK items and preparing them for a merge with the knowledge items
df_prior03_dk <- 
  df_prior03 %>% 
  select(starts_with("ms"), id) %>%
  gather(know_item, dk, starts_with("ms")) %>% 
  separate(know_item, into = c("wave", "item"), sep="_")%>% 
  separate(wave, into = c("drop", "wave"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  select(-drop)

## Merging knowledge and DK items
dk_summaries_03 <- left_join(df_prior03_kn, df_prior03_dk)

## Summarizes the percentage per response per wave 
dk_summaries_03_dk <-
  dk_summaries_03 %>% 
  mutate(correct = ifelse(correct == 0 & dk == 1, -1, correct)) %>% 
  group_by(wave, item, correct) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  group_by(wave, item) %>% 
  mutate(question_n = sum(n),
         percent = round(n/question_n,4),
         response_label = ifelse(correct == 1, "correct",
                                 ifelse(correct == 0, "incorrect", 
                                        ifelse(correct == -1, "dk", NA)))) %>% 
  group_by(wave, response_label) %>% 
  summarize(mean = mean(percent, na.rm = TRUE))

dk_summaries_03_dk

## Prior 2003 
## getting all the DK items and preparing them for a merge with the knowledge items
## Second wave has a visual knowledge treatment, first wave has a few questions that have different pictures
## Summarizes the percentage per response per wave 
dk_summaries_03_vis1 <-
  dk_summaries_03 %>% 
  filter(wave == 1) %>% 
  mutate(correct = ifelse(correct == 0 & dk == 1, -1, correct)) %>% 
  group_by(wave, item, correct) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  group_by(wave, item) %>% 
  mutate(question_n = sum(n),
         percent = round(n/question_n,4),
         response_label = ifelse(correct == 1, "correct",
                                 ifelse(correct == 0, "incorrect", 
                                        ifelse(correct == -1, "dk", NA)))) %>% 
  group_by(wave, response_label) %>% 
  summarize(mean = mean(percent, na.rm = TRUE))

dk_summaries_03_vis2 <-
  dk_summaries_03 %>% 
  filter(wave ==2) %>% 
  mutate(correct = ifelse(correct == 0 & dk == 1, -1, correct)) %>% 
  group_by(wave, mode_w2, item, correct) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  group_by(wave, mode_w2, item) %>% 
  mutate(question_n = sum(n),
         percent = round(n/question_n,4),
         response_label = ifelse(correct == 1, "correct",
                                 ifelse(correct == 0, "incorrect", 
                                        ifelse(correct == -1, "dk", NA)))) %>% 
  group_by(wave, mode_w2,response_label) %>% 
  summarize(mean = mean(percent, na.rm = TRUE))

dk_summaries_03_vis <- rbind(dk_summaries_03_vis1, dk_summaries_03_vis2)

dk_summaries_03_vis