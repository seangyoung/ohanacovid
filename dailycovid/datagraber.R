############################################################################################
# 
# datagraber.R
# Grabs and cleans COVID-19 Data
# 
############################################################################################

## LIBRARIES ####
library(tidyverse)
library(lubridate)

## VARIABLES AND PATHS ####
covidtracking.path = "https://covidtracking.com/api/v1/states/daily.csv"
ohana.states = c("AR", "HI", "UT")

## READ IN DATA ####
indata = read_csv(covidtracking.path)

## CLEAN DATA ####
cleandata = indata %>%
  mutate(date = ymd(date),
         state = as.factor(state)) %>%
  filter(state %in% ohana.states) %>%
  select(state, date, positive, negative, recovered, death, hospitalized, totalTestResults, deathIncrease, positiveIncrease)

slimtable = cleandata %>%
  filter(date >= max(date)-1) %>%
  select(state, date, positive, death, recovered) %>%
  group_by(state) %>%
  summarize(`Confirmed Cases` = max(positive), `Prior Day Cases` = min(positive), Deaths = max(death), Recoveries = max(recovered)) %>%
  mutate_if(is.double, as.integer)

## CLEAN UP ####
rm(indata, ohana.states, covidtracking.path)