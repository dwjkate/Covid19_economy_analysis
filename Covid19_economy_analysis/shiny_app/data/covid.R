library(tidyverse)

covid <- read.csv(
  'https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/COVID%20-%20State%20-%20Daily.csv')


covid %>%
  select(year,month, day, statefips, new_case_count, new_death_count) %>%
  mutate(across(starts_with("new"), ~parse_number(., na = c("", "NA", ".")))) %>%
  mutate(across(contains("(year)|(day)|(month)"), ~parse_integer(., na = c("", "NA", ".")))) %>%
  unite(`year_month_day`, sep = "-", year, month, day, remove = FALSE, na.rm = FALSE) %>%
  mutate(year_month_day = lubridate::ymd(year_month_day)) -> covid
