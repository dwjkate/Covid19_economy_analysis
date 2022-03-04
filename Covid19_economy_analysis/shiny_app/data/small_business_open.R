library(tidyverse)
source("../data/covid.R")
source("../data/geoID.R")

womply <- read.csv(
  'https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Womply%20-%20State%20-%20Daily.csv'
  )

womply %>% 
  unite(year_month_day, year, month, day, sep = ".", remove = FALSE) %>%
  mutate(year_month_day = lubridate::ymd(year_month_day)) %>%
  left_join(geoID, by = "statefips") %>%
  relocate(statename:state_pop2019, .after = day) -> small_business_open

small_business_open %>%
  left_join(covid, by = c("year_month_day", "statefips", "year", "month","day")) %>%
  filter(year_month_day >= "2020-01-22") %>%
  rename("New Covid Case" = new_case_count,
         "New Covid Death" = new_death_count,
         "Date" = year_month_day) -> small_business_open

# Average of all the variables
small_business_open %>%
  summarize(
    across(.cols = `revenue_all`:last_col(), mean),
  ) %>%
  pivot_longer(cols = `revenue_all`:last_col(), names_to = "variables",
               values_to = "Average") -> mean_small_business

# Standard Deviations of all the variables
small_business_open %>%
  summarize(
    across(.cols = `revenue_all`:last_col(), sd),
  ) %>%
  pivot_longer(cols = `revenue_all`:last_col(), names_to = "variables",
               values_to = "Standard Deviation") -> sd_small_business


# Median of all the variables
small_business_open %>%
  summarize(
    across(.cols = `revenue_all`:last_col(), median),
  ) %>%
  pivot_longer(cols = `revenue_all`:last_col(), names_to = "variables",
               values_to = "Median") -> median_small_business

# Min of all the variables
small_business_open %>%
  summarize(
    across(.cols = `revenue_all`:last_col(), min),
  ) %>%
  pivot_longer(cols = `revenue_all`:last_col(), names_to = "variables",
               values_to = "Min") -> min_small_business


# Max of all the variables
small_business_open %>%
  summarize(
    across(.cols = `revenue_all`:last_col(), max),
  ) %>%
  pivot_longer(cols = `revenue_all`:last_col(), names_to = "variables",
               values_to = "Max") -> max_small_business


mean_small_business %>%
  left_join(sd_small_business, by = "variables") %>%
  left_join(median_small_business, by = "variables") %>%
  left_join(min_small_business, by = "variables") %>%
  left_join(max_small_business, by = "variables") -> summary_small_business

# Keeping the quantitative variables only
small_business_open %>%
  select(-c(Date:statefips)) %>%
  select(where(is.numeric)) -> small_business_quant_only # In case other character variables are added
na.omit(small_business_quant_only) -> small_business_quant_only

# Avg Employment with Covid Cases
small_business_open %>%
  select(Date, stateabbrev, `revenue_all`:`merchants_retail`) %>%
  group_by(Date) %>%
  summarise(across(`revenue_all`:last_col(), ~(mean(., na.rm = TRUE)))) -> avg_small_business_open


small_business_open %>%
  select(Date, stateabbrev, `New Covid Case`) %>%
  group_by(Date) %>%
  summarise(sum_case = sum(`New Covid Case`,na.rm=T)) -> sum_case_small_business


full_join(avg_small_business_open, sum_case_small_business, by = "Date") -> avg_small_business_open_covid


avg_small_business_open_covid %>%
  mutate(covid_percent_change = NA) -> avg_small_business_open_covid

avg_small_business_open_covid$covid_percent_change[1] <- 0

for(i in 2:nrow(avg_small_business_open_covid)) {
  avg_small_business_open_covid$covid_percent_change[i] <- ((avg_small_business_open_covid$sum_case[i] - avg_small_business_open_covid$sum_case[i-1]) / avg_small_business_open_covid$sum_case[i-1])} 

# introducing days in lockdown dataset
days_in_lockdown <- read_csv("../data/State Lockdown.csv")

days_in_lockdown %>%
  rename(statename = "State Name",
         stateabbrev = "GeoCode",
         `Days in Lockdown` = "DaysInLockdown") %>%
  select(stateabbrev, stayathome_start, statewide_stayathome_end, `Days in Lockdown`) -> lockdown

left_join(small_business_open, lockdown, by = "stateabbrev") -> small_business_open_lockdown

