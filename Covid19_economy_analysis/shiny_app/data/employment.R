library(tidyverse)
source("../data/covid.R")
source("../data/geoID.R")

employment <- read.csv(
  "https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Employment%20-%20State%20-%20Daily.csv")


employment %>%
  left_join(geoID, by = 'statefips') %>%
  mutate(year_month_day = paste(month, "-", day, "-", year),
         year_month_day = parse_date(year_month_day, format = "%m - %d - %Y")) %>%
  relocate(statename:year_month_day, .after = day) %>%
  relocate(year_month_day, .before = year) %>%
  left_join(covid, by = c("statefips", "year","month","day", "year_month_day")) %>%
  filter(year_month_day >= "2020-01-22") %>%
  mutate(across(`emp`:`emp_ss70`, ~parse_double(., na = c("", "NA", "."))))%>%
  rename("New Covid Case" = new_case_count,
         "New Covid Death" = new_death_count,
         "Date" = year_month_day) -> employment

# Average of the each variables
employment %>%
  summarize(
    across(.cols = `emp`:`emp_ss70`, mean, na.rm=T),
  ) %>%
  pivot_longer(cols = `emp`:`emp_ss70`, names_to = "variables",
               values_to = "Average") -> mean_employment

# Standard Deviation of the each variables
employment %>%
  summarize(
    across(.cols = `emp`:`emp_ss70`, sd, na.rm=T),
  ) %>%
  pivot_longer(cols = `emp`:`emp_ss70`, names_to = "variables",
               values_to = "Standard Deviation") -> sd_employment


# Median of all the variables
employment %>%
  summarize(
    across(.cols = `emp`:`emp_ss70`, median, na.rm=T),
  ) %>%
  pivot_longer(cols = `emp`:`emp_ss70`, names_to = "variables",
               values_to = "Median") -> median_employment

# Min of all the variables
employment %>%
  summarize(
    across(.cols = `emp`:`emp_ss70`, min, na.rm=T),
  ) %>%
  pivot_longer(cols = `emp`:`emp_ss70`, names_to = "variables",
               values_to = "Min") -> min_employment


# Max of all the variables
employment %>%
  summarize(
    across(.cols = `emp`:`emp_ss70`, max, na.rm=T),
  ) %>%
  pivot_longer(cols = `emp`:`emp_ss70`, names_to = "variables",
               values_to = "Max") -> max_employment


mean_employment %>%
  left_join(sd_employment, by = "variables") %>%
  left_join(median_employment, by = "variables") %>%
  left_join(min_employment, by = "variables") %>%
  left_join(max_employment, by = "variables") -> summary_employment


# Keeping the quantitative variables only
employment %>%
  select(-c(Date:statefips)) %>%
  select(where(is.numeric)) -> employment_quant_only # In case other character variables are added
na.omit(employment_quant_only) -> employment_quant_only

# Avg Employment with Covid Cases
employment %>%
  select(Date, stateabbrev, `emp`:`emp_ss70`) %>%
  group_by(Date) %>%
  summarise(across(`emp`:`emp_ss70`, ~(mean(., na.rm = TRUE)))) -> avg_employment
            

employment %>%
  select(Date, stateabbrev, `New Covid Case`) %>%
  group_by(Date) %>%
  summarise(sum_case = sum(`New Covid Case`,na.rm=T)) -> sum_case_employment


full_join(avg_employment, sum_case_employment, by = "Date") -> avg_employment_covid


avg_employment_covid %>%
  mutate(covid_percent_change = NA) -> avg_employment_covid

avg_employment_covid$covid_percent_change[1] <- 0

for(i in 2:nrow(avg_employment_covid)) {
  avg_employment_covid$covid_percent_change[i] <- ((avg_employment_covid$sum_case[i] - avg_employment_covid$sum_case[i-1]) / avg_employment_covid$sum_case[i-1])} 


# introducing days in lockdown dataset
days_in_lockdown <- read_csv("../data/State Lockdown.csv")

days_in_lockdown %>%
  rename(statename = "State Name",
         stateabbrev = "GeoCode",
         `Days in Lockdown` = "DaysInLockdown") %>%
  select(stateabbrev, stayathome_start, statewide_stayathome_end, `Days in Lockdown`) -> lockdown

left_join(employment, lockdown, by = "stateabbrev") -> employment_lockdown
