library(tidyverse)
source("../data/covid.R")
source("../data/geoID.R")

affinity <- read.csv(
  'https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Affinity%20-%20State%20-%20Daily.csv')

affinity %>%
  left_join(geoID, by = "statefips") %>%
  mutate(year_month_day = paste(month, "-", day, "-", year),
         year_month_day = parse_date(year_month_day, format = "%m - %d - %Y")) %>%
  relocate(statename:year_month_day, .after = day) %>%
  relocate(year_month_day, .before = year) %>%
  select(-c(freq, provisional)) %>%
  mutate(across(`spend_all`:`spend_all_q4`, ~parse_double(., na = c("", ".", "NA")))) %>%
  left_join(covid, by = c("year_month_day", "year", "month", "day", "statefips")) %>%
  filter(year_month_day >= "2020-01-22") %>%
  rename("New Covid Case" = new_case_count,
         "New Covid Death" = new_death_count,
         "Date" = year_month_day) -> consumer_spending

# Average of all the variables
consumer_spending %>%
  summarize(
    across(.cols = `spend_all`:last_col(), mean),
  ) %>%
  pivot_longer(cols = `spend_all`:last_col(), names_to = "variables",
               values_to = "Average") -> mean_consumer_spending

# Standard Deviations of all the variables
consumer_spending %>%
  summarize(
    across(.cols = `spend_all`:last_col(), sd),
  ) %>%
  pivot_longer(cols = `spend_all`:last_col(), names_to = "variables",
               values_to = "Standard Deviation") -> sd_consumer_spending


# Median of all the variables
consumer_spending %>%
  summarize(
    across(.cols = `spend_all`:last_col(), median),
  ) %>%
  pivot_longer(cols = `spend_all`:last_col(), names_to = "variables",
               values_to = "Median") -> median_consumer_spending

# Min of all the variables
consumer_spending %>%
  summarize(
    across(.cols = `spend_all`:last_col(), min),
  ) %>%
  pivot_longer(cols = `spend_all`:last_col(), names_to = "variables",
               values_to = "Min") -> min_consumer_spending


# Max of all the variables
consumer_spending %>%
  summarize(
    across(.cols = `spend_all`:last_col(), max),
  ) %>%
  pivot_longer(cols = `spend_all`:last_col(), names_to = "variables",
               values_to = "Max") -> max_consumer_spending



mean_consumer_spending %>%
  left_join(sd_consumer_spending, by = "variables") %>%
  left_join(median_consumer_spending, by = "variables") %>%
  left_join(min_consumer_spending, by = "variables") %>%
  left_join(max_consumer_spending, by = "variables") -> summary_consumer_spending

# Keeping the quantitative variables only
consumer_spending %>%
  select(-c(Date:statefips)) %>%
  select(where(is.numeric)) -> consumer_spending_quant_only # In case other character variables are added
na.omit(consumer_spending_quant_only) -> consumer_spending_quant_only

# Avg consumer spending with covid cases

consumer_spending %>%
  select(Date, stateabbrev, `spend_all`:`spend_retail_w_grocery`, -`spend_hic`) %>%
  group_by(Date) %>%
  summarise(across(`spend_all`:`spend_retail_w_grocery`, ~(mean(., na.rm = TRUE)))) -> avg_consumer_spending

consumer_spending %>%
  select(Date, stateabbrev, `New Covid Case`) %>%
  group_by(Date) %>%
  summarise(sum_case = sum(`New Covid Case`,na.rm=T)) -> sum_case_consumer


full_join(avg_consumer_spending, sum_case_consumer, by = "Date") -> avg_consumer_spending_covid


avg_consumer_spending_covid %>%
  mutate(covid_percent_change = NA) -> avg_consumer_spending_covid

avg_consumer_spending_covid$covid_percent_change[1] <- 0

for(i in 2:nrow(avg_consumer_spending_covid)) {
  avg_consumer_spending_covid$covid_percent_change[i] <- ((avg_consumer_spending_covid$sum_case[i] - avg_consumer_spending_covid$sum_case[i-1]) / avg_consumer_spending_covid$sum_case[i-1])} 


# introducing days in lockdown dataset
days_in_lockdown <- read_csv("../data/State Lockdown.csv")

days_in_lockdown %>%
  rename(statename = "State Name",
         stateabbrev = "GeoCode",
         `Days in Lockdown` = "DaysInLockdown") %>%
  select(stateabbrev, stayathome_start, statewide_stayathome_end, `Days in Lockdown`) -> lockdown

left_join(consumer_spending, lockdown, by = "stateabbrev") -> consumer_spending_lockdown

