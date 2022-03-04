##correlation
library(corrplot)

small_business_num <- small_business_open %>% 
  select(all_revenue:retail_open)

small_business_cor <- cor(small_business_num)

corrplot(small_business_cor, method = 'color', type = 'lower', order = 'hclust')

consumer_spending_num <- consumer_spending %>% 
  select(accomodation:retail_no_grocery) %>% 
  na.omit(consumer_spending)

consumer_spending_cor <- cor(consumer_spending_num)

corrplot(consumer_spending_cor, method = 'color', type = 'lower', order = 'hclust')

employment_num <- employment %>% 
  select(all_emp:leisure_hospital_emp)

employment_cor <- cor(employment_num)

corrplot(employment_cor, method = 'color', type = 'lower')

##Time_series
##Probably not going to use since statistical anlysis is doing time series
select_small <- small_business_open %>% 
  select(year_month_day|statename|all_revenue)

select_small_long <- select_small %>% 
  pivot_wider(names_from = statename, values_from = all_revenue) %>% 
  na.omit() %>%
  select(-year_month_day)

small_date <- select_small %>% 
  pivot_wider(names_from = statename, values_from = all_revenue) %>% 
  na.omit() %>% 
  select(year_month_day)

select_small_long$mean <- rowMeans(select_small_long)

select_small_long <- select_small_long %>% select(mean)

small_combined <- cbind(small_date, select_small_long)

small_combined %>% ggplot(aes(x = year_month_day, y = mean)) + 
  geom_line()


select_consumer <- consumer_spending_ld %>% 
  select(year_month_day|statename|all_category)

select_consumer_long <- select_consumer %>% 
  pivot_wider(names_from = statename, values_from = all_category) %>% 
  na.omit() %>%
  select(-date)

consumer_date <- select_consumer %>% 
  pivot_wider(names_from = statename, values_from = all_category) %>% 
  na.omit() %>% 
  select(date)


select_consumer_long$mean <- rowMeans(select_consumer_long)

select_consumer_long <- select_consumer_long %>% select(mean)

consumer_combined <- cbind(consumer_date, select_consumer_long)

consumer_combined %>% ggplot(aes(x = date, y = mean)) + 
  geom_line()


#Boxplot

boxplot(consumer_spending_num)

boxplot(small_business_num) 

boxplot(employment_num)
