library( tidyverse )

#looking at yougov information 
load("data/yougov_ideology.RData")

yougov %>% 
  distinct(person_id) %>% 
  tally()

ls()

yougov_data <- get("yougov_ideology")  # Replace "yougov_ideology" with the correct object name if different

yougov_data %>% 
  summarise(unique_person_ids = n_distinct(person_id))

# 443 people from the yougov dataset

yougov_data %>% 
  filter( slant == 'right' ) %>% 
  summarise(unique_person_ids = n_distinct(person_id))

# 139 people who are right affiliated

yougov_data %>% 
  filter( slant == 'left' ) %>% 
  summarise(unique_person_ids = n_distinct(person_id))

# 172 people who are left affiliated 

yougov_data %>% 
  filter( slant == 'neutral' ) %>% 
  summarise(unique_person_ids = n_distinct(person_id))

# 53 people who have netural political affilation

yougov_data %>% 
  group_by(slant) %>% 
  summarise(unique_person_ids = n_distinct(person_id))

# who goes to more sites? who browses more?

web_browse <- yougov_data %>%
  group_by(slant) %>%
  summarise(unique_subdomains = n_distinct(subdomain, na.rm = TRUE))

ggplot( data = )

# who spend more time online? right or left leaning people?

time_online <- yougov_data %>% 
  group_by(person_id) %>% 
  mutate(total_time_online = sum(page_duration, na.rm = TRUE)) 

time_online %>% 
  filter( total_time_online < 4320000 ) %>% 
  ggplot( ) + geom_boxplot( aes(x=slant, y = total_time_online/3600 )) 

# find persons who spent the most amout of time online 
View( time_online %>% select( person_id, total_time_online ) %>% distinct() %>% 
        arrange( desc(total_time_online)) )


# summarise( stats = list( summary( total_time_online / 3600 ) ) ) %>%
# unnest_wider( stats )

# when did it start 
start_time <- ymd_hms("2022-02-22 00:00:00")
end_time <- ymd_hms("2022-06-04 23:59:00")

time_difference <- end_time - start_time
as.period(time_difference)


print(avg_time_online)

# 103 days 









