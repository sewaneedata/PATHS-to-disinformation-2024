<<<<<<< HEAD
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


# who spend more time online? right or left leaning people?
library(lubridate)


yougov_data %>% 
  group_by(slant) %>% 
  summarise(total_page_duration_min = minute(sum(page_duration, na.rm = TRUE)))

print(avg_time_online)











=======
>>>>>>> parent of c04a6de (commt)
