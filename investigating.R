library( tidyverse )

#looking at yougov information 
yougov <- load("data/yougov_ideology.RData")


yougov %>% group_by( distinct( person_id ) ) %>% tally()