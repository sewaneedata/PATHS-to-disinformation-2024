library(tidyverse)
library(readxl)
library(ggalluvial)

# Load data
ideology <- load("data/yougov_ideology.RData")
classify_yougov <- read_csv("data/classify_yougov.csv")

# Join data
yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() 



explore_yougov <- classify_yougov %>% 
  left_join(yougov_join, by="person_id") %>% 
  arrange(person_id, start_time) %>% 
  mutate( label = case_when( label == "left bias" ~ "Left Bias",
                             label == "right bias" ~ "Right Bias",
                             TRUE ~ label )) %>% 
  mutate(next_label = lead(label))

####### ####### ####### ####### ####### ####### ####### 
####### ####### Creating the Alluvial Diagrams #######
####### ####### ####### ####### ####### ####### ####### 

explore_yougov <- bind_rows(
  explore_yougov %>% filter(!is.na(label), page_duration > 4),
  explore_yougov %>% filter(is.na(label))
) %>% 
  arrange(person_id, start_time) %>% 
  mutate(type = case_when(
    ref_media == "referrals" & other == "socialmedia" ~ "Both",
    ref_media == "referrals" & other != "socialmedia" ~ "Referrals",
    ref_media != "referrals" & other == "socialmedia" ~ "Social Media",
    ref_media == "referrals" & is.na(other) ~ "Referrals",
    is.na(ref_media) & other == "socialmedia" ~ "Social Media"
  ))

foo <- explore_yougov %>% 
  mutate(axis2 = ifelse(next_label %in% c("Left Bias", "Right Bias"), next_label, "non")) %>% 
  mutate(type = factor(type, levels = c("Referrals", "Social Media", "Both")))

tallied_data <- foo %>% 
  group_by(type, axis2, slant) %>% 
  tally()

tallied_data %>% mutate( slant = case_when( slant == "slant" ~ "Political Affiliation", TRUE ~ slant ))


# Plot with updated colors
ggplot(tallied_data %>% drop_na(type) %>% filter(axis2 != "non"), aes(axis1 = type, axis2 = axis2, y = n)) +
  geom_alluvium(aes(fill = slant)) +
  scale_fill_manual( values = c( "blue", "grey", "red")) + 
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Type", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Times New Roman")) +
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Domain Media Visits",
       fill = "Political Affiliation")


##### Partisanship graph


partisanship <- explore_yougov %>%
  filter( ref_media == "media" ) %>%
  group_by( domain, slant ) %>%
  summarize( n = n_distinct(person_id) ) %>%
  mutate( total = sum(n), pct = n/total ) %>%
  filter( total >= 10 ) %>%
  arrange( desc( pct ) ) %>%
  slice_head(n=1) %>%
  select( domain, partisanship = slant )

foo <- left_join( foo, partisanship, by = "domain")

tallied_data_foo <- foo %>% 
  group_by( partisanship, axis2, slant) %>% 
  tally()



ggplot( tallied_data_foo %>% drop_na(partisanship) %>%  filter(axis2 != "non", partisanship != "Neutral"), aes( axis1 = partisanship, axis2 = axis2, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c( "blue", "grey", "red")) + 
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Type", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Times New Roman")) +
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Partisan Visits",
       fill = "Political Affiliation")


####### ############################################### ####### 
####### ####### ####### Exploring the data in R ###############
####### ################################################ ####### 

# how many people in the data set?? 370, 1:49 - 450
total_people <- yougov_ideology %>% 
  filter(!is.na( slant)) %>% 
  summarise(unique_ids = n_distinct(person_id)) %>% 
  pull(unique_ids)
total_people


# how many people have visited fake news sources? - 333
fake_news_visitors <- explore_yougov %>% 
  filter(!is.na( slant)) %>%
  filter(page_duration > 8) %>% 
  summarise(unique_ids = n_distinct(person_id))
  
fake_news_visitors

# 75% of people visted fake news sites
fake_news_visitors / total_people

non_fake_news_visitors <- total_people - fake_news_visitors
non_fake_news_visitors

#left vist 217
yougov_ideology %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Left visits = 171
explore_yougov %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

171/217*100 #78.42029% of people from left wing visit untrustworthy sites

#Total right = 126
yougov_ideology %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Right visits = 90
explore_yougov %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

90/126*100 #71.43% of people from right wing visit untrustworthy sites

#Total neutral = 107
yougov_ideology %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Neutral visits = 72
explore_yougov %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))

72/107*100 #67.29 of people who are neutral visit untrustworthy sites

#Who has the most visits in this dataset? ---person_id=41773241, visits=193921, right wing
yougov_ideology %>% 
  group_by(person_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Where does this person go the most? ---google, visit=16785
yougov_ideology %>% 
  filter(!is.na(domain)) %>% 
  filter(person_id=='us:41773241') %>% 
  group_by(domain, extension) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Who visited disinformation site the most? ---person_id=us:2381143, visit=8325, right wing
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(person_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)
#Using pretty much the same code, also found that there's a person who only visited site (1), but this person is super conservative (10)

#Where did this person go? Majority is right bias--5542 visits
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(label) %>% 
  tally() %>% 
  arrange(desc(n))



#Top 10 sites get the most visit? fox news--21351 visits (1)
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Distribution of type of sites that neutral affiliation visited? Majority of them visit left bias sites--3034
explore_yougov %>% 
  filter(slant=="Neutral", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()

#Where did they go? Majority is people.com--981 visits
explore_yougov %>% 
  filter(slant=="Neutral", label=="left bias") %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits))

#Who visit disinformation site the most in right wing? --person_id=us:2381143, visits=8325
explore_yougov %>% 
  filter(!is.na(label), slant=="Right wing") %>% 
  group_by(person_id) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Where did this person go? --fox news, visits=2799
explore_yougov %>% 
  filter(person_id=='us:2381143', !is.na(label)) %>% 
  group_by(label, domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)