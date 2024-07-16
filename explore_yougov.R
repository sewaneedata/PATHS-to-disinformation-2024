library(tidyverse)
library(readxl)
library(ggalluvial)


# make sure to move the created datasets to the data folder!
# Load data
load("data/yougov_ideology.RData")
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
  mutate(next_label = lead(label)) %>% 
  mutate( prev_domain = lag( extension ), next_domain = lead( extension )) %>% 
  mutate(prev_domain=ifelse(prev_domain %in% c("google.com", "youtube.com", "facebook.com"), prev_domain, "non")) %>% 
  mutate(prev_domain=case_when(prev_domain=="google.com" ~ "Google",
                               prev_domain=="youtube.com" ~ "Youtube",
                               prev_domain=="facebook.com" ~ "Facebook",
                               TRUE ~ prev_domain)) %>%
  mutate(disinformation=case_when(extension=="cnn.com" ~ "CNN",
                                  extension=="foxnews.com" ~ "Fox News",
                                  TRUE ~ extension)) %>% 
  mutate(disinformation = ifelse(disinformation %in% c("CNN", "Fox News"), disinformation, "non"))





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
    is.na(ref_media) & other == "socialmedia" ~ "Social Media")) %>% 
  mutate(next_domain=ifelse(next_domain %in% c("google.com", "cnn.com", "foxnews.com"), next_domain, "non")) %>% 
  mutate(next_domain=case_when(next_domain=="google.com" ~ "Browsing",
                               next_domain=="cnn.com" ~ "CNN",
                               next_domain=="foxnews.com" ~ "Fox News",
                               TRUE ~ next_domain))

explore_yougov <- explore_yougov %>% 
  mutate(axis2 = ifelse(next_label %in% c("Left Bias", "Right Bias"), next_label, "non")) %>% 
  mutate(type = factor(type, levels = c("Referrals", "Social Media", "Both")))

path_1 <- explore_yougov %>% 
  group_by(type, axis2, slant) %>% 
  tally()

path_1 <- path_1 %>% mutate( slant = case_when( slant == "slant" ~ "Political Affiliation", TRUE ~ slant ))

#Disinformation visit dataset for the graph
top_3 <- explore_yougov %>% 
  group_by(prev_domain, next_domain, slant, disinformation) %>% 
  tally()

#Disinformation visits
ggplot(top_3 %>% filter( prev_domain != "non", next_domain != "non", disinformation != "non", !is.na(slant)), aes(axis1 = prev_domain, axis2 = disinformation, axis3=next_domain, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c("blue", "grey", "red") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Previous", "Current", "Next"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Disinformation Visits",
       fill = "Political affiliation")


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


ideology <- explore_yougov %>%
  filter( ref_media == "media" ) %>%
  group_by( extension, slant ) %>%
  summarize( n = n_distinct(person_id) ) %>%
  mutate( total = sum(n), pct = n/total ) %>%
  filter( total >= 10 ) %>%
  arrange( desc( pct ) ) %>%
  slice_head(n=1) %>%
  select( extension, ideology = slant )
  

explore_yougov <- left_join( explore_yougov, ideology, by = "extension")

path_2 <- explore_yougov %>% 
  group_by( ideology, axis2, slant) %>% 
  tally()



ggplot( path_2 %>% drop_na(ideology) %>%  filter(axis2 != "non", ideology != "neutral"), aes( axis1 = ideology, axis2 = axis2, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c( "blue", "grey", "red")) + 
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Partisan media", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Times New Roman")) +
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Partisan Media Visits",
       fill = "Political affiliation")



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

#Save dataset
save(explore_yougov, file="data/explore_yougov.RData")
