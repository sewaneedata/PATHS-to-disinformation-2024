#This will join people's ideology and domain label from Forthright

#Load library + import data
library(tidyverse)
library(readxl)
library(ggalluvial)

load("data/forthright_ideology.RData")
load("data/classify_forthright.RData")


#Join dataset
forthright_join <- forthright_ideology %>% 
  select(member_id, Q13, slant) %>%
  distinct() 

explore_forthright <- forthright_label %>% 
  left_join(forthright_join, by="member_id") %>% 
  arrange( member_id, start_time_local ) %>% 
  #Looking at domain, referrals media, and category of that site from previous visit
  # mutate( prev_domain = lag( domain ), next_domain = lead( domain ) ) %>% 
  # mutate(prev_ref_media = lag(ref_media), next_ref_media = lead( ref_media )) %>% 
  # mutate(prev_other = lag(other), next_domain = lead( other )) %>% 
  mutate(label=case_when(label=="left bias" ~ "Left bias",
                         label=="right bias" ~ "Right bias", 
                         TRUE ~ label)) %>% 
  # looking at what kind of disinformation site people visited after the previous vist
  mutate(next_label= lead(label))


explore_forthright <- bind_rows( explore_forthright %>% filter(!is.na(label), duration_seconds > 4),
                                 explore_forthright %>% filter (is.na(label)) ) %>%
  arrange( member_id, start_time_local ) %>% 
  # categorize referrals and social media sites
  mutate(type=case_when(ref_media=="referrals" & other=="socialmedia" ~ "Both",
                        ref_media=="referrals" & other!="socialmedia" ~ "Referrals",
                        ref_media!="referrals" & other=="socialmedia" ~ "Social media",
                        ref_media=="referrals" & is.na(other) ~ "Referrals",
                        is.na(ref_media) & other=="socialmedia" ~ "Social media"))
                                 
  #We remove those visits disinformation sites that are less than 4 seconds because that mean they didn't really visit the site
  # filter(!is.na(label), duration_seconds > 4) %>% 
  # bind_rows(explore_forthright %>% filter (is.na(label)))


##Explore dataset----------
#Observations = 584
forthright_ideology %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total left = 207
forthright_ideology %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total right = 231
forthright_ideology %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total neutral = 144
forthright_ideology %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Who has the most visits in this dataset? ---member_id=4723361, visit=1150602, right wing
forthright_ideology %>% 
  group_by(member_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)

#People who visited untrustworthy sites = 338
explore_forthright %>% 
  summarise(unique_ids = n_distinct(member_id))

#Who are they and their visits?
explore_forthright %>% 
  group_by(member_id) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) 

#Where did this person go? Majority is left bias--55 visits
explore_forthright %>% 
  filter(member_id==4723361) %>% 
  group_by(label) %>% 
  tally()

#What sites? Majority is cnn.com--8 visits
explore_forthright %>% 
  filter(member_id==4723361, label=="left bias") %>% 
  group_by(domain, ref_media, other) %>% 
  tally()

#Top 10 sites get the most visit? democraticunderground.com--8288 visits
explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

##Explore survey-------------
screener_data <- read_excel("data/FORTHRIGHT/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")

#Create a dataset with variables that I want to look at
explore_survey <- forthright_ideology %>% 
  #If the person visit disinformation site, mark as 1, otherwise, mark as 0
  mutate(dis=ifelse(forthright_ideology$member_id %in% forthright_label$member_id, 1, 0)) %>% 
  select(member_id, slant, dis) %>% 
  distinct()
explore_survey <- explore_survey %>% 
  left_join(screener_data %>% 
              select(member_id,
                     Q28, #Household income
                     Q25, #Birth year
                     QAGE, #Age group
                     Q26, #Gender
                     Q2, #How often do you intentionally try to avoid political news?
                     Q7, #Generally speaking, how interested are you in politics?
                     Q27, #What is the highest degree or level of school you have completed?
                     ), by="member_id") %>% 
  mutate(age=2023-Q25)

#Import dataset
member_demos <- read_excel("data/FORTHRIGHT/305021_Member_Demos.xlsx")
explore_survey <- explore_survey %>% 
  left_join(member_demos %>% select(member_id, race_id), by="member_id")

#Add age, gender, and race to explore_forthright
explore_forthright <- explore_forthright %>% 
  left_join(explore_survey %>% 
              select(age, Q26, race_id, member_id), by="member_id") %>% 
  rename(gender=Q26)

forthright_ideology <- forthright_ideology %>% 
  left_join(explore_survey %>% 
              select(age, Q26, race_id, member_id), by="member_id") %>% 
  rename(gender=Q26)

#Save dataset
save(explore_forthright, file="data/explore_forthright.RData")
save(forthright_ideology, file="data/forthright_ideology.RData")
write_csv(explore_survey, "data/forthright_survey.csv")

####ALLUVIUM----

# partisan sites -- media that are determined by the amount of audience (example: majority of left wing visited this site -> it's a left wing site)
partisan <- explore_forthright %>% 
  filter( ref_media == "media" ) %>% 
  group_by( domain, slant ) %>% 
  summarize( n = n_distinct(member_id) ) %>% 
  mutate( total = sum(n), pct = n/total ) %>%
  filter( total >= 10 ) %>% 
  arrange( desc( pct ) ) %>%
  slice_head(n=1) %>% 
  select( domain, partisanship = slant ) 

test <- explore_forthright %>%
  # make a column to only look closely at left or right bias from disinformation sites
  mutate(axis2 = ifelse(next_label %in% c("Left bias", "Right bias"), next_label, "non")) %>% 
  mutate( type = factor( type, levels = c("Referrals", "Social media", "Both"))) %>% 
  left_join(partisan, by = "domain" ) 
      

# test <- test %>%
#   mutate( type = ifelse( is.na( type ), partisanship, type ) )

# Group and tally the data
paths_1 <- test %>%
  group_by(type, axis2, slant) %>%
  tally()

paths_2 <- test %>% 
  group_by(partisanship, axis2, slant) %>% 
  tally()

# Merge tallied data back with the original dataset to get the counts
#test <- left_join(test, tallied_data, by = "domain")

# Plot using ggplot
ggplot(paths_1 %>% drop_na(type) %>% filter( axis2 != "non"), aes(axis1 = type, axis2 = axis2, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c("blue", "grey", "red") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Type", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Domain Visits",
       fill = "Political affiliation")

#Plot partisan paths
ggplot(paths_2 %>% drop_na(partisanship) %>% filter( axis2 != "non", partisanship!="Neutral"), aes(axis1 = partisanship, axis2 = axis2, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c("blue", "grey", "red") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Partisan media", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Partisan Media Visits",
       fill = "Political affiliation")

#Disinformation visits
explore_survey$dis <- as.factor(explore_survey$dis)

ggplot(explore_survey %>% drop_na(slant), aes(x=slant))+
  geom_bar(aes(fill=dis))
