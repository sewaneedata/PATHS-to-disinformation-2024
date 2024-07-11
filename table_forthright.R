# This script provides discriptive tables about the information of those who were in the survey
# for the Forthright Dataset
#############################################################################################
####PEOPLE------
#Load library
library(tidyverse)
library(kableExtra)

#Import dataset that we want to use for descriptive
explore_survey <- read_csv("data/forthright_survey.csv")

#Gender
gender_table <- explore_survey %>% 
  group_by(Q26) %>% 
  tally %>% 
  rename(gender=Q26) %>% 
  # the code book for gender is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(gender=case_when(gender==1 ~ "Male",
                          gender==2 ~ "Female",
                          gender==3 ~ "Non-binary", TRUE~NA))
#Education
education_table <- explore_survey %>% 
  group_by(Q27) %>% 
  tally %>% 
  rename(edu=Q27) %>% 
  # the code book for education is here (https://docs.google.com/spreadsheets/d/1D8J5wKwC87S5VBqbRTeeNUBYk5HtcOSK/edit?gid=900019369#gid=900019369)
  mutate(edu=case_when(edu==1 ~"< High school",
                        edu==2 ~ "High school",
                        edu==3 ~ "Some college credit, no degree",
                        edu==4 ~ "Trade/technical/vocational training",
                        edu==5 ~ "Associate degree",
                        edu==6 ~ "Bachelor's degree",
                        edu==7 ~ "Master's degree",
                        edu==8 ~ "Professional degree",
                        edu==9 ~ "Doctorate degree", TRUE~NA))

#Race
race_table <- explore_survey %>% 
  group_by(race_id) %>% 
  tally %>% 
  rename(race=race_id) %>% 
  # the code book for race is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(race=case_when(race==1 ~ "Some other race",
                        race==2 ~ "Mixed racial heritage",
                        race==3 ~ "White",
                        race==4 ~ "Black or African American",
                        race==5 ~ "American Indian or Alaska Native",
                        race==6 ~ "Asian Indian",
                        race==7 ~ "Chinese",
                        race==8 ~ "Filipino",
                        race==9 ~ "Japanese",
                        race==10 ~ "Korean",
                        race==11 ~ "Vietnamese",
                        race==12 ~ "Other Asian",
                        race==13 ~ "Native Hawaiian",
                        race==14 ~ "Guamanian or Chamorro",
                        race==15 ~ "Samoan",
                        race==16 ~ "Other Pacific Islander", TRUE~NA))

#Age
age_table <- explore_survey %>% 
  group_by(QAGE) %>% 
  tally %>% 
  rename(age=QAGE) %>% 
  # the code book for age is here (https://docs.google.com/spreadsheets/d/1D8J5wKwC87S5VBqbRTeeNUBYk5HtcOSK/edit?gid=900019369#gid=900019369)
  mutate(age=case_when(age==1 ~ "17 or younger",
                       age==2~ "18-34",
                       age==3 ~ "35-44",
                       age==4 ~ "45-54",
                       age==5 ~ "55-64",
                       age==6 ~ "65+", TRUE~NA))

#Political affiliation 
ideology_table <- explore_survey %>% 
  group_by(slant) %>% 
  tally %>% 
  rename(pol=slant)

#Household income per year
house_inc_table <- explore_survey %>% 
  group_by(Q28) %>% 
  tally %>% 
  rename(inc=Q28) %>% 
  # the code book for household income is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(inc=case_when(inc==1 ~ "< 5,000",
                       inc==2 ~ "5,000-9,999",
                       inc==3 ~ "10,000-14,999",
                       inc==4 ~ "15,000-19,999",
                       inc==5 ~ "20,000-24,999",
                       inc==6 ~ "25,000-29,999",
                       inc==7 ~ "30,000-34,999",
                       inc==8 ~ "35,000-39,999",
                       inc==9 ~ "40,000-44,999",
                       inc==10 ~ "45,000-49,999",
                       inc==11 ~ "50,000-59,999",
                       inc==12 ~ "60,000-69,999",
                       inc==13 ~ "70,000-99,999",
                       inc==14 ~ "100,000-149,999",
                       inc==15 ~ "> 150,000",
                       inc==16 ~ "Don’t know",
                       inc==17 ~ "Prefer not to answer", TRUE~NA))

#Avoid political news
avoid_pol_table <- explore_survey %>% 
  group_by(Q2) %>% 
  tally %>% 
  rename(freq=Q2) %>%
  # the code book for how often people avoid political news is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(freq=case_when(freq==1 ~ "Daily",
                        freq==2 ~ "Weekly",
                        freq==3 ~ "Several times a month",
                        freq==4 ~ "1-2 days a month",
                        freq==5 ~ "Less often",
                        freq==6 ~ "Never",
                        freq==7 ~ "Don't know", TRUE~NA))


#Interested in politics
interest_table <- explore_survey %>% 
  group_by(Q7) %>% 
  tally %>% 
  rename(interest=Q7) %>%
  # the code book for people's interest in politics is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(interest=case_when(interest==1 ~ "Very interested",
                            interest==2 ~ "Somewhat interested",
                            interest==3 ~ "Not very interested",
                            interest==4 ~ "Not at all interested", TRUE~NA))

#Putting all small tables in one dataset
table_df <- bind_rows(age_table, gender_table, ideology_table, education_table, house_inc_table, race_table, avoid_pol_table, interest_table) %>% 
  mutate(Variables=coalesce(age, gender, pol, edu, inc, race, freq, interest)) %>% 
  select(Variables, n)

#Make table looks pretty
#install.packages("kableExtra")
#Nika's code of making table
kbl(table_df, caption = 'Descriptive Demographic Table') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', font_size = 12)) %>%
  pack_rows('Age group', 1, 6) %>% # pack_rows puts the rows in groups
  pack_rows('Gender', 7, 10) %>% 
  pack_rows('Political affiliation', 11, 14) %>%
  pack_rows('Education level', 15, 24) %>% 
  pack_rows('Household income (per year)', 25, 42) %>% 
  pack_rows('Race', 43, 56) %>% 
  pack_rows('News avoidance', 57, 64) %>% 
  pack_rows('Interest in politics', 65, 69) %>% 
  kable_minimal()

#Gender + Political
ggplot(explore_survey, aes(x=Q26, fill=slant, na.rm=TRUE))+
  geom_bar()+
  labs(x="Gender", fill="Political affiliation", title="Distribution of political affiliation among gender")

####WEBSITES-----
#Import dataset
load("data/explore_forthright.RData")

#Table of social media site that people visited
socialmedia <- explore_forthright %>% 
  filter(other=="socialmedia") %>% 
  group_by(domain) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

#Table of entertainment site that people visited
entertainment <- explore_forthright %>% 
  filter(other=="entertainment") %>% 
  group_by(domain) %>% 
  tally()%>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

#Table of alternative media that people visited
alt_media <- explore_forthright %>% 
  filter(other=="alternativemedia") %>% 
  group_by(domain) %>% 
  tally()%>% 
  arrange(desc(n))

#Table of search engines people visited
referral <- explore_forthright %>% 
  filter(ref_media=="referrals") %>% 
  group_by(domain) %>% 
  tally()%>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

media <- explore_forthright %>% 
  filter(ref_media=="media") %>% 
  group_by(domain) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(10)
# 3608 observations, how should we deal with this? Taking top 10?

#Making table for website
web_df <- bind_rows(referral, media, socialmedia, entertainment, alt_media)

kbl(web_df, caption = 'Descriptive Table for Media') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', font_size = 12)) %>%
  pack_rows('Referrals', 1, 15) %>% # pack_rows puts the rows in groups
  pack_rows('Media', 16, 25) %>% 
  pack_rows('Social media', 26, 37) %>% 
  pack_rows('Entertainment', 38, 49) %>% 
  pack_rows('Alternative media', 50, 55) %>% 
  kable_minimal()

# should we take off the domain with less than 10 visits? YES!
