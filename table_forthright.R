#This script is for making a descriptive table

#Load library
library(tidyverse)

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

#Visited disinformation site(s)
dis_table <- explore_survey %>% 
  group_by(dis) %>% 
  tally %>% 
  mutate(dis=case_when(dis==0 ~ "Not visited",
                       dis==1 ~ "Visited", TRUE~NA))

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

#Interested in News about domestic or international politics
pol_news_table <- explore_survey %>% 
  group_by(Q6r1) %>% 
  tally %>% 
  rename(pol_news=Q6r1) %>%
  # the code book for people's interest in type of news is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(pol_news=case_when(pol_news==1 ~ "Very interested",
                            pol_news==2 ~ "Somewhat interested",
                            pol_news==3 ~ "Not very interested",
                            pol_news==4 ~ "Not at all interested", TRUE~NA))

#Interested in Economy, Business and financial news
econ_news_table <- explore_survey %>% 
  group_by(Q6r2) %>% 
  tally %>% 
  rename(econ_news=Q6r2) %>%
  # the code book for people's interest in type of news is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(econ_news=case_when(econ_news==1 ~ "Very interested",
                            econ_news==2 ~ "Somewhat interested",
                            econ_news==3 ~ "Not very interested",
                            econ_news==4 ~ "Not at all interested", TRUE~NA))

#Putting all small tables in one dataset
table_df <- bind_rows(age_table, gender_table, dis_table, ideology_table, education_table, house_inc_table, race_table, avoid_pol_table, interest_table, pol_news_table, econ_news_table) %>% 
  mutate(Variables=coalesce(age, gender, dis, pol, edu, inc, race, freq, interest, pol_news, econ_news)) %>% 
  select(Variables, n)

#Make table looks pretty
#install.packages("kableExtra")
library(kableExtra)

#Nika's code
kbl(table_df, caption = 'Descriptive Table') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', font_size = 12)) %>%
  pack_rows('Age group', 1, 6) %>% # pack_rows puts the rows in groups
  pack_rows('Gender', 7, 10) %>% 
  pack_rows('Visited disinformation sites?', 11, 12) %>%
  pack_rows('Political affiliation', 13, 16) %>%
  pack_rows('Education level', 17, 26) %>% 
  pack_rows('Household income (per year)', 27, 44) %>% 
  pack_rows('Race', 45, 58) %>% 
  pack_rows('News avoidance', 59, 66) %>% 
  pack_rows('Interest in politics', 67, 71) %>% 
  pack_rows('Interest in news about domestic or international politics', 72, 76) %>% 
  pack_rows('Interest in economy, business and financial news', 77, 81) %>% 
  kable_minimal()



