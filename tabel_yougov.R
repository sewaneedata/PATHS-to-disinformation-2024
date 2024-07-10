library(tidyverse)
library(readxl)

ideology <- load("data/yougov_ideology.RData")

ideology <- yougov_ideology %>% 
  filter(!is.na(ideology))

survey_data <- read_excel("data/SURVEY WAVE 1/US/SAV for client (topic) 20220222 28.9.2022 - CODES.xlsx")


# change person id to lower so that it can be joined
survey_data <- survey_data %>% mutate(person_id = tolower(person_id))


# change gender to numeric so that it doesn't cause any issues
survey_data <- survey_data %>%
  mutate(gender = as.numeric(as.character(gender)))

# join people from the survey onto ideology based on gender creating a table 
yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() %>%
  left_join(survey_data %>% select(person_id, gender), by = "person_id")


# Convert gender to human-readable form after joining
yougov_join <- yougov_join %>%
  mutate(gender = case_when(
    gender == 1.0 ~ "Male",
    gender == 2.0 ~ "Female",
    TRUE ~ NA_character_  # Handle any other values or NA
  ))


yougov_join %>% group_by(gender) %>%  tally()

# Convert educ to numeric where possible
survey_data <- survey_data %>%
  mutate(educ = as.numeric(as.character(educ)))

# Replace numeric codes with education labels
education_table <- survey_data %>%
  mutate(educ = case_when(
    educ == 1.0 ~ "No High school",
    educ == 2.0 ~ "High school",
    educ == 3.0 ~ "Some college credit, no degree",
    educ == 4.0 ~ "Associate degree",
    educ == 5.0 ~ "Bachelor's degree",
    educ == 6.0 ~ "Higher Education",
    TRUE ~ "Unknown"
  )) %>%
  group_by(educ) %>%
  tally()

education_table


survey_data <- survey_data %>%
  mutate(qage = as.numeric(as.character(qage)))


mean(survey_data$qage, na.rm = TRUE)

age_table <- survey_data %>% 
  filter(!is.na(qage)) %>% 
  mutate(qage = case_when(
    qage <= 17 ~ "17 or younger",
    qage <= 34 ~ "18-34",
    qage <= 44 ~ "35-44",
    qage <= 54 ~ "45-54",
    qage <= 64 ~ "55-64",
    qage >= 65 ~ "65+",
    TRUE ~ NA_character_  # Ensure NA handling
  )) %>%
  group_by(qage) %>% 
  tally()

age_table


# solving for disinformation ( not working )

classify_yougov <- read_csv("data/classify_yougov.csv")


yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() 

explore_yougov <- classify_yougov %>% 
  left_join(yougov_join, by="person_id")

# Check for NA values in label
sum(is.na(classify_yougov$label))

# Check for non-NA values in label
sum(!is.na(classify_yougov$label))


#Visited disinformation site(s)
dis_table <- classify_yougov %>% 
  group_by(person_id) %>% 
  summarize(dis = sum(!is.na(label))) %>%
  mutate(dis = case_when(
    dis == 0 ~ "Not visited",
    dis >= 1 ~ "Visited"
  )) %>%
  ungroup() %>%  # Ungroup to remove groupings
  count(dis) %>%  # Count the number of individuals in each category
  complete(dis = c("Not visited", "Visited"), fill = list(n = 0))

dis_table

# how many people have visited fake news sources?
explore_yougov %>% 
  summarise(unique_ids = n_distinct(person_id))


# ideology tables
ideology_table <- yougov_ideology %>% 
  filter(!is.na(slant)) %>% 
  group_by(slant) %>% 
  tally %>% 
  rename(pol=slant)

ideology_table


# grouping by slant
yougov_ideology %>% 
  group_by(slant) %>% 
  summarise(unique_ids = n_distinct(person_id))




survey_data <- survey_data %>%
  mutate(Q26 = as.numeric(as.character(Q26)))

house_inc_table <- survey_data %>% 
  filter(!is.na(Q26)) %>%
  group_by(Q26) %>% 
  tally %>% 
  rename(inc=Q26) %>% 
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
                       inc==996.0 ~ "Don’t know",
                       inc==997.0 ~ "Prefer not to answer", TRUE~NA))

house_inc_table

survey_data <- survey_data %>%
  filter(!is.na(Q6)) %>%
  mutate(Q6 = as.numeric(as.character(Q6)))


  
interest_table <- survey_data %>% 
  filter(!is.na(Q6)) %>%
  group_by(Q6) %>% 
  tally() %>% 
  rename(interest = Q6) %>%
  mutate(interest = case_when(
    interest == 1 ~ "Very interested",
    interest == 2 ~ "Somewhat interested",
    interest == 3 ~ "Not very interested",
    interest == 4 ~ "Not at all interested",
    TRUE ~ "Unknown"  # Handle any unexpected values
  ))
interest_table


survey_data <- survey_data %>%
  filter(!is.na(Q5_politics)) %>%
  mutate(Q5_politics = as.numeric(as.character(Q5_politics)))

#Interested in News about domestic or international politics
pol_news_table <- survey_data %>% 
  group_by(Q5_politics) %>% 
  tally %>% 
  rename(pol_news=Q5_politics) %>%
  # the code book for people's interest in type of news is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(pol_news=case_when(pol_news==1 ~ "Very interested",
                            pol_news==2 ~ "Somewhat interested",
                            pol_news==3 ~ "Not very interested",
                            pol_news==4 ~ "Not at all interested", TRUE~NA))
interest_table



survey_data <- survey_data %>%
  filter(!is.na(Q5_economys)) %>%
  mutate(Q5_economys = as.numeric(as.character(Q5_economys)))

#Interested in Economy, Business and financial news
econ_news_table <- survey_data %>% 
  group_by(Q5_economys) %>% 
  tally %>% 
  rename(econ_news=Q5_economys) %>%
  # the code book for people's interest in type of news is here (https://docs.google.com/spreadsheets/d/1iak_hLNUfix-T7ZWQXSwYrJYCBwpeeqe/edit?gid=744930251#gid=744930251)
  mutate(econ_news=case_when(econ_news==1 ~ "Very interested",
                             econ_news==2 ~ "Somewhat interested",
                             econ_news==3 ~ "Not very interested",
                             econ_news==4 ~ "Not at all interested", TRUE~NA))
econ_news_table

#install.packages("kableExtra")
library(dplyr)
library(kableExtra)


table_df <- bind_rows(
  age_table, gender_table, ideology_table, education_table, 
  house_inc_table, pol_news_table, econ_news_table
) %>%
  mutate(Variables = coalesce(qage, gender, pol, educ, inc, pol_news, econ_news)) %>%
  select(Variables, n)

head( table_df, 10)

view(table_df)

# Create styled table
styled_table <- kbl(table_df, caption = 'Descriptive Table') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed'), font_size = 12) %>%
  pack_rows("Age group", 1, 5) %>%
  pack_rows("Gender", 6, 7) %>%
  pack_rows("Political affiliation", 8, 10) %>%
  pack_rows("Education level", 11, 16) %>%
  pack_rows("Household income (per year)", 17, 33) %>%
  pack_rows("Interest in news about domestic or international politics", 34, 37) %>%
  pack_rows("Interest in economy, business and financial news", 38, 41)

# table is working missing a few things that the other one but my survey sheet doesn't have those things in it anyways. 
# also disinformation is not working and needs to be fixed 
styled_table


load("data/explore_")

#Disinformation websites
dis_sites <- explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(label) %>%
  summarise(visits=n(), num_sites=n_distinct(domain))

