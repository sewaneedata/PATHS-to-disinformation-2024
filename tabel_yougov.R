# Load necessary libraries
library(tidyverse)
library(readxl)
library(kableExtra)

# Load and prepare ideology data
load("data/yougov_ideology.RData")

ideology <- yougov_ideology %>% 
  filter(!is.na(ideology))

# Load survey data
survey_data <- read_excel("data/SURVEY WAVE 1/US/SAV for client (topic) 20220222 28.9.2022 - CODES.xlsx")

# Change person_id to lower so that it can be joined
survey_data <- survey_data %>% mutate(person_id = tolower(person_id))

survey_data <- survey_data %>%
  semi_join(ideology, by = "person_id")

# Change gender to numeric so that it doesn't cause any issues
survey_data <- survey_data %>%
  mutate(gender = as.numeric(as.character(gender)))

# Join people from the survey onto ideology based on gender creating a table
yougov_join <- ideology %>% 
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

gender_table <- yougov_join %>% group_by(gender) %>%  tally()

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

# Convert qage to numeric where possible
survey_data <- survey_data %>%
  mutate(qage = as.numeric(as.character(qage)))

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

# Ideology tables
ideology_table <- ideology %>%
  filter(!is.na(slant)) %>% 
  group_by(slant) %>% 
  tally() %>% 
  rename(pol = slant)

# Household income table
survey_data <- survey_data %>%
  mutate(Q26 = as.numeric(as.character(Q26)))

house_inc_table <- survey_data %>%
  filter(!is.na(Q26)) %>%
  group_by(Q26) %>%
  tally() %>%
  rename(inc = Q26) %>%
  mutate(inc = case_when(
    inc == 1 ~ "< 5,000",
    inc == 2 ~ "5,000-9,999",
    inc == 3 ~ "10,000-14,999",
    inc == 4 ~ "15,000-19,999",
    inc == 5 ~ "20,000-24,999",
    inc == 6 ~ "25,000-29,999",
    inc == 7 ~ "30,000-34,999",
    inc == 8 ~ "35,000-39,999",
    inc == 9 ~ "40,000-44,999",
    inc == 10 ~ "45,000-49,999",
    inc == 11 ~ "50,000-59,999",
    inc == 12 ~ "60,000-69,999",
    inc == 13 ~ "70,000-99,999",
    inc == 14 ~ "100,000-149,999",
    inc == 15 ~ "> 150,000",
    inc == 996.0 ~ "Don’t know",
    inc == 997.0 ~ "Prefer not to answer",
    TRUE ~ NA
  ))

# Interest in politics table
survey_data <- survey_data %>%
  mutate(Q5_politics = as.numeric(as.character(Q5_politics)))

pol_news_table <- survey_data %>%
  filter(!is.na(Q5_politics)) %>%
  group_by(Q5_politics) %>%
  tally() %>%
  rename(pol_news = Q5_politics) %>%
  mutate(pol_news = case_when(
    pol_news == 1 ~ "Very interested",
    pol_news == 2 ~ "Somewhat interested",
    pol_news == 3 ~ "Not very interested",
    pol_news == 4 ~ "Not at all interested",
    TRUE ~ NA
  ))

# Interest in economy news table
survey_data <- survey_data %>%
  mutate(Q5_economys = as.numeric(as.character(Q5_economys)))

econ_news_table <- survey_data %>%
  filter(!is.na(Q5_economys)) %>%
  group_by(Q5_economys) %>%
  tally() %>%
  rename(econ_news = Q5_economys) %>%
  mutate(econ_news = case_when(
    econ_news == 1 ~ "Very interested",
    econ_news == 2 ~ "Somewhat interested",
    econ_news == 3 ~ "Not very interested",
    econ_news == 4 ~ "Not at all interested",
    TRUE ~ NA
  ))

# Good at understanding political issues table
survey_data <- survey_data %>%
  mutate(Q7_1 = as.numeric(as.character(Q7_1)))

good_at_important_pol_issues <- survey_data %>%
  filter(!is.na(Q7_1)) %>%
  group_by(Q7_1) %>%
  tally() %>%
  rename(good_at_important_pol_issues = Q7_1) %>%
  mutate(good_at_important_pol_issues = case_when(
    good_at_important_pol_issues == 1 ~ "Strongly Disagree",
    good_at_important_pol_issues == 2 ~ "Disagree",
    good_at_important_pol_issues == 3 ~ "Neither agree nor disagree",
    good_at_important_pol_issues == 4 ~ "Agree", 
    good_at_important_pol_issues == 5 ~ "Strongly Agree",
    TRUE ~ NA
  ))

# Skeptical of mainstream media table
survey_data <- survey_data %>%
  mutate(Q8_3 = as.numeric(as.character(Q8_3)))

skeptical_of_mainstream_meadia <- survey_data %>%
  filter(!is.na(Q8_3)) %>%
  group_by(Q8_3) %>%
  tally() %>%
  rename(skeptical_of_mainstream_meadia = Q8_3) %>%
  mutate(skeptical_of_mainstream_meadia = case_when(
    skeptical_of_mainstream_meadia == 1 ~ "Strongly Disagree",
    skeptical_of_mainstream_meadia == 2 ~ "Disagree",
    skeptical_of_mainstream_meadia == 3 ~ "Neither agree nor disagree",
    skeptical_of_mainstream_meadia == 4 ~ "Agree", 
    skeptical_of_mainstream_meadia == 5 ~ "Strongly Agree",
    TRUE ~ NA
  ))

# Race table
survey_data <- survey_data %>%
  mutate(race = as.numeric(as.character(race)))

race_table <- survey_data %>%
  filter(!is.na(race)) %>%
  group_by(race) %>%
  tally() %>%
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    race == 4 ~ "Asian",
    race == 5 ~ "Native American",
    race == 6 ~ "Two or more races",
    race == 7 ~ "Other",
    TRUE ~ NA
  ))

# Combine all tables into a single dataframe with consistent column names
combined_table <- bind_rows(
  age_table %>% rename(Variable = qage),
  gender_table %>% rename(Variable = gender),
  ideology_table %>% rename(Variable = pol),
  education_table %>% rename(Variable = educ),
  house_inc_table %>% rename(Variable = inc),
  pol_news_table %>% rename(Variable = pol_news),
  econ_news_table %>% rename(Variable = econ_news),
  good_at_important_pol_issues %>% rename(Variable = good_at_important_pol_issues),
  skeptical_of_mainstream_meadia %>% rename(Variable = skeptical_of_mainstream_meadia),
  race_table %>% rename(Variable = race)
) %>%
  select(Variable, n)

# Display the combined table
print(combined_table)

# Create styled table
styled_table <- kbl(combined_table, caption = 'YouGov Information Table') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed'), font_size = 12) %>%
  pack_rows("Age group", 1, 5) %>%
  pack_rows("Gender", 6, 7) %>%
  pack_rows("Political affiliation", 8, 10) %>%
  pack_rows("Education level", 11, 17) %>%
  pack_rows("Household income (per year)", 18, 33) %>%
  pack_rows("Interest in news about domestic or international politics", 34, 37) %>%
  pack_rows("Interest in economy, business and financial news", 38, 41) %>% 
  pack_rows("It is important to be skeptical of what the mainstream media reports", 42, 46) %>% 
  pack_rows("Good at understanding important political issues", 47, 51) %>% 
  pack_rows("Race", 52, 58)

print(styled_table)
