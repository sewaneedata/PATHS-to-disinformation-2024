#load libraies



#getting YouGov ideology column 

survey_info <- read.csv("data/YouGov/US_survey.csv")
webtracking_yougov <- read.csv("data/YouGov/webtracking.RData")

survey_info <- survey_info %>% rename(person_id = caseid.x)

# Perform the left join
merged_data <- webtracking_yougov %>% 
  left_join(survey_info %>% select(person_id, q13), by = "person_id")

# Select the necessary columns, including the q13 column
yougov_ideology <- merged_data %>%
  select(everything(), q13)
