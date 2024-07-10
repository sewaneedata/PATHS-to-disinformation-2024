# this is the file that will join the ideology of forthright with the webtracking data
#load libs
library(tidyverse)
library(readxl)

# Read the Excel file into a data frame
screener_data <- read_excel("data/forthright/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")
load("data/FORTHRIGHT/forthright_webtracking_nicole.RData")

# Assuming webtracking_forthright is the correct object name, if not, adjust accordingly

# Perform a left join to add the q13 column to the second dataset based on member_id

merged_data <- webtracking %>%
  left_join(screener_data %>% select(member_id, Q13), by = "member_id")

# Now merged_data contains all columns of webtracking_data and q13 from screener_data
# If you want to keep only the original columns of webtracking_data plus the q13 column, select the columns of interest:
forthright_ideology <- merged_data %>%
  select(everything(), Q13) %>% 
  # Create a new column to classify political affiliation based on Q13
  mutate(slant=case_when(Q13<=4 ~ "Left wing",
                         Q13==5 ~ "Neutral",
                         Q13>=6 ~ "Right wing", TRUE~NA))


#save
save(forthright_ideology, file ="data/forthright_ideology.RData")

