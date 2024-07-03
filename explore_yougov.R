library(tidyverse)
ideology <- load("data/yougov_ideology.RData")
classify_yougov <- read_csv("data/classify_yougov.csv")


yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() 

explore_yougov <- classify_yougov %>% 
  left_join(yougov_join, by="person_id")

# how many people in the data set?? 443
total_people <- yougov_ideology %>% 
  summarise(unique_ids = n_distinct(person_id)) %>% 
  pull(unique_ids)
total_people


# how many people have visited fake news sources?
fake_news_visitors <- explore_yougov %>% 
  summarise(unique_ids = n_distinct(person_id)) %>% 
  pull(unique_ids)
fake_news_visitors



# 75% of people visted fake news sites
fake_news_visitors / total_people


non_fake_news_visitors <- total_people - fake_news_visitors
non_fake_news_visitors


# Political affiliation counts
# Total left
yougov_ideology %>% 
  filter(slant == 'left') %>% 
  summarise(unique_ids = n_distinct(person_id))

# Total right
yougov_ideology %>% 
  filter(slant == 'right') %>% 
  summarise(unique_ids = n_distinct(person_id))

# Total neutral
yougov_ideology %>% 
  filter(slant == 'neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))

# Visit counts by political affiliation
# Left visits
explore_yougov %>% 
  filter(slant == 'left') %>% 
  summarise(unique_ids = n_distinct(person_id))

# Right visits
explore_yougov %>% 
  filter(slant == 'right') %>% 
  summarise(unique_ids = n_distinct(person_id))

# Neutral visits
explore_yougov %>% 
  filter(slant == 'neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))


# Top fake news visitor
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(person_id, slant) %>% 
  summarise(visits = n(), .groups = "drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)

# Distribution of site types visited by neutral affiliation
explore_yougov %>% 
  filter(slant == "neutral", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()

# Top visited site by neutral affiliation
explore_yougov %>% 
  filter(slant == "neutral", label == "right bias") %>% 
  group_by(domain) %>% 
  summarise(visits = n()) %>% 
  arrange(desc(visits))


# Top fake news visitor in left wing
explore_yougov %>% 
  filter(!is.na(label), slant == "left") %>% 
  group_by(person_id) %>% 
  summarise(visits = n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

# Distribution of site types visited by left affiliation
explore_yougov %>% ``
  filter(slant == "left", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()

# Top visited site by left affiliation
explore_yougov %>% 
  filter(slant == "left", label == "fake") %>% 
  group_by(domain) %>% 
  tally()

# Distribution of site types visited by right affiliation
explore_yougov %>% 
  filter(slant == "right", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()


# Top visited site by right affiliation
explore_yougov %>% 
  filter(slant == "right", label == "fake") %>% 
  group_by(domain) %>% 
  summarise(visits = n()) %>% 
  arrange(desc(visits))


# ### ## graph sections

# Create a dataframe for plotting total vs fake news visitors
plot_data_total_vs_fake <- tibble(
  category = c("Total People", "Fake News Visitors"),
  count = c(total_people, fake_news_visitors)
)

ggplot(plot_data_total_vs_fake, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Total Number of People vs. Fake News Visitors", 
       x = "Category", 
       y = "Number of People") +
  theme_minimal() +
  scale_fill_manual(values = c("Total People" = "blue", "Fake News Visitors" = "red")) +
  geom_text(aes(label = count), vjust = -0.5)


# Plot 2: Proportion of Fake News Visitors
proportion_data <- tibble(
  category = c("Fake News Visitors", "Non-Fake News Visitors"),
  count = c(fake_news_visitors, non_fake_news_visitors)
)

ggplot(proportion_data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Fake News Visitors", 
       x = "", 
       y = "Number of People") +
  theme_minimal() +
  scale_fill_manual(values = c("Fake News Visitors" = "red", "Non-Fake News Visitors" = "blue")) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5))

# Plot 3: Political Affiliation Counts
plot_data_affiliation <- yougov_ideology %>%
  filter(slant %in% c("left", "right", "neutral", "na")) %>%
  group_by(slant) %>%
  summarise(count = n_distinct(person_id))

ggplot(plot_data_affiliation, aes(x = slant, y = count, fill = slant)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Political Affiliation Counts", 
       x = "Affiliation", 
       y = "Number of People") +
  theme_minimal() +
  scale_fill_manual(values = c("left" = "blue", "right" = "red", "neutral" = "grey", "na" = 'black')) +
  geom_text(aes(label = count), vjust = -0.5)

# Plot 4: Visits by Political Affiliation
plot_data_visits <- explore_yougov %>%
  filter(slant %in% c("left", "right", "neutral")) %>%
  group_by(slant) %>%
  summarise(count = n_distinct(person_id))

ggplot(plot_data_visits, aes(x = slant, y = count, fill = slant)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Visits by Political Affiliation", 
       x = "Affiliation", 
       y = "Number of People") +
  theme_minimal() +
  scale_fill_manual(values = c("left" = "blue", "right" = "red", "neutral" = "grey")) +
  geom_text(aes(label = count), vjust = -0.5)
