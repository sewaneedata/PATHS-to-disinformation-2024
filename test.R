#This is a test script

library(tidyverse)
library(readxl)
library(ggalluvial)

load("data/explore_forthright.RData")

###FUN-----
screener_data <- read_excel("data/FORTHRIGHT/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")
#Import the survey before process to the next step^^
fun <- forthright_ideology %>% 
  left_join(screener_data %>% 
              select(member_id, 
                     Q25, #Birthyear
                     Q26, #Gender
          ), by="member_id") %>% 
  mutate(age=2023-Q25)

fun %>% 
  group_by(Q26, slant) %>% 
  filter(grepl("xvideos", domain, ignore.case = TRUE)) %>% 
  summarise(unique_id= n_distinct(member_id))

fun %>% 
  group_by(Q26, slant, age) %>% 
  filter(grepl("porn", domain, ignore.case = TRUE)) %>% 
  summarise(unique_id= n_distinct(member_id))

###TESTING---
load("data/explore_forthright.RData")

test %>% 
  group_by(domain) %>% 
  filter(ref_media=="referrals") %>% 
  tally() %>% 
  arrange(desc(n))

test %>% 
  group_by(domain, label) %>% 
  filter(!is.na(label)) %>% 
  summarise(n=n_distinct(member_id)) %>% 
  arrange(desc(n))

test <- explore_forthright %>% 
  mutate(prev_domain=lag(domain), next_domain=lead(domain)) %>% 
  mutate(prev_domain=ifelse(prev_domain %in% c("google.com", "youtube.com", "facebook.com"), prev_domain, "non")) %>% 
  mutate(next_domain=ifelse(next_domain %in% c("cnn.com", "foxnews.com", "dailymail.co.uk"), next_domain, "non")) %>% 
  mutate(prev_domain=case_when(prev_domain=="google.com" ~ "Google",
                               prev_domain=="youtube.com" ~ "Youtube",
                               prev_domain=="facebook.com" ~ "Facebook",
                               TRUE ~ prev_domain)) %>%
  mutate(next_domain=case_when(next_domain=="cnn.com" ~ "CNN",
                               next_domain=="foxnews.com" ~ "Fox News",
                               TRUE ~ next_domain)) %>% 
  mutate(label=case_when(label=="left bias" ~ "Left bias",
                         label=="right bias" ~ "Right bias", 
                         TRUE ~ label)) %>% 
  # looking at what kind of disinformation site people visited after the previous vist
  mutate(next_label= lead(label)) %>% 
  mutate(left_right = ifelse(next_label %in% c("Left bias", "Right bias"), next_label, "non"))

top3 <- test %>% 
  group_by(prev_domain, next_domain, slant, gender, left_right) %>% 
  tally()

top3$gender <- as.factor(top3$gender)

ggplot(top3 %>% filter( prev_domain != "non", next_domain != "non"), aes(axis1 = prev_domain, axis2 = next_domain, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c("blue", "grey", "red") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Media", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Domain Visits",
       fill = "Political affiliation")

ggplot(top3 %>% filter( prev_domain != "non", next_domain != "non", left_right != "non"), aes(axis1 = prev_domain, axis2 = next_domain, axis3=left_right, y = n)) +
  geom_alluvium(aes(fill=slant)) +
  scale_fill_manual( values = c("blue", "grey", "red") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Media", "Site", "Disinformation"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths",
       title = "Alluvial Diagram of Domain Visits",
       fill = "Political affiliation")

####MAKING THINGS-----
# simple data fram to set the state for presentation
df <- data.frame(
  previous = c("Google", "Google"),
  current = c("Food", "Fashion"),
  next_visit = c("Browsing", "Stay"),
  frequency = c(2, 3),
  color = c("limegreen", "royalblue")
)


# making alluvial
ggplot(df , aes(axis1 = previous, axis2 = current, axis3=next_visit, y = frequency)) +
  geom_alluvium(aes(fill=color)) +
  scale_fill_manual( values = c("limegreen", "royalblue") ) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Previous", "Current", "Next"), expand = c(0.15, 0.05)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman")) + 
  labs(y = "Number of Visits",
       x = "Paths")
