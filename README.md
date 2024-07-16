# PATHS-to-disinformation-2024
A summer 2024 project in collaboration with DataLab and the Open University of Catalonia.

This is the repository for the PATHS to Disinformation 2024 project by Sewanee DataLab fellows. The project partner, Professor Ana Cardinal, is a political science professor at the Open University of Catalonia. Our partner requested an analysis of two data sets, brought by YouGov (370 participants) and Forthright (584 participants), that track the participants' browsing history in a period that spanned 2-3 months. What our analysis targets is identifying the digital pathways that lead people to misinformation and to see how political ideology or partisanship can affect a person's internet behavior.   

To run this code you have to install R studio and install every library per each script. Mainly what most of the scripts on this repository do is clean the dense data sets that we had and put them in a state from which observations can be inferred. Our observations/results are represented as both our identified pattern of how people come across misinformation, and a linear regression model that offers a quantifiable conclusion to the relationship between political affiliation bias and the consumption of disinformation. 

Gabe Rice C'25 

Nicole Nguyen C'26

Mohamed Salem C'26

Jawaria Jaleel C'27

Perla Guzman C'27


# Paths Script Order

Based on the data provided by our client, we’ve made several adjustments across multiple scripts. It’s important to note that, since we worked with two different web tracking processes, each dataset (YouGov and Forthright) has its own individual scripts that essentially perform the same functions. However, it’s advisable to start with the YouGov scripts, as they include the disinformation list and the WP3 list of media sources.

## YouGov Scripts

### 1. `clean_yougov.R`
- **Generates**: `yougov_Ideology.RData`
- **Function**: Merges the `person_id` from the web tracking data with the `person_id` from the YouGov survey, allowing us to incorporate the Ideology data.
- **Additional Task**: Creates a cleaned version of the disinformation list used for site classification.

### 2. `explore_yougov.R`
- **Function**: Utilizes both the Ideology list and the disinformation list to analyze users who visited disinformation sites.
- **Focus**: Creating Alluvial Diagrams to visualize the paths to disinformation.
- **Additional Code**: Answers various questions about the data, such as how many individuals have visited fake news sources.

### 3. `table_yougov.R`
- **Function**: Offers a more effective way to visualize survey questions by creating a comprehensive table to display the responses.

## Forthright Scripts

### 1. `clean_forthright.R`
- **Generates**: `forthright_Ideology.RData`
- **Function**: Merges the `member_id` from the web tracking data with the `member_id` from the Forthright survey, allowing us to incorporate the Ideology data.
- **Additional Task**: Takes the cleaned version of the disinformation list (from `clean_yougov.R`) for classifying disinformation sites.

### 2. `explore_forthright.R`
- **Function**: Utilizes both the Ideology list and the disinformation list to analyze users who visited disinformation sites.
- **Focus**: Creating Alluvial Diagrams to visualize the paths to disinformation.
- **Additional Code**: Answers various questions about the data, such as how many individuals have visited fake news sources.

### 3. `table_forthright.R`
- **Function**: Offers a more effective way to visualize survey questions by creating a comprehensive table to display the responses.
