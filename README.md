# PATHS-to-disinformation-2024
A summer 2024 project in collaboration with DataLab and the Open University of Catalonia.

This is the repository for the PATHS to Disinformation 2024 project by Sewanee DataLab fellows. Our project partner, Professor Ana Sofía Cardenal, is a political science professor at the Open University of Catalonia. Our partner requested an analysis of two data sets, brought by YouGov and Forthright (~700 participants), that track the participants' browsing history in a period that spanned 3-4 months. What our analysis targets is identifying the digital pathways that lead people to misinformation and to see how political ideology or partisanship can affect a person's internet behavior.   

To install and run this project:
1. Make sure you have git installed (Learn how to do that [here](https://github.com/git-guides/install-git)).  
2. Install R Studio (Learn how to do that [here](https://posit.co/download/rstudio-desktop/)).  
3. Open R Studio.  
4. Clone our GitHub repository!  
      1. Go to the Project tab in the upper right corner RStudio.  
      2. Click New Project -> Version Control -> Git  
      3. Enter the URL of our repository (https://github.com/sewaneedata/PATHS-to-disinformation-2024/tree/main) and specify the directory where you want to clone the repository.  
      4. Click Create Project!  
5. Install the following dependencies using the install.packages() function. Learn how to do that using [this link](https://support.posit.co/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages).
   1. "tidyverse"
   2. "ggalluvial"
   3. "readxl"
   4. "kableExtra"


Mainly what most of the scripts on this repository do is clean the dense data sets that we had and put them in a state from which observations can be analyzed. Our observations/results are represented with alluvial diagrams of how people get to disinformation sites, from which relationships between political affiliations and the consumption of disinformation can be inferred.

The data zip can be found in [this link](https://drive.google.com/drive/u/3/folders/17Mc1pOXn5D2gfnLgD6iWsDB3VuHIx9le) to the Google Drive in the data folder. After downloading it, unzip it and put the data folder in the project folder. If you need access to the Google Drive data, please contact datalab@sewanee.edu.

# Paths Script Order

Based on the data provided by our client, we’ve made several adjustments across multiple scripts. It’s important to note that, since we worked with two different web tracking processes, each dataset (YouGov and Forthright) has its own individual scripts that essentially perform the same functions. However, it’s advisable to start with the YouGov scripts, as they will create a more complete list of disinformation sources for later use.

**Run the following scripts in the following order:**

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

# Final Report
The `Report.Rmd` file generates our final report that was sent to our partner.

