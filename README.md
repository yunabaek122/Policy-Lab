# Policy-Lab
[Winter 2025] Policy Lab - IDinsight

## Project Title: How to Measure Dignity Globally in the Development Sector
## Client Organization: IDinsight

This project aims to improve the understanding of how experiences of dignity vary. 
Using a conceptual framework and survey data, this project explores the potential sources 
of variation, draw conclusions, and propose an instrument and analysis plan for 
a multi-country study of experiences of respect for dignity. 
Findings are expected to contribute to IDinsight’s Dignity Initiative.

## Overview
This repository contains analysis codes used to conduct analyses using the survey data 
from Arab Barometer (Wave 8). The analyses investigates the 
relationships between people's experiences and perceptions of respect and dignity and key 
predictive variables (e.g., institutional trust, political participation, and etc.) 
along with demographic factors (e.g., gender, education, etc.). Each step in the 
code is documented to facilitate transparency, reproducibility, and ease of adaptation 
for future analyses. While extensive exploratory analyses were performed, the code 
provided in the markdown files prioritizes the final analyses that led to the key findings. 

## Dataset
[Arab Barometer WIII](https://www.arabbarometer.org/surveys/arab-barometer-wave-viii/)

## Variables of interest
**Dependent variables**
Q550A and Q551A
Six pillars of dignity: 
1. Equality under the law
2. Absence of corruption
3. Basic necessities
4. Ability to freely choose political leaders in elections
5. Feeling safe from physical danger
6. Basic civil rights are guaranteed

**Independent variables**
- Six pillars of dignity
- Misogyny
- Discrimination
- Others/Foreign Aid
- Access to services
- Security
- Social engagement
- Political engagement
- Agency
- Representation
- SES


### Code Script
#### File name
Arab Wave 8 Statistical Analysis Code.Rmd

#### Purpose: 
* Conducts exploratory analyses and descriptive statistics for the Afrobarometer Round 9 dataset
* Explores key survey variables, examine their distributions, clean and recode the dataset, visualize relationships, and perform initial inferential statistical modeling, including logistic regressions with interaction terms

#### Setup:
* Change path.to.data to replicate codes
```
path.to.data <- "your_path"
arab_df <- read_csv(path.to.data)
```

#### Code Structure:
#### 1. Data Wrangling and Cleaning
* Recodes special response values (e.g., 98, 99, 97) to missing (NA) across all relevant variables.
* Transforms key survey questions (e.g., Q551A as dependent variables, demographic variables, socio-economic status variables) into binary dummy variables to facilitate logistic regression modeling.

#### 2. Balance tests
* Explores differences in means using two-proportion z-test in main pillar selection across thee demographics groups of interest: gender, education, and urban/rural.

#### 3. Data Visualization
* Bar plots of binary economic variables to check for distributions including missing data.
* Heatmap visualizations of Spearman correlation matrices among key socioeconomic variables.

#### 4. Logit Model
  - Two custom functions, `run_glm_models` and `run_int_models`, are used to explore and perform the following analyses:
    - `run_glm_models`: Runs logit models with and without demographic variables as control variables.
    - `run_int_models`: Runs a logit model with demographic variables as interaction terms.
