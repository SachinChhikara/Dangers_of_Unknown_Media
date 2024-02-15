#### Preamble ####
# Purpose: Simulates the dataset regarding the Denmark primary schools and their response type regarding Muslim and Danish admission to the school
# Author: Fares Alkorani
# Date: 11 February 2024
# Contact: 156137245+faralk@users.noreply.github.com
# License: MIT
# Pre-requisites: -


#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Simulate data ####
set.seed(123)

simulated_school_data <-
  # A tibble including six attributes which pertain to
  # 1) whether the school is based in a big city (1) or not (0)
  # 2) whether the school is public (1) or private (0)
  # 3) the region of the school
  # 4) whether the school has a number of students above the median number of students in all primary schools in Denmark (1) or not (0)
  # 5) whether the school has above the median number of grades in all primary Denmark schools (1) or not (0)
  # 6) whether the school has above the median number of non-native Danish students (1) or not (0) for the sample
    tibble(
      bigcity = sample(x=0:1, size = 1000, replace=TRUE),
      is_public = sample(x=0:1, size = 1000, replace=TRUE),
      region = sample(c("Hovedstaden", "Midtjylland", "Nordjylland", "Sjælland", "Syddanmark"), 1000, replace=TRUE),
      above_median_student = sample(x=0:1, size = 1000, replace=TRUE),
      above_median_grades = sample(x=0:1, size = 1000, replace=TRUE),
      above_median_non_danish = sample(x=0:1, size = 1000, replace=TRUE)
    )

simulated_response_data <-
  # A tibble with four attributes which pertain to
  # 1) whether the email is from 'Mohamed' (1) or not (0)
  # 2) whether the student was rejected admission into the school (1) or not (0)
  # 3) whether it was unclear if the student is offered admission into the school (1) or not (0)
  # 4) whether the student was offered admission into the school (1) or not (0)
  tibble(
    email_from_mohamed = sample(x=0:1, size=1000, replace=TRUE),
    rejection = sample(x=0:1, size=1000, replace=TRUE),
    unclear = ifelse(rejection == 0, sample(x=0:1, size=1000, replace=TRUE), 0),
    acceptance = ifelse(unclear == 0 & rejection == 0, 1, 0)
  )

# Merge the two for the final dataset

simulated_data <-
  cbind(simulated_response_data, simulated_school_data)

simulated_data