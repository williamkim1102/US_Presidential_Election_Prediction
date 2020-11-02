#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from "https://www.voterstudygroup.org/publication/nationscape-data-set"

# Author: Eung Kyu Kim, Dong Kyu Kim, Jiwon Chai

# Data: November 2nd, 2020

# Contact: wek.kim@mail.utoronto.ca, dongk.kim@mail.utoronto.ca, jiwon.chai@mail.utoronto.ca 

# License: MIT


# Pre-requisites: 
# - Need to have downloaded the data from "https://www.voterstudygroup.org/publication/nationscape-data-set" 
# and save the folder that you're interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
 #install.packages("labelled")
library(haven)
library(tidyverse)


setwd("C:/Users/willi/Desktop/UT 20-21/STA304/Problem Set/PS3/working_directory/ps3_directory")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625_new.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

#selecting which variables to keep in the reduced data set
#certain variables were chosen that might be interesting to analyze it with
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_2020,
         vote_intention,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#creating a variable called "vote_trump" and it gives a value of 1 if they vote for trump and 0 if not voting for trump
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020 == "Donald Trump", 1, 0)) %>%
  mutate(vote_biden =
           ifelse(vote_2020 == "Joe Biden", 1,0))


#filtering "not registered" in the registration variable and "No, I am not eligible to vote" because in the upcoming election, they will
#not be able to vote, so their opinions and data need to be taken out

reduced_data <- reduced_data %>%
  filter(registration != "Not registered") %>%
  filter(vote_intention != "No, I am not eligible to vote") %>%
  arrange(age) 


# Saving the survey/sample data as a csv file 

write_csv(reduced_data, "survey_data.csv")




