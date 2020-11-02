#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from "https://usa.ipums.org/usa/index.shtml"

# Author: Eung Kyu Kim, Dong Kyu Kim, Jiwon Chai

# Data: November 2nd, 2020

# Contact: wek.kim@mail.utoronto.ca, dongk.kim@mail.utoronto.ca, jiwon.chai@mail.utoronto.ca 

# License:  MIT

# Pre-requisites: 
   
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!



#### Workspace setup ####
 
#install.packages("labelled")
library(haven)
library(tidyverse)

# Read in the raw data.
setwd("C:/Users/willi/Desktop/UT 20-21/STA304/Problem Set/PS3/working_directory/ps3_directory")
raw_data_post <- read_dta("usa_00001.dta")


# Add the labels
raw_data_post <- labelled::to_factor(raw_data_post)


#selecting which variables to include in the data frame
census_data_post <- 
  raw_data_post %>% 
  select( 
         age,
         stateicp
         ) 
         
#counting and grouping using some variables

census_data_post <- 
  census_data_post %>%
  count(age,stateicp) %>%
  group_by(age)  



census_data_post <- 
  census_data_post %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>%
  
  #filtering one of the states called "district of columbia because it is not really a state"
  
  filter(stateicp != "district of columbia") %>%
  
  #"filtering all the ages that cannot vot"
  
  filter(age != "1") %>%
  filter(age != "2") %>%
  filter(age != "3") %>%
  filter(age != "4") %>%
  filter(age != "5") %>%
  filter(age != "6") %>%
  filter(age != "7") %>%
  filter(age != "8") %>%
  filter(age != "9") %>%
  filter(age != "10") %>%
  filter(age != "11") %>%
  filter(age != "12") %>%
  filter(age != "13") %>%
  filter(age != "14") %>%
  filter(age != "15") %>%
  filter(age != "16") %>%
  filter(age != "17") 

#Making the "age" variable an integer  

  

#changing the name of the state code and name to match the two different data sets
  
census_data_post$stateicp <- as.character(census_data_post$stateicp)
census_data_post$stateicp[census_data_post$stateicp == "alabama"] <- "AL" 
census_data_post$stateicp[census_data_post$stateicp == "alaska"] <- "AK" 
census_data_post$stateicp[census_data_post$stateicp == "arizona"] <- "AZ" 
census_data_post$stateicp[census_data_post$stateicp == "arkansas"] <- "AR" 
census_data_post$stateicp[census_data_post$stateicp == "california"] <- "CA" 
census_data_post$stateicp[census_data_post$stateicp == "colorado"] <- "CO" 
census_data_post$stateicp[census_data_post$stateicp == "connecticut"] <- "CT" 
census_data_post$stateicp[census_data_post$stateicp == "delaware"] <- "DE" 
census_data_post$stateicp[census_data_post$stateicp == "florida"] <- "FL" 
census_data_post$stateicp[census_data_post$stateicp == "georgia"] <- "GA" 
census_data_post$stateicp[census_data_post$stateicp == "hawaii"] <- "HI" 
census_data_post$stateicp[census_data_post$stateicp == "idaho"] <- "ID" 
census_data_post$stateicp[census_data_post$stateicp == "illinois"] <- "IL" 
census_data_post$stateicp[census_data_post$stateicp == "indiana"] <- "IN" 
census_data_post$stateicp[census_data_post$stateicp == "iowa"] <- "IA" 
census_data_post$stateicp[census_data_post$stateicp == "kansas"] <- "KS" 
census_data_post$stateicp[census_data_post$stateicp == "kentucky"] <- "KY" 
census_data_post$stateicp[census_data_post$stateicp == "louisiana"] <- "LA" 
census_data_post$stateicp[census_data_post$stateicp == "maine"] <- "ME" 
census_data_post$stateicp[census_data_post$stateicp == "maryland"] <- "MD" 
census_data_post$stateicp[census_data_post$stateicp == "massachusetts"] <- "MA" 
census_data_post$stateicp[census_data_post$stateicp == "michigan"] <- "MI" 
census_data_post$stateicp[census_data_post$stateicp == "minnesota"] <- "MN" 
census_data_post$stateicp[census_data_post$stateicp == "mississippi"] <- "MS" 
census_data_post$stateicp[census_data_post$stateicp == "missouri"] <- "MO" 
census_data_post$stateicp[census_data_post$stateicp == "montana"] <- "MT" 
census_data_post$stateicp[census_data_post$stateicp == "nebraska"] <- "NE" 
census_data_post$stateicp[census_data_post$stateicp == "nevada"] <- "NV" 
census_data_post$stateicp[census_data_post$stateicp == "new hampshire"] <- "NH" 
census_data_post$stateicp[census_data_post$stateicp == "new jersey"] <- "NJ" 
census_data_post$stateicp[census_data_post$stateicp == "new mexico"] <- "NM" 
census_data_post$stateicp[census_data_post$stateicp == "new york"] <- "NY" 
census_data_post$stateicp[census_data_post$stateicp == "north carolina"] <- "NC" 
census_data_post$stateicp[census_data_post$stateicp == "north dakota"] <- "ND" 
census_data_post$stateicp[census_data_post$stateicp == "ohio"] <- "OH"
census_data_post$stateicp[census_data_post$stateicp == "oklahoma"] <- "OK" 
census_data_post$stateicp[census_data_post$stateicp == "oregon"] <- "OR" 
census_data_post$stateicp[census_data_post$stateicp == "pennsylvania"] <- "PA" 
census_data_post$stateicp[census_data_post$stateicp == "rhode island"] <- "RI" 
census_data_post$stateicp[census_data_post$stateicp == "south carolina"] <- "SC"
census_data_post$stateicp[census_data_post$stateicp == "south dakota"] <- "SD" 
census_data_post$stateicp[census_data_post$stateicp == "tennessee"] <- "TN"
census_data_post$stateicp[census_data_post$stateicp == "texas"] <- "TX"
census_data_post$stateicp[census_data_post$stateicp == "utah"] <- "UT" 
census_data_post$stateicp[census_data_post$stateicp == "vermont"] <- "VT" 
census_data_post$stateicp[census_data_post$stateicp == "virginia"] <- "VA" 
census_data_post$stateicp[census_data_post$stateicp == "washington"] <- "WA" 
census_data_post$stateicp[census_data_post$stateicp == "west virginia"] <- "WV"
census_data_post$stateicp[census_data_post$stateicp == "wisconsin"] <- "WI" 
census_data_post$stateicp[census_data_post$stateicp == "wyoming"] <- "WY" 

#renaming the state variable to match each others
census_data_post <- census_data_post %>%
  rename(state = stateicp)


census_data_post$age <- as.integer(census_data_post$age)
#Saving the data frame called "census_data_post" as a csv file as "census_data_post.csv"


write_csv(census_data_post, "census_data_post.csv")
write_csv(census_data_post, "census_data_post2.csv")


         