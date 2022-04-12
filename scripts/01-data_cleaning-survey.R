#### Preamble ####
# Purpose: Data preparation of survey data downloaded from Voter Study Group
# Author: Nayan Saxena
# Data: 2 April 2022
# Contact: nayan.saxena@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Voter Group Website after requesting access
# and save the folder that you're interested in to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_nationscape_data <- read_dta("inputs/data/ns20201001/ns20201001.dta")
# Add the labels


# The Stata format separates labels so reunite those
raw_nationscape_data <- 
  labelled::to_factor(raw_nationscape_data)

# Just keep relevant variables
nationscape_data <- 
  raw_nationscape_data |> 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
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
         age,
         language)

# For simplicity, remove anyone undecided or planning to vote for someone other than Biden/Trump and make vote a binary variable: 1 for Biden, 0 for Trump.
nationscape_data <- 
  nationscape_data |> 
  filter(vote_2020 == "Joe Biden" | vote_2020 == "Donald Trump") |> 
  mutate(vote_biden = if_else(vote_2020 == "Joe Biden", 1, 0)) |> 
  select(-vote_2020)

# Create the dependent variables by grouping the existing variables
nationscape_data <- 
  nationscape_data |> 
  filter(!is.na(household_income)) |> 
  mutate(
    age_group = case_when( # case_when works in order and exits when there's a match
      age <= 29 ~ 'age_18-29',
      age <= 44 ~ 'age_30-44',
      age <= 59 ~ 'age_45-59',
      age >= 60 ~ 'age_60_or_more',
      TRUE ~ 'Trouble'
    ),
    gender = case_when(
      gender == "Female" ~ 'female',
      gender == "Male" ~ 'male',
      TRUE ~ 'Trouble'
    ),
    race = case_when(
      race_ethnicity == "White" ~ "white",
      race_ethnicity == "Black, or African American" ~ "black",
      race_ethnicity == "American Indian or Alaska Native" ~ "other race, nec",
      race_ethnicity == "Asian (Chinese)" ~ "asian",
      race_ethnicity == "Asian (Japanese)" ~ "asian",
      race_ethnicity == "Asian (Asian Indian)" ~ "asian",
      race_ethnicity == "Asian (Filipino)" ~ "asian",
      race_ethnicity == "Asian (Korean)" ~ "asian",
      race_ethnicity == "Asian (Vietnamese)" ~ "asian",
      race_ethnicity == "Asian (Other)" ~ "asian",
      race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "asian",
      race_ethnicity == "Pacific Islander (Guamanian)" ~ "asian",
      race_ethnicity == "Pacific Islander (Samoan)" ~ "asian",
      race_ethnicity == "Pacific Islander (Other)" ~ "asian",
      race_ethnicity == "Some other race" ~ "other race, nec"
    ),
    hispanic = ifelse(hispanic == "Not Hispanic", "not hispanic", "hispanic"),
    education_level = case_when(
      education == "3rd Grade or less" ~ "High school or less",
      education == "Middle School - Grades 4 - 8" ~ "High school or less",
      education == "Completed some high school" ~ "High school or less",
      education == "High school graduate" ~ "High school or less",
      education == "Other post high school vocational training" ~ "Some post secondary",
      education == "Completed some college, but no degree" ~ "Some post secondary",
      education == "Associate Degree" ~ "Post secondary or higher",
      education == "College Degree (such as B.A., B.S.)" ~ "Post secondary or higher",
      education == "Completed some graduate, but no degree" ~ "Post secondary or higher",
      education == "Masters degree" ~ "Graduate degree",
      education == "Doctorate degree" ~ "Graduate degree",
      TRUE ~ 'Trouble'
    )
  ) |> 
  select(-education, -age)

nationscape_data$household_income<- fct_collapse(nationscape_data$household_income,
                                                 "Less than $50,000" = levels(nationscape_data$household_income)[1:8],
                                                 "$50,000 to $99,999" = levels(nationscape_data$household_income)[9:18],
                                                 "$100,000 to $149,999"= levels(nationscape_data$household_income)[19:20],
                                                 "$150,000 to $199,999"= levels(nationscape_data$household_income)[21:22],
                                                 "$200,000 to $249,999" = levels(nationscape_data$household_income)[23],
                                                 "$250,000 and above" = levels(nationscape_data$household_income)[24])


tests <- 
  nationscape_data |> 
  mutate(test = stringr::str_detect(age_group, 'Trouble'),
         test = if_else(test == TRUE, TRUE, 
                        stringr::str_detect(education_level, 'Trouble')),
         test = if_else(test == TRUE, TRUE, 
                        stringr::str_detect(gender, 'Trouble'))
  ) |> 
  filter(test == TRUE)

if(nrow(tests) != 0) {
  print("Check nationscape_data")
} else {
  rm(tests)
}

nationscape_data |> 
  head()
#> # A tibble: 6 × 5
#>   gender state vote_biden age_group      education_level    
#>   <chr>  <chr>      <dbl> <chr>          <chr>              
#> 1 female WI             0 age_45-59      Post secondary or …
#> 2 female VA             0 age_45-59      Post secondary or …
#> 3 female TX             0 age_60_or_more High school or less
#> 4 female WA             0 age_45-59      High school or less
#> 5 female MA             1 age_18-29      Some post secondary
#> 6 female TX             1 age_30-44      Some post secondary
#> 
#> 


# Format state names so the whole state name is written out, to match IPUMS data
states_names_and_abbrevs <- 
  tibble(stateicp = state.name, state = state.abb)

nationscape_data <-
  nationscape_data |>
  left_join(states_names_and_abbrevs)
#> Joining, by = "state"

rm(states_names_and_abbrevs)

# Make lowercase to match IPUMS data
nationscape_data <- 
  nationscape_data |> 
  mutate(stateicp = tolower(stateicp))

# Replace NAs with DC
nationscape_data$stateicp <- 
  replace_na(nationscape_data$stateicp, "district of columbia")

# Tidy the class
nationscape_data <- 
  nationscape_data |> 
  mutate(across(c(gender, stateicp, education_level, age_group, race, hispanic, household_income), as_factor))

# Save data
write_csv(nationscape_data, "outputs/paper/data/polling_data.csv")

nationscape_data |> 
  head()
#> # A tibble: 6 × 6