#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA website
# Author: Nayan Saxena
# Data: 02 April 2022
# Contact: nayan.saxena@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


  
  
  library(haven)
  library(tidyverse)
  
  raw_poststrat_data <- 
    read_dta(here::here("inputs/data/usa_00002.dta.gz"))

  
  # The Stata format separates labels so reunite those
  raw_poststrat_data <- 
    labelled::to_factor(raw_poststrat_data)
  head(raw_poststrat_data)
  

  
  
  
  #> # A tibble: 6 × 28
  #>   year  sample serial cbserial  hhwt cluster region stateicp
  #>   <fct> <fct>   <dbl>    <dbl> <dbl>   <dbl> <fct>  <fct>   
  #> 1 2018  2018 …      2  2.02e12 392.  2.02e12 east … alabama 
  #> 2 2018  2018 …      7  2.02e12  94.1 2.02e12 east … alabama 
  #> 3 2018  2018 …     13  2.02e12  83.7 2.02e12 east … alabama 
  #> 4 2018  2018 …     18  2.02e12  57.5 2.02e12 east … alabama 
  #> 5 2018  2018 …     23  2.02e12 157.  2.02e12 east … alabama 
  #> 6 2018  2018 …     28  2.02e12 157.  2.02e12 east … alabama 
  #> # … with 20 more variables: strata <dbl>, gq <fct>,
  #> #   pernum <dbl>, perwt <dbl>, sex <fct>, age <fct>,
  #> #   marst <fct>, race <fct>, raced <fct>, hispan <fct>,
  #> #   hispand <fct>, bpl <fct>, bpld <fct>, citizen <fct>,
  #> #   educ <fct>, educd <fct>, empstat <fct>, empstatd <fct>,
  #> #   labforce <fct>, inctot <dbl>
  
  raw_poststrat_data$age <- as.numeric(raw_poststrat_data$age)
  
  poststrat_data <- 
    raw_poststrat_data |> 
    filter(inctot < 9999999) |> 
    filter(age >= 18) |> 
    filter(race != "two major races", race != "three or more major races") %>% 
    mutate(gender = sex) |> 
    mutate(
      age_group = case_when( # case_when works in order and exits when there's a match
        age <= 29 ~ 'age_18-29',
        age <= 44 ~ 'age_30-44',
        age <= 59 ~ 'age_45-59',
        age >= 60 ~ 'age_60_or_more',
        TRUE ~ 'Trouble'
      ),
      education_level = case_when(
        educd == "nursery school, preschool" ~ "High school or less",
        educd == "kindergarten" ~ "High school or less",
        educd == "grade 1" ~ "High school or less",
        educd == "grade 2" ~ "High school or less",
        educd == "grade 3" ~ "High school or less",
        educd == "grade 4" ~ "High school or less",
        educd == "grade 5" ~ "High school or less",
        educd == "grade 6" ~ "High school or less",
        educd == "grade 7" ~ "High school or less",
        educd == "grade 8" ~ "High school or less",
        educd == "grade 9" ~ "High school or less",
        educd == "grade 10" ~ "High school or less",
        educd == "grade 11" ~ "High school or less",
        educd == "12th grade, no diploma" ~ "High school or less",
        educd == "regular high school diploma" ~ "High school or less",
        educd == "ged or alternative credential" ~ "High school or less",
        educd == "some college, but less than 1 year" ~ "Some post secondary",
        educd == "1 or more years of college credit, no degree" ~ "Some post secondary",
        educd == "associate's degree, type not specified" ~ "Post secondary or higher",
        educd == "bachelor's degree" ~ "Post secondary or higher",
        educd == "master's degree" ~ "Graduate degree",
        educd == "professional degree beyond a bachelor's degree" ~ "Graduate degree",
        educd == "doctoral degree" ~ "Graduate degree",
        educd == "no schooling completed" ~ "High school or less",
        TRUE ~ 'Trouble'
      ),
      race = ifelse(race == "white", "white",
                     ifelse(race == "black/african american/negro", "black",
                            ifelse(race == "other race, nec", "other race, nec", "asian"))),
      hispanic = ifelse(hispan == "not hispanic", "not hispanic", "hispanic"),
      household_income = case_when( # case_when works in order and exits when there's a match
        hhincome <= 49999 ~ "Less than $50,000" ,
        hhincome <= 99999 ~ "$50,000 to $99,999"  ,
        hhincome <= 149999 ~  "$100,000 to $149,999" ,
        hhincome <= 199999 ~  "$150,000 to $199,999" ,
        hhincome <= 249999 ~  "$200,000 to $249,999" ,
        hhincome >= 250000 ~  "$250,000 and above" ,
        TRUE ~ 'Trouble'
      ),
      
    )
  
  # Just keep relevant variables
  poststrat_data <- 
    poststrat_data |> 
    select(gender,
           age_group,
           education_level,
           stateicp, race, hispanic,
           region,
           stateicp,
           sex, 
           age, 
           race, 
           hispan,
           marst, 
           bpl,
           citizen,
           educd,
           language,
           labforce,
           inctot,
           empstatd,
           vetstat)
  
  # Tidy the class
  poststrat_data <- 
    poststrat_data |> 
    mutate(across(c(gender, stateicp, education_level, age_group, race, hispanic,  household_income), as_factor))
  
  # Save data
  write_csv(poststrat_data, "outputs/paper/data/us_poststrat.csv")
  
  poststrat_data |> 
    head()
  #> # A tibble: 6 × 4
  #>   gender age_group      education_level     stateicp
  #>   <fct>  <fct>          <fct>               <fct>   
  #> 1 female age_18-29      Some post secondary alabama 
  #> 2 female age_60_or_more Some post secondary alabama 
  #> 3 male   age_45-59      Some post secondary alabama 
  #> 4 male   age_30-44      High school or less alabama 
  #> 5 female age_60_or_more High school or less alabama 
  #> 6 male   age_30-44      High school or less alabama
  # This dataset is on an individual level. So we’ll create counts of each sub-cell, and then proportions by state.
  
  poststrat_data_cells <- 
    poststrat_data |> 
    group_by(stateicp, gender, age_group, education_level,race, hispanic,  household_income) |> 
    count()
