# Towards a Multilevel Approach to Election Forecasting

## Case study on the 2020 US Presidential Election

This repo authored by Nayan Saxena includes code and data for forcasting the popular vote outcome of the US 2020 presidential election. The 2020 US Presidential elections took place not only during a global pandemic but also after several major events like the "Black lives matter" movement. They also occured after a shocking 2016 US presidential outcome where several polling based models incorrectly predicted the outcome of the election--revealing several issues with statistical approaches to election forecasting. In this paper we attempt to forecast the 2020 US Presidential elections using multilevel-regression with post-stratification using survey data collected prior to the election. Our model predicts an expected 51% +-6% chance of a majority vote and 274 electoral college votes for Joe Biden designating a clear victory for the Democratic presidential nominee across both fronts.

## Inputs.

[Nationscape data:](https://www.voterstudygroup.org/publication/nationscape-data-set) Enter your email to submit a request for access, and it will be emailed to you in a folder.

[ACS data:](https://usa.ipums.org/usa-action/variables/group) . The variables used are included by default within the ACS 2019 sample.

## Important Scripts

These scripts will help prepare the survey and post-stratification data.

-   01-data_cleaning-survey.R

-   02-data_cleaning-post-strat1.R

## Packages

```{r}
library(statebins)
library(broom)
library(kableExtra)
library(here)
library(brms)
library(rstan)
library(tidybayes)
library(DeclareDesign)
library(arm)
library(rstanarm)
library(lme4)
library("glmmTMB")
library(Pmisc)
library(sjPlot)
library(modelsummary)
library(ggthemes)
library(fiftystater)
library(bayesplot)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(palmerpenguins)
library(usmap)
library(scales)
```
