#### Preamble ####
# Purpose: Cleans the raw plane data recorded by time periods
# Author: Yiming Tang
# Date: 21 March 2024
# Contact: ym.tang@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
#### Load Library ####
library(tidyverse)
library(janitor)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
library(knitr)
library(dplyr)

#### Import raw data  ####
raw_data1 <-
  read.csv(here::here("data/raw_data/Pre-2023 Apartment Building Evaluations.csv"))

raw_data2 <-
  read.csv(here::here("data/raw_data/Apartment Building Evaluations 2023 - current.csv"))

#### Clean data and export ####
cleaned_data1 <- raw_data1 |>
  select(YEAR_EVALUATED, YEAR_BUILT, PROPERTY_TYPE, CONFIRMED_UNITS, GRAFFITI ,SCORE) |>
  filter(YEAR_EVALUATED>=2020)

cleaned_data2 <- raw_data2 |>
  select(YEAR.EVALUATED, YEAR.BUILT, PROPERTY.TYPE, CONFIRMED.UNITS, GRAFFITI ,CURRENT.BUILDING.EVAL.SCORE) |>
  filter(YEAR.EVALUATED>=2020)

colnames(cleaned_data2) = colnames(cleaned_data1)

cleaned_data <- cleaned_data1 |>
  rbind(cleaned_data2) |>
  mutate( AGE = YEAR_EVALUATED-YEAR_BUILT)

cleaned_data <- na.omit(cleaned_data)

#### Save data ####
write_csv(cleaned_data, "data/analysis_data/cleaned_department_evaluation.csv")
