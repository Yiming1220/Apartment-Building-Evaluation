#### Preamble ####
# Purpose: EDA Data exploration
# Author: Yiming Tang
# Date: 31 March 2024
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
library(here)
library(RColorBrewer)
library(kableExtra)
library(patchwork)
library(readr)
library(palmerpenguins)

#### Read data ####
cleaned_data <-
  read.csv(here::here("data/analysis_data/cleaned_department_evaluation.csv"))

#### Creating Graph for comparison property types  against evaluation years ####
ggplot(data=cleaned_data, aes(x =as.factor(YEAR_EVALUATED), fill = PROPERTY_TYPE)) + 
  geom_bar(width =.8) +
  theme_classic() + 
  labs(
    x = "Year Evaluated",
    y = "# of Samples ",
    fill = "Property Type",
    caption = "2021 GSS"
  ) +
  scale_fill_brewer(palette="Spectral") + 
  theme(text = element_text(size=6))

score_type_2019 <- cleaned_data |>
  filter(YEAR_EVALUATED==2019)|>
  ggplot(aes(x=PROPERTY_TYPE , y = SCORE, fill = PROPERTY_TYPE)) +geom_boxplot()+ scale_fill_discrete(name="PropertyType") +
  theme_classic()+labs( x = "Property Type", y = "Score" , title = "Scores for Different Property Types(2019)")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=6))
  
score_type_2020 <- cleaned_data |>
  filter(YEAR_EVALUATED==2020)|>
  ggplot(aes(x=PROPERTY_TYPE , y = SCORE, fill = PROPERTY_TYPE)) +geom_boxplot()+ scale_fill_discrete(name="PropertyType") +
  theme_classic()+labs( x = "Property Type", y = "Score" , title = "Scores for Different Property Types(2020)")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=6))

score_type_2021 <- cleaned_data |>
  filter(YEAR_EVALUATED==2021)|>
  ggplot(aes(x=PROPERTY_TYPE , y = SCORE, fill = PROPERTY_TYPE)) +geom_boxplot()+ scale_fill_discrete(name="PropertyType") +
  theme_classic()+labs( x = "Property Type", y = "Score" , title = "Scores for Different Property Types(2021)")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=6))

score_type_2023 <- cleaned_data |>
  filter(YEAR_EVALUATED==2023)|>
  ggplot(aes(x=PROPERTY_TYPE , y = SCORE, fill = PROPERTY_TYPE)) +geom_boxplot()+ scale_fill_discrete(name="PropertyType") +
  theme_classic()+labs( x = "Property Type", y = "Score" , title = "Scores for Different Property Types(2023)")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=6))


#### Using Patchwork Package to Combine four Graphs ####
(score_type_2019) + (score_type_2020) + (score_type_2021) + (score_type_2023) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 2)+
  plot_layout(guides = "collect")

### Mean Score for Different Property Types in each Year Evaluated ###
cleaned_data |>
  group_by(PROPERTY_TYPE, YEAR_EVALUATED) |>
  summarize(mean_score = mean(SCORE, na.rm = TRUE) , .groups = "drop") |>
  pivot_wider(names_from = PROPERTY_TYPE, values_from = mean_score) |> 
  mutate(across(everything(), ~ round(., 2))) |>
  kable(booktabs = TRUE) |>
  kable_styling(font_size = 12 )

### Mean Score for Different Property Types against Graffiti Score in each Year  ###
cleaned_data |>
  mutate( GRAFFITI= as.integer(GRAFFITI)) |>
  group_by(PROPERTY_TYPE, YEAR_EVALUATED , GRAFFITI) |>
  summarize(mean_score = mean(SCORE, na.rm = TRUE) , .groups = "drop") |>
  pivot_wider(names_from = GRAFFITI, values_from = mean_score) |> 
  mutate(across(-c( PROPERTY_TYPE , YEAR_EVALUATED), ~ round(., 2))) |>
  kable(booktabs = TRUE) |>
  kable_styling(font_size = 12 )

###Mean Score Grouped by Age for Different Property Types###
cleaned_data |>
  group_by(PROPERTY_TYPE, AGE) |>
  summarize(mean_score = mean(SCORE, na.rm = TRUE) , .groups = "drop") |>
  ggplot(aes(x=AGE , y=mean_score)) + geom_point() +facet_wrap(~PROPERTY_TYPE)