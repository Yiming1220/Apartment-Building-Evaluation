#### Preamble ####
# Purpose: Build model
# Author: Yiming Tang
# Date: 31 March 2024
# Contact: ym.tang@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
library(broom)

#### Read data ####
cleaned_data <-
  read.csv(here::here("data/analysis_data/cleaned_department_evaluation.csv"))

### Model data ####
date_score_year <- cleaned_data |>
  filter(AGE<=150) |>
  group_by( AGE) |>
  summarize(mean_score = mean(SCORE, na.rm = TRUE) , .groups = "drop") 

model <- lm(mean_score ~ AGE, data = date_score_year)


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)

# Create scatter plot for model
ggplot(date_score_year, aes(x = AGE, y = mean_score)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "AGE", y = "SCORE") 

###Model summary###
model_summary <- tidy(summary(model))
model_summary |>
  kable( caption = "Model Summary", align = "c" , digits = 2)

###Model Justification###
plot(model)
