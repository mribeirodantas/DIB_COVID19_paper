library(readr)
library(ggplot2)
library(dplyr)

df <- read_delim(file = 'data/preprocessed/DIB_dataset.tsv', delim = '\t')

# What countries have more than 100 confirmed deaths by COVID-19?
df %>%
  filter(acc_deaths > 100) %>%
  group_by(country_name) %>%
  select(country_name, acc_cases) %>%
  slice(n()) %>%
  arrange(-acc_cases) %>% View

# What are the top 10 countries by lethality rate?
df %>%
  select(country_name, lethality_rate_percent) %>%
  unique %>%
  arrange(-lethality_rate_percent) %>%
  slice(1:10) %>%
  View
