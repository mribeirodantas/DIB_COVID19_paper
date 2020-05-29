library(readr)
library(ggplot2)
library(dplyr)
library(corrr)

df <- read_delim(file = 'data/preprocessed/DIB_dataset.tsv', delim = '\t')

# What countries have more than 100 confirmed deaths by COVID-19?
df %>%
  filter(acc_deaths > 100) %>%
  group_by(locality_name) %>%
  select(locality_name, acc_cases) %>%
  slice(n()) %>%
  arrange(-acc_cases) %>% View

# What are the top 10 countries by lethality rate?
df %>%
  select(locality_name, lethality_rate_percent) %>%
  unique %>%
  arrange(-lethality_rate_percent) %>%
  slice(1:10) %>%
  View


# What's the pearson correlation among the localization categories in Google's
# Community Mobility Reports dataset?
df %>%
  select(retail_recreation, grocery_pharmacy, parks, transit_stations,
         workplaces, residential) %>%
  na.omit() %>%
  cor %>% View

# What about number of physicians per 1000 people, lethality and population
# density?
df %>%
  select(lethality_rate_percent,
         `health_personnel:_physicians_(per_1000_population)_2018`,
         population_density_2019) %>%
  na.omit() %>%
  cor %>% View