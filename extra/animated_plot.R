library(readr)
library(dplyr)


# Preparing data ----------------------------------------------------------

dataset <- read_delim('../data/preprocessed/DIB_dataset.tsv', delim='\t')

# https://www.kaggle.com/statchaitya/country-to-continent
country_to_continent <- read_delim('countryContinent.csv', delim=',')
colnames(country_to_continent)[1] <- 'locality_name'

# Fixing country names
country_to_continent %>%
  mutate(locality_name =
           case_when(
             locality_name == 'United States of America' ~ 'United States',
             locality_name == 'Venezuela (Bolivarian Republic of)' ~ 'Venezuela',
             locality_name == 'Bolivia (Plurinational State of)' ~ 'Bolivia',
             locality_name == 'Cabo Verde' ~ 'Cape Verde',
             locality_name == 'Czech Republic' ~ 'Czechia',
             locality_name == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
             locality_name == 'Korea (Republic of)' ~ 'South Korea',
             locality_name == 'Bahamas' ~ 'The Bahamas',
             locality_name == 'Lao People\'s Democratic Republic' ~ 'Laos',
             locality_name == 'Moldova (Republic of)' ~ 'Moldova',
             locality_name == 'Macedonia (the former Yugoslav Republic of)' ~ 'North Macedonia',
             locality_name == 'Myanmar' ~ 'Myanmar (Burma)',
             locality_name == 'Taiwan, Province of China' ~ 'Taiwan',
             locality_name == 'Tanzania, United Republic of' ~ 'Tanzania',
             locality_name == 'Viet Nam' ~ 'Vietnam',
             # locality_name == '' ~ 'Réunion',
             # locality_name == '' ~ 'Côte d\'Ivoire',
             TRUE ~ locality_name)
  ) -> country_to_continent

dataset <- left_join(dataset, country_to_continent, by='locality_name')

dataset %>%
  mutate(continent =
           case_when(
             locality_name == 'Réunion' ~ 'Africa',
             locality_name == 'Côte d\'Ivoire' ~ 'Africa',
             TRUE ~ continent)
  ) -> dataset
rm(country_to_continent)

# Preparing animation -----------------------------------------------------

library(ggplot2)
library(gganimate)
theme_set(theme_bw())

p <- ggplot(
  dataset,
  aes(x = workplaces, y=residential, size = acc_cases, colour = continent)
) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Variation in visit/stay at a workplace", y = "Variation in visit/Stay at a residence",
       color = 'Continent', size = 'Accumulated number of cases',
       title = "Impact of COVID-19 pandemic on mobility",
       caption = "Ribeiro-Dantas et al (2020) 'Dataset for country profile and mobility analysis in the assessment of COVID19\npandemic', Data in Brief. http://mribeirodantas.me") +
  theme(legend.position = "bottom", legend.box="vertical") +
  transition_states(date) +
  labs(subtitle = 'Every point is a country. Date: {closest_state}') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(p, fps = 7, nframes = 7*83, height=1000, width=1000, res = 70)

anim_save('GIF_by_country.gif', animation = last_animation(), path = '.')
