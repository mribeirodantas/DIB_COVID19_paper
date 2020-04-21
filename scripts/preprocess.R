library(readr)
library(dplyr)
library(tidyr)
library(tidyselect)
library(lubridate)


# Reading raw data --------------------------------------------------------

# Google Mobility Report (GMR) dataset

raw_dataset <- read_csv(file = 'data/raw/Global_Mobility_Report.csv',
                        col_types = paste(c(rep('c', 4),
                                                'D',
                                                rep('d', 6)),
                                              collapse=''))

colnames(raw_dataset) <- c('country_code', 'country_name', 'region_name',
                           'county_name', 'date', 'retail_recreation',
                           'grocery_pharmacy', 'parks', 'transit_stations',
                           'workplaces', 'residential')

# COVID19 dataset from ECDC

covid <- read_delim(file = 'data/raw/COVID19_worldwide_raw.csv', na = '',
                    col_types = cols('c', 'i', 'i', 'i', 'i', 'i', 'c', 'c',
                                     'c', 'i'),
                    delim = ',')

colnames(covid) <- c('date', 'day', 'month', 'year', 'new_cases', 'new_deaths',
                     'country_name', 'country_id', 'territory_id',
                     'pop_data_2018')

# Country details from UN Data
# There were ~ in 0 or 0.0 numbers in the raw data file. I had to manually
# replace ~0 and ~0.0 by 0 and 0.0, otherwise R wouldn't understand this is
# a number.
country_details <- read_delim(file = 'data/raw/UN_dataset.tsv', delim = '\t',
                            col_types = paste(c('c',
                                                rep('d', 173),
                                                'c'),
                                              collapse=''))

# Preprocessing step ---------------------------------------------------------

####
#
# Working on GMR dataset
#
####

# Create a long table from the wide original version
preprocessed_dataset <- pivot_longer(raw_dataset, cols=6:11, names_to = 'plot_name',
                         values_to = 'variation')
rm(raw_dataset)

####
#
# Working on COVID19 dataset
#
####

covid %>%
  # These columns are useless
  select(-c('day', 'month', 'year', 'country_id', 'territory_id')) %>%
  # Convert date to Date
  mutate(date, date = dmy(covid$date)) %>%
  # Replace _ by space in country names
  mutate(country_name,
         country_name = gsub('_',
                        ' ',
                        country_name)) -> covid

# Some countries have different names between the datasets
covid %>%
  mutate(country_name, country_name = case_when(
    country_name == 'United States of America' ~ 'United States',
    country_name == 'United Republic of Tanzania' ~ 'Tanzania',
    country_name == 'Guinea Bissau' ~ 'Guinea-Bissau',
    country_name == 'Bahamas' ~ 'The Bahamas',
    country_name == 'Myanmar' ~ 'Myanmar (Burma)',
    country_name == 'Cote dIvoire' ~ 'Côte d\'Ivoire',
    TRUE ~ country_name)
    ) -> covid

# Some rows are missing like March 2nd and 3rd for Brazil. Apparently,
# this happens when there is no case/death. Complete with these rows.
covid %>%
  complete(date = seq.Date(min(date), max(date), by='day'), country_name) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases)) %>%
  mutate(new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)) %>%
  group_by(country_name) %>%
  fill(pop_data_2018, .direction = 'updown') -> covid

####
#
# Merge World COVID19 and GMR datasets
#
####

preprocessed_dataset <- left_join(preprocessed_dataset, covid,
                                  by = c('date', 'country_name'))
rm(covid)

####
#
# Working on dataset after 1st merge
#
####

# Create column with accumulate cases/death
preprocessed_dataset %>%
  group_by(country_name, plot_name, region_name) %>%
  arrange(date) %>%
  dplyr::mutate(acc_cases = cumsum(new_cases)) %>%
  dplyr::mutate(acc_deaths = cumsum(new_deaths)) %>%
  ungroup -> preprocessed_dataset

# Set new_cases and new_deaths to NA for regions, because our COVID19 dataset
# only has data for countries.
preprocessed_dataset %>%
  mutate(new_cases = ifelse(!is.na(region_name),
                            NA,
                            new_cases)) %>%
  mutate(new_deaths = ifelse(!is.na(region_name),
                             NA,
                             new_deaths)) -> preprocessed_dataset

####
#
# Working on country details dataset (UN)
#
####

# Before merging to get more info about the countries, we must make sure all
# country names are the same.
# unique(preprocessed_dataset$country_name)[which(
#      unique(preprocessed_dataset$country_name) %in%
#        unique(country_details$country) == FALSE
#    )
#  ]

country_details %>%
  mutate(region_name = case_when(
    region_name == 'Bolivia (Plurinational State of)' ~ 'Bolivia',
    region_name == 'Bahamas'  ~ 'The Bahamas',
    region_name == 'Republic of Korea'  ~ 'South Korea',
    region_name == 'Cabo Verde'  ~ 'Cape Verde',
    region_name == 'China, Hong Kong SAR'  ~ 'Hong Kong',
    region_name == 'Republic of Moldova'  ~ 'Moldova',
    region_name == 'Lao People\'s Democratic Republic'  ~ 'Laos',
    region_name == 'Myanmar'  ~ 'Myanmar (Burma)',
    region_name == 'United Rep. of Tanzania' ~ 'Tanzania',
    region_name == 'United States of America'  ~ 'United States',
    region_name == 'Venezuela (Boliv. Rep. of)'  ~ 'Venezuela',
    region_name == 'Viet Nam'  ~ 'Vietnam',
    TRUE ~ region_name)
  ) -> country_details

####
#
# Merge country details and preprocessed_dataset
#
####

preprocessed_dataset <- left_join(preprocessed_dataset, country_details,
                                  by = c('country_name' = 'region_name'))

rm(country_details)

# Remove rows that are related to regions
preprocessed_dataset %>%
  filter(is.na(region_name)) %>%
  select(-c('region_name', 'county_name')) -> preprocessed_dataset

# There are two columns for population (2018, World Bank and 2019, UN).
# Keep the most updated one.
preprocessed_dataset %>%
  select(-c(pop_data_2018)) -> preprocessed_dataset

colnames(preprocessed_dataset)[10:182] %>%
  # Make them all lowercase
  tolower %>%
  # Replace space by underscore
  gsub(' ', '_', .) %>%
  gsub('-', '_', .) %>%
  gsub('_+', '_', .) %>%
  gsub(',', '', .) -> colnames(preprocessed_dataset)[10:182]

# Create columns for lethality rate
preprocessed_dataset %>%
  group_by(country_name) %>%
  arrange(date) %>%
  mutate(lethality_rate_percent = (acc_deaths[n()]/acc_cases[n()])*100) %>%
  ungroup() -> preprocessed_dataset

# Bring plot_names from row to column
preprocessed_dataset %>%
  pivot_wider(names_from = plot_name, values_from = variation) -> preprocessed_dataset

# Add epidemiological week to column
preprocessed_dataset %>%
  group_by(country_name) %>%
  mutate(first_case_date = min(date[acc_cases > 0])) %>%
  mutate(n_days_since_1st_case =
           if_else(acc_cases > 0,
                   as.numeric(date - min(date[acc_cases > 0])+1),
                   0)) %>%
  ungroup() -> preprocessed_dataset

# Set manually first case for countries whose first case happened before Feb 15
# Wikipedia contributors, "2019–20 coronavirus pandemic", Wikipedia, The Free
# Encyclopedia,
# https://en.wikipedia.org/w/index.php?title=2019%E2%80%9320_coronavirus_pandemic&oldid=952187370
# (accessed April 21, 2020).
preprocessed_dataset %>%
  mutate(first_case_date = case_when(
    # country_name == 'China' ~ dmy('01-12-2019'),
    country_name == 'Thailand' ~ dmy('13-01-2020'),
    country_name == 'Japan' ~ dmy('16-01-2020'),
    country_name == 'South Korea' ~ dmy('20-01-2020'),
    country_name == 'United States' ~ dmy('20-01-2020'),
    country_name == 'Taiwan' ~ dmy('21-01-2020'),
    country_name == 'Hong Kong' ~ dmy('22-01-2020'),
    country_name == 'Singapore' ~ dmy('23-01-2020'),
    country_name == 'Vietnam' ~ dmy('23-01-2020'),
    country_name == 'France' ~ dmy('24-01-2020'),
    country_name == 'Nepal' ~ dmy('24-01-2020'),
    country_name == 'Australia' ~ dmy('25-01-2020'),
    country_name == 'Canada' ~ dmy('25-01-2020'),
    country_name == 'Malaysia' ~ dmy('25-01-2020'),
    country_name == 'Cambodia' ~ dmy('27-01-2020'),
    country_name == 'Germany' ~ dmy('27-01-2020'),
    country_name == 'Sri Lanka' ~ dmy('27-01-2020'),
    country_name == 'Finalnd' ~ dmy('29-01-2020'),
    country_name == 'United Arab Emirates' ~ dmy('29-01-2020'),
    country_name == 'India' ~ dmy('30-01-2020'),
    country_name == 'Italy' ~ dmy('30-01-2020'),
    country_name == 'Philippines' ~ dmy('30-01-2020'),
    country_name == 'Spain' ~ dmy('31-01-2020'),
    country_name == 'Sweden' ~ dmy('31-01-2020'),
    country_name == 'United Kingdom' ~ dmy('31-01-2020'),
    country_name == 'Belgium' ~ dmy('04-02-2020'),
    country_name == 'Egypt' ~ dmy('14-01-2020'),
    TRUE ~ first_case_date
    )
  ) -> preprocessed_dataset

# Saving final preprocessed dataset ---------------------------------------

# Save full dataset
write_tsv(x = preprocessed_dataset, path = 'data/preprocessed/DIB_dataset.tsv', quote_escape = FALSE)
