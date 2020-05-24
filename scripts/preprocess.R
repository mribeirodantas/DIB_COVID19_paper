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

colnames(raw_dataset) <- c('locality_code', 'locality_name', 'region_name',
                           'county_name', 'date', 'retail_recreation',
                           'grocery_pharmacy', 'parks', 'transit_stations',
                           'workplaces', 'residential')

# COVID19 dataset from ECDC

covid <- read_delim(file = 'data/raw/COVID19_worldwide_raw.csv', na = '',
                    col_types = cols('c', 'i', 'i', 'i', 'i', 'i', 'c', 'c',
                                     'c', 'i', 'c'),
                    delim = ',')

colnames(covid) <- c('date', 'day', 'month', 'year', 'new_cases', 'new_deaths',
                     'locality_name', 'country_id', 'territory_id',
                     'pop_data_2018', 'continent')

# COVID-19 data for Réunion and Hong Kong  that are missing in the ECDC dataset
# Source: John Hopkins University https://github.com/CSSEGISandData/COVID-19
covid_hk_re <- read_delim(file = 'data/raw/hk-reunion-covid-19.csv',
                          col_types = cols('c', 'c', 'd', 'd'),
                          delim = ',')

covid_hk_re %>%
  mutate(locality_name = ifelse(locality_name == 'Reunion',
                                'Réunion',
                                locality_name)) -> covid_hk_re

# Country details from UN Data
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
  select(-c('day', 'month', 'year', 'country_id', 'territory_id',
            'pop_data_2018', 'continent')) %>%
  # Convert date to Date
  mutate(date, date = dmy(covid$date)) %>%
  # Replace _ by space in country names
  mutate(locality_name,
         locality_name = gsub('_',
                        ' ',
                        locality_name)) -> covid

covid <- rbind(covid, covid_hk_re)

rm(covid_hk_re)

# Some countries have different names between the datasets
covid %>%
  mutate(locality_name, locality_name = case_when(
    locality_name == 'United States of America' ~ 'United States',
    locality_name == 'United Republic of Tanzania' ~ 'Tanzania',
    locality_name == 'Guinea Bissau' ~ 'Guinea-Bissau',
    locality_name == 'Bahamas' ~ 'The Bahamas',
    locality_name == 'Myanmar' ~ 'Myanmar (Burma)',
    locality_name == 'Cote dIvoire' ~ 'Côte d\'Ivoire',
    TRUE ~ locality_name)
    ) -> covid

# Some rows are missing like March 2nd and 3rd for Brazil. Apparently,
# this happens when there is no case/death. Complete with these rows.
covid %>%
  complete(date = seq.Date(min(date), max(date), by='day'), locality_name) %>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases)) %>%
  mutate(new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)) -> covid

####
#
# Merge World COVID19 and GMR datasets
#
####

preprocessed_dataset <- left_join(preprocessed_dataset, covid,
                                  by = c('date', 'locality_name'))
rm(covid)

####
#
# Working on dataset after 1st merge
#
####

# Create column with accumulate cases/death
preprocessed_dataset %>%
  group_by(locality_name, plot_name, region_name) %>%
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
#unique(preprocessed_dataset$locality_name)[which(
#     unique(preprocessed_dataset$locality_name) %in%
#       unique(country_details$region_name) == FALSE
#   )
# ]

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

# In some datastes US appears as United States, and in others as United States
# of America. The naming was fixed earlier, but we have two rows for US. Fix.
# ids <- which(country_details$region_name == 'United States')

# Some variables are there for US, others are there only for United States.
# Merge.
country_details[88,][,17:42] <- country_details[217,][,17:42]
country_details[88,][,45:51] <- country_details[217,][,45:51]
country_details[88,][,53:55] <- country_details[217,][,53:55]
country_details[88,][,63:72] <- country_details[217,][,63:72]
country_details[88,][,77:80] <- country_details[217,][,77:80]
country_details[88,][,90:93] <- country_details[217,][,90:93]
country_details[88,][,97:104] <- country_details[217,][,97:104]
country_details[88,][,112:114] <- country_details[217,][,112:114]
country_details[88,][,116:118] <- country_details[217,][,116:118]
country_details[88,][,128:133] <- country_details[217,][,128:133]
country_details[88,][,143:148] <- country_details[217,][,143:148]
country_details[88,][,150] <- country_details[217,][,150]
country_details[88,][,152:157] <- country_details[217,][,152:157]
country_details[88,][,159:175] <- country_details[217,][,159:175]
country_details <- country_details[-217, ]

####
#
# Merge country details and preprocessed_dataset
#
####

preprocessed_dataset <- left_join(preprocessed_dataset, country_details,
                                  by = c('locality_name' = 'region_name'))

rm(country_details)

# Remove rows that are related to regions
preprocessed_dataset %>%
  filter(is.na(region_name)) %>%
  select(-c('region_name', 'county_name')) -> preprocessed_dataset

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
  group_by(locality_name) %>%
  arrange(date) %>%
  mutate(lethality_rate_percent = (acc_deaths[n()]/acc_cases[n()])*100) %>%
  ungroup() -> preprocessed_dataset

# Bring plot_names from row to column
preprocessed_dataset %>%
  pivot_wider(names_from = plot_name, values_from = variation) -> preprocessed_dataset

# Add n_days_since_1st_case column
preprocessed_dataset %>%
  group_by(locality_name) %>%
  mutate(first_case_date = min(date[acc_cases > 0])) %>%
  mutate(n_days_since_1st_case =
           if_else(acc_cases > 0,
                   as.numeric(date - min(date[acc_cases > 0])),
                   0)) %>%
  ungroup() -> preprocessed_dataset

# Add n_days_since_1st_death column
preprocessed_dataset %>%
  group_by(locality_name) %>%
  mutate(first_death_date = min(date[acc_deaths > 0])) %>%
  mutate(n_days_since_1st_death =
           if_else(acc_deaths > 0,
                   as.numeric(date - min(date[acc_deaths > 0])),
                   0)) %>%
  ungroup() -> preprocessed_dataset
# There is a warning here, because 15 countries don't have any date with
# deaths. So the minimum of nothing will throw a warning.
# preprocessed_dataset %>%
#   group_by(locality_name) %>%
#   arrange(date) %>%
#   filter(date == '2020-04-17') %>%
#   filter(acc_deaths == 0) %>%
#   select(locality_name) %>%
#   unique

# Set manually first case for countries whose first case happened before Feb 15
# Wikipedia contributors, "2019–20 coronavirus pandemic", Wikipedia, The Free
# Encyclopedia,
# https://en.wikipedia.org/w/index.php?title=2019%E2%80%9320_coronavirus_pandemic&oldid=952187370
# (accessed April 21, 2020).
preprocessed_dataset %>%
  mutate(first_case_date = case_when(
    # locality_name == 'China' ~ dmy('01-12-2019'),
    locality_name == 'Thailand' ~ dmy('13-01-2020'),
    locality_name == 'Japan' ~ dmy('16-01-2020'),
    locality_name == 'South Korea' ~ dmy('20-01-2020'),
    locality_name == 'United States' ~ dmy('20-01-2020'),
    locality_name == 'Taiwan' ~ dmy('21-01-2020'),
    locality_name == 'Hong Kong' ~ dmy('22-01-2020'),
    locality_name == 'Singapore' ~ dmy('23-01-2020'),
    locality_name == 'Vietnam' ~ dmy('23-01-2020'),
    locality_name == 'France' ~ dmy('24-01-2020'),
    locality_name == 'Nepal' ~ dmy('24-01-2020'),
    locality_name == 'Australia' ~ dmy('25-01-2020'),
    locality_name == 'Canada' ~ dmy('25-01-2020'),
    locality_name == 'Malaysia' ~ dmy('25-01-2020'),
    locality_name == 'Cambodia' ~ dmy('27-01-2020'),
    locality_name == 'Germany' ~ dmy('27-01-2020'),
    locality_name == 'Sri Lanka' ~ dmy('27-01-2020'),
    locality_name == 'Finalnd' ~ dmy('29-01-2020'),
    locality_name == 'United Arab Emirates' ~ dmy('29-01-2020'),
    locality_name == 'India' ~ dmy('30-01-2020'),
    locality_name == 'Italy' ~ dmy('30-01-2020'),
    locality_name == 'Philippines' ~ dmy('30-01-2020'),
    locality_name == 'Spain' ~ dmy('31-01-2020'),
    locality_name == 'Sweden' ~ dmy('31-01-2020'),
    locality_name == 'United Kingdom' ~ dmy('31-01-2020'),
    locality_name == 'Belgium' ~ dmy('04-02-2020'),
    locality_name == 'Egypt' ~ dmy('14-01-2020'),
    TRUE ~ first_case_date
    )
  ) -> preprocessed_dataset

# Fix n_days since 1st case for countries that had 1st case before Feb 11
countries <- c('Thailand', 'Japan', 'South Korea', 'United States', 'Taiwan',
               'Hong Kong', 'Singapore', 'Vietnam', 'France', 'Nepal',
               'Australia', 'Canada', 'Malaysia', 'Cambodia', 'Germany',
               'Sri Lanka', 'Finland', 'United Arab Emirates', 'India',
               'Italy', 'Philippines', 'Spain', 'Sweden', 'United Kingdom',
               'Belgium', 'Egypt')
preprocessed_dataset %>%
  group_by(locality_name) %>%
  mutate(n_days_since_1st_case =
           if_else(locality_name %in% countries,
                   as.numeric(date - first_case_date)+1,
                   n_days_since_1st_case)) %>%
  ungroup() -> preprocessed_dataset
rm(countries)

# Check documentation and see if variable names are correct ###
# According to the file below, maternal mortality rate is per 100k livebirths,
# not 100k people.
# SYB62_246_201907_Population growth and indicators of fertility and mortality.pdf
id = which(colnames(preprocessed_dataset) ==
             'maternal_mortality_ratio_(deaths_per_100000_population)_2015')
colnames(preprocessed_dataset)[id] <- paste0('maternal_mortality_ratio_(deaths',
                                             '_per_100000_livebirths)_2015')

# According to the file below, it's participation rate.
# SYB62_329_201904_Labour Force and Unemployment.pdf
id = which(colnames(preprocessed_dataset) == paste0('labour_force_participatio',
                                                    'n_total_2019'))
colnames(preprocessed_dataset)[id] <- paste0('labour_force_participation_rate_',
                                             'total_2019')
id = which(colnames(preprocessed_dataset) == paste0('labour_force_participatio',
                                                    'n_female_2019'))
colnames(preprocessed_dataset)[id] <- paste0('labour_force_participation_rate_',
                                             'female_2019')
id = which(colnames(preprocessed_dataset) == paste0('labour_force_participatio',
                                                    'n_male_2019'))
colnames(preprocessed_dataset)[id] <- paste0('labour_force_participation_rate_',
                                             'male_2019')


# Saving final preprocessed dataset ---------------------------------------

# Save full dataset
write_tsv(x = preprocessed_dataset, path = 'data/preprocessed/DIB_dataset.tsv', quote_escape = FALSE)
