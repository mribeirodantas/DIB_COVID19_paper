library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

repro <- FALSE

# Checking Google Community Mobility Reports data. ------------------------

read_csv(file = 'data/raw/Global_Mobility_Report.csv',
         col_types = cols_only(country_region_code = 'c',
                               country_region= 'c',
                               sub_region_1 = 'c',
                               sub_region_2 = 'c',
                               date = 'D',
                               retail_and_recreation_percent_change_from_baseline = 'd',
                               grocery_and_pharmacy_percent_change_from_baseline = 'd',
                               parks_percent_change_from_baseline = 'd',
                               transit_stations_percent_change_from_baseline = 'd',
                               workplaces_percent_change_from_baseline = 'd',
                               residential_percent_change_from_baseline = 'd')) %>%
  select(date) %>%
  pull %>%
  max -> last_date

GMR_url <- paste0('https://www.gstatic.com/covid19/mobility/',
                  'Global_Mobility_Report.csv')
latest_GMR_dataset <- read_csv(file = GMR_url,
                               col_types = cols_only(country_region_code = 'c',
                                                     country_region= 'c',
                                                     sub_region_1 = 'c',
                                                     sub_region_2 = 'c',
                                                     metro_area = 'c',
                                                     date = 'D',
                                                     retail_and_recreation_percent_change_from_baseline = 'd',
                                                     grocery_and_pharmacy_percent_change_from_baseline = 'd',
                                                     parks_percent_change_from_baseline = 'd',
                                                     transit_stations_percent_change_from_baseline = 'd',
                                                     workplaces_percent_change_from_baseline = 'd',
                                                     residential_percent_change_from_baseline = 'd'))

if (max(latest_GMR_dataset$date) > last_date) {
  print(paste0('There is more data available from Google Community Mobility ',
               'Reports dataset. Starting to update...'))
  write.table(latest_GMR_dataset, 'data/raw/Global_Mobility_Report.csv',
              row.names = FALSE, quote = TRUE, sep=',', na = '')
  print('Google Community Mobility Reports raw dataset updated.')
  repro <- TRUE
} else {
  print('Google Community Mobility Reports dataset is up to date.')
}


# Checking ECDC data ------------------------------------------------------

read_delim(file = 'data/raw/COVID19_worldwide_raw.csv', na = '',
           col_types = cols_only(dateRep = 'c', day = 'i', month = 'i',
                                 year = 'i', cases = 'i', deaths = 'i',
                                 countriesAndTerritories = 'c', geoId = 'c',
                                 countryterritoryCode = 'c',
                                 popData2018 = 'i', continentExp = 'c'),
           delim = ',') %>%
  select(dateRep) %>%
  pull %>%
  dmy(.) %>%
  max -> last_date

ECDC_url <- 'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'
latest_ECDC_dataset <- read_delim(file = ECDC_url, na = '',
                                  col_types = cols_only(dateRep = 'c', day = 'i', month = 'i',
                                                        year = 'i', cases = 'i', deaths = 'i',
                                                        countriesAndTerritories = 'c', geoId = 'c',
                                                        countryterritoryCode = 'c',
                                                        # The fact it's 2019 now does not matter,
                                                        # after all, we do not use this variable
                                                        popData2019 = 'i', continentExp = 'c'),
                                  delim = ',')

if (max(dmy(latest_ECDC_dataset$dateRep)) > last_date) {
  print(paste0('There is more data available from the European Centre for ',
               'Disease Prevention and Control COVID-19 dataset. Starting to ',
               'update...'))
  write.table(latest_ECDC_dataset, 'data/raw/COVID19_worldwide_raw.csv',
              row.names = FALSE, quote = TRUE, sep=',', na = '')
  print('ECDC COVID-19 raw dataset updated.')
  repro <- TRUE
} else {
  print('ECDC COVID-19 dataset is up to date.')
}

# Checking JHU data ----------------------------------------------------

read_delim(file = 'data/raw/hk-reunion-covid-19.csv', na = '',
           col_types = cols_only(locality_name = 'c', date = 'c',
                                 new_cases = 'd', new_deaths = 'd'),
           delim = ',') %>%
  select(date) %>%
  pull %>%
  max -> last_date

# Get datasets and join
cases_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-1',
                    '9/master/csse_covid_19_data/csse_covid_19_time_series/tim',
                    'e_series_covid19_confirmed_global.csv')
deaths_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-1',
                     '9/master/csse_covid_19_data/csse_covid_19_time_series/ti',
                     'me_series_covid19_deaths_global.csv')

latest_JHU_dataset <- read_delim(file = cases_url, na = '',
                                 col_types = cols(.default = 'd',
                                                  `Province/State` = 'c',
                                                  `Country/Region` = 'c'),
                                  delim = ',') %>%
  select(-c('Lat', 'Long', 'Country/Region')) %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = 'date',
               values_to = 'new_cases') %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` %in% c('Hong Kong', 'Reunion'))

deaths_dataset <- read_delim(file = deaths_url, na = '',
                             col_types = cols(.default = 'd',
                                              `Province/State` = 'c',
                                              `Country/Region` = 'c'),
                             delim = ',') %>%
  select(-c('Lat', 'Long', 'Country/Region')) %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = 'date',
               values_to = 'new_deaths') %>%
  mutate(date = mdy(date)) %>%
  filter(`Province/State` %in% c('Hong Kong', 'Reunion'))

latest_JHU_dataset <- left_join(latest_JHU_dataset, deaths_dataset,
                                  by = c('date', 'Province/State'))

latest_JHU_dataset <- latest_JHU_dataset %>%
  group_by(`Province/State`) %>%
  arrange(date) %>%
  mutate(new_cases = new_cases - lag(new_cases, default = first(new_cases))) %>%
  mutate(new_deaths = new_deaths - lag(new_deaths, default = first(new_deaths)))

colnames(latest_JHU_dataset)[1] <- 'locality_name'
latest_JHU_dataset$date %>%
  format('%m/%d/%y') %>%
  mdy(.) -> latest_JHU_dataset$date

# Check if it's new

if (max(latest_JHU_dataset$date) > last_date) {
  print(paste0('There is more data available from the John Hopkins University ',
               'dataset. Starting to update...'))
  write.table(latest_JHU_dataset, 'data/raw/hk-reunion-covid-19.csv',
              row.names = FALSE, quote = FALSE, sep=',', na = '')
  print('JHU COVID-19 raw dataset updated.')
  repro <- TRUE
} else {
  print('JHU COVID-19 dataset is up to date.')
}

# Update remote / git repo ------------------------------------------------

#if (repro == TRUE) {
#  system('dvc repro preprocess.dvc')
#  system(paste0('git add data/raw/COVID19_worldwide_raw.csv.dvc preprocess.dvc',
#                ' data/raw/hk-reunion-covid-19.csv.dvc data/raw/Global_Mobilit',
#                'y_Report.csv.dvc'))
#  commit_msg <- paste0('Updates raw datasets \'', today(), '\'')
#  system(paste0('git commit -m \"', commit_msg, '\"'))
#  system('git push')
#} else {
#  print('Everything is up to date. Nothing else to do.')
#}
##
