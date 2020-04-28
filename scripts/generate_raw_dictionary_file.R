library(readr)
library(dplyr)
library(tidyr)
library(WriteXLS)

# Read all raw datasets from UN into one dataframe ---------

df <- list.files(path='data/raw/UN Data/', full.names = TRUE) %>%
  lapply(read_delim, delim = ',', skip = 1) %>%
  bind_rows

# Keep the variables required to create the data dictionary

df %>%
  select(Series, Year, Source) -> df

# Keep only latest data for each variable
df %>%
  group_by(Series) %>%
  filter(Year == max(Year)) %>%
  ungroup() -> df

# Variables are repeated for they occur for every country. Let's remove the
# duplicates.
df %>%
  distinct(Series, .keep_all = TRUE) -> df

# Fix names according to preprocessed dataset
df %>%
  # Make them all lowercase
  mutate(Series = tolower(Series)) %>%
  # Replace special chars
  mutate(Series = gsub(' ', '_', Series)) %>%
  mutate(Series = gsub('-', '_', Series)) %>%
  mutate(Series = gsub('_+', '_', Series)) %>%
  mutate(Series = gsub(',', '', Series)) -> df

# Add _year to the end of variable name, just like in the preprocessed file
df %>%
  unite("Series", Series:Year, remove=FALSE) -> df

# Add engineering UN variables
df <- rbind(df, c('whos_major_trade_partner_exp_1', 2018,
                  paste0('United Nations Statistics Division, New York, ',
                         'Commodity Trade Statistics Database (UN COMTRADE), ',
                         'last accessed May 2019.')))

# Add COVID-19 and engineering columns
df <- rbind(df,
            c('country_code', '2020', paste0('European Centre for Disease ',
                                             'Prevention and Control. Last ',
                                             'accessed 28 April, 2020.')),
            c('country_name', '2020', paste0('European Centre for Disease ',
                                             'Prevention and Control. Last ',
                                             'accessed 28 April, 2020.')),
            c('date', '2020', paste0('European Centre for Disease ',
                                     'Prevention and Control. Last ',
                                     'accessed 28 April, 2020.')),
            c('new_cases', '2020', paste0('European Centre for Disease ',
                                          'Prevention and Control. Last ',
                                          'accessed 28 April, 2020.')),
            c('new_deaths', '2020',  paste0('European Centre for Disease ',
                                            'Prevention and Control. Last ',
                                            'accessed 28 April, 2020.')),
            c('pop_data_2018', '2018', paste0('European Centre for Disease ',
                                              'Prevention and Control ',
                                              'collected from World Bank. ',
                                              'Last accessed 28 April, 2020.')),
            c('acc_cases', '2020', paste0('Engineered based on data from ',
                                          'European Centre for Disease ',
                                          'Prevention and Control. Last ',
                                          'accessed 28 April, 2020.')),
            c('acc_deaths', '2020', paste0('Engineered based on data from ',
                                           'European Centre for Disease ',
                                           'Prevention and Control. Last ',
                                           'accessed 28 April, 2020.')),
            c('lethality_rate_percent', '2020', paste0('Engineered based on data from ',
                                                       'European Centre for Disease ',
                                                       'Prevention and Control. Last ',
                                                       'accessed April, 2020.')),
            c('retail_recreation', '2020', paste0('Google Community Mobility ',
                                                  'Report. Last accessed 28 ',
                                                  'April, 2020')),
            c('grocery_pharmacy', '2020', paste0('Google Community Mobility ',
                                                 'Report. Last accessed 28 ',
                                                 'April, 2020')),
            c('parks', '2020', paste0('Google Community Mobility ',
                                      'Report. Last accessed 28 ',
                                      'April, 2020')),
            c('transit_stations', '2020', paste0('Google Community Mobility ',
                                                 'Report. Last accessed 28 ',
                                                 'April, 2020')),
            c('workplaces', '2020', paste0('Google Community Mobility ',
                                           'Report. Last accessed 28 ',
                                           'April, 2020')),
            c('residential', '2020', paste0('Google Community Mobility ',
                                            'Report. Last accessed 28 ',
                                            'April, 2020')),
            c('first_case_date', '2020',
              paste0('https://en.wikipedia.org/w/index.php?title=2019%E2%80%93',
                     '20_coronavirus_pandemic_by_country_and_territory&oldid=9',
                     '53662872 Last accessed April 28, 2020')),
            c('n_days_since_1st_case', '2020',
              paste0('Engineering from ECDC and https://en.wikipedia.org/w/index.php?title=2019%E2%80%93',
                     '20_coronavirus_pandemic_by_country_and_territory&oldid=9',
                     '53662872 Last accessed April 28, 2020')),
            c('first_death_date', '2020', paste0('Engineering based on ',
                                                 'data from ECDC ',
                                                 'counting the first death ',
                                                 'after February, 15th.')),
            c('n_days_since_1st_death', '2020', paste0('Engineering based on ',
                                                       'data from ECDC ',
                                                       'counting the first death ',
                                                       'after February, 15th.'))
            )
colnames(df) <- c('Variable name', 'Year', 'Source')

# Add description for some variables
df$Description <- NULL



WriteXLS(x = df, ExcelFileName = 'data_dictionary.xls',
         SheetNames = 'Data Dictionary')
